# Required inputs----

#...........................................................
# HTN Targets Aim 2----
#...........................................................

# Pure helpers are defined before the execution guard so the QA script can
# source them without running the full intervention-input pipeline.

aim2_model_location_map <- c(
  "Brunei"                         = "Brunei Darussalam",
  "Cape Verde"                     = "Cabo Verde",
  "Cote d'Ivoire"                  = "Ivory Coast",
  "Czech Republic"                 = "Czechia",
  "Federated States of Micronesia" = "Micronesia (Federated States of)",
  "Iran"                           = "Iran (Islamic Republic of)",
  "Laos"                           = "Lao People's Democratic Republic",
  "Macedonia"                      = "North Macedonia",
  "Moldova"                        = "Republic of Moldova",
  "South Korea"                    = "Republic of Korea",
  "Swaziland"                      = "Eswatini",
  "Syria"                          = "Syrian Arab Republic",
  "The Bahamas"                    = "Bahamas",
  "The Gambia"                     = "Gambia",
  "Venezuela"                      = "Venezuela (Bolivarian Republic of)",
  "Vietnam"                        = "Viet Nam",
  "North Korea"                    = "Democratic People's Republic of Korea"
)

aim2_recode_locations <- function(x) {
  mapped <- unname(aim2_model_location_map[x])
  fifelse(is.na(mapped), x, mapped)
}

aim2_decompose_control <- function(observed_control,
                                   diabetes_share_among_htn_assumed,
                                   requested_gap = 0.15,
                                   tolerance = 1e-12) {
  if (anyNA(observed_control) || anyNA(diabetes_share_among_htn_assumed)) {
    stop("Missing observed control or assumed diabetes share in Aim 2 strata.")
  }
  if (any(observed_control < 0 | observed_control > 1) ||
      any(diabetes_share_among_htn_assumed < 0 |
          diabetes_share_among_htn_assumed > 1)) {
    stop("Observed control and assumed diabetes shares must lie in [0, 1].")
  }

  w_d <- diabetes_share_among_htn_assumed
  max_gap_lower <- fifelse(w_d > 0, observed_control / w_d, Inf)
  max_gap_upper <- fifelse(w_d < 1, (1 - observed_control) / (1 - w_d), Inf)
  gap_used <- pmin(requested_gap, max_gap_lower, max_gap_upper)

  c_n <- observed_control - w_d * gap_used
  c_d <- observed_control + (1 - w_d) * gap_used
  if (any(c_n < -tolerance | c_n > 1 + tolerance |
          c_d < -tolerance | c_d > 1 + tolerance)) {
    stop("Bounded-gap decomposition produced a control rate outside [0, 1].")
  }

  # Values within floating-point tolerance are exact boundary solutions, not
  # silent clamping of an infeasible 15-percentage-point decomposition.
  c_n[abs(c_n) <= tolerance] <- 0
  c_d[abs(c_d - 1) <= tolerance] <- 1
  list(control_gap_used = gap_used,
       baseline_control_no_diabetes = c_n,
       baseline_control_diabetes = c_d)
}

aim2_solve_no_diabetes_scale <- function(htn_population_no_diabetes,
                                         baseline_control_no_diabetes,
                                         required_additional,
                                         tolerance_people = 1) {
  if (required_additional < -tolerance_people) {
    stop("Diabetes scale-up exceeds the full Aim 2 target; no non-diabetes residual remains.")
  }
  if (required_additional <= tolerance_people) {
    return(list(scale_factor = 1,
                target_control = baseline_control_no_diabetes,
                achieved_additional = 0,
                number_capped_strata = 0L))
  }

  scalable <- baseline_control_no_diabetes > 0 & htn_population_no_diabetes > 0
  capacity <- sum(htn_population_no_diabetes[scalable] *
                    (1 - baseline_control_no_diabetes[scalable]))
  if (required_additional > capacity + tolerance_people) {
    stop(sprintf(
      paste0("Non-diabetes proportional scale-up capacity is insufficient: ",
             "required %.3f million, available %.3f million."),
      required_additional / 1e6, capacity / 1e6
    ))
  }

  upper <- max(1 / baseline_control_no_diabetes[scalable])
  objective <- function(scale_factor) {
    sum(htn_population_no_diabetes *
          (pmin(1, scale_factor * baseline_control_no_diabetes) -
             baseline_control_no_diabetes)) - required_additional
  }
  if (objective(1) > tolerance_people || objective(upper) < -tolerance_people) {
    stop("Could not bracket the non-diabetes proportional scale-factor root.")
  }

  solved <- uniroot(objective, interval = c(1, upper),
                    tol = .Machine$double.eps^0.5)
  target <- pmin(1, solved$root * baseline_control_no_diabetes)
  achieved <- sum(htn_population_no_diabetes *
                    (target - baseline_control_no_diabetes))
  if (abs(achieved - required_additional) > tolerance_people) {
    stop(sprintf("Non-diabetes root reconciliation differs by %.6f people.",
                 achieved - required_additional))
  }

  list(scale_factor = solved$root,
       target_control = target,
       achieved_additional = achieved,
       number_capped_strata = sum(target >= 1 - 1e-12))
}

# Joint solver for allocation_mode = "diabetes_capped_to_target". A single shared
# multiplicative scale factor s raises baseline control in BOTH subgroups, capped
# at cap_diabetes (diabetes) and cap_no_diabetes (no diabetes), so that the total
# additional controlled equals the policy target EXACTLY by construction
# (Delta_D + Delta_N = policy_target). The diabetes cap (0.80) is an aspirational
# ceiling, not a hard floor; the achieved diabetes control rate may be < 0.80.
aim2_solve_joint_scale <- function(htn_population_diabetes,
                                   baseline_control_diabetes,
                                   htn_population_no_diabetes,
                                   baseline_control_no_diabetes,
                                   policy_target = 150e6,
                                   cap_diabetes = 0.80,
                                   cap_no_diabetes = 1,
                                   tolerance_people = 1) {
  additional_at <- function(s) {
    td <- pmin(cap_diabetes, s * baseline_control_diabetes)
    tn <- pmin(cap_no_diabetes, s * baseline_control_no_diabetes)
    sum(htn_population_diabetes * (td - baseline_control_diabetes)) +
      sum(htn_population_no_diabetes * (tn - baseline_control_no_diabetes))
  }

  # Proportional scaling cannot lift a stratum whose baseline control is 0, so its
  # reachable capacity is 0; count capacity only where baseline control > 0.
  cap_gain_d <- fifelse(baseline_control_diabetes > 0,
                        pmax(cap_diabetes - baseline_control_diabetes, 0), 0)
  cap_gain_n <- fifelse(baseline_control_no_diabetes > 0,
                        pmax(cap_no_diabetes - baseline_control_no_diabetes, 0), 0)
  capacity <- sum(htn_population_diabetes * cap_gain_d) +
    sum(htn_population_no_diabetes * cap_gain_n)
  if (policy_target > capacity + tolerance_people) {
    stop(sprintf(paste0(
      "Joint Aim 2 allocation is infeasible: policy target %.3f million exceeds ",
      "the maximum controllable capacity %.3f million (diabetes capped at %.2f, ",
      "no-diabetes capped at %.2f)."),
      policy_target / 1e6, capacity / 1e6, cap_diabetes, cap_no_diabetes),
      call. = FALSE)
  }
  if (policy_target <= tolerance_people) {
    return(list(scale_factor = 1,
                target_control_diabetes = baseline_control_diabetes,
                target_control_no_diabetes = baseline_control_no_diabetes,
                achieved_additional = 0,
                number_capped_strata = 0L))
  }

  pos_d <- baseline_control_diabetes > 0
  pos_n <- baseline_control_no_diabetes > 0
  upper <- max(c(
    if (any(pos_d)) cap_diabetes / min(baseline_control_diabetes[pos_d]) else 1,
    if (any(pos_n)) cap_no_diabetes / min(baseline_control_no_diabetes[pos_n]) else 1,
    1 + 1e-6
  ))
  objective <- function(s) additional_at(s) - policy_target
  if (objective(1) > tolerance_people || objective(upper) < -tolerance_people) {
    stop("Could not bracket the joint Aim 2 proportional scale-factor root.")
  }

  solved <- uniroot(objective, interval = c(1, upper),
                    tol = .Machine$double.eps^0.5)
  s <- solved$root
  td <- pmin(cap_diabetes, s * baseline_control_diabetes)
  tn <- pmin(cap_no_diabetes, s * baseline_control_no_diabetes)
  achieved <- sum(htn_population_diabetes * (td - baseline_control_diabetes)) +
    sum(htn_population_no_diabetes * (tn - baseline_control_no_diabetes))
  if (abs(achieved - policy_target) > tolerance_people) {
    stop(sprintf("Joint Aim 2 root reconciliation differs by %.6f people.",
                 achieved - policy_target))
  }

  list(scale_factor = s,
       target_control_diabetes = td,
       target_control_no_diabetes = tn,
       achieved_additional = achieved,
       number_capped_strata = sum(td >= cap_diabetes - 1e-12) +
         sum(tn >= cap_no_diabetes - 1e-12))
}

aim2_prepare_strata <- function(wd_data, wd_raw) {
  data_in <- fread(paste0(wd_data, "bp_data6.csv"))
  setnames(data_in, "location_gbd", "location")
  data_in[, location := aim2_recode_locations(location)]
  data_in[, c("Year", "Country") := NULL]

  htn_prev <- fread(paste0(
    wd_raw,
    "NCD-RisC_Lancet_2021_Hypertension_age_specific_estimates_by_country.csv"
  ))
  htn_prev <- htn_prev[
    Year == 2019,
    .(location = Country, sex = Sex, Age.group = Age,
      htn_prevalence = `Prevalence of hypertension`)
  ]
  htn_prev[, sex := fifelse(sex == "Men", "Male", "Female")]
  htn_prev[, location := fcase(
    location == "Viet Nam", "Vietnam",
    location == "United States of America", "United States",
    location == "Lao PDR", "Laos",
    location %in% c("Congo, Dem. Rep.", "DR Congo"),
      "Democratic Republic of the Congo",
    location == "Cabo Verde", "Cape Verde",
    location == "Gambia", "The Gambia",
    location == "Bahamas", "The Bahamas",
    location == "Micronesia (Federated States of)",
      "Federated States of Micronesia",
    location == "Syrian Arab Republic", "Syria",
    location %in% c("Palestine, State of", "Occupied Palestinian Territory"),
      "Palestine",
    location %in% c("North Macedonia", "Macedonia (TFYR)"), "Macedonia",
    location == "Taiwan", "Taiwan (Province of China)",
    location == "Brunei Darussalam", "Brunei",
    location == "Guinea Bissau", "Guinea-Bissau",
    default = location
  )]
  htn_prev[, location := aim2_recode_locations(location)]

  add_age <- function(source_age, target_age) {
    htn_prev[Age.group == source_age,
             .(location, sex, Age.group = target_age, htn_prevalence)]
  }
  htn_prev <- unique(rbindlist(list(
    htn_prev,
    add_age("30-34", "20-24"),
    add_age("30-34", "25-29"),
    add_age("75-79", "80-84"),
    add_age("75-79", "85plus")
  )), by = c("location", "sex", "Age.group"))

  population <- as.data.table(readRDS(paste0(
    wd_data, "PopulationsSingleAge0050.rds"
  )))
  if ("year_id" %in% names(population)) setnames(population, "year_id", "year")
  population <- population[age >= 20]

  # UNWPP 2024 single-year population is the FIXED 2025 denominator. A handful of
  # small Pacific states (Samoa, Tonga, ...) have no 2025 single-year row; for
  # those, fall back to their most recent available year (2023) as a documented
  # approximation of the fixed denominator. This mirrors the statins section
  # below, which already applies a 2023 fallback for Samoa/American Samoa. It is a
  # transparent, recorded fallback (population_fallback_locations), NOT silent
  # imputation, so these locations are retained in the policy universe instead of
  # being dropped and forcing a fatal abort.
  pop_year_used <- population[, .(
    year_used = as.integer(if (any(year == 2025)) 2025L else max(year))
  ), by = location]
  population_fallback_locations <- sort(pop_year_used[year_used != 2025L, location])
  population <- merge(population, pop_year_used, by = "location")
  population <- population[year == year_used]
  population[, Age.group := fifelse(
    age >= 85, "85plus",
    paste0(5L * (age %/% 5L), "-", 5L * (age %/% 5L) + 4L)
  )]
  population <- population[
    , .(population_2025 = sum(Nx)),
    by = .(location, sex, Age.group)
  ]

  # The policy universe follows the explicit exclusions recorded in
  # Scenarios.xlsx Sheet1. The workbook's calculated target columns are not read.
  excluded_locations <- c(
    "Palestine", "Bermuda", "Puerto Rico", "Greenland",
    "Taiwan (Province of China)", "American Samoa"
  )
  fine <- data_in[!location %in% excluded_locations,
                  .(location, sex, Age.group, htncov2, diabetes)]
  fine <- merge(fine, htn_prev,
                by = c("location", "sex", "Age.group"), all.x = TRUE)
  fine <- merge(fine, population,
                by = c("location", "sex", "Age.group"), all.x = TRUE)

  missing <- fine[is.na(htn_prevalence) | is.na(population_2025),
                  .(missing_strata = .N,
                    missing_htn_prevalence = any(is.na(htn_prevalence)),
                    missing_population_2025 = any(is.na(population_2025))),
                  by = location]
  complete <- fine[complete.cases(
    fine[, .(htncov2, diabetes, htn_prevalence, population_2025)]
  )]
  complete[, `:=`(
    htn_population_age = population_2025 * htn_prevalence,
    htn_population_diabetes_age =
      population_2025 * htn_prevalence * diabetes,
    htn_population_no_diabetes_age =
      population_2025 * htn_prevalence * (1 - diabetes),
    controlled_overall_age =
      population_2025 * htn_prevalence * htncov2
  )]

  locsex <- complete[, .(
    population_2025 = sum(population_2025),
    htn_population_2025 = sum(htn_population_age),
    htn_population_diabetes = sum(htn_population_diabetes_age),
    htn_population_no_diabetes = sum(htn_population_no_diabetes_age),
    controlled_overall_2025 = sum(controlled_overall_age)
  ), by = .(location, sex)]
  locsex[, `:=`(
    diabetes_share_among_htn_assumed =
      htn_population_diabetes / htn_population_2025,
    observed_overall_control =
      controlled_overall_2025 / htn_population_2025
  )]

  decomposition <- aim2_decompose_control(
    locsex$observed_overall_control,
    locsex$diabetes_share_among_htn_assumed
  )
  locsex[, `:=`(
    control_gap_requested = 0.15,
    control_gap_used = decomposition$control_gap_used,
    baseline_control_no_diabetes =
      decomposition$baseline_control_no_diabetes,
    baseline_control_diabetes = decomposition$baseline_control_diabetes
  )]

  list(strata = locsex,
       missing = missing,
       excluded_locations = excluded_locations,
       population_fallback_locations = population_fallback_locations,
       data_in = data_in)
}

# allocation_mode controls how the 150-million target is split between the
# diabetes and non-diabetes subgroups when the 0.80 diabetes ceiling alone would
# already exceed 150M (which it does with the real 2025 inputs: Delta_D = 242.7M):
#   "diabetes_capped_to_target"    (DEFAULT) - 0.80 is an aspirational CEILING.
#       One shared solver raises both subgroups (diabetes capped at 0.80,
#       no-diabetes capped at 1.0) so Delta_D + Delta_N = 150M EXACTLY. The
#       achieved diabetes control rate may be < 0.80.
#   "diabetes_floor_then_reconcile" - 0.80 is a HARD FLOOR. If Delta_D > 150M the
#       function stop()s with a clear diagnostic (matches the spec's literal
#       "Fail clearly if Delta_D > 150 million"). Usable only when Delta_D <= 150M.
#   "diabetes_floor_uncapped_total" - 0.80 is a hard floor and the total is
#       allowed to exceed 150M (150M becomes a floor, not an exact target).
# Spec tension: aim2_bp_control_refactor_prompt.md text ("Fail clearly if
# Delta_D > 150 million") reads like mode 2, but Part A3 requires script 04 to
# run end-to-end and reconcile to 150M, which only mode 1 satisfies with real
# data. Default is therefore mode 1; confirm this is the intended policy.
aim2_build_target_tables <- function(strata,
                                     missing = data.table(),
                                     excluded_locations = character(),
                                     population_fallback_locations = character(),
                                     policy_target = 150e6,
                                     diabetes_target = 0.80,
                                     allocation_mode = c("diabetes_capped_to_target",
                                                          "diabetes_floor_then_reconcile",
                                                          "diabetes_floor_uncapped_total"),
                                     tolerance_people = 1) {
  allocation_mode <- match.arg(allocation_mode)
  x <- copy(strata)
  required <- c(
    "location", "sex", "population_2025", "htn_population_2025",
    "htn_population_diabetes", "htn_population_no_diabetes",
    "observed_overall_control", "diabetes_share_among_htn_assumed",
    "baseline_control_no_diabetes", "baseline_control_diabetes"
  )
  absent <- setdiff(required, names(x))
  if (length(absent)) {
    stop("Aim 2 stratum table is missing: ", paste(absent, collapse = ", "))
  }
  if (anyDuplicated(x, by = c("location", "sex"))) {
    stop("Aim 2 stratum table has duplicate location-sex keys.")
  }

  partition_error <- x$htn_population_no_diabetes +
    x$htn_population_diabetes - x$htn_population_2025
  reconstruction_error <-
    x$htn_population_no_diabetes * x$baseline_control_no_diabetes +
    x$htn_population_diabetes * x$baseline_control_diabetes -
    x$htn_population_2025 * x$observed_overall_control
  if (max(abs(partition_error)) > tolerance_people ||
      max(abs(reconstruction_error)) > tolerance_people) {
    stop("Hypertension partition or baseline-control reconstruction failed.")
  }

  # Locations with genuinely unavailable 2025 inputs were dropped upstream
  # (complete.cases in aim2_prepare_strata) and are recorded in `missing`. Their
  # exclusion is documented in the audit summary below (never silently imputed);
  # it is NOT a fatal error. A fatal stop() is raised only if the remaining
  # policy universe cannot reach the target (handled inside the solvers).

  if (allocation_mode == "diabetes_capped_to_target") {
    # Mode 1 (DEFAULT): joint shared-scale solver, diabetes capped at 0.80.
    joint <- aim2_solve_joint_scale(
      htn_population_diabetes      = x$htn_population_diabetes,
      baseline_control_diabetes    = x$baseline_control_diabetes,
      htn_population_no_diabetes    = x$htn_population_no_diabetes,
      baseline_control_no_diabetes  = x$baseline_control_no_diabetes,
      policy_target    = policy_target,
      cap_diabetes     = diabetes_target,
      cap_no_diabetes  = 1,
      tolerance_people = tolerance_people
    )
    x[, target_control_diabetes := joint$target_control_diabetes]
    x[, target_control_no_diabetes := joint$target_control_no_diabetes]
    delta_d <- x[, sum(htn_population_diabetes *
                         (target_control_diabetes - baseline_control_diabetes))]
    delta_n <- x[, sum(htn_population_no_diabetes *
                         (target_control_no_diabetes - baseline_control_no_diabetes))]
    scale_factor <- joint$scale_factor
    number_capped_strata <- joint$number_capped_strata
    allocation_method_diabetes <-
      "joint_shared_proportional_scale_cap_diabetes_0.80"
    allocation_method_no_diabetes <-
      "joint_shared_proportional_scale_cap_1.0"

  } else if (allocation_mode == "diabetes_floor_then_reconcile") {
    # Mode 2: 0.80 as a hard floor; fail clearly if it alone exceeds 150M.
    x[, target_control_diabetes := pmax(baseline_control_diabetes,
                                        diabetes_target)]
    delta_d <- x[, sum(htn_population_diabetes *
                         (target_control_diabetes - baseline_control_diabetes))]
    if (delta_d > policy_target + tolerance_people) {
      stop(sprintf(paste0(
        "Aim 2 allocation_mode='diabetes_floor_then_reconcile' is infeasible: ",
        "the 0.80 diabetes floor alone adds %.3f million controlled people, ",
        "exceeding the %.3f million policy target by %.3f million (implied ",
        "minimum total %.3f million). Use allocation_mode='diabetes_capped_to_",
        "target' to reconcile to %.0f million, or 'diabetes_floor_uncapped_",
        "total' to let the total exceed it."),
        delta_d / 1e6, policy_target / 1e6, (delta_d - policy_target) / 1e6,
        delta_d / 1e6, policy_target / 1e6), call. = FALSE)
    }
    solved <- aim2_solve_no_diabetes_scale(
      x$htn_population_no_diabetes, x$baseline_control_no_diabetes,
      policy_target - delta_d, tolerance_people
    )
    x[, target_control_no_diabetes := solved$target_control]
    delta_n <- solved$achieved_additional
    scale_factor <- solved$scale_factor
    number_capped_strata <- solved$number_capped_strata
    allocation_method_diabetes <- "diabetes_floor_0.80_no_reduction"
    allocation_method_no_diabetes <- "residual_proportional_scale_with_cap"

  } else {
    # Mode 3: 0.80 hard floor, total allowed to exceed 150M (150M is a floor).
    x[, target_control_diabetes := pmax(baseline_control_diabetes,
                                        diabetes_target)]
    delta_d <- x[, sum(htn_population_diabetes *
                         (target_control_diabetes - baseline_control_diabetes))]
    residual_n <- policy_target - delta_d
    if (residual_n > tolerance_people) {
      solved <- aim2_solve_no_diabetes_scale(
        x$htn_population_no_diabetes, x$baseline_control_no_diabetes,
        residual_n, tolerance_people
      )
      x[, target_control_no_diabetes := solved$target_control]
      delta_n <- solved$achieved_additional
      scale_factor <- solved$scale_factor
      number_capped_strata <- solved$number_capped_strata
    } else {
      x[, target_control_no_diabetes := baseline_control_no_diabetes]
      delta_n <- 0
      scale_factor <- 1
      number_capped_strata <- 0L
    }
    allocation_method_diabetes <- "diabetes_floor_0.80_no_reduction"
    allocation_method_no_diabetes <-
      "residual_proportional_scale_uncapped_total"
  }

  # Achieved (population-weighted) diabetes control rate in 2030 (may be < 0.80).
  diabetes_control_2030_effective <-
    x[, sum(htn_population_diabetes * target_control_diabetes) /
        sum(htn_population_diabetes)]

  scenario_ids <- c(
    "baseline", "bp_no_diabetes_only", "bp_diabetes_only", "bp_combined"
  )
  targets <- rbindlist(lapply(scenario_ids, function(id) {
    no_diabetes_target <- if (id %in% c("bp_no_diabetes_only", "bp_combined")) {
      x$target_control_no_diabetes
    } else x$baseline_control_no_diabetes
    diabetes_target_rate <- if (id %in% c("bp_diabetes_only", "bp_combined")) {
      x$target_control_diabetes
    } else x$baseline_control_diabetes

    rbind(
      x[, .(
        scenario_id = id, location, sex,
        subgroup = "htn_no_diabetes",
        baseline_year = 2025L, scaleup_start_year = 2026L,
        target_year = 2030L, population_2025,
        htn_population_2025 = htn_population_no_diabetes,
        diabetes_share_among_htn_assumed,
        baseline_control = baseline_control_no_diabetes,
        target_control = no_diabetes_target,
        control_gap_requested, control_gap_used,
        allocation_method = fifelse(
          id %in% c("bp_no_diabetes_only", "bp_combined"),
          allocation_method_no_diabetes, "inactive_at_baseline"
        )
      )],
      x[, .(
        scenario_id = id, location, sex,
        subgroup = "htn_diabetes",
        baseline_year = 2025L, scaleup_start_year = 2026L,
        target_year = 2030L, population_2025,
        htn_population_2025 = htn_population_diabetes,
        diabetes_share_among_htn_assumed,
        baseline_control = baseline_control_diabetes,
        target_control = diabetes_target_rate,
        control_gap_requested, control_gap_used,
        allocation_method = fifelse(
          id %in% c("bp_diabetes_only", "bp_combined"),
          allocation_method_diabetes, "inactive_at_baseline"
        )
      )]
    )
  }))
  targets[, `:=`(
    annual_control_increment = (target_control - baseline_control) /
      (target_year - baseline_year),
    controlled_2025 = htn_population_2025 * baseline_control,
    controlled_2030 = htn_population_2025 * target_control
  )]
  targets[, additional_controlled_2030 := controlled_2030 - controlled_2025]
  setcolorder(targets, c(
    "scenario_id", "location", "sex", "subgroup", "baseline_year",
    "scaleup_start_year", "target_year", "population_2025",
    "htn_population_2025", "diabetes_share_among_htn_assumed",
    "baseline_control", "target_control", "annual_control_increment",
    "controlled_2025", "controlled_2030", "additional_controlled_2030",
    "allocation_method", "control_gap_requested", "control_gap_used"
  ))

  scenario_summary <- targets[, .(
    achieved_total = sum(additional_controlled_2030),
    additional_controlled_no_diabetes =
      sum(additional_controlled_2030[subgroup == "htn_no_diabetes"]),
    additional_controlled_diabetes =
      sum(additional_controlled_2030[subgroup == "htn_diabetes"])
  ), by = scenario_id]
  # For modes 1 and 2 the combined additional controlled equals policy_target
  # exactly; for mode 3 it may exceed it, so the combined scenario_target is the
  # achieved total and 150M is enforced only as a floor below.
  combined_target <- if (allocation_mode == "diabetes_floor_uncapped_total") {
    delta_d + delta_n
  } else {
    policy_target
  }
  scenario_summary[, scenario_target := fcase(
    scenario_id == "baseline", 0,
    scenario_id == "bp_no_diabetes_only", delta_n,
    scenario_id == "bp_diabetes_only", delta_d,
    scenario_id == "bp_combined", combined_target
  )]
  scenario_summary[, `:=`(
    reconciliation_difference = achieved_total - scenario_target,
    allocation_mode = allocation_mode,
    diabetes_target = diabetes_target,
    diabetes_control_2030_effective = diabetes_control_2030_effective,
    solved_non_diabetes_scale_factor = scale_factor,
    number_capped_strata = number_capped_strata,
    number_bounded_gap_strata = sum(x$control_gap_used < 0.15 - 1e-12),
    number_population_fallback_locations = length(population_fallback_locations),
    population_fallback_locations =
      paste(population_fallback_locations, collapse = " | "),
    number_missing_or_excluded_locations =
      uniqueN(c(missing$location, excluded_locations)),
    missing_or_excluded_locations =
      paste(sort(unique(c(missing$location, excluded_locations))), collapse = " | ")
  )]
  setcolorder(scenario_summary, c(
    "scenario_id", "scenario_target", "achieved_total",
    "additional_controlled_no_diabetes", "additional_controlled_diabetes",
    "reconciliation_difference", "allocation_mode", "diabetes_target",
    "diabetes_control_2030_effective", "solved_non_diabetes_scale_factor",
    "number_capped_strata", "number_bounded_gap_strata",
    "number_population_fallback_locations", "population_fallback_locations",
    "number_missing_or_excluded_locations", "missing_or_excluded_locations"
  ))

  combined <- scenario_summary[scenario_id == "bp_combined"]
  if (allocation_mode == "diabetes_floor_uncapped_total") {
    if (combined$achieved_total < policy_target - tolerance_people) {
      stop("Uncapped-total Aim 2 allocation fell below the 150-million floor.")
    }
  } else if (abs(combined$reconciliation_difference) > tolerance_people) {
    stop("Combined Aim 2 target does not reconcile to 150 million.")
  }
  if (anyNA(targets[, .(scenario_id, location, sex, subgroup)]) ||
      anyDuplicated(targets,
                    by = c("scenario_id", "location", "sex", "subgroup"))) {
    stop("Long Aim 2 target table has missing or duplicate keys.")
  }
  if (any(targets$baseline_control < 0 | targets$baseline_control > 1 |
          targets$target_control < 0 | targets$target_control > 1) ||
      any(targets$htn_population_2025 < 0 |
          targets$additional_controlled_2030 < -tolerance_people)) {
    stop("Long Aim 2 target table failed rate or non-negative-count checks.")
  }

  list(targets = targets, summary = scenario_summary, strata = x)
}

# Tests set this option to FALSE so sourcing exposes only the pure helpers.
if (isTRUE(getOption("who_cvd.execute_04", TRUE))) {

aim2_prepared <- aim2_prepare_strata(wd_data, wd_raw)
data.in <- aim2_prepared$data_in

# allocation_mode default = "diabetes_capped_to_target" (see aim2_build_target_tables
# header). Override here (or via getOption below) to switch policy modes.
aim2_allocation_mode <- getOption(
  "who_cvd.aim2_allocation_mode", "diabetes_capped_to_target"
)
aim2_outputs <- aim2_build_target_tables(
  aim2_prepared$strata,
  aim2_prepared$missing,
  aim2_prepared$excluded_locations,
  population_fallback_locations = aim2_prepared$population_fallback_locations,
  allocation_mode = aim2_allocation_mode
)

# Write only after all feasibility and reconciliation checks have passed. This
# prevents a failed run from overwriting the last valid processed target file.
fwrite(aim2_outputs$targets,
       paste0(wd_data, "htn_control_targets_by_loc.csv"))
fwrite(aim2_outputs$summary,
       paste0(wd_data, "htn_control_targets_summary.csv"))

local({
  s <- aim2_outputs$summary[scenario_id == "bp_combined"]
  cat(sprintf(paste0(
    "Aim 2 BP target [%s]: %.6f million additional controlled people using ",
    "fixed 2025 denominators (diabetes %.3fM + no-diabetes %.3fM; achieved ",
    "diabetes control %.3f).\n"),
    s$allocation_mode, s$achieved_total / 1e6,
    s$additional_controlled_diabetes / 1e6,
    s$additional_controlled_no_diabetes / 1e6,
    s$diabetes_control_2030_effective))
  if (length(aim2_prepared$population_fallback_locations)) {
    cat("  Population 2023 fallback (no 2025 UNWPP row):",
        paste(aim2_prepared$population_fallback_locations, collapse = ", "), "\n")
  }
  if (nrow(aim2_prepared$missing)) {
    cat("  Excluded (missing 2025 inputs):",
        paste(aim2_prepared$missing$location, collapse = ", "), "\n")
  }
})


#...........................................................
# STATINS AIM 2: Country-level baseline/target table ----
#...........................................................
#
# Goal: one row per country with:
#   - 2025 baseline statin coverage (from statin_data.rds)
#   - 2030 target coverage (fixed at 80%)
#   - population age 40+ (UNWPP 2024)
#   - diabetes prevalence and count among 40+ (from Scenarios.xlsx or bp_data6 fallback)
#   - primary prevention eligible count (pop without CVD x AF, x prop_athero for istroke)
#   - secondary prevention eligible count (prevalent IHD + atherosclerotic istroke, age 40+)
#   - iso3 code (from covfxn2.csv)
#
# Sources:
#   Pop 40+        : PopulationsSingleAge0050.rds (UNWPP 2024)
#   Diabetes prev  : Scenarios.xlsx Sheet1 A6:O197 (grep for "diab" column);
#                    fallback -> data.in$diabetes from bp_data6.csv (mean over age-sex strata)
#   Statin baseline: data/processed/Statins/statin_data.rds (statins_current column)
#   AF for statins : data/processed/af_statins.rds (location x cause x af_statins)
#   Secondary prev : adjusted*.rds files (PREVt0 x UNWPP Nx, cause = ihd or istroke, age >= 40)
#   Primary prev   : (1 - PREVt0) x AF x Nx; ischemic stroke scaled by prop_athero_stroke
#   ISO3 map       : data/processed/covfxn2.csv


# -- 1. Population age 40+ in 2025, by location ------------------------------
# Source: UNWPP 2024 single-year age (same file used earlier for HTN section)
# Note: pop2095 was removed above; reload here under a new object name

pop_raw_s <- as.data.table(readRDS(paste0(wd_data, "PopulationsSingleAge0050.rds")))
if ("year_id" %in% names(pop_raw_s)) setnames(pop_raw_s, "year_id", "year")

# Samoa / American Samoa: no 2025 data in UNWPP -> fall back to 2023 (matches HTN logic above)
pop_samoa_s <- pop_raw_s[
  location %in% c("Samoa", "American Samoa") & year == 2023 & age >= 40,
  .(pop_40plus_2025 = sum(Nx, na.rm = TRUE)),
  by = .(location)
]

pop40_2025_s <- pop_raw_s[
  age >= 40 & year == 2025,
  .(pop_40plus_2025 = sum(Nx, na.rm = TRUE)),
  by = .(location)
]
pop40_2025_s <- rbind(pop40_2025_s[!location %in% c("Samoa", "American Samoa")], pop_samoa_s)

# Also build 5-year-age x sex population (needed to merge with b_rates strata for prev counts)
# b_rates stores age as the lower bound of each 5-yr bin (40, 45, ..., 95)
pop_samoa_5yr_s <- pop_raw_s[
  location %in% c("Samoa", "American Samoa") & year == 2023 & age >= 40
]
pop_5yr_2025_s <- rbind(
  pop_raw_s[age >= 40 & year == 2025],
  pop_samoa_5yr_s
)
pop_5yr_2025_s[, age5 := pmin(5L * (age %/% 5L), 95L)]  # collapse single years -> 5-yr bins, cap at 95
pop_5yr_2025_s <- pop_5yr_2025_s[, .(Nx_unwpp = sum(Nx, na.rm = TRUE)), by = .(location, sex, age5)]
setnames(pop_5yr_2025_s, "age5", "age")

rm(pop_raw_s, pop_samoa_s, pop_samoa_5yr_s)


# -- 2. ISO3 lookup ----------------------------------------------------------
# Source: covfxn2.csv -- has iso3 and location (GBD naming convention)
# Unmatched locations will simply have NA iso3 in the output
iso3_map_s <- unique(
  fread(paste0(wd_data, "covfxn2.csv"))[, .(location, iso3)],
  by = "location"
)


# -- 3. Diabetes prevalence by location --------------------------------------
# Primary source: Scenarios.xlsx Sheet1 A6:O197 (same file as aim2_loc above)
# We reload with all 15 columns and search for any column whose name contains "diab".
# If none found, fall back to data.in$diabetes (bp_data6.csv, averaged over age-sex strata).
#
# NOTE: inspect the printed column names below to confirm which column is selected.

aim2_loc_full_s <- as.data.table(
  read_excel("C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/Scenarios.xlsx",
             sheet = "Sheet1", range = "A6:O197")
)

cat("=== Columns in Scenarios.xlsx Sheet1 (A6:O197) ===\n")
print(names(aim2_loc_full_s))

diab_cols_s <- grep("diab", names(aim2_loc_full_s), ignore.case = TRUE, value = TRUE)

if (length(diab_cols_s) > 0) {
  # Use the first matching column (e.g. "diabetes_prev", "diab_2025", etc.)
  diab_col_use_s <- diab_cols_s[1]
  cat("Using diabetes prevalence column from Scenarios.xlsx:", diab_col_use_s, "\n")
  aim2_diab_s <- aim2_loc_full_s[
    , .(location, diabetes_prev_2025 = as.numeric(get(diab_col_use_s)))
  ]
} else {
  # Fallback: data.in (bp_data6.csv) has diabetes by location x age x sex.
  # Average over all age-sex strata within each location as an approximation.
  # For a more accurate estimate, weight by age-sex population (not done here).
  warning("No 'diab*' column found in Scenarios.xlsx -- falling back to data.in$diabetes (bp_data6.csv).")
  diab_col_use_s <- "data.in$diabetes (bp_data6.csv)"
  aim2_diab_s <- data.in[
    , .(diabetes_prev_2025 = mean(diabetes, na.rm = TRUE)),
    by = location
  ]
}

rm(aim2_loc_full_s, diab_cols_s)


# -- 4. Baseline statin coverage in 2025 -------------------------------------
# Source: statin_data.rds -- column statins_current holds the 2025 observed coverage
# (constant across all years; equals the baseline pp_cov from FDC_coverage_data_statints_pp.csv)
# NA statins_current means no coverage data for that location -> treated as 0 in the model

# NOTE: statin_data.rds lives at the processed root (as read by both 06 scripts);
# the previous "Statins/" subfolder prefix did not exist here and aborted script 04
# end-to-end. Minimal path correction only (no change to statin logic).
dt_statin_s <- readRDS(file = paste0(wd_data, "statin_data.rds"))

statin_base_s <- unique(
  dt_statin_s[, .(location, statins_baseline_2025 = statins_current)],
  by = "location"
)

rm(dt_statin_s)


# -- 5. Attributable fractions for statins by location x cause ---------------
# Source: af_statins.rds -- columns: location, cause, af_statins
#   cause values used here: "ihd" and "istroke"
# Global defaults (GBD 2021 High Fasting Plasma Glucose AFs) used where data are missing:
#   af_ihd     = 0.1497
#   af_istroke = 0.1161

af_ihd_def_s     <- 0.1497
af_istroke_def_s <- 0.1161

dt_af_s <- readRDS(file = paste0(wd_data, "af_statins.rds"))

# Pivot to wide (one row per location)
dt_af_wide_s <- dcast(
  dt_af_s[cause %in% c("ihd", "istroke")],
  location ~ cause,
  value.var = "af_statins"
)
# Rename so cause levels become column names af_ihd / af_istroke
if ("ihd"     %in% names(dt_af_wide_s)) setnames(dt_af_wide_s, "ihd",     "af_ihd")
if ("istroke" %in% names(dt_af_wide_s)) setnames(dt_af_wide_s, "istroke", "af_istroke")
if (!"af_ihd"     %in% names(dt_af_wide_s)) dt_af_wide_s[, af_ihd     := af_ihd_def_s]
if (!"af_istroke" %in% names(dt_af_wide_s)) dt_af_wide_s[, af_istroke := af_istroke_def_s]

dt_af_wide_s[is.na(af_ihd),     af_ihd     := af_ihd_def_s]
dt_af_wide_s[is.na(af_istroke), af_istroke := af_istroke_def_s]

rm(dt_af_s)


# -- 6. Secondary & primary prevention counts (from calibrated baseline rates) -
#
# Load adjusted*.rds files as b_rates_s (same pattern as 05_build_baseline.R).
# Keep only the columns needed to reduce memory.
#
# PREVt0 = calibrated initial prevalence (proportion in "sick" state at model start).
#          Constant across years in the output files; we extract from the earliest year.
#
# SECONDARY prevention eligible:
#   People who already HAVE IHD or atherosclerotic ischemic stroke (age >= 40).
#   Count = PREVt0 x Nx_unwpp, where Nx_unwpp is UNWPP 2025 population.
#   For ischemic stroke: only the atherosclerotic fraction (prop_athero_stroke) is counted.
#
# PRIMARY prevention eligible:
#   People WITHOUT existing CVD who are at risk (dyslipidaemia-driven risk via AF).
#   Count = (1 - PREVt0) x AF x Nx_unwpp
#   For ischemic stroke: further scaled by prop_athero_stroke.
#
# NOTE: Primary and secondary counts are reported separately; their sum
#       (total_eligible_2025) assumes non-overlapping groups (standard simplification).

prop_athero_stroke_s <- 0.60   # share of ischemic strokes that are atherosclerotic

files_adj_s   <- list.files(path = wd_data, pattern = "adjusted", full.names = TRUE)
dt_list_adj_s <- lapply(files_adj_s, function(f) {
  dt <- readRDS(f)
  setDT(dt)
  needed <- intersect(names(dt), c("location", "year", "age", "sex", "cause", "PREVt0"))
  dt[, .SD, .SDcols = needed]
})
b_rates_s <- rbindlist(dt_list_adj_s, use.names = TRUE, fill = TRUE)
rm(dt_list_adj_s, files_adj_s)

# Harmonise location names (same renames as 05_build_baseline.R)
b_rates_s[location == "United States of America",           location := "United States"]
b_rates_s[location == "Bolivia (Plurinational State of)",   location := "Bolivia"]
b_rates_s[location == "United Republic of Tanzania",        location := "Tanzania"]

# Extract PREVt0 from the earliest available year (expected: 2017)
# PREVt0 does not vary by year; using the first year avoids duplicating rows.
if ("year" %in% names(b_rates_s)) {
  earliest_yr_s <- min(b_rates_s$year, na.rm = TRUE)
  b_prev_s <- b_rates_s[year == earliest_yr_s & age >= 40 & cause %in% c("ihd", "istroke"),
                         .(location, sex, age, cause, PREVt0)]
} else {
  earliest_yr_s <- NA_integer_
  b_prev_s <- b_rates_s[age >= 40 & cause %in% c("ihd", "istroke"),
                         .(location, sex, age, cause, PREVt0)]
}
rm(b_rates_s)

# Merge PREVt0 strata with UNWPP 2025 population (5-yr bins)
# This gives us 2025-population-weighted prevalence counts
b_prev_s <- merge(
  b_prev_s,
  pop_5yr_2025_s,
  by    = c("location", "sex", "age"),
  all.x = TRUE
)
# Strata with no UNWPP population match contribute 0 to counts
b_prev_s[is.na(Nx_unwpp), Nx_unwpp := 0]

# Merge location-level AFs (does not vary by age/sex within a location)
b_prev_s <- merge(b_prev_s, dt_af_wide_s, by = "location", all.x = TRUE)
b_prev_s[is.na(af_ihd),     af_ihd     := af_ihd_def_s]
b_prev_s[is.na(af_istroke), af_istroke := af_istroke_def_s]

# -- 6a. Secondary prevention counts (prevalent cases, age >= 40) ------------
sec_prev_s <- b_prev_s[, .(
  sec_prev_ihd_2025     = sum(ifelse(cause == "ihd",
                                     PREVt0 * Nx_unwpp,
                                     0), na.rm = TRUE),
  sec_prev_istroke_2025 = sum(ifelse(cause == "istroke",
                                     prop_athero_stroke_s * PREVt0 * Nx_unwpp,
                                     0), na.rm = TRUE)
), by = .(location)]

sec_prev_s[, sec_prev_total_2025 := sec_prev_ihd_2025 + sec_prev_istroke_2025]

# -- 6b. Primary prevention counts (at-risk without existing disease, age >= 40) -
prim_prev_s <- b_prev_s[, .(
  prim_prev_ihd_2025     = sum(ifelse(cause == "ihd",
                                      (1 - PREVt0) * af_ihd * Nx_unwpp,
                                      0), na.rm = TRUE),
  prim_prev_istroke_2025 = sum(ifelse(cause == "istroke",
                                      (1 - PREVt0) * prop_athero_stroke_s * af_istroke * Nx_unwpp,
                                      0), na.rm = TRUE)
), by = .(location)]

prim_prev_s[, prim_prev_total_2025 := prim_prev_ihd_2025 + prim_prev_istroke_2025]

rm(b_prev_s, dt_af_wide_s, pop_5yr_2025_s)


# -- 7. Assemble final country-level table -----------------------------------

statins_targets_s <- merge(pop40_2025_s,     aim2_diab_s,   by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, iso3_map_s,    by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, statin_base_s, by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, sec_prev_s,    by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, prim_prev_s,   by = "location", all.x = TRUE)

# Derived fields
statins_targets_s[, `:=`(
  year                     = 2025L,
  diabetes_pop_40plus_2025 = pop_40plus_2025 * diabetes_prev_2025,
  total_eligible_2025      = sec_prev_total_2025 + prim_prev_total_2025,
  statins_target_2030      = 0.80
)]


# -- 8. Column ordering ------------------------------------------------------
col_order_s <- c(
  "location", "iso3", "year",
  "pop_40plus_2025",
  "diabetes_prev_2025", "diabetes_pop_40plus_2025",
  "statins_baseline_2025", "statins_target_2030",
  "prim_prev_ihd_2025", "prim_prev_istroke_2025", "prim_prev_total_2025",
  "sec_prev_ihd_2025",  "sec_prev_istroke_2025",  "sec_prev_total_2025",
  "total_eligible_2025"
)
setcolorder(statins_targets_s, col_order_s[col_order_s %in% names(statins_targets_s)])


# -- 9. Sanity checks --------------------------------------------------------
cat("=== statins_control_targets_by_loc: Diagnostics ===\n")
cat("  Unique countries:               ", uniqueN(statins_targets_s$location),                          "\n")
cat("  Missing statins baseline:       ", sum(is.na(statins_targets_s$statins_baseline_2025)), "\n")
cat("  Missing diabetes prevalence:    ", sum(is.na(statins_targets_s$diabetes_prev_2025)),   "\n")
cat("  Missing pop 40+:                ", sum(is.na(statins_targets_s$pop_40plus_2025)),       "\n")
cat("  Missing secondary prev total:   ", sum(is.na(statins_targets_s$sec_prev_total_2025)),  "\n")
cat("  Missing primary prev total:     ", sum(is.na(statins_targets_s$prim_prev_total_2025)), "\n")
cat("  Rows with iso3 matched:         ", sum(!is.na(statins_targets_s$iso3)),                "\n")
cat("  Diabetes source used:           ", diab_col_use_s,                                     "\n")


# -- 10. Write CSV -----------------------------------------------------------
# Path consistent with project convention: fwrite to wd_data (data/processed/)
fwrite(statins_targets_s, paste0(wd_data, "statins_control_targets_by_loc.csv"))
cat("Written: statins_control_targets_by_loc.csv\n")


# -- 11. Clean up ------------------------------------------------------------
rm(statins_targets_s, pop40_2025_s, aim2_diab_s, iso3_map_s, statin_base_s,
   sec_prev_s, prim_prev_s, prop_athero_stroke_s,
   af_ihd_def_s, af_istroke_def_s, diab_col_use_s,
   col_order_s, earliest_yr_s)
}
