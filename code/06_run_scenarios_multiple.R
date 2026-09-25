
# =============================================================================
# Aim 2 BP-control split-population helpers (Part B refactor)
# =============================================================================
# PORTED VERBATIM from 06_run_scenarios_targets.R (the source-of-truth Aim 2
# script) so this multi-intervention pipeline uses the SAME mutually-exclusive,
# additive-population-mixture BP-by-diabetes logic (no multiplicative cross-term,
# no double application of diabetes prevalence or raisedBP). Defined BEFORE the
# execution guard so tests can source this file with
# options(who_cvd.execute_06 = FALSE) to exercise the helpers without loading the
# full model inputs or starting the parallel cluster. calculate_antihypertensive_split
# uses get.bp.prob / calculate_baseline_incidence_gbd / expand_to_single_year_ages /
# calculate_aggregate_coverage / ETIHAD_RR_BIN, which are defined inside the guard
# for real runs and stubbed by the test harness.

aim2_control_trajectory <- function(year, baseline_control, target_control,
                                    baseline_year = 2025L,
                                    target_year = 2030L) {
  if (anyNA(c(year, baseline_control, target_control)) ||
      any(baseline_control < 0 | baseline_control > 1 |
          target_control < 0 | target_control > 1)) {
    stop("Aim 2 trajectory inputs must be non-missing rates in [0, 1].")
  }
  fraction <- pmin(1, pmax(0, (year - baseline_year) /
                             (target_year - baseline_year)))
  baseline_control + fraction * (target_control - baseline_control)
}

aim2_incremental_effect <- function(effect_size, coverage_t, coverage_0) {
  denominator <- 1 - effect_size * coverage_0
  if (anyNA(c(effect_size, coverage_t, coverage_0)) ||
      any(coverage_t < coverage_0 - 1e-12) ||
      any(coverage_0 < 0 | coverage_0 > 1 |
          coverage_t < 0 | coverage_t > 1) ||
      any(denominator <= 0)) {
    stop("Invalid subgroup coverage or effect-size input for Aim 2.")
  }
  effect_size * (coverage_t - coverage_0) / denominator
}

aim2_subgroup_incidence_multiplier <- function(effect_no_diabetes,
                                               effect_diabetes,
                                               diabetes_share) {
  if (anyNA(c(effect_no_diabetes, effect_diabetes, diabetes_share)) ||
      any(diabetes_share < 0 | diabetes_share > 1)) {
    stop("Diabetes shares used in the Aim 2 mixture must lie in [0, 1].")
  }
  1 - (1 - diabetes_share) * effect_no_diabetes -
    diabetes_share * effect_diabetes
}

validate_htn_target_table <- function(targets) {
  required <- c(
    "scenario_id", "location", "sex", "subgroup", "baseline_year",
    "scaleup_start_year", "target_year", "baseline_control",
    "target_control", "diabetes_share_among_htn_assumed"
  )
  missing_columns <- setdiff(required, names(targets))
  if (length(missing_columns)) {
    stop("HTN target table is missing: ", paste(missing_columns, collapse = ", "))
  }
  if (anyNA(targets[, .(scenario_id, location, sex, subgroup)]) ||
      anyDuplicated(targets,
                    by = c("scenario_id", "location", "sex", "subgroup"))) {
    stop("HTN target table has missing or duplicate keys.")
  }
  expected_subgroups <- c("htn_no_diabetes", "htn_diabetes")
  key_check <- targets[, .(
    n_rows = .N,
    n_subgroups = uniqueN(subgroup),
    correct_subgroups = setequal(subgroup, expected_subgroups)
  ), by = .(scenario_id, location, sex)]
  if (key_check[, any(n_rows != 2L | n_subgroups != 2L | !correct_subgroups)]) {
    stop("Each scenario-location-sex key must contain exactly both HTN subgroups.")
  }
  if (any(targets$baseline_control < 0 | targets$baseline_control > 1 |
          targets$target_control < 0 | targets$target_control > 1)) {
    stop("HTN target-table rates must lie in [0, 1].")
  }
  if (any(targets$baseline_year != 2025L |
          targets$scaleup_start_year != 2026L |
          targets$target_year != 2030L)) {
    stop("Aim 2 trajectories require baseline 2025, scale-up start 2026, and target 2030.")
  }
  invisible(TRUE)
}

calculate_antihypertensive_split <- function(intervention_rates,
                                             Country,
                                             DT.in,
                                             dt_gbd_rr,
                                             target_rows,
                                             active_no_diabetes = TRUE,
                                             active_diabetes = TRUE,
                                             etihad_rr_table = ETIHAD_RR_BIN,
                                             bp_baseline = NULL,
                                             diabetes_age_prepared = NULL) {
  cat(" - Calculating mutually exclusive antihypertensive subgroup impacts\n")
  validate_htn_target_table(target_rows)
  if (uniqueN(target_rows$location) != 1L ||
      unique(target_rows$location) != Country ||
      uniqueN(target_rows$scenario_id) != 1L) {
    stop("Target rows must contain exactly one country and one scenario.")
  }
  
  targets_wide <- dcast(
    target_rows,
    location + sex + baseline_year + scaleup_start_year + target_year ~ subgroup,
    value.var = c("baseline_control", "target_control",
                  "diabetes_share_among_htn_assumed")
  )
  setnames(
    targets_wide,
    c("baseline_control_htn_no_diabetes",
      "baseline_control_htn_diabetes",
      "target_control_htn_no_diabetes",
      "target_control_htn_diabetes"),
    c("baseline_control_no_diabetes", "baseline_control_diabetes",
      "target_control_no_diabetes", "target_control_diabetes")
  )
  redundant_share_col <- "diabetes_share_among_htn_assumed_htn_diabetes"
  share_col <- "diabetes_share_among_htn_assumed_htn_no_diabetes"
  if (any(abs(targets_wide[[share_col]] -
              targets_wide[[redundant_share_col]]) > 1e-12)) {
    stop("Target-table diabetes shares differ between subgroup rows.")
  }
  targets_wide[, diabetes_share_among_htn_assumed := get(share_col)]
  targets_wide[, c(share_col, redundant_share_col) := NULL]
  if (!active_no_diabetes) {
    targets_wide[, target_control_no_diabetes := baseline_control_no_diabetes]
  }
  if (!active_diabetes) {
    targets_wide[, target_control_diabetes := baseline_control_diabetes]
  }
  
  if (is.null(bp_baseline)) {
    bp_prob_base <- get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
    dt_baseline <- calculate_baseline_incidence_gbd(
      copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr
    )
  } else {
    # All subsequent merges/updates are local to this scenario.
    dt_baseline <- copy(bp_baseline)
  }
  
  if (is.null(diabetes_age_prepared)) {
    diabetes_age <- expand_to_single_year_ages(DT.in)
    diabetes_age <- unique(diabetes_age[, .(
      location, Year, age, sex, bp_cat,
      diabetes_share_among_htn_assumed_age_specific = diabetes
    )])
    setnames(diabetes_age, "Year", "year")
  } else {
    diabetes_age <- diabetes_age_prepared
  }
  if (anyNA(diabetes_age$diabetes_share_among_htn_assumed_age_specific) ||
      any(diabetes_age$diabetes_share_among_htn_assumed_age_specific < 0 |
          diabetes_age$diabetes_share_among_htn_assumed_age_specific > 1)) {
    stop(paste0(
      "The age-specific diabetes variable is interpreted as overall-population ",
      "prevalence and, by assumption, as P(D|H); values must lie in [0, 1]."
    ))
  }
  dt_baseline <- merge(
    dt_baseline, diabetes_age,
    by = c("location", "year", "age", "sex", "bp_cat"), all.x = TRUE
  )
  dt_baseline <- merge(dt_baseline, targets_wide,
                       by = c("location", "sex"), all.x = TRUE)
  if (anyNA(dt_baseline[, .(
    baseline_control_no_diabetes, baseline_control_diabetes,
    target_control_no_diabetes, target_control_diabetes,
    diabetes_share_among_htn_assumed_age_specific
  )])) {
    stop("Missing Aim 2 subgroup target or diabetes-share join in model strata.")
  }
  
  lookup_key <- paste(dt_baseline$cause, dt_baseline$bp_cat, sep = "_")
  table_key <- paste(etihad_rr_table$cause, etihad_rr_table$bp_cat, sep = "_")
  lookup_index <- match(lookup_key, table_key)
  if (anyNA(lookup_index)) {
    missing_keys <- unique(lookup_key[is.na(lookup_index)])
    stop("Missing Ettehad subgroup effect sizes for: ",
         paste(missing_keys, collapse = ", "))
  }
  dt_baseline[, `:=`(
    effect_size_no_diabetes =
      etihad_rr_table$effect_size_nodiabetes[lookup_index],
    effect_size_diabetes =
      etihad_rr_table$effect_size_diabetes[lookup_index],
    control_no_diabetes_t = aim2_control_trajectory(
      year, baseline_control_no_diabetes, target_control_no_diabetes,
      baseline_year, target_year
    ),
    control_diabetes_t = aim2_control_trajectory(
      year, baseline_control_diabetes, target_control_diabetes,
      baseline_year, target_year
    )
  )]
  
  hypertensive_bins <- c("140-149", "150-159", "160-169", "170-179", "180+")
  dt_baseline[, `:=`(
    incremental_effect_no_diabetes = 0,
    incremental_effect_diabetes = 0
  )]
  dt_baseline[bp_cat %in% hypertensive_bins, `:=`(
    incremental_effect_no_diabetes = aim2_incremental_effect(
      effect_size_no_diabetes, control_no_diabetes_t,
      baseline_control_no_diabetes
    ),
    incremental_effect_diabetes = aim2_incremental_effect(
      effect_size_diabetes, control_diabetes_t,
      baseline_control_diabetes
    )
  )]
  dt_baseline[, incidence_multiplier := aim2_subgroup_incidence_multiplier(
    incremental_effect_no_diabetes,
    incremental_effect_diabetes,
    diabetes_share_among_htn_assumed_age_specific
  )]
  dt_baseline[, IR_bin_new := IR_bin * incidence_multiplier]
  dt_baseline[, IR_new := sum(IR_bin_new * prob),
              by = .(age, sex, location, cause, year)]
  dt_baseline[, eff_ir := fifelse(IR == 0 & IR_new == 0, 1, IR_new / IR)]
  
  dt_baseline[, coverage_delta_weighted :=
                (1 - diabetes_share_among_htn_assumed_age_specific) *
                (control_no_diabetes_t - baseline_control_no_diabetes) +
                diabetes_share_among_htn_assumed_age_specific *
                (control_diabetes_t - baseline_control_diabetes)]
  coverage_aggregate <- calculate_aggregate_coverage(
    dt_baseline,
    hypertensive_bins = hypertensive_bins,
    bp_col = "bp_cat", coverage_col = "coverage_delta_weighted",
    prob_col = "prob",
    grouping_vars = c("age", "sex", "location", "cause", "year"),
    hypertensive_only = TRUE
  )
  dt_baseline <- merge(
    dt_baseline, coverage_aggregate,
    by = c("age", "sex", "location", "cause", "year"), all.x = TRUE
  )
  dt_baseline[is.na(coverage_agg), coverage_agg := 0]
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  dt_baseline[, CF_new := CF * (1 - cf_reduction_per_control * coverage_agg)]
  dt_baseline[cause == "aod" & age < 60, CF_new := CF]
  dt_baseline[, eff_cf := fifelse(CF == 0 & CF_new == 0, 1, CF_new / CF)]
  
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year,
    IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx,
    eff_ir, eff_cf
  )])
  setorder(dt_final, year, sex, location, cause, age)
  dt_final
}

# Everything below runs only in the full pipeline (guard default TRUE). Tests set
# who_cvd.execute_06 = FALSE to source the pure helpers above without executing.
if (isTRUE(getOption("who_cvd.execute_06", TRUE))) {
  
  # #...........................................................
  # # Interventions and Targets ----
  # #...........................................................
  
  #...........................................................
  ## GBD Relative Risks Setup ----
  #...........................................................
  
  # Load and prepare GBD relative risks (same as OLD model)
  dt_gbd_rr <- as.data.table(read_excel(paste0(wd_raw, "IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx"),
                                        sheet = "Sheet1", range = "A3:AB20"))
  
  dt_gbd_rr[, c("Category / Units", "Morbidity / Mortality", "Sex", "All-age") := NULL]
  dt_gbd_rr[, `20-24 years` := `25-29 years`]
  dt_gbd_rr[, (2:8) := NULL]
  
  dt_gbd_rr <- melt(dt_gbd_rr,
                    id.vars = c("Risk-Outcome"),
                    variable.name = "age",
                    value.name = "rr_per_10mmhg")
  
  dt_gbd_rr[, age := gsub(" years", "", age)]
  dt_gbd_rr[, rr_per_10mmhg := as.numeric(sub("^\\s*([0-9.]+).*", "\\1", rr_per_10mmhg))]
  
  dt_gbd_rr[, cause := fcase(
    `Risk-Outcome` == "Ischaemic heart disease", "ihd",
    `Risk-Outcome` == "Ischaemic stroke", "istroke",
    `Risk-Outcome` == "Intracerebral hemorrhage", "hstroke",
    `Risk-Outcome` == "Hypertensive heart disease", "hhd",
    default = NA_character_
  )]
  dt_gbd_rr[, `Risk-Outcome` := NULL]
  dt_gbd_rr <- dt_gbd_rr[cause %in% c("ihd", "istroke", "hstroke", "hhd")]
  
  # Expand age groups to single years
  expand_age <- function(age_group) {
    if (grepl("\\+", age_group)) {
      start <- as.numeric(sub("\\+", "", age_group))
      return(start:95)
    } else {
      bounds <- as.numeric(unlist(strsplit(age_group, "-")))
      return(bounds[1]:bounds[2])
    }
  }
  
  dt_expanded <- dt_gbd_rr[, .(age_single = expand_age(age)), by = .(age, rr_per_10mmhg, cause)]
  dt_expanded[, age_single := as.integer(age_single)]
  dt_expanded[, age := age_single]
  dt_expanded[, age_single := NULL]
  dt_gbd_rr <- copy(dt_expanded)
  
  #...........................................................
  ## Helpers - Reusable components ----
  #...........................................................
  
  # # ETIHAD relative risks for 10 mmHg BP reduction
  # ETIHAD_RR <- data.table(
  #   cause = c("ihd", "hhd", "istroke", "hstroke", "aod"),
  #   rr_per_10mmhg = c(0.83, 0.72, 0.73, 0.73, 0.93)
  # )
  
  #ETIHAD_RR <- fread(paste0(wd_data, "ettehad_rr_bp_reduction_10mmHg.csv"))
  
  ETIHAD_RR <- fread(paste0(wd_data, "ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv"))
  
  # rename to cause column
  ETIHAD_RR[, cause := fcase(
    Cause == "Coronary heart disease", "ihd",
    Cause == "Heart failure", "hhd",
    Cause == "Stroke", "istroke",
    default = NA_character_
  )]
  
  # keep only relevant causes
  ETIHAD_RR <- ETIHAD_RR[cause %in% c("ihd", "hhd", "istroke","hstroke"),
                         c("cause","SBP_Category","RR"),with=F]
  
  # hstroke from istroke
  etihad_hstroke_rr <- ETIHAD_RR[cause=="istroke",]
  etihad_hstroke_rr[, cause := "hstroke"]
  ETIHAD_RR <- rbind(ETIHAD_RR, etihad_hstroke_rr)
  
  #remove bp total
  ETIHAD_RR <- ETIHAD_RR[SBP_Category != "Total", ]
  
  #rename columns
  setnames(ETIHAD_RR, c("SBP_Category", "RR"), c("bp_cat", "rr_per_10mmhg"))
  
  # Standard BP categories used in model
  bp_full <- c("<120", "120-129", "130-139", "140-149",
               "150-159", "160-169", "170-179", "180+")
  
  # mapping function
  map_bp <- function(x){
    fcase(
      x %in% c("<120", "120-129", "<130")        , "<130",
      x == "130-139"                             , "130-139",
      x == "140-149"                             , "140-149",
      x == "150-159"                             , "150-159",
      x %in% c("160-169","170-179","180+","≥160"), ">=160"
    )
  }
  
  # build mapping table
  bp_map <- data.table(
    bp_cat_full = bp_full,
    bp_cat = map_bp(bp_full)
  )
  
  # merge only on matching categories (carry forward RR)
  expanded <- bp_map[
    ETIHAD_RR,
    on = .(bp_cat),
    allow.cartesian = TRUE
  ][
    , .(cause, bp_cat_full, rr = rr_per_10mmhg)
  ][
    order(cause, bp_cat_full)
  ]
  
  ETIHAD_RR <- copy(expanded)
  
  #rename etihad
  setnames(ETIHAD_RR, c("bp_cat_full", "rr"), c("bp_cat", "rr_per_10mmhg"))
  
  # cleaning
  rm(expanded, etihad_hstroke_rr)
  
  # Calculate coverage for a given year based on linear scale-up
  # from start_year (0% coverage) to target_year (target_coverage%)
  calculate_coverage_by_year <- function(year, 
                                         start_year = 2026, 
                                         target_year = 2050,
                                         target_coverage = 0.50) {
    
    # Years elapsed since start
    
    # # original
    # years_elapsed <- pmax(0, year - start_year)
    
    # no delay
    years_elapsed <- pmax(0, year - start_year + 1)
    
    # Total years in scale-up period
    # # original
    # total_years <- target_year - start_year
    
    # no delay
    total_years <- target_year - start_year + 1
    
    # Linear interpolation
    coverage <- pmin(target_coverage * years_elapsed / total_years, 
                     target_coverage)
    
    # Before start_year, coverage is 0
    coverage[year < start_year] <- 0
    
    # After target_year, coverage is at target
    coverage[year > target_year] <- target_coverage
    
    return(coverage)
  }
  
  # Vectorized version for data.table
  add_coverage_by_year <- function(dt, 
                                   year_col = "year",
                                   start_year = 2026,
                                   target_year = 2050,
                                   target_coverage = 0.50,
                                   coverage_col = "coverage_t") {
    
    dt[, (coverage_col) := calculate_coverage_by_year(
      get(year_col), 
      start_year, 
      target_year, 
      target_coverage
    )]
    
    return(dt)
  }
  
  # Calculate weighted average coverage across hypertensive BP bins
  # Used for case fatality calculations
  calculate_aggregate_coverage <- function(dt, 
                                           hypertensive_bins = c("140-149", "150-159", 
                                                                 "160-169", "170-179", "180+"),
                                           bp_col = "bp_cat",
                                           coverage_col = "coverage_t",
                                           prob_col = "prob",
                                           grouping_vars = c("age", "sex", "location", 
                                                             "cause", "year"),
                                           hypertensive_only = NULL) {
    
    # hypertensive_only = NULL then use all bins (default behavior)
    # hypertensive_only = TRUE then use only hypertensive bins
    # hypertensive_only = FALSE then explicitly use all bins
    
    
    dt_use <- copy(dt)
    
    if (!is.null(hypertensive_only) && hypertensive_only == TRUE) {
      dt_use <- dt_use[get(bp_col) %in% hypertensive_bins]
    }
    
    # Compute weighted mean coverage
    coverage_agg <- dt_use[, 
                           .(
                             coverage_agg = weighted.mean(get(coverage_col), 
                                                          get(prob_col), 
                                                          na.rm = TRUE)
                           ),
                           by = grouping_vars
    ]
    
    return(coverage_agg)
  }
  
  # Import from excel .xlsx file ettehad_rr_bp_reduction_effects
  ETIHAD_RR_BIN<- as.data.table(read_excel(paste0(wd_data, "ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx"), 
                                           sheet = "Sheet1"))
  
  # ETIHAD_RR_BIN<- as.data.table(read_excel(paste0(wd_data, "ettehad_rr_bp_reduction_effects.xlsx"), 
  #                                          sheet = "Sheet1"))
  
  # Calculate cumulative ETIHAD effect size for BP bins
  # For each bin, calculate cumulative effect of reducing BP by 10 mmHg steps
  
  calculate_etihad_cumulative_rr <- function(bp_cat, 
                                             cause_name, 
                                             diabetes_weight = 0.1,
                                             etihad_rr_table = ETIHAD_RR_BIN) {
    
    # Input lengths must match
    if (length(bp_cat) != length(cause_name)) {
      stop("bp_cat and cause_name must have the same length")
    }
    
    # Build key for matching
    lookup_key <- paste(cause_name, bp_cat, sep = "_")
    table_key  <- paste(etihad_rr_table$cause, etihad_rr_table$bp_cat, sep = "_")
    
    # Indices in lookup table
    idx <- match(lookup_key, table_key)
    
    if (any(is.na(idx))) {
      stop("Some bp_cat–cause combinations were not found in ETIHAD_RR")
    }
    
    # Extract effect sizes from table
    effect_no_diab <- etihad_rr_table$effect_size_nodiabetes[idx]
    effect_diab    <- etihad_rr_table$effect_size_diabetes[idx]
    
    # Weighted average
    effect_weighted <- 
      (1 - diabetes_weight) * effect_no_diab +
      diabetes_weight   * effect_diab
    
    return(effect_weighted)
  }
  
  
  # Load GBD relative risks (per 10 mmHg increase)
  # Expected format: columns for age, sex, cause, rr_per_10mmhg
  # dt_gbd_rr <- readRDS("path/to/gbd_rr_data.rds")
  
  # Assign GBD relative risks based on BP category
  # RRs are relative to <120 mmHg reference
  get_gbd_relative_risks <- function(bp_cat, age, cause, dt_gbd_rr = NULL) {
    
    if (is.null(dt_gbd_rr)) {
      dt_gbd_rr <- get("dt_gbd_rr", envir = .GlobalEnv)
    }
    
    # Calculate midpoint SBP for each category
    sbp_midpoint <- case_when(
      bp_cat == "<120" ~ 110,
      bp_cat == "120-129" ~ 125,
      bp_cat == "130-139" ~ 135,
      bp_cat == "140-149" ~ 145,
      bp_cat == "150-159" ~ 155,
      bp_cat == "160-169" ~ 165,
      bp_cat == "170-179" ~ 175,
      bp_cat == "180+" ~ 185,
      TRUE ~ NA_real_
    )
    
    # 2. Increment from 120
    inc_10 <- (sbp_midpoint - 120) / 10
    
    # 3. Merge single-row-by-row using a fast join
    # Create temporary table with input values
    tmp <- data.table(age = age, cause = cause)
    
    # Join RRs
    tmp <- dt_gbd_rr[tmp, on = c("age", "cause")]
    
    rr10 <- tmp$rr_per_10mmhg
    
    # 4. Compute RR
    rr <- ifelse(inc_10 > 0, rr10^inc_10, 1)
    
    return(rr)
  }
  
  # Expand age groups to single-year ages
  expand_to_single_year_ages <- function(dt) {
    dt[, age := as.numeric(substr(age, 1, 2))]
    dt <- dt[rep(seq_len(nrow(dt)), each = 5)]
    dt[, age2 := rep(1:5, nrow(dt)/5)][, age := age + age2 - 1]
    
    over90 <- dt[age == 89]
    over90 <- over90[rep(seq_len(nrow(over90)), each = 6)]
    over90[, age2 := rep(1:6, nrow(over90)/6)][, age := age + age2]
    
    rbindlist(list(dt, over90))[, age2 := NULL]
  }
  
  # Calculate BP probabilities with optional treatment effect
  
  # That last line overwrites everything and makes covinc always 0, 
  # so treatment never shifts BP probabilities. since we apply ETTEHAD 
  # via coverage-by-bin, so any redistribution across BP bins, it’s currently disabled.
  
  get.bp.prob <- function(DT, rx, drugaroc = "baseline") {
    # Select appropriate coverage increment variable
    cov_var <- switch(
      drugaroc,
      "baseline" = "aroc2",
      "p75" = "p_change2",
      "p975" = "a_change2",
      "ideal" = "ideal",
      stop("Invalid 'drugaroc' argument. Must be one of: baseline, p75, p975, ideal")
    )
    
    # Apply coverage increment only if antihypertensive treatment (rx == 1)
    DT[, covinc := if (rx == 1) get(cov_var) else aroc2]
    
    # Patch missing coverage increments to 0 ommit aroc
    DT[, covinc := 0]
    
    # Define BP cutpoints
    bp_breaks <- c(-Inf, 120, 130, 140, 150, 160, 170, 180, Inf)
    bp_labels <- c("<120", "120-129", "130-139", "140-149", 
                   "150-159", "160-169", "170-179", "180+")
    
    # Compute BP category probabilities using vectorized operations
    for (i in seq_along(bp_labels)) {
      lower <- bp_breaks[i]
      upper <- bp_breaks[i + 1]
      DT[bp_cat == bp_labels[i], 
         prob := pnorm(upper, Mean, stdev) - pnorm(lower, Mean, stdev)]
    }
    
    # Adjust probabilities for antihypertensive treatment coverage
    if (rx == 1) {
      DT[, shift := prob * covinc]
      DT[bp_cat %in% c("<120", "120-129", "130-139"), shift := 0]
      
      # Compute reallocation between BP bins by diabetes status
      DT[, add130 := sum(shift * diabetes), by = .(age, sex, Year)]
      DT[, add140 := sum(shift * (1 - diabetes)), by = .(age, sex, Year)]
      
      # Update category probabilities
      DT[, prob := prob - shift]
      DT[bp_cat == "120-129", prob := prob + add130]
      DT[bp_cat == "130-139", prob := prob + add140]
    }
    
    # Return relevant variables
    return(DT[, .(age, sex, Year, bp_cat, prob, location)])
  }
  
  # Calculate baseline incidence rates using GBD RRs
  calculate_baseline_incidence_gbd <- function(bp_prob, intervention_rates, 
                                               Country, dt_gbd_rr) {
    cat("  - Calculating baseline incidence with GBD RRs\n")
    
    # Expand to single-year ages
    bp_prob <- expand_to_single_year_ages(bp_prob)
    
    # Add GBD relative risks for all causes
    causes <- c("ihd", "hhd", "istroke", "hstroke", "aod")
    
    for (cause in causes) {
      col_name <- paste0("RRi_", toupper(cause))
      bp_prob[, (col_name) := get_gbd_relative_risks(bp_cat, age,
                                                     cause, dt_gbd_rr)]
    }
    
    # Calculate alphas (normalization factors)
    # alpha = sum(prob * RR) across all BP categories
    alphas <- bp_prob[, .(
      ihd = sum(prob * RRi_IHD),
      istroke = sum(prob * RRi_ISTROKE),
      hstroke = sum(prob * RRi_HSTROKE),
      hhd = sum(prob * RRi_HHD),
      aod = sum(prob * RRi_AOD)
    ), by = .(age, sex, location, Year)]
    
    alphas <- melt(alphas, id.vars = c("age", "sex", "location", "Year"),
                   variable.name = "cause", value.name = "alpha")
    
    # Prepare RRi data (long format)
    # Use the same cause-specific GBD RRs in the bins and their normalisers.
    rris <- bp_prob[, .(age, sex, Year, location, bp_cat, prob,
                        RRi_IHD, RRi_HHD, RRi_ISTROKE, RRi_HSTROKE, RRi_AOD)]
    
    setnames(rris, 
             c("RRi_IHD", "RRi_HHD", "RRi_ISTROKE", "RRi_HSTROKE", "RRi_AOD"),
             c("ihd", "hhd", "istroke", "hstroke", "aod"))
    
    rris <- melt(rris, id.vars = c("age", "sex", "location", "bp_cat", "prob", "Year"),
                 variable.name = "cause", value.name = "RRi")
    
    # Merge with alphas
    bp_prob_full <- merge(rris, alphas, 
                          by = c("age", "sex", "location", "cause", "Year"))
    setnames(bp_prob_full, "Year", "year")
    
    # Merge with intervention rates
    dt <- merge(intervention_rates[location == Country], bp_prob_full,
                by = c("age", "sex", "location", "cause", "year"))
    
    # Calculate BP bin-specific baseline incidence: IR_bin = (RRi * IR) / alpha
    dt[, IR_bin := (RRi * IR) / alpha]
    
    return(dt)
  }
  
  # # Calculate Ettehad-based effect size for BP reduction
  
  apply_coverage_adjustment <- function(effect_size, 
                                        coverage_t, 
                                        coverage_0 = 0) {
    
    if (all(coverage_0 == 0)) {
      # Simplified formula when baseline coverage is 0
      return(effect_size * coverage_t)
    } else {
      # Full formula when baseline coverage > 0
      numerator <- effect_size * (coverage_t - coverage_0)
      denominator <- 1 - effect_size * coverage_0
      return(numerator / denominator)
    }
  }
  #...........................................................
  ## Anti hypertensive therapy ----
  #...........................................................
  
  
  
  # For aim 2 (150 million more controlled people) upload targets file
  
  dt_hbp_targets <- fread(paste0(wd_data,"htn_control_targets_by_loc.csv"))
  
  # The two former BP functions (calculate_antihypertensive_impact_etihad and
  # calculate_antihypertensive_diabetes) were REMOVED in the Part B refactor.
  # They are replaced by the single mutually-exclusive split-population function
  # calculate_antihypertensive_split() defined at the top of this file (ported
  # verbatim from 06_run_scenarios_targets.R): the two subgroups are combined
  # additively as a population mixture (no multiplicative cross-term) and neither
  # diabetes prevalence nor raisedBP is applied twice.
  
  #...........................................................
  ## Statins ----
  #...........................................................
  
  # Compute scale up scenario targeting DM high risk population
  dt_statin_scenarios <- readRDS(file = paste0(wd_data,"statin_data.rds"))
  
  # # Add Diabetes Proportion
  # dt_diabetes <- data.in[,c("location","age","sex","diabetes"),with=F]
  # dt_diabetes <- unique(dt_diabetes)
  # setnames(dt_diabetes, c("age"), c("age_group"))
  
  # High Fasting Plasma Glucose Attributable Fraction 
  # for IHD and ischaemic stroke, using country- & cause-specific AFs supplied in dt_af_statins.
  
  dt_af_statins <- readRDS(file = paste0(wd_data,"af_statins.rds"))
  
  # Function to calculate statins impact on IHD and stroke
  
  # RR of major CVD events per 1.0 mmol/L reduction in LDL cholesterol at 1 year after randomization:
  # Statin vs. control incidence 0.78 (0.77 - 0.81)
  # RR coronary heart disease 0·80, 99% CI 0·74–0·87
  # Source: Trials 2015
  # https://doi.org/10.1016/S0140-6736(10)61350-5
  # This is applied directly to incidence of IHD and stroke
  
  
  # Attributable fraction for statins
  # GBD 2021 Risk factor attribution High Fasting Plasma glucose
  # https://vizhub.healthdata.org/gbd-compare/
  
  ## Statins
  
  calculate_statins_impact <- function(dt_statin_scenarios,
                                       intervention_rates,
                                       Country,
                                       dt_af_statins,
                                       adherence_ir = 1,   # primary prevention adherence (IR)
                                       adherence_cf = 1,   # secondary prevention adherence (CF)
                                       prop_athero_stroke = 0.60,
                                       statin_target_coverage = 0.60,
                                       statin_start_year = 2026,
                                       statin_target_year = 2050,
                                       baseline_statin_coverage = NULL) {
    cat("  - Calculating statins impact\n")
    
    #..........................................................
    # STEP 1: Define relative risks from major statin trials
    #..........................................................
    
    rr_ir_ihd      <- 0.74
    rr_ir_istroke  <- 0.80
    rr_cf_ihd      <- 0.80
    rr_cf_istroke  <- 0.96
    
    #..........................................................
    # STEP 2: Default attributable fractions for IHD & ischaemic stroke
    #..........................................................
    
    af_ihd     <- 0.1497
    af_istroke <- 0.1161
    
    # Subset country-specific intervention table
    dt <- intervention_rates[location == Country]
    
    # Age groups (kept just in case needed downstream)
    gbd_breaks <- c(seq(20, 85, 5), Inf)
    gbd_labels <- c(paste0(seq(20, 80, 5), "-", seq(24, 84, 5)), "85plus")
    dt[, age_group := as.character(
      cut(age, breaks = gbd_breaks, labels = gbd_labels,
          right = FALSE, include.lowest = TRUE)
    )]
    
    #..........................................................
    # STEP 3: Merge statin baseline scenario
    #  We will OVERRIDE statins_uptake_delta with linear scale-up.
    #..........................................................
    
    dt <- merge(dt,
                dt_statin_scenarios[location == Country],
                by = c("location", "year"),
                all.x = TRUE)
    
    #..........................................................
    # STEP 4: Determine baseline statin coverage and increment
    #   baseline_statin_coverage: coverage at statin_start_year
    #   statin_target_coverage  : total coverage by statin_target_year
    #   incr_target             : additional coverage above baseline
    #..........................................................
    
    if (!is.null(baseline_statin_coverage)) {
      baseline_cov <- baseline_statin_coverage
    } else {
      baseline_cov <- dt[year == statin_start_year & cause == "ihd",
                         mean(statins_current, na.rm = TRUE)]
    }
    
    if (is.na(baseline_cov)) baseline_cov <- 0
    baseline_cov <- max(min(baseline_cov, 1), 0)
    
    incr_target <- max(statin_target_coverage - baseline_cov, 0)
    
    # Build linear scale-up in incremental coverage (relative to baseline)
    # Using the same helper as antihypertensives: coverage_t in [0, incr_target]
    dt[, statins_uptake_delta := calculate_coverage_by_year(
      year,
      start_year      = statin_start_year,
      target_year     = statin_target_year,
      target_coverage = incr_target
    )]
    
    # Set baseline coverage (current coverage used in denominator)
    dt[, statins_current := baseline_cov]
    
    # Before start year: ensure no additional coverage
    dt[year < statin_start_year, statins_uptake_delta := 0]
    
    # Safety: clamp to [0, 1]
    dt[is.na(statins_uptake_delta) | statins_uptake_delta < 0, statins_uptake_delta := 0]
    dt[statins_uptake_delta > 1, statins_uptake_delta := 1]
    
    #..........................................................
    # STEP 5: Merge attributable fractions
    #..........................................................
    
    dt <- merge(dt, dt_af_statins, by = c("location", "cause"), all.x = TRUE)
    
    dt[is.na(af_statins) & cause == "ihd",     af_statins := af_ihd]
    dt[is.na(af_statins) & cause == "istroke", af_statins := af_istroke]
    
    dt[, IR_0     := IR]
    dt[, CF_0     := CF]
    dt[, eff_ir_0 := eff_ir]
    dt[, eff_cf_0 := eff_cf]
    
    #..........................................................
    # STEP 6: Compute effect sizes for IR (CF do not use AF)
    #
    #   effect_size = AF × 1-RR × Δcoverage × adherence
    #                  ______________________________
    #                  (1 − 1-RR × baseline_coverage × adherence)
    #
    # where Δcoverage = statins_uptake_delta and baseline_coverage = statins_current
    #..........................................................
    
    dt[, `:=`(
      effect_size_cf = fcase(
        cause == "ihd",
        (1- rr_cf_ihd) * statins_uptake_delta * adherence_cf /
          (1 - (1-rr_cf_ihd) * (statins_current * adherence_cf)),
        
        cause == "istroke",
        prop_athero_stroke * (1-rr_cf_istroke) *
          statins_uptake_delta * adherence_cf /
          (1 - (1-rr_cf_istroke) * (statins_current * adherence_cf)),
        
        default = NA_real_
      ),
      
      effect_size_ir = fcase(
        cause == "ihd",
        af_statins * (1-rr_ir_ihd) * (statins_uptake_delta * adherence_ir) /
          (1 - (1-rr_ir_ihd) * (statins_current * adherence_ir)),
        
        cause == "istroke",
        af_statins * (1-rr_ir_istroke) * statins_uptake_delta * adherence_ir /
          (1 - (1-rr_ir_istroke) * (statins_current * adherence_ir)),
        
        default = NA_real_
      )
    )]
    
    # If baseline already >= target, incr_target == 0 → effect_size_* == 0 automatically
    # but just in case numerical noise:
    if (incr_target == 0 || baseline_cov >= statin_target_coverage) {
      dt[, `:=`(effect_size_ir = 0, effect_size_cf = 0)]
    }
    
    #..........................................................
    # STEP 7: Apply statin effects ONLY for adults ≥40
    #   CF_new = CF × (1 − effect_size_cf)
    #   IR_new = IR × (1 − effect_size_ir)
    #..........................................................
    
    dt[age >= 40 & cause %in% c("ihd", "istroke"),
       `:=`(
         CF = CF * (1 - effect_size_cf),
         IR = IR * (1 - effect_size_ir)
       )]
    
    #..........................................................
    # STEP 8: Update effect ratios for tracking
    #..........................................................
    
    dt[!is.na(effect_size_ir), eff_ir := eff_ir_0 * (1 - effect_size_ir)]
    dt[!is.na(effect_size_cf), eff_cf := eff_cf_0 * (1 - effect_size_cf)]
    
    dt[, c("statins_uptake", "statins_target",
           "statins_uptake_lag", "statins_uptake_delta_lag",
           "CF_0", "IR_0", "eff_ir_0", "eff_cf_0",
           "effect_size_ir", "effect_size_cf",
           "af_statins", "age_group") := NULL]
    
    # keep statins_current & statins_uptake_delta if you want diagnostics
    # otherwise you can also drop them:
    # dt[, c("statins_current", "statins_uptake_delta") := NULL]
    
    setorder(dt, year, sex, location, cause, age)
    
    if (dt[, any(is.na(CF))] || dt[, any(is.na(IR))]) {
      stop("Computation produced NA values in CF or IR.", call. = FALSE)
    }
    
    cat("    Baseline statin coverage:", round(baseline_cov, 3), "\n")
    cat("    Target coverage:", round(statin_target_coverage, 3),
        "by", statin_target_year, "\n")
    
    dt[]
  }
  
  #...........................................................
  # Model. Project.all function ----
  #...........................................................
  # 
  
  # #Test
  # Country <-"China"
  # drugcov <- "p75"
  #
  # baseline_ctrl  <- 0.1585683
  # #baseline_ctrl  <- 0
  # target_control <- 0.5
  #
  # control_start_year  <- 2025
  # control_target_year <- 2030
  #
  # coverage_0 <- baseline_ctrl
  # target_year <- control_target_year
  # start_year <- control_start_year

  # baseline_statin_coverage <- 0.04737402
  # statin_target_coverage <- 0.60
  # statin_start_year      <- 2025
  # statin_target_year     <- 2050
  # adherence <- 1
  # prop_athero_stroke <- 0.6
  
  # Build immutable country inputs once for the six scenario projections.
  prepare_country_context <- function(Country, make_bp_baseline = TRUE) {
    base_rates <- b_rates[location == Country & year >= 2017]
    DT <- unique(data.in[location == Country][, Year := 2017][,
                                                              -c("Lower95", "Upper95")])
    DT.in <- as.data.table(left_join(
      DT[rep(seq_len(nrow(DT)), 34L)][, Year := repYear(.I)],
      inc %>% select(-location),
      by = c("iso3", "Year")
    ))
    DT.in[, c("aroc", "aroc2", "p_change", "p_change2", "a_change",
              "a_change2", "ideal", "drugaroc") := 0]
    bp_baseline <- diabetes_age <- NULL
    if (make_bp_baseline) {
      baseline_rates <- copy(base_rates)
      baseline_rates[, `:=`(eff_ir = 1, eff_cf = 1,
                            intervention = "baseline")]
      bp_prob <- get.bp.prob(copy(DT.in), rx = 0, drugaroc = "baseline")
      bp_baseline <- calculate_baseline_incidence_gbd(
        copy(bp_prob), baseline_rates, Country, dt_gbd_rr
      )
      diabetes_age <- expand_to_single_year_ages(copy(DT.in))
      diabetes_age <- unique(diabetes_age[, .(
        location, Year, age, sex, bp_cat,
        diabetes_share_among_htn_assumed_age_specific = diabetes
      )])
      setnames(diabetes_age, "Year", "year")
    }
    list(base_rates = base_rates, DT.in = DT.in,
         bp_baseline = bp_baseline, diabetes_age = diabetes_age)
  }
  
  project.all <- function(Country,
                          interventions = c("antihypertensive_no_diabetes",
                                            "antihypertensive_diabetes",
                                            "statins"),
                          # Aim 2 BP-control targets are selected by scenario_id from
                          # the long table produced by 04_define_interventions.R.
                          htn_scenario_id,
                          dt_hbp_targets,
                          # explicit Statins params
                          statin_target_coverage,
                          statin_start_year,
                          statin_target_year,
                          adherence_ir = adherence_ir,
                          adherence_cf = adherence_cf,
                          # implicit statins parameter
                          baseline_statin_coverage = NULL,
                          country_context = NULL
  ) {
    
    cat("\n========================================\n")
    cat("STARTING PROJECTION FOR:", Country, "\n")
    cat("Interventions:", paste(interventions, collapse = ", "), "\n")
    cat("========================================\n\n")
    
    # Validate interventions (mutually exclusive BP subgroups replace the old
    # "antihypertensive" / "antihypertensive_diabetes" broken pair).
    valid_interventions <- c(
      "antihypertensive_no_diabetes", "antihypertensive_diabetes",
      "statins"
    )
    if (!all(interventions %in% valid_interventions)) {
      stop("Invalid intervention(s). Must be one or more of: ",
           paste(valid_interventions, collapse = ", "))
    }
    
    # Accept a shared context for batch runs; preserve stand-alone calls.
    if (is.null(country_context)) {
      country_context <- prepare_country_context(
        Country, make_bp_baseline = any(c(
          "antihypertensive_no_diabetes", "antihypertensive_diabetes"
        ) %in% interventions)
      )
    }
    base_rates <- country_context$base_rates
    DT.in <- country_context$DT.in
    
    #...............................................................
    # Aim 2 BP-control subgroup targets (long scenario table, by scenario_id).
    # Fetched ONLY when a BP subgroup intervention is active; script 06 does not
    # recalculate the 150M allocation or re-infer a diabetes baseline.
    bp_interventions <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
    has_bp <- any(bp_interventions %in% interventions)
    htn_target_rows <- NULL
    if (has_bp) {
      if (is.null(dt_hbp_targets)) {
        stop("The long Aim 2 HTN target table is required for BP interventions.")
      }
      htn_target_rows <- dt_hbp_targets[
        location == Country & scenario_id == htn_scenario_id
      ]
      if (!nrow(htn_target_rows)) {
        stop("No Aim 2 target rows for ", Country, " / ", htn_scenario_id, ".")
      }
      validate_htn_target_table(htn_target_rows)
      cat("  HTN scenario:", htn_scenario_id, "\n")
    }
    
    #...............................................................
    # Baseline for statins intervention
    if (!is.null(baseline_statin_coverage)) {
      baseline_statin_cov <- baseline_statin_coverage
    } else {
      baseline_statin_cov <- dt_statin_scenarios[
        location == Country & year == 2024,
        mean(statins_current, na.rm = TRUE)
      ]
    }
    baseline_statin_cov <- max(min(baseline_statin_cov, 1), 0)
    
    #...............................................................
    # Initialize baseline scenario
    intervention_rates <- copy(base_rates)
    intervention_rates[, `:=`(
      eff_ir = 1,
      eff_cf = 1,
      intervention = "baseline"
    )]
    
    # Store baseline for combining BP-related interventions
    intervention_rates_bau <- copy(intervention_rates)
    
    # Track which interventions have been applied (human-readable labels that the
    # downstream reports 07_output_dalys.R / aim1_report.Rmd already recognize).
    applied_interventions <- character()
    intervention_label <- "baseline"
    
    # Store individual (independent) intervention effects for multiplicative combination
    intervention_effects <- list()
    
    #..................................
    ## Apply mutually exclusive antihypertensive interventions ----
    ## The two BP subgroups are combined ADDITIVELY as a population mixture INSIDE
    ## calculate_antihypertensive_split() -> a SINGLE eff_ir_bp / eff_cf_bp pair
    ## (no multiplicative cross-term between the subgroups).
    #..................................
    if (has_bp) {
      cat("\n=== Applying split-population antihypertensive therapy ===\n")
      intervention_rates_drug <- calculate_antihypertensive_split(
        intervention_rates = intervention_rates_bau,
        Country = Country,
        DT.in = DT.in,
        dt_gbd_rr = dt_gbd_rr,
        target_rows = htn_target_rows,
        bp_baseline = country_context$bp_baseline,
        diabetes_age_prepared = country_context$diabetes_age,
        active_no_diabetes =
          "antihypertensive_no_diabetes" %in% interventions,
        active_diabetes = "antihypertensive_diabetes" %in% interventions
      )
      intervention_effects[["antihypertensive_split"]] <-
        intervention_rates_drug[, .(
          age, sex, location, cause, year,
          eff_ir_bp = eff_ir, eff_cf_bp = eff_cf
        )]
      if ("antihypertensive_no_diabetes" %in% interventions) {
        applied_interventions <- c(applied_interventions, "BP")
      }
      if ("antihypertensive_diabetes" %in% interventions) {
        applied_interventions <- c(applied_interventions, "BP_diabetes")
      }
    }
    
    #..................................
    ## Combine BP-related interventions (Antihypertensive split) ----
    ## The two BP subgroups were already combined additively inside the split.
    #..................................
    if (length(intervention_effects) > 0) {
      cat("\n=== Combining BP-related intervention effects ===\n")
      
      intervention_rates <- copy(intervention_rates_bau)
      
      for (int_name in names(intervention_effects)) {
        intervention_rates <- merge(
          intervention_rates,
          intervention_effects[[int_name]],
          by = c("age", "sex", "location", "cause", "year"),
          all.x = TRUE
        )
      }
      
      if (has_bp) {
        intervention_rates[, `:=`(
          eff_ir = eff_ir_bp,
          eff_cf = eff_cf_bp
        )]
      }
      
      # Handle missing values (e.g., small countries / unmatched rows)
      intervention_rates[is.na(eff_cf), eff_cf := 1]
      intervention_rates[is.na(eff_ir), eff_ir := 1]
      
      # Apply combined effects to rates
      intervention_rates[, `:=`(
        CF = CF * eff_cf,
        IR = IR * eff_ir
      )]
      
      # Clean up temporary effect columns
      effect_cols <- grep("^eff_(ir|cf)_bp$", names(intervention_rates), value = TRUE)
      intervention_rates[, (effect_cols) := NULL]
      
      cat("  Combined effects applied to CF and IR\n")
    }
    
    #..................................
    ## Apply Statins Intervention ----
    #..................................
    if ("statins" %in% interventions) {
      cat("\n=== Applying Statins Intervention ===\n")
      
      intervention_rates <- calculate_statins_impact(
        dt_statin_scenarios,
        intervention_rates,
        Country,
        dt_af_statins,
        adherence_ir = adherence_ir,
        adherence_cf = adherence_cf,
        prop_athero_stroke     = 0.60,
        statin_target_coverage = statin_target_coverage,
        statin_start_year      = statin_start_year,
        statin_target_year     = statin_target_year,
        baseline_statin_coverage  = baseline_statin_cov
      )
      
      applied_interventions <- c(applied_interventions, "Statins")
    }
    
    # Create intervention label
    if (length(applied_interventions) > 0) {
      intervention_label <- paste(applied_interventions, collapse = " + ")
    }
    intervention_rates[, intervention := intervention_label]
    
    #..................................
    ## Initial States ----
    #..................................
    cat("\n=== Setting Initial Population States ===\n")
    
    intervention_rates[year == 2017 | age == 20, `:=`(
      sick = Nx * PREVt0,
      dead = Nx * DIS.mx.t0,
      well = Nx * (1 - (PREVt0 + BG.mx)),
      pop = Nx,
      all.mx = Nx * DIS.mx.t0 + Nx * BG.mx
    )]
    
    intervention_rates[CF > 0.99, CF := 0.99]
    intervention_rates[IR > 0.99, IR := 0.99]
    
    setorder(intervention_rates, sex, location, cause, age)
    
    #..................................
    ## STATE TRANSITIONS ----
    #..................................
    cat("\n=== Running State Transition Model ===\n")
    cat("Projecting from 2017 to 2058...\n")
    
    for(i in 1:41) {
      if (i %% 10 == 0) cat("  Year", 2017 + i, "\n")
      
      b2 <- intervention_rates[year <= 2017 + i & year >= 2017 + i - 1]
      b2[, age2 := age + 1]
      
      b2[, newcases2 := shift(well) * IR,
         by = .(sex, location, cause, age, intervention)]
      
      b2[, sick2 := shift(sick) * (1 - (CF + BG.mx + covid.mx)) + shift(well) * IR,
         by = .(sex, location, cause, age, intervention)]
      b2[sick2 < 0, sick2 := 0]
      
      b2[, dead2 := shift(sick) * CF,
         by = .(sex, location, cause, age, intervention)]
      b2[dead2 < 0, dead2 := 0]
      
      b2[, pop2 := shift(pop) - shift(all.mx),
         by = .(sex, location, cause, age, intervention)]
      b2[pop2 < 0, pop2 := 0]
      
      b2[, all.mx2 := sum(dead2),
         by = .(sex, location, year, age, intervention)]
      b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all) + (pop2 * covid.mx)]
      b2[all.mx2 < 0, all.mx2 := 0]
      
      b2[, well2 := pop2 - all.mx2 - sick2]
      b2[well2 < 0, well2 := 0]
      
      b2 <- b2[year == 2017 + i & age2 < 96,
               .(age2, newcases2, sick2, dead2, well2, pop2, all.mx2,
                 sex, location, cause, intervention)]
      setnames(b2, "age2", "age")
      
      intervention_rates[year == 2017 + i & age > 20, `:=`(
        newcases = b2$newcases2,
        sick = b2$sick2,
        dead = b2$dead2,
        well = b2$well2,
        pop = b2$pop2,
        all.mx = b2$all.mx2
      )]
    }
    
    cat("\n=== Projection Complete ===\n")
    cat("Final intervention label:", intervention_label, "\n\n")
    
    out.df <- intervention_rates[, .(
      age, cause, sex, year, well, sick, newcases,
      dead, pop, all.mx, intervention, location, eff_ir, eff_cf
    )]
    
    return(out.df)
  }
  
  # #...........................................................
  # # Checking inputs ----
  # #...........................................................
  
  # # Check location names (the key to merge))
  
  b_rates[CF>=1, CF:=0.99]
  b_rates[IR>=1, IR:=0.99]
  b_rates[CF<0, CF:=0]
  b_rates[IR<0, IR:=0]
  
  # #...........................................................
  # # Example Usage ----
  # #...........................................................
  # 
  # # Run just antihypertensive therapy
  # results_bp_only <- project.all(
  #   Country = "China",
  #   interventions = c("antihypertensive"),
  #   drugcov = "p75"
  # )
  # 
  # # Run antihypertensive + statins
  # results_bp_statins <- project.all(
  #   Country = "China",
  #   interventions = c("antihypertensive", "statins"),
  #   drugcov = "p75"
  # )
  # #
  # # # Run baseline (no interventions) - useful for comparison
  # results_baseline <- project.all(
  #   Country = "China",
  #   interventions = character(0)  # Empty vector = no interventions
  # )
  
  #...........................................................
  ## Batch Runner for Multiple Scenarios ----
  #...........................................................
  
  run_multiple_scenarios <- function(Country,
                                     scenario_list,
                                     htn_scenario_ids,
                                     dt_hbp_targets,
                                     statin_target_coverage,
                                     statin_start_year,
                                     statin_target_year,
                                     adherence_ir = 1,
                                     adherence_cf = 1,
                                     baseline_statin_coverage = NULL) {

    results <- vector("list", length(scenario_list))
    names(results) <- names(scenario_list)
    country_context <- prepare_country_context(
      Country, make_bp_baseline = any(vapply(scenario_list, function(scenario) {
        any(c("antihypertensive_no_diabetes",
              "antihypertensive_diabetes") %in% scenario)
      }, logical(1)))
    )
    
    for (scenario_name in names(scenario_list)) {
      cat("\n##########################################\n")
      cat("RUNNING SCENARIO:", scenario_name, "\n")
      cat("##########################################\n")
      
      results[[scenario_name]] <- project.all(
        Country             = Country,
        interventions       = scenario_list[[scenario_name]],
        htn_scenario_id     = htn_scenario_ids[[scenario_name]],
        dt_hbp_targets      = dt_hbp_targets,
        statin_target_coverage   = statin_target_coverage,
        statin_start_year        = statin_start_year,
        statin_target_year       = statin_target_year,
        adherence_ir             = adherence_ir,
        adherence_cf             = adherence_cf,
        baseline_statin_coverage = baseline_statin_coverage,
        country_context = country_context
      )
    }
    
    combined_results <- rbindlist(results, idcol = "scenario")
    return(combined_results)
  }
  
  # Scenario bundles for the multi-intervention runner. BP entries use the
  # mutually-exclusive subgroup interventions; bp_combined is the 150M policy
  # scenario (neither "only" scenario alone achieves the full 150M target).
  scenarios <- list(
    baseline            = character(0),
    bp_no_diabetes_only = "antihypertensive_no_diabetes",
    bp_diabetes_only    = "antihypertensive_diabetes",
    bp_combined         = c("antihypertensive_no_diabetes",
                            "antihypertensive_diabetes"),
    statins_only        = "statins",
    all_interventions   = c("antihypertensive_no_diabetes",
                            "antihypertensive_diabetes",
                            "statins")
  )
  
  # Map each scenario to the long-table scenario_id whose BP targets it consumes.
  # baseline / statins_only have no BP intervention, so their id is unused (any
  # valid id works); all_interventions consumes the bp_combined BP targets.
  htn_scenario_ids <- list(
    baseline            = "baseline",
    bp_no_diabetes_only = "bp_no_diabetes_only",
    bp_diabetes_only    = "bp_diabetes_only",
    bp_combined         = "bp_combined",
    statins_only        = "baseline",
    all_interventions   = "bp_combined"
  )
  
  #...........................................................
  ## Comparison Helper Functions ----
  #...........................................................
  
  compare_scenarios <- function(results_dt, 
                                metric = "dead",
                                years = c(2030, 2040, 2050),
                                reference_scenario = "baseline") {
    #' Compare outcomes across scenarios
    #' 
    #' @param results_dt Data.table with results from run_multiple_scenarios()
    #' @param metric Character, which metric to compare (dead, newcases, sick, etc.)
    #' @param years Numeric vector, which years to compare
    #' @param reference_scenario Character, scenario to use as reference
    #' 
    #' @return Data.table with comparisons
    
    comparison <- results_dt[year %in% years, 
                             .(total = sum(get(metric))),
                             by = .(scenario, year, intervention)]
    
    if (reference_scenario %in% comparison$scenario) {
      ref_values <- comparison[scenario == reference_scenario, 
                               .(year, intervention, ref_total = total)]
      
      comparison <- merge(comparison, ref_values, 
                          by = c("year", "intervention"), 
                          all.x = TRUE)
      
      comparison[, `:=`(
        absolute_difference = total - ref_total,
        percent_change = (total - ref_total) / ref_total * 100,
        averted = ref_total - total
      )]
    }
    
    setorder(comparison, year, scenario)
    return(comparison)
  }
  
  # # Example usage:
  # deaths_comparison <- compare_scenarios(
  #   all_results,
  #   metric = "dead",
  #   years = c(2030, 2040, 2050),
  #   reference_scenario = "baseline"
  # )
  
  calculate_cumulative_impact <- function(results_dt,
                                          metric = "dead",
                                          start_year = 2025,
                                          end_year = 2050) {
    #' Calculate cumulative impact over time period
    #' 
    #' @param results_dt Data.table with results from run_multiple_scenarios()
    #' @param metric Character, which metric to sum
    #' @param start_year Numeric, starting year
    #' @param end_year Numeric, ending year
    #' 
    #' @return Data.table with cumulative totals and differences vs baseline
    
    # Compute cumulative totals
    cumulative <- results_dt[year >= start_year & year <= end_year,
                             .(cumulative_total = sum(get(metric))),
                             by = .(scenario, intervention)]
    
    # Get baseline value
    baseline_val <- cumulative[scenario == "baseline", cumulative_total]
    
    # Add difference columns
    cumulative[, diff_vs_baseline := abs(cumulative_total - baseline_val)]
    cumulative[, diff_pct_vs_baseline := abs(100 * (cumulative_total - baseline_val) / baseline_val)]
    
    # Order output
    setorder(cumulative, scenario)
    
    return(cumulative)
  }
  
  # # # Example:
  # cumulative_deaths <- calculate_cumulative_impact(
  #   all_results,
  #   metric = "dead",
  #   start_year = 2025,
  #   end_year = 2050
  # )
  
  
  #...........................................................
  ## Validation Helper ----
  #...........................................................
  
  validate_intervention_results <- function(results_dt) {
    #' Run basic validation checks on results
    #' 
    #' @param results_dt Data.table with model results
    #' 
    #' @return List with validation results and any issues found
    
    issues <- list()
    
    # Check for negative values
    neg_cols <- c("well", "sick", "dead", "pop", "newcases")
    for (col in neg_cols) {
      if (results_dt[, any(get(col) < 0, na.rm = TRUE)]) {
        issues[[paste0("negative_", col)]] <- 
          results_dt[get(col) < 0, .(scenario, year, age, sex, cause, value = get(col))]
      }
    }
    
    # Check for NA values
    na_cols <- c("eff_ir", "eff_cf", "dead", "newcases")
    for (col in na_cols) {
      if (results_dt[, any(is.na(get(col)))]) {
        issues[[paste0("na_", col)]] <- 
          results_dt[is.na(get(col)), .(scenario, year, age, sex, cause)]
      }
    }
    
    # Check population consistency
    pop_check <- results_dt[, .(
      total_population = sum(well + sick, na.rm = TRUE),
      recorded_pop = sum(pop, na.rm = TRUE)
    ), by = .(scenario, year)]
    
    pop_check[, diff := abs(total_population - recorded_pop)]
    if (pop_check[, any(diff > 0.01 * recorded_pop)]) {
      issues[["population_mismatch"]] <- pop_check[diff > 0.01 * recorded_pop]
    }
    
    # Check that effects are bounded
    if (results_dt[, any(eff_ir < 0 | eff_ir > 2, na.rm = TRUE)]) {
      issues[["eff_ir_out_of_bounds"]] <- 
        results_dt[eff_ir < 0 | eff_ir > 2, .(scenario, year, age, cause, eff_ir)]
    }
    
    if (results_dt[, any(eff_cf < 0 | eff_cf > 2, na.rm = TRUE)]) {
      issues[["eff_cf_out_of_bounds"]] <- 
        results_dt[eff_cf < 0 | eff_cf > 2, .(scenario, year, age, cause, eff_cf)]
    }
    
    validation_result <- list(
      passed = length(issues) == 0,
      n_issues = length(issues),
      issues = issues
    )
    
    if (validation_result$passed) {
      cat("\n Ok All validation checks passed!\n")
    } else {
      cat("\n Not Ok Validation found", length(issues), "issue(s):\n")
      print(names(issues))
    }
    
    return(validation_result)
  }
  
  # Example:
  #validation <- validate_intervention_results(all_results)
  
  
  #...........................................................
  # Parallel execution across countries----
  #...........................................................
  
  #...........................................................
  ## Parameters ----
  #...........................................................
  # 1. Define your intervention parameters BEFORE starting cluster
  
  # 1. Define intervention parameters BEFORE starting the cluster. The scenarios /
  #    htn_scenario_ids objects are defined above (with run_multiple_scenarios).
  
  ## Statins explicit params (match calculate_statins_impact)
  # Statins adherence from Basios et al 2025  
  # https://academic.oup.com/eurjpc/advance-article/doi/10.1093/eurjpc/zwaf769/8381680
  
  statin_target_coverage <- 0.50
  statin_start_year      <- 2026
  statin_target_year     <- 2030
  adherence_ir <-  0.575
  adherence_cf <- 0.644
  
  # Validate the long Aim 2 target table and confirm every BP scenario_id we need
  # is present. Targets come from 04_define_interventions.R; script 06 never
  # recalculates the 150M allocation or re-infers a diabetes baseline.
  validate_htn_target_table(dt_hbp_targets)
  needed_ids  <- unique(unlist(htn_scenario_ids))
  missing_ids <- setdiff(needed_ids, unique(dt_hbp_targets$scenario_id))
  if (length(missing_ids)) {
    stop("Long HTN target table is missing scenario_id values: ",
         paste(missing_ids, collapse = ", "))
  }
  
  # 2. Detect and start cluster
  ncores <- 10
  cl     <- makeCluster(ncores)
  registerDoParallel(cl)
  
  clusterExport(
    cl,
    varlist = c(
      "project.all",
      "prepare_country_context",
      "run_multiple_scenarios",
      "get.bp.prob",
      "get_gbd_relative_risks",
      "expand_to_single_year_ages",
      "calculate_baseline_incidence_gbd",
      "calculate_etihad_cumulative_rr",
      "calculate_coverage_by_year",
      "add_coverage_by_year",
      "calculate_aggregate_coverage",
      "apply_coverage_adjustment",
      "aim2_control_trajectory",
      "aim2_incremental_effect",
      "aim2_subgroup_incidence_multiplier",
      "validate_htn_target_table",
      "calculate_antihypertensive_split",
      "calculate_statins_impact",
      "repYear",
      "data.in",
      "b_rates",
      "inc",
      "dt_hbp_targets",
      "dt_gbd_rr",
      "ETIHAD_RR",
      "ETIHAD_RR_BIN",
      "dt_statin_scenarios",
      "dt_af_statins",
      "scenarios",
      "htn_scenario_ids",
      "wd_outp"
    ),
    envir = globalenv()
  )
  
  clusterEvalQ(cl, {
    library(data.table)
    library(dplyr)
    setDTthreads(1L)  # one data.table thread per country worker
  })
  
  # 3. Model countries: intersect model inputs with the validated policy target
  #    table. Locations absent from the target table are the explicit Aim 2
  #    exclusions / missing-input locations recorded by script 04.
  model_locs  <- unique(data.in$location)
  target_locs <- unique(dt_hbp_targets$location)
  locs <- intersect(model_locs, target_locs)
  locs <- locs[!locs %in% c("Greenland", "Bermuda")]  # legacy model exclusions
  excluded_from_jobs <- setdiff(model_locs, locs)
  # Use options(who_cvd.pilot_country = "Colombia") to test one complete
  # country before committing to the full batch; default runs every country.
  pilot_country <- getOption("who_cvd.pilot_country", NULL)
  if (!is.null(pilot_country)) {
    if (length(pilot_country) != 1L || !pilot_country %in% locs) {
      stop("who_cvd.pilot_country must identify one eligible country.")
    }
    locs <- pilot_country
  }
  if (length(excluded_from_jobs)) {
    cat("Locations excluded from Aim 1 jobs (no BP target / legacy exclusion):",
        paste(excluded_from_jobs, collapse = ", "), "\n")
  }
  
  #...........................................................
  ## Parallel execution: one job per country, all scenarios ----
  #...........................................................
  
  time_start <- Sys.time()
  
  results_list <- foreach(
    country        = locs,
    .packages      = c("data.table", "dplyr"),
    .errorhandling = "pass",
    .verbose       = TRUE
  ) %dopar% {
    
    log_file <- file.path(
      wd_outp, "out_model",
      paste0("log_", country, ".txt")
    )
    sink(log_file, split = FALSE)
    
    cat("\n==============================\n")
    cat("Country :", country, "\n")
    cat("Time    :", as.character(Sys.time()), "\n")
    cat("==============================\n")
    
    country_error <- NULL
    res <- tryCatch({
      run_multiple_scenarios(
        Country             = country,
        scenario_list       = scenarios,
        htn_scenario_ids    = htn_scenario_ids,
        dt_hbp_targets      = dt_hbp_targets,
        statin_target_coverage   = 0.50,
        statin_start_year        = 2026,
        statin_target_year       = 2030,
        adherence_ir             = 0.575,
        adherence_cf             = 0.644,
        baseline_statin_coverage = NULL
      )
    }, error = function(e) {
      country_error <<- conditionMessage(e)
      cat("ERROR in", country, ":", country_error, "\n")
      return(NULL)
    })
    
    if (!is.null(res)) {
      # Single Aim 1 target regime (150M BP-control split from script 04). Kept as
      # a column because 07_output_dalys.R and 08_economic_value_calculation.R
      # require htn_target_scenario as a group/join key.
      res[, htn_target_scenario := "aim1"]
      
      output_file <- file.path(
        wd_outp, "out_model",
        paste0("model_output_", country, ".rds")
      )
      saveRDS(res, file = output_file)
      cat("Saved:", output_file, "\n")
    } else {
      cat("No results to save for", country, "\n")
    }
    
    sink()
    list(ok = !is.null(res), error = country_error)
  }
  
  time_end <- Sys.time()
  cat("Total runtime:", round(difftime(time_end, time_start, units = "mins"), 1), "minutes\n")
  
  stopCluster(cl)
  
  # Check which countries succeeded
  successful <- vapply(results_list, function(result) {
    is.list(result) && isTRUE(result$ok)
  }, logical(1))
  cat("\nSuccessful runs:", sum(successful), "out of", length(locs), "\n")
  if (any(!successful)) {
    cat("Failed countries:", paste(locs[!successful], collapse = ", "), "\n")
    failed_indices <- which(!successful)
    for (idx in head(failed_indices, 5L)) {
      result <- results_list[[idx]]
      message <- if (inherits(result, "error")) {
        conditionMessage(result)
      } else if (is.list(result) && !is.null(result$error)) {
        result$error
      } else {
        "No error message returned; inspect the country log."
      }
      cat("  ", locs[idx], ": ", message, "\n", sep = "")
    }
  }
  
  # Combine all results (if not too large)
  #all_results <- rbindlist(results_list, fill = TRUE)
  
  #...........................................................
  # Cleaning up the workspace ----
  #...........................................................
  
  rm(list = ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
  suppressWarnings(rm(list = intersect(c("is", "bpcats", "locs", "i", "time1", "time2"), ls())))
  
}
