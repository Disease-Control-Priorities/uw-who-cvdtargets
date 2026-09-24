# =============================================================================
# 091_psa_parameters.R -- Aim 1 PSA: parameter register, draws, Sheet1 rebuild
# =============================================================================
# Sourced by 09_uncertainty_psa.R (and tests/test_aim1_psa.R). Defines functions
# and constants only; sourcing it has no side effects.
#
# Primary BP efficacy source: BPLTTC 2021, Appendix S6 (HR per 5 mmHg SBP
# reduction by baseline-SBP stratum), read from the "Appendix S6" sheet of
# data/processed/ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx. Despite its
# name, 06's ETIHAD_RR_BIN is Sheet1 of that BPLTTC workbook. Sheet1 column C
# holds hard-coded HR5^2 values (RR per 10 mmHg); columns D-G are live Excel
# formulas (cumulative products over traversed BP bins). The PSA samples ONLY the
# 15 Appendix S6 source HRs actually traversed by those formulas, then rebuilds
# the Sheet1 derived columns for every draw with the same formulas.
#
# Structural assumption carried over unchanged from 06 (NOT established by the
# source article): Appendix S6 strata are baseline-SBP subgroup treatment
# effects, yet Sheet1 multiplies them as if they were successive 10-mmHg
# increments. Alternative constructions belong in separate structural scenarios,
# not inside this PSA.

#...........................................................
# Constants ----
#...........................................................

PSA_BP_WORKBOOK     <- "ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx"
PSA_BP_SODIUM_CSV   <- "ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv"
PSA_S6_SHEET        <- "Appendix S6"
PSA_SHEET1          <- "Sheet1"

# 95% CI -> SE on the log scale, as specified: (log U - log L) / 3.92.
PSA_CI_DIVISOR      <- 3.92
PSA_CI_LEVEL        <- 0.95

PSA_S6_OUTCOMES <- c("Major cardiovascular events", "Stroke",
                     "Ischaemic heart disease", "Heart failure",
                     "Cardiovascular death", "All-cause death")
PSA_S6_STRATA   <- c("<120", "120-129", "130-139", "140-149",
                     "150-159", "160-169", ">=170")

# The 15 source parameters traversed by the Sheet1 formulas for hypertensive bins.
PSA_SAMPLED_OUTCOMES <- c("Ischaemic heart disease", "Stroke", "Heart failure")
PSA_SAMPLED_STRATA   <- c("130-139", "140-149", "150-159", "160-169", ">=170")

PSA_BP_BINS  <- c("<120", "120-129", "130-139", "140-149",
                  "150-159", "160-169", "170-179", "180+")
PSA_HYP_BINS <- c("140-149", "150-159", "160-169", "170-179", "180+")

# Model cause -> Appendix S6 outcome, exactly as Sheet1 column C is built:
# Stroke is shared by istroke and hstroke (hstroke is a subtype extrapolation);
# Heart failure is shared by hhd and by the inactive aod placeholder rows
# (workbook C34:C41 = +C2:C9; comment on A34: "Dementia is not been modeled").
PSA_CAUSE_OUTCOME <- c(hhd = "Heart failure", hstroke = "Stroke",
                       ihd = "Ischaemic heart disease", istroke = "Stroke",
                       aod = "Heart failure")
PSA_SHEET1_CAUSES <- names(PSA_CAUSE_OUTCOME)   # workbook row order

# Model BP bin -> Appendix S6 stratum (>=170 shared by 170-179 and 180+).
PSA_BIN_STRATUM <- c("<120" = "<120", "120-129" = "120-129",
                     "130-139" = "130-139", "140-149" = "140-149",
                     "150-159" = "150-159", "160-169" = "160-169",
                     "170-179" = ">=170", "180+" = ">=170")

# Legacy 10-mmHg CSV (sodium pathway) names IHD "Coronary heart disease".
PSA_SODIUM_CSV_OUTCOME <- c("Coronary heart disease" = "Ischaemic heart disease")

psa_param_id <- function(outcome, stratum) {
  o <- c("Major cardiovascular events" = "MCE", "Stroke" = "STROKE",
         "Ischaemic heart disease" = "IHD", "Heart failure" = "HF",
         "Cardiovascular death" = "CVDEATH", "All-cause death" = "ACDEATH")[outcome]
  s <- c("<120" = "LT120", "120-129" = "120_129", "130-139" = "130_139",
         "140-149" = "140_149", "150-159" = "150_159", "160-169" = "160_169",
         ">=170" = "GE170")[stratum]
  unname(paste0("S6_", o, "_", s))
}

#...........................................................
# Source tables ----
#...........................................................

psa_read_appendix_s6 <- function(wd_data) {
  f  <- file.path(wd_data, PSA_BP_WORKBOOK)
  s6 <- as.data.table(readxl::read_excel(f, sheet = PSA_S6_SHEET))
  need <- c("Cause", "SBP_Category", "HR_per_5mmHg", "CI_lower_5mmHg",
            "CI_upper_5mmHg", "RR_per_10mmHg", "CI_lower_10mmHg",
            "CI_upper_10mmHg", "Source")
  miss <- setdiff(need, names(s6))
  if (length(miss)) stop("Appendix S6 sheet is missing: ", paste(miss, collapse = ", "))
  s6[, source_row := .I + 1L]                    # Excel row (header in row 1)
  setnames(s6, c("Cause", "SBP_Category"), c("outcome", "stratum"))
  if (nrow(s6) != 42L ||
      !setequal(s6$outcome, PSA_S6_OUTCOMES) || !setequal(s6$stratum, PSA_S6_STRATA) ||
      anyDuplicated(s6, by = c("outcome", "stratum"))) {
    stop("Appendix S6 no longer has the expected 6 outcomes x 7 strata = 42 rows.")
  }
  num <- c("HR_per_5mmHg", "CI_lower_5mmHg", "CI_upper_5mmHg")
  if (anyNA(s6[, ..num]) || s6[, any(CI_lower_5mmHg <= 0 | CI_lower_5mmHg > HR_per_5mmHg |
                                     CI_upper_5mmHg < HR_per_5mmHg)]) {
    stop("Appendix S6 HR/CI values are missing or not ordered lower <= HR <= upper.")
  }
  # The sheet's per-10-mmHg columns are derived (=HR5^2); confirm, never sample them.
  chk <- s6[, max(abs(c(RR_per_10mmHg - HR_per_5mmHg^2,
                        CI_lower_10mmHg - CI_lower_5mmHg^2,
                        CI_upper_10mmHg - CI_upper_5mmHg^2)))]
  if (chk > 1e-12) stop("Appendix S6 RR_per_10mmHg columns are not HR5^2 (max diff ", chk, ").")
  s6[, param_id := psa_param_id(outcome, stratum)]
  s6[, sampled := outcome %in% PSA_SAMPLED_OUTCOMES & stratum %in% PSA_SAMPLED_STRATA]
  if (sum(s6$sampled) != 15L) stop("Expected exactly 15 sampled Appendix S6 parameters.")
  s6[, `:=`(meanlog = log(HR_per_5mmHg),
            sdlog   = (log(CI_upper_5mmHg) - log(CI_lower_5mmHg)) / PSA_CI_DIVISOR)]
  setcolorder(s6, c("param_id", "outcome", "stratum", "sampled", "HR_per_5mmHg",
                    "CI_lower_5mmHg", "CI_upper_5mmHg", "meanlog", "sdlog"))
  s6[]
}

# Sheet1 exactly as 06 reads it (06o:485-486).
psa_read_sheet1 <- function(wd_data) {
  as.data.table(readxl::read_excel(file.path(wd_data, PSA_BP_WORKBOOK), sheet = PSA_SHEET1))
}

#...........................................................
# Parameter register ----
#...........................................................

# run_args: the executed 06 run arguments (psa_06_run_args()); the statin
# adherence row reports the values actually used. "06o:<line>" locations refer
# to 06_run_scenarios_multiple_optimized.R at commit fee6d22.
psa_parameter_register <- function(s6, run_args = NULL) {
  excl_reason <- function(outcome, stratum) {
    fcase(
      outcome %in% PSA_SAMPLED_OUTCOMES & stratum %in% c("<120", "120-129"),
      paste0("Stratum not traversed by the Sheet1 formulas (cumulative products ",
             "start at 130-139 for diabetes and 140-149 without diabetes); the ",
             "value sits in Sheet1 column C but never reaches an effect size used ",
             "by 06 (incremental effects are 0 in bins <140)."),
      outcome == "Major cardiovascular events",
      "Composite endpoint; not mapped to any modelled cause in Sheet1.",
      outcome == "Cardiovascular death",
      paste0("Mortality endpoint, not a conditional case-fatality effect; not ",
             "used by 06. BP case-fatality constants stay fixed."),
      outcome == "All-cause death",
      "All-cause mortality endpoint; not used by 06.",
      default = NA_character_)
  }
  maps_to <- function(outcome) {
    fcase(outcome == "Ischaemic heart disease", "ihd",
          outcome == "Stroke", "istroke; hstroke (subtype extrapolation)",
          outcome == "Heart failure", "hhd; aod (inactive placeholder rows)",
          default = "none")
  }
  bp <- s6[, .(
    param_id, family = "BP efficacy (BPLTTC 2021 Appendix S6)",
    status = fifelse(sampled, "SAMPLED", "EXCLUDED"),
    source = paste0("BPLTTC 2021 Appendix S6 (workbook sheet '", PSA_S6_SHEET,
                    "', row ", source_row, "; ", Source, ")"),
    source_location = paste0("data/processed/", PSA_BP_WORKBOOK),
    outcome, stratum, estimate = HR_per_5mmHg,
    ci_lower = CI_lower_5mmHg, ci_upper = CI_upper_5mmHg, ci_level = PSA_CI_LEVEL,
    scale = "HR per 5 mmHg SBP reduction",
    distribution = fifelse(sampled, "log-normal: log(HR5) ~ N(meanlog, sdlog^2)", NA_character_),
    meanlog = fifelse(sampled, meanlog, NA_real_),
    sdlog = fifelse(sampled, sdlog, NA_real_),
    transformation = fifelse(sampled,
      paste0("sdlog = (log(U) - log(L)) / ", PSA_CI_DIVISOR, "; RR10 = HR5^2; ",
             "Sheet1 D/F cumulative products; effect = 1 - product"), NA_character_),
    maps_to = fifelse(sampled, maps_to(outcome), "none"),
    sharing = fifelse(sampled,
      paste0("One draw per iteration shared by all countries, ages, sexes, years ",
             "and scenarios", fifelse(stratum == ">=170",
             "; >=170 feeds both 170-179 and 180+", ""),
             fifelse(stratum == "130-139",
             "; 130-139 enters only the diabetes product (R_DM = RR10[130-139] x R_noDM)", ""),
             "; the same stratum draw feeds the diabetes and no-diabetes products"),
      NA_character_),
    correlation = fifelse(sampled,
      "Independent across the 15 parameters (working assumption; no covariance reported)",
      NA_character_),
    affected_equations = fifelse(sampled,
      paste0("Sheet1 cummulative_rr_(no)diabetes -> effect_size_(no)diabetes -> ",
             "aim2_incremental_effect -> aim2_subgroup_incidence_multiplier -> ",
             "IR_bin_new -> eff_ir (hypertensive bins; Well->Sick)"), NA_character_),
    injection_point = fifelse(sampled,
      paste0("psa_build_sheet1_draws() -> calculate_antihypertensive_split(",
             "etihad_rr_table = draw table) and psa_bp_eff_ir_batch(); the sodium ",
             "pathway receives the same draws via psa_build_sodium_rr10_draws()"),
      NA_character_),
    rationale = fifelse(sampled,
      "Endpoint (fatal + non-fatal events) matches the incidence transition used by 06.",
      excl_reason(outcome, stratum))
  )]

  fx <- function(param_id, family, status, estimate_text, source_location, rationale) {
    data.table(param_id, family, status, source = estimate_text,
               source_location, rationale)
  }
  fixed <- rbindlist(list(
    fx("BPCF_rho", "BP case-fatality reduction per unit incremental control",
       "FIXED", "rho: ihd 0.24, istroke 0.36, hstroke 0.76, hhd 0.20, aod 0.047",
       "06o:244-247 (cf_etihad in calculate_antihypertensive_split)",
       paste0("No endpoint-matched source with uncertainty. Appendix S6 ",
              "'Cardiovascular death' HRs are mortality endpoints, not conditional ",
              "case-fatality effects. Not a function of the sampled HRs.")),
    fx("STATIN_RR_IR_IHD", "Statin effect", "FIXED", "rr_ir_ihd = 0.74",
       "06o:1103", paste0("Equals the lower 99% limit of the CTT CHD RR quoted beside ",
       "it (0.80, 0.74-0.87), not a point estimate (review defect D7); no ",
       "endpoint-matched distribution. Architecture left unchanged.")),
    fx("STATIN_RR_IR_ISTROKE", "Statin effect", "FIXED", "rr_ir_istroke = 0.80",
       "06o:1104", paste0("No source in the repository. CTT diabetes stroke RR 0.79 ",
       "(0.67-0.93) is a different estimate and is not substituted.")),
    fx("STATIN_RR_CF_IHD", "Statin effect", "FIXED", "rr_cf_ihd = 0.80",
       "06o:1105", paste0("A coronary-death/event RR applied as a case-fatality ",
       "multiplier (estimand mismatch); cannot be sampled without an ",
       "endpoint-consistent decomposition.")),
    fx("STATIN_RR_CF_ISTROKE", "Statin effect", "FIXED", "rr_cf_istroke = 0.96",
       "06o:1106", paste0("Stroke-death RR 0.96 (0.84-1.09) is a mortality endpoint ",
       "applied as case fatality; an IHD effect must not be extrapolated to stroke ",
       "mortality.")),
    {
      a_ir <- if (is.null(run_args$adherence_ir)) 1 else run_args$adherence_ir
      a_cf <- if (is.null(run_args$adherence_cf)) 1 else run_args$adherence_cf
      full <- isTRUE(all.equal(c(a_ir, a_cf), c(1, 1)))
      fx("STATIN_ADHERENCE", "Statin adherence", "FIXED",
         sprintf("adherence_ir = %s, adherence_cf = %s in the executed run call", a_ir, a_cf),
         "run_multiple_scenarios() call inside the 06 %dopar% loop",
         if (full) paste0("Full adherence in the executed model, so the Basios et al. ",
                          "adherence prevalences (57.5%, 64.4%) are not modelled or sampled.")
         else paste0("Fixed at the executed values (Basios et al. 2025 adherence ",
                     "prevalences); no uncertainty distribution in the repository, not sampled."))
    },
    fx("STATIN_AF", "Statin attributable fraction", "FIXED",
       "af_statins.rds (GBD high-FPG PAF); defaults 0.1497 / 0.1161",
       "06o:1068, 1112-1113", "Excluded from this PSA by design (diabetes proxy)."),
    fx("STATIN_PROP_ATHERO", "Statin effect", "FIXED", "prop_athero_stroke = 0.60",
       "06o:1584", "Excluded from this PSA by design (atherosclerotic stroke fraction)."),
    fx("STATIN_POLICY", "Policy", "FIXED",
       "target coverage 0.50, start 2026, target year 2030; baseline statins_current (2024)",
       "06o:2128-2133", "Policy definition / baseline input."),
    fx("TFA_RR", "Trans-fat (legacy)", "FIXED",
       "RR per 1%E by age 1.21 ... 1.07; target 0 %E from 2028",
       "06o:999-1014, 2138-2139",
       paste0("No uncertainty source in the repository. Runs only inside ",
              "all_interventions, where it is applied to IHD case fatality (review ",
              "defect D2).")),
    fx("SODIUM", "Sodium (legacy)", "INACTIVE",
       "salteff = 0; 10-mmHg RRs from the BPLTTC CSV",
       "06o:330-393, 786-948, 2135",
       paste0("Inert by design, but its RR mapping leaves NA in 5 of 8 bins, so ",
              "eff_ir_salt is NA from 2026 and 06 resets the combined eff_ir to 1 ",
              "(review defect D1). The PSA passes the same S6 draws to this ",
              "pathway, and reproduces the defect exactly.")),
    fx("GBD_RR_10MMHG", "BP exposure-risk gradient", "FIXED",
       "GBD 2019 RR per 10 mmHg by age and cause",
       "data/raw/IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx",
       paste0("Baseline epidemiology (allocates incidence across BP bins); outside ",
              "this intervention-effect PSA. Candidate for an extended PSA.")),
    fx("BP_DISTRIBUTION", "Baseline", "FIXED", "Mean/SD SBP by country-age-sex (bp_data6.csv)",
       "data/processed/bp_data6.csv", "Baseline input; not an intervention effect."),
    fx("DIABETES_SHARE", "Baseline", "FIXED",
       "P(D|H) = population diabetes prevalence; 15-pp control gap",
       "code/04_define_interventions.R", "Baseline/structural assumption; not sampled."),
    fx("HTN_TARGETS", "Policy", "FIXED",
       "150M additional controlled by 2030; diabetes cap 0.80; linear 2025->2030",
       "data/processed/htn_control_targets_by_loc.csv", "Policy targets are not drawn."),
    fx("BASELINE_EPI", "Baseline", "FIXED",
       "IR, CF, BG.mx, prevalence, population, 80% CF trend, COVID excess",
       "code/05_build_baseline.R", "Baseline epidemiology/demography is not drawn."),
    fx("DW_LE", "Burden of disease", "FIXED",
       "GBD 2023 disability weights (YLD/prevalence); WPP 2024 life expectancy",
       "code/07_output_dalys.R", "Fixed reference inputs for YLD/YLL."),
    fx("ECONOMIC", "Economic valuation", "FIXED",
       "US VSL/GNI 160; elasticity 0.8/1.2 (primary), 1.0, 1.5; floor 20; r = 3%; base 2026",
       "code/08_economic_value_calculation.R",
       "Deterministic; e1_0/e1_5 remain deterministic sensitivity bounds."),
    fx("STRUCTURE_SHEET1", "Structural", "STRUCTURAL",
       "Cumulative multiplication of stratum RRs over traversed bins",
       "Sheet1 columns D-G",
       paste0("Structural modelling assumption reproduced unchanged; alternative ",
              "constructions are separate structural scenarios, not random switches.")),
    fx("STRUCTURE_MAPPING", "Structural", "STRUCTURAL",
       "Stroke -> istroke & hstroke; Heart failure -> hhd",
       "Sheet1 column C", "Outcome mapping reproduced unchanged.")
  ), fill = TRUE)
  reg <- rbindlist(list(bp, fixed), fill = TRUE)
  setcolorder(reg, c("param_id", "family", "status"))
  reg[]
}

#...........................................................
# Draws ----
#...........................................................

# Stable iteration identifiers: draw k uses the k-th row of a draw-major normal
# matrix, so draws 1..10 are identical whether n_draws is 10 or 10,000. Draw 0 is
# the deterministic point run (z = 0).
psa_generate_draws <- function(s6, n_draws, seed,
                               correlation_mode = c("independent",
                                                    "comonotone_within_stratum",
                                                    "comonotone_all")) {
  correlation_mode <- match.arg(correlation_mode)
  stopifnot(length(n_draws) == 1L, n_draws >= 0, n_draws == round(n_draws))
  par <- s6[sampled == TRUE][order(outcome, stratum)]
  k <- nrow(par)
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  set.seed(seed)
  z <- if (n_draws > 0) matrix(rnorm(n_draws * k), nrow = n_draws, ncol = k, byrow = TRUE)
       else matrix(numeric(0), 0, k)
  colnames(z) <- par$param_id
  if (correlation_mode == "comonotone_within_stratum") {
    for (s in unique(par$stratum)) {
      cols <- par[stratum == s, param_id]
      z[, cols] <- z[, cols[1L]]
    }
  } else if (correlation_mode == "comonotone_all") {
    z[] <- z[, 1L]
  }
  z <- rbind(matrix(0, 1L, k, dimnames = list(NULL, par$param_id)), z)
  draws <- data.table(draw_id = rep(0:n_draws, times = k),
                      param_id = rep(par$param_id, each = n_draws + 1L),
                      z = as.vector(z))
  draws <- par[, .(param_id, outcome, stratum, meanlog, sdlog, hr5_point = HR_per_5mmHg)][
    draws, on = "param_id"]
  draws[, log_hr5 := meanlog + sdlog * z]
  draws[, hr5 := exp(log_hr5)]
  draws[draw_id == 0L, hr5 := hr5_point]          # exact point values for draw 0
  setkey(draws, draw_id, param_id)
  attr(draws, "psa_meta") <- list(seed = seed, n_draws = n_draws, k = k,
                                  correlation_mode = correlation_mode,
                                  rng = c(kind = "Mersenne-Twister",
                                          normal.kind = "Inversion",
                                          sample.kind = "Rejection"),
                                  ci_divisor = PSA_CI_DIVISOR)
  draws[]
}

# Wide HR5 table (draw x outcome x stratum) including the unsampled strata at their
# point values so the reconstructed Sheet1 column C is complete.
psa_hr5_full <- function(hr5_draws, s6) {
  ids <- sort(unique(hr5_draws$draw_id))
  fixed <- s6[outcome %in% PSA_SAMPLED_OUTCOMES & !sampled,
              .(outcome, stratum, hr5 = HR_per_5mmHg)]
  fixed <- fixed[, .(draw_id = ids), by = .(outcome, stratum, hr5)]
  rbind(hr5_draws[, .(draw_id, outcome, stratum, hr5)], fixed, use.names = TRUE)
}

#...........................................................
# Sheet1 reconstruction ----
#...........................................................

# Rebuild the Sheet1 derived columns for every draw with the workbook's formulas:
#   C = HR5^2 (RR per 10 mmHg);  D_b = C_b x C_(b-1) x ... x C_140-149;
#   F_b = D_b x C_130-139 (Excel: +C_b*...*C4, evaluated left to right);
#   E = 1 - D;  G = 1 - F;  bins < 140 keep the workbook placeholders D = F = 0.
psa_build_sheet1_draws <- function(hr5_draws, s6) {
  full <- psa_hr5_full(hr5_draws, s6)
  grid <- CJ(cause = PSA_SHEET1_CAUSES, bp_cat = PSA_BP_BINS, sorted = FALSE)
  grid[, `:=`(outcome = PSA_CAUSE_OUTCOME[cause], stratum = PSA_BIN_STRATUM[bp_cat])]
  x <- full[grid, on = .(outcome, stratum), allow.cartesian = TRUE]
  if (anyNA(x$hr5)) stop("Sheet1 rebuild: missing HR5 for some cause/bin/draw.")
  x[, rr_per_10mmhg := hr5^2]
  w <- dcast(x, draw_id + cause ~ bp_cat, value.var = "rr_per_10mmhg")
  c130 <- w[["130-139"]]; c140 <- w[["140-149"]]; c150 <- w[["150-159"]]
  c160 <- w[["160-169"]]; c170 <- w[["170-179"]]; c180 <- w[["180+"]]
  d <- list(`140-149` = c140,
            `150-159` = c150 * c140,
            `160-169` = c160 * c150 * c140,
            `170-179` = c170 * c160 * c150 * c140,
            `180+`    = c180 * c170 * c160 * c150 * c140)
  f <- list(`140-149` = c140 * c130,
            `150-159` = c150 * c140 * c130,
            `160-169` = c160 * c150 * c140 * c130,
            `170-179` = c170 * c160 * c150 * c140 * c130,
            `180+`    = c180 * c170 * c160 * c150 * c140 * c130)
  cum <- rbindlist(lapply(PSA_BP_BINS, function(b) {
    data.table(draw_id = w$draw_id, cause = w$cause, bp_cat = b,
               cummulative_rr_nodiabetes = if (b %in% PSA_HYP_BINS) d[[b]] else 0,
               cummulative_rr_diabetes   = if (b %in% PSA_HYP_BINS) f[[b]] else 0)
  }))
  out <- x[, .(draw_id, cause, bp_cat, rr_per_10mmhg)][cum, on = .(draw_id, cause, bp_cat)]
  out[, `:=`(effect_size_nodiabetes = 1 - cummulative_rr_nodiabetes,
             effect_size_diabetes   = 1 - cummulative_rr_diabetes)]
  out[, `:=`(cause_ord = match(cause, PSA_SHEET1_CAUSES), bin_ord = match(bp_cat, PSA_BP_BINS))]
  setorder(out, draw_id, cause_ord, bin_ord)
  out[, c("cause_ord", "bin_ord") := NULL]
  setcolorder(out, c("draw_id", "cause", "bp_cat", "rr_per_10mmhg",
                     "cummulative_rr_nodiabetes", "effect_size_nodiabetes",
                     "cummulative_rr_diabetes", "effect_size_diabetes"))
  out[]
}

# One draw's table in the shape 06's lookups expect (cause, bp_cat, effect_size_*).
psa_sheet1_for_draw <- function(sheet1_draws, id) {
  x <- sheet1_draws[draw_id == id]
  if (nrow(x) != length(PSA_SHEET1_CAUSES) * length(PSA_BP_BINS)) {
    stop("Sheet1 draw table for draw ", id, " is incomplete.")
  }
  x[, draw_id := NULL][]
}

# Compare the draw-0 rebuild with the workbook's Sheet1 (deterministic reference).
psa_check_sheet1_reconstruction <- function(sheet1_draw0, sheet1_wb, tol = 1e-12) {
  cols <- c("rr_per_10mmhg", "cummulative_rr_nodiabetes", "effect_size_nodiabetes",
            "cummulative_rr_diabetes", "effect_size_diabetes")
  wb <- sheet1_wb[, c("cause", "bp_cat", cols), with = FALSE]
  m <- merge(sheet1_draw0, wb, by = c("cause", "bp_cat"), suffixes = c("_psa", "_wb"))
  if (nrow(m) != nrow(wb) || nrow(m) != nrow(sheet1_draw0)) {
    stop("Sheet1 rebuild and workbook Sheet1 do not have the same cause x bin rows.")
  }
  res <- rbindlist(lapply(cols, function(v) {
    d <- abs(m[[paste0(v, "_psa")]] - m[[paste0(v, "_wb")]])
    data.table(column = v, max_abs_diff = max(d), n_rows = length(d))
  }))
  res[, pass := max_abs_diff <= tol]
  res[]
}

#...........................................................
# Sodium pathway RR table (06o:330-393), rebuilt per draw ----
#...........................................................

# The legacy CSV carries the same Appendix S6 values as RR per 10 mmHg
# (=HR5^2). 06's mapping (map_bp) expects Ettehad strata, so it leaves bp_cat NA
# for <120, 120-129, 160-169 and >=170. That NA pattern is draw-invariant and is
# reproduced verbatim; the RR values themselves come from the S6 draws.
psa_etihad_rr10_transform <- function(etihad_rr) {
  ETIHAD_RR <- copy(etihad_rr)
  by_draw <- "draw_id" %in% names(ETIHAD_RR)
  if (!by_draw) ETIHAD_RR[, draw_id := 0L]
  ETIHAD_RR[, cause := fcase(
    Cause == "Coronary heart disease", "ihd",
    Cause == "Heart failure", "hhd",
    Cause == "Stroke", "istroke",
    default = NA_character_
  )]
  ETIHAD_RR <- ETIHAD_RR[cause %in% c("ihd", "hhd", "istroke", "hstroke"),
                         c("draw_id", "cause", "SBP_Category", "RR"), with = FALSE]
  etihad_hstroke_rr <- ETIHAD_RR[cause == "istroke", ]
  etihad_hstroke_rr[, cause := "hstroke"]
  ETIHAD_RR <- rbind(ETIHAD_RR, etihad_hstroke_rr)
  ETIHAD_RR <- ETIHAD_RR[SBP_Category != "Total", ]
  setnames(ETIHAD_RR, c("SBP_Category", "RR"), c("bp_cat", "rr_per_10mmhg"))
  bp_full <- c("<120", "120-129", "130-139", "140-149",
               "150-159", "160-169", "170-179", "180+")
  map_bp <- function(x) {
    fcase(
      x %in% c("<120", "120-129", "<130")        , "<130",
      x == "130-139"                             , "130-139",
      x == "140-149"                             , "140-149",
      x == "150-159"                             , "150-159",
      x %in% c("160-169", "170-179", "180+", "≥160"), ">=160"
    )
  }
  bp_map <- data.table(bp_cat_full = bp_full, bp_cat = map_bp(bp_full))
  expanded <- bp_map[ETIHAD_RR, on = .(bp_cat), allow.cartesian = TRUE][
    , .(draw_id, cause, bp_cat_full, rr = rr_per_10mmhg)][
    order(draw_id, cause, bp_cat_full)]
  setnames(expanded, c("bp_cat_full", "rr"), c("bp_cat", "rr_per_10mmhg"))
  if (!by_draw) expanded[, draw_id := NULL]
  expanded[]
}

psa_build_sodium_rr10_draws <- function(hr5_draws, s6, sodium_csv) {
  csv <- copy(sodium_csv)
  csv[, outcome := fifelse(Cause %in% names(PSA_SODIUM_CSV_OUTCOME),
                           PSA_SODIUM_CSV_OUTCOME[Cause], Cause)]
  chk <- merge(csv, s6[, .(outcome, stratum, RR_per_10mmHg)],
               by.x = c("outcome", "SBP_Category"), by.y = c("outcome", "stratum"))
  if (nrow(chk) != nrow(csv) || chk[, max(abs(RR - RR_per_10mmHg))] > 1e-9) {
    stop("Sodium CSV RRs do not equal Appendix S6 HR5^2; shared-draw mapping invalid.")
  }
  full <- psa_hr5_full(hr5_draws, s6)
  ids  <- sort(unique(hr5_draws$draw_id))
  csv[, row_ord := .I]                    # keep the CSV row order 06 sees
  base <- csv[, .(draw_id = ids), by = .(row_ord, Cause, SBP_Category, outcome, RR)]
  setnames(base, "SBP_Category", "stratum")
  base <- merge(base, full, by = c("draw_id", "outcome", "stratum"), all.x = TRUE)
  base[!is.na(hr5), RR := hr5^2]     # the 3 mapped outcomes follow the S6 draws
  setorder(base, draw_id, row_ord)
  psa_etihad_rr10_transform(base[, .(draw_id, Cause, SBP_Category = stratum, RR)])
}
