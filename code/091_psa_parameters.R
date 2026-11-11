# =============================================================================
# 091_psa_parameters.R -- Aim 1 PSA: parameter register, draws, Sheet1 rebuild
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions and constants only;
# sourcing it has no side effects.
#
# Two sampled parameter families (everything else is fixed):
#
# 1. BP efficacy -- BPLTTC 2021, Appendix S6 (HR per 5 mmHg SBP reduction by
#    baseline-SBP stratum), read from the "Appendix S6" sheet of
#    data/processed/ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx. 06's
#    ETIHAD_RR_BIN is Sheet1 of that workbook: column C holds HR5^2 (RR per 10
#    mmHg) and columns D-G are cumulative products over the traversed BP bins.
#    The PSA samples the 15 Appendix S6 HRs those formulas traverse and rebuilds
#    the Sheet1 derived columns for every draw with the same formulas.
#    Structural assumption carried over unchanged from 06 (not established by
#    the source article): the stratum-specific subgroup effects are multiplied as
#    if they were successive 10-mmHg increments.
#
# 2. Statin efficacy -- the four relative risks hard-coded in 06's
#    calculate_statins_impact() (rr_ir_ihd, rr_ir_istroke, rr_cf_ihd,
#    rr_cf_istroke). Each is drawn log-normally around the RETAINED model value
#    with the log-scale SD implied by a verified CTT trial confidence interval
#    (PSA_STATIN_PARAMS documents source, endpoint, CI level and mapping).

#...........................................................
# Constants ----
#...........................................................

PSA_BP_WORKBOOK     <- "ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx"
PSA_S6_SHEET        <- "Appendix S6"
PSA_SHEET1          <- "Sheet1"

# BP (unchanged from the existing PSA): 95% CI -> SE on the log scale,
# (log U - log L) / 3.92.
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
# (workbook C34:C41 = +C2:C9; dementia is not modelled).
PSA_CAUSE_OUTCOME <- c(hhd = "Heart failure", hstroke = "Stroke",
                       ihd = "Ischaemic heart disease", istroke = "Stroke",
                       aod = "Heart failure")
PSA_SHEET1_CAUSES <- names(PSA_CAUSE_OUTCOME)   # workbook row order

# Model BP bin -> Appendix S6 stratum (>=170 shared by 170-179 and 180+).
PSA_BIN_STRATUM <- c("<120" = "<120", "120-129" = "120-129",
                     "130-139" = "130-139", "140-149" = "140-149",
                     "150-159" = "150-159", "160-169" = "160-169",
                     "170-179" = ">=170", "180+" = ">=170")

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
# Statin parameters (verified sources) ----
#...........................................................

# Fulcher J, et al. (CTT Collaboration). Efficacy and safety of LDL-lowering
#   therapy among men and women: meta-analysis of individual data from 174 000
#   participants in 27 randomised trials. Lancet 2015;385:1397-405.
#   doi:10.1016/S0140-6736(14)61368-4. Figures give men and women with 99% CIs
#   and the total with a 95% CI ("we present only summary rate ratios with 95%
#   CIs; all other rate ratios are presented with 99% CIs").
# CTT Collaboration. Efficacy and safety of more intensive lowering of LDL
#   cholesterol: a meta-analysis of data from 170 000 participants in 26
#   randomised trials. Lancet 2010;376:1670-81. doi:10.1016/S0140-6736(10)61350-5
#   (the DOI cited beside the RRs in 06; open full text PMC2988224).
# All RRs are per 1.0 mmol/L LDL-cholesterol reduction. 06 applies them directly
# as the effect of statin use (no rescaling to the LDL reduction achieved), and
# that scaling is retained here unchanged.
PSA_CTT2015 <- paste0("Fulcher J, et al. (CTT Collaboration). Lancet 2015;385:1397-405. ",
                      "doi:10.1016/S0140-6736(14)61368-4")
PSA_CTT2010 <- paste0("CTT Collaboration. Lancet 2010;376:1670-81. ",
                      "doi:10.1016/S0140-6736(10)61350-5 (PMC2988224)")

PSA_STATIN_PARAMS <- data.table::data.table(
  param_id = c("STATIN_RR_IR_IHD", "STATIN_RR_IR_ISTROKE", "STATIN_RR_CF_IHD", "STATIN_RR_CF_ISTROKE"),
  model_variable = c("rr_ir_ihd", "rr_ir_istroke", "rr_cf_ihd", "rr_cf_istroke"),
  model_point = c(0.74, 0.80, 0.80, 0.96),
  model_use = c(
    "IHD incidence (Well->Sick), ages >=40: IR x (1 - AF x (1-RR) x dcov x adh_ir / (1 - (1-RR) x cov0 x adh_ir))",
    "Ischaemic stroke incidence (Well->Sick), ages >=40: same form as IHD incidence",
    "IHD case fatality (Sick->Dead), ages >=40: CF x (1 - (1-RR) x dcov x adh_cf / (1 - (1-RR) x cov0 x adh_cf))",
    "Ischaemic stroke case fatality (Sick->Dead), ages >=40: CF x (1 - 0.60 x (1-RR) x dcov x adh_cf / (1 - (1-RR) x cov0 x adh_cf))"),
  source = c(PSA_CTT2015, PSA_CTT2015, PSA_CTT2010, PSA_CTT2010),
  source_location = c(
    "Figure 3 and webappendix Webfigure 5 ('Major coronary events', Overall, Total row)",
    "Webappendix Webfigure 7 ('Stroke subtypes', Ischaemic stroke, Subtotal row)",
    "Abstract (Findings) and Figure 4: deaths due to coronary heart disease, all 26 trials",
    "Abstract (Findings) and Figure 4: deaths due to stroke, all 26 trials"),
  source_endpoint = c(
    "Major coronary events (first non-fatal myocardial infarction or coronary death)",
    "First ischaemic stroke (fatal or non-fatal)",
    "Death due to coronary heart disease (unconditional mortality in trial populations)",
    "Death due to stroke, any type (unconditional mortality in trial populations)"),
  source_population = c(
    "All 27 trials (22 statin vs control, 5 more vs less intensive), men and women combined (n=174,149)",
    "All 27 trials, men and women combined",
    "All 26 trials (21 statin vs control, 5 more vs less intensive), both sexes (n~170,000)",
    "All 26 trials, both sexes"),
  source_point = c(0.76, 0.80, 0.80, 0.96),
  ci_lower = c(0.73, 0.75, 0.74, 0.84),
  ci_upper = c(0.79, 0.86, 0.87, 1.09),
  ci_level = c(0.95, 0.95, 0.99, 0.95),
  ldl_scale = "per 1.0 mmol/L LDL-cholesterol reduction (applied unscaled, as in 06)",
  centering = c(
    paste0("Centred on the retained model value 0.74, which differs from the source point 0.76: precision ",
           "is borrowed from the all-participant major-coronary-event 95% CI. 0.74 equals the men-only ",
           "estimate in the same figure (RR 0.74, 99% CI 0.70-0.78; women 0.83, 99% CI 0.74-0.93) and, ",
           "numerically, the lower 99% limit of the CTT 2010 CHD-death RR quoted beside it in 06 ",
           "(0.80, 0.74-0.87). Its provenance is therefore ambiguous; the model value is not changed."),
    "Centred on the model value, which equals the published all-participant point estimate.",
    paste0("Centred on the model value, which equals the published point estimate. CTT 2015 does not ",
           "report coronary death separately (only any vascular death, RR 0.88, 95% CI 0.84-0.91), so the ",
           "endpoint-specific interval is taken from CTT 2010, the source cited in 06."),
    paste0("Centred on the model value, which equals the published point estimate (CTT 2010). CTT 2015 ",
           "reports no stroke-death estimate.")),
  estimand_note = c(
    "Endpoint matches the incidence transition (first fatal or non-fatal coronary event).",
    paste0("Endpoint matches the incidence transition. In 06 the effect is also scaled by the GBD high-FPG ",
           "attributable fraction (AF), a fixed structural assumption outside this PSA."),
    paste0("MODELLING ASSUMPTION: an unconditional CHD-mortality RR is used as a multiplier on conditional ",
           "case fatality among prevalent IHD cases. The trials did not estimate conditional case fatality; ",
           "part of the mortality reduction reflects fewer incident events, which rr_ir_ihd already captures."),
    paste0("MODELLING ASSUMPTION: an all-type stroke-mortality RR is used as a multiplier on conditional ",
           "ischaemic-stroke case fatality (further scaled by prop_athero_stroke = 0.60). The trials did not ",
           "estimate conditional case fatality. The 95% CI crosses 1, so some draws imply harm (not truncated)."))
)
PSA_STATIN_IDS <- PSA_STATIN_PARAMS$model_variable

# Log-scale SD from a verified interval [L, U] at confidence level c.
psa_se_log <- function(lower, upper, level) (log(upper) - log(lower)) / (2 * qnorm((1 + level) / 2))

psa_statin_param_table <- function(model_points = NULL) {
  p <- data.table::copy(PSA_STATIN_PARAMS)
  if (!is.null(model_points)) {
    mp <- unlist(model_points)[p$model_variable]
    if (anyNA(mp) || any(abs(mp - p$model_point) > 0)) {
      stop("06 statin RRs (", paste(names(mp), mp, sep = "=", collapse = ", "),
           ") differ from the documented PSA_STATIN_PARAMS model points; update the register first.")
    }
  }
  p[, `:=`(meanlog = log(model_point), sdlog = psa_se_log(ci_lower, ci_upper, ci_level))]
  p[]
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

# Sheet1 exactly as 06 reads it (ETIHAD_RR_BIN).
psa_read_sheet1 <- function(wd_data) {
  as.data.table(readxl::read_excel(file.path(wd_data, PSA_BP_WORKBOOK), sheet = PSA_SHEET1))
}

#...........................................................
# Parameter register ----
#...........................................................

# run_args: the executed 06 run arguments; st: psa_statin_param_table().
psa_parameter_register <- function(s6, run_args, st) {
  excl_reason <- function(outcome, stratum) {
    fcase(
      outcome %in% PSA_SAMPLED_OUTCOMES & stratum %in% c("<120", "120-129"),
      paste0("Stratum not traversed by the Sheet1 formulas (cumulative products start at 130-139 ",
             "for diabetes and 140-149 without diabetes); never reaches an effect size used by 06."),
      outcome == "Major cardiovascular events",
      "Composite endpoint; not mapped to any modelled cause in Sheet1.",
      outcome == "Cardiovascular death",
      paste0("Mortality endpoint, not a conditional case-fatality effect; not used by 06. ",
             "BP case-fatality constants stay fixed."),
      outcome == "All-cause death", "All-cause mortality endpoint; not used by 06.",
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
    model_variable = fifelse(sampled, "ETIHAD_RR_BIN effect_size_(no)diabetes (via Sheet1 rebuild)", NA_character_),
    model_point = HR_per_5mmHg,
    source = paste0("BPLTTC 2021 Appendix S6 (", Source, ")"),
    source_location = paste0("data/processed/", PSA_BP_WORKBOOK, ", sheet '", PSA_S6_SHEET,
                             "', row ", source_row),
    source_endpoint = outcome, source_population = paste0("Baseline SBP stratum ", stratum),
    source_point = HR_per_5mmHg, ci_lower = CI_lower_5mmHg, ci_upper = CI_upper_5mmHg,
    ci_level = PSA_CI_LEVEL, scale = "HR per 5 mmHg SBP reduction",
    distribution = fifelse(sampled, "log-normal: log(HR5) ~ N(log(point), sdlog^2)", NA_character_),
    meanlog = fifelse(sampled, meanlog, NA_real_),
    sdlog = fifelse(sampled, sdlog, NA_real_),
    transformation = fifelse(sampled,
      paste0("sdlog = (log(U) - log(L)) / ", PSA_CI_DIVISOR, "; RR10 = HR5^2; Sheet1 D/F cumulative ",
             "products; effect = 1 - product"), NA_character_),
    maps_to = fifelse(sampled, maps_to(outcome), "none"),
    sharing = fifelse(sampled,
      paste0("One draw per iteration shared by all countries, ages, sexes, years and scenarios",
             fifelse(stratum == ">=170", "; >=170 feeds both 170-179 and 180+", ""),
             fifelse(stratum == "130-139", "; 130-139 enters only the diabetes product", ""),
             "; the same stratum draw feeds the diabetes and no-diabetes products"), NA_character_),
    correlation = fifelse(sampled,
      "Independent across parameters by default (working assumption; no covariance reported)",
      NA_character_),
    affected_equations = fifelse(sampled,
      paste0("Sheet1 cumulative RR -> effect_size_(no)diabetes -> aim2_incremental_effect -> ",
             "aim2_subgroup_incidence_multiplier -> IR_bin_new -> eff_ir (hypertensive bins; Well->Sick)"),
      NA_character_),
    scenarios_affected = fifelse(sampled,
      "bp_no_diabetes_only, bp_diabetes_only, bp_combined, all_interventions", NA_character_),
    notes = fifelse(sampled,
      "Endpoint (fatal + non-fatal events) matches the incidence transition used by 06.",
      excl_reason(outcome, stratum))
  )]

  stat <- st[, .(
    param_id, family = "Statin efficacy (CTT)", status = "SAMPLED",
    model_variable = paste0(model_variable, " in calculate_statins_impact()"),
    model_point, source, source_location, source_endpoint, source_population, source_point,
    ci_lower, ci_upper, ci_level, scale = ldl_scale,
    distribution = "log-normal: log(RR) ~ N(log(model_point), sdlog^2); draw 0 = model_point",
    meanlog, sdlog,
    transformation = "sdlog = (log(U) - log(L)) / (2 * qnorm((1 + c) / 2)); rr = exp(meanlog + sdlog * z)",
    maps_to = model_use,
    sharing = paste0("One four-parameter vector per iteration shared by all countries, ages, sexes, ",
                     "years and scenarios (statins_only and all_interventions)"),
    correlation = paste0("Independent of each other and of the BP parameters (working assumption: ",
                         "no joint covariance is published)"),
    affected_equations = paste0("calculate_statins_impact(): effect_size_ir / effect_size_cf, including the ",
                                "baseline-coverage denominator (1 - (1-RR) x cov0 x adherence), rebuilt per draw"),
    scenarios_affected = "statins_only, all_interventions",
    notes = paste(centering, estimand_note,
                  "RR draws above 1 are kept (no truncation); 06's CF/IR <= 0.99 caps still apply."))]

  fx <- function(param_id, family, status, model_point_text, source_location, notes) {
    data.table(param_id, family, status, model_variable = model_point_text, source_location, notes)
  }
  a_ir <- if (is.null(run_args$adherence_ir)) 1 else run_args$adherence_ir
  a_cf <- if (is.null(run_args$adherence_cf)) 1 else run_args$adherence_cf
  fixed <- rbindlist(list(
    fx("BPCF_rho", "BP case-fatality reduction per unit incremental control", "FIXED",
       "cf_reduction_per_control: ihd 0.24, istroke 0.36, hstroke 0.76, hhd 0.20",
       "calculate_antihypertensive_split() (cf_etihad)",
       paste0("GAP: no endpoint-matched source with uncertainty is available in the repository. Appendix S6 ",
              "'Cardiovascular death' HRs are mortality endpoints, not conditional case-fatality effects. ",
              "Left fixed and disclosed.")),
    fx("STATIN_ADHERENCE", "Statin adherence", "FIXED",
       sprintf("adherence_ir = %s, adherence_cf = %s (executed 06 run call)", a_ir, a_cf),
       "run_multiple_scenarios() call inside the 06 %dopar% loop",
       "Basios et al. 2025 adherence prevalences; fixed by task scope."),
    fx("STATIN_AF", "Statin attributable fraction", "FIXED",
       "af_statins.rds (GBD high-FPG PAF); defaults 0.1497 (IHD) / 0.1161 (ischaemic stroke)",
       "calculate_statins_impact()", "Structural/epidemiological input; fixed by task scope."),
    fx("STATIN_PROP_ATHERO", "Statin effect scaling", "FIXED", "prop_athero_stroke = 0.60",
       "project.all() call to calculate_statins_impact()",
       "Atherosclerotic share of ischaemic stroke; fixed by task scope."),
    fx("STATIN_POLICY", "Policy", "FIXED",
       sprintf("target coverage %s, start %s, target year %s; baseline statins_current (2024); ages >= 40",
               run_args$statin_target_coverage, run_args$statin_start_year, run_args$statin_target_year),
       "06 run call; statin_data.rds", "Policy definition / baseline coverage; not sampled."),
    fx("GBD_RR_10MMHG", "BP exposure-risk gradient", "FIXED",
       "GBD 2019 RR per 10 mmHg by age and cause",
       "data/raw/IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx",
       "Baseline epidemiology (allocates incidence across BP bins); outside this intervention-effect PSA."),
    fx("BP_DISTRIBUTION", "Baseline", "FIXED", "Mean/SD SBP by country-age-sex (bp_data6.csv)",
       "data/processed/bp_data6.csv", "Baseline input; not an intervention effect."),
    fx("DIABETES_SHARE", "Baseline", "FIXED", "P(D|H) = population diabetes prevalence",
       "code/04_define_interventions.R", "Baseline/structural assumption; not sampled."),
    fx("HTN_TARGETS", "Policy", "FIXED",
       "150M additional controlled by 2030 (diabetes capped at 0.80); linear 2025 -> 2030",
       "data/processed/htn_control_targets_by_loc.csv", "Policy targets are not drawn."),
    fx("BASELINE_EPI", "Baseline", "FIXED",
       "Calibrated IR/CF (stage 03), coherent secular trends (stage 031), BG mortality, prevalence, UNWPP population, COVID excess",
       "data/processed/adjusted_searo_part*.rds via code/05_build_baseline.R",
       "Calibration, trends and baseline epidemiology are not drawn."),
    fx("STRUCTURE_SHEET1", "Structural", "STRUCTURAL",
       "Cumulative multiplication of stratum RRs over traversed bins", "Sheet1 columns D-G",
       "Structural modelling assumption reproduced unchanged."),
    fx("STRUCTURE_MAPPING", "Structural", "STRUCTURAL",
       "BP: Stroke -> istroke & hstroke; Heart failure -> hhd. Statins: IHD and ischaemic stroke only",
       "Sheet1 column C; calculate_statins_impact()", "Outcome mapping reproduced unchanged.")
  ), fill = TRUE)
  reg <- rbindlist(list(bp, stat, fixed), fill = TRUE)
  setcolorder(reg, c("param_id", "family", "status"))
  reg[]
}

#...........................................................
# Draws ----
#...........................................................

# Stable iteration identifiers: draw k uses the k-th row of a draw-major standard
# normal matrix (15 BP columns, then 4 statin columns), so draws 1..N do not
# depend on N. Draw 0 is the deterministic point run (z = 0, exact point values).
# Correlation modes act on the BP columns only; statins are always independent.
psa_generate_draws <- function(s6, st, n_draws, seed,
                               correlation_mode = c("independent",
                                                    "comonotone_within_stratum",
                                                    "comonotone_all")) {
  correlation_mode <- match.arg(correlation_mode)
  stopifnot(length(n_draws) == 1L, n_draws >= 0, n_draws == round(n_draws))
  par <- s6[sampled == TRUE][order(outcome, stratum)]
  kb <- nrow(par); ks <- nrow(st); k <- kb + ks
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  set.seed(seed)
  z <- if (n_draws > 0) matrix(rnorm(n_draws * k), nrow = n_draws, ncol = k, byrow = TRUE)
       else matrix(numeric(0), 0, k)
  zb <- z[, seq_len(kb), drop = FALSE]; zs <- z[, kb + seq_len(ks), drop = FALSE]
  colnames(zb) <- par$param_id; colnames(zs) <- st$param_id
  if (correlation_mode == "comonotone_within_stratum") {
    for (s in unique(par$stratum)) {
      cols <- par[stratum == s, param_id]
      zb[, cols] <- zb[, cols[1L]]
    }
  } else if (correlation_mode == "comonotone_all") {
    zb[] <- zb[, 1L]
  }
  zb <- rbind(matrix(0, 1L, kb, dimnames = list(NULL, par$param_id)), zb)
  zs <- rbind(matrix(0, 1L, ks, dimnames = list(NULL, st$param_id)), zs)
  bp <- data.table(draw_id = rep(0:n_draws, times = kb),
                   param_id = rep(par$param_id, each = n_draws + 1L), z = as.vector(zb))
  bp <- par[, .(param_id, outcome, stratum, meanlog, sdlog, hr5_point = HR_per_5mmHg)][bp, on = "param_id"]
  bp[, hr5 := exp(meanlog + sdlog * z)]
  bp[draw_id == 0L, hr5 := hr5_point]             # exact point values for draw 0
  setkey(bp, draw_id, param_id)
  stl <- data.table(draw_id = rep(0:n_draws, times = ks),
                    param_id = rep(st$param_id, each = n_draws + 1L), z = as.vector(zs))
  stl <- st[, .(param_id, model_variable, meanlog, sdlog, rr_point = model_point)][stl, on = "param_id"]
  stl[, rr := exp(meanlog + sdlog * z)]
  stl[draw_id == 0L, rr := rr_point]
  setkey(stl, draw_id, param_id)
  statin_wide <- dcast(stl, draw_id ~ model_variable, value.var = "rr")
  setcolorder(statin_wide, c("draw_id", PSA_STATIN_IDS))
  list(bp = bp[], statin_long = stl[], statin = statin_wide[],
       meta = list(seed = seed, n_draws = n_draws, k_bp = kb, k_statin = ks,
                   correlation_mode = correlation_mode,
                   rng = c(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"),
                   column_order = c(par$param_id, st$param_id)))
}

# Long table of every sampled value (parameter_draws.csv).
psa_parameter_draws_table <- function(draws) {
  rbind(
    draws$bp[, .(draw_id, family = "BP", param_id, endpoint = outcome, stratum, scale = "HR per 5 mmHg",
                 z, point = hr5_point, value = hr5)],
    draws$statin_long[, .(draw_id, family = "statin", param_id, endpoint = model_variable,
                          stratum = NA_character_, scale = "RR per 1.0 mmol/L LDL-C",
                          z, point = rr_point, value = rr)])[order(draw_id, family, param_id)]
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

# One draw's statin RR vector (named by the 06 variable names).
psa_statin_for_draw <- function(statin_wide, id) {
  x <- statin_wide[draw_id == id]
  if (nrow(x) != 1L) stop("Statin draw ", id, " is missing or duplicated.")
  unlist(x[, ..PSA_STATIN_IDS])
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
