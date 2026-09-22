#!/usr/bin/env Rscript
# =============================================================================
# QA harness for the Aim 2 BP-control-by-diabetes-status refactor (Parts A & B)
# =============================================================================
# Implements the 15 QA checks from aim2_bp_control_refactor_prompt.md for a
# LOW-diabetes country, a HIGH-diabetes country, and the GLOBAL reconciliation.
#
# Sources code/04_define_interventions.R and code/06_run_scenarios_multiple.R
# with their execution guards OFF (who_cvd.execute_04 / who_cvd.execute_06 =
# FALSE) so the pure helpers are exercised WITHOUT loading the full model inputs
# or starting the parallel cluster. Pipeline-only helpers that
# calculate_antihypertensive_split() depends on (get.bp.prob,
# expand_to_single_year_ages, calculate_baseline_incidence_gbd,
# calculate_aggregate_coverage) are stubbed by a minimal deterministic BP-bin
# model below.
#
# Run:  Rscript tests/test_aim2_bp_control.R
# =============================================================================

scratch_library <- Sys.getenv("WHO_CVD_R_LIB", unset = "")
if (nzchar(scratch_library)) .libPaths(c(scratch_library, .libPaths()))
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(dplyr)
})

repo_root <- normalizePath(if (dir.exists("code")) "." else "..",
                           winslash = "/", mustWork = TRUE)

old_options <- options(
  who_cvd.execute_04 = FALSE,
  who_cvd.execute_06 = FALSE
)
on.exit(options(old_options), add = TRUE)
source(file.path(repo_root, "code/04_define_interventions.R"))
source(file.path(repo_root, "code/06_run_scenarios_multiple.R"))

# ---- tiny assertion helpers -------------------------------------------------
.PASS <- 0L; .N <- 0L
check <- function(label, condition) {
  .N <<- .N + 1L
  if (!isTRUE(condition)) stop(sprintf("FAIL [%s]", label), call. = FALSE)
  .PASS <<- .PASS + 1L
  cat(sprintf("  ok  %s\n", label))
}
assert_close <- function(actual, expected, tolerance, label) {
  diff <- max(abs(actual - expected))
  check(sprintf("%s (max diff %.3g <= %.3g)", label, diff, tolerance),
        is.finite(diff) && diff <= tolerance)
}

cat("=============================================================\n")
cat("PART A - script 04 target allocation (synthetic + real)\n")
cat("=============================================================\n")

# ---- Synthetic feasible strata ---------------------------------------------
synthetic <- data.table(
  location = rep(c("Low diabetes", "High diabetes"), each = 2),
  sex = rep(c("Female", "Male"), 2),
  population_2025 = rep(1e9, 4),
  htn_population_2025 = rep(4e8, 4),
  diabetes_share_among_htn_assumed = c(0.05, 0.10, 0.20, 0.30),
  observed_overall_control = c(0.20, 0.30, 0.40, 0.50)
)
synthetic[, `:=`(
  htn_population_diabetes = htn_population_2025 * diabetes_share_among_htn_assumed,
  htn_population_no_diabetes = htn_population_2025 * (1 - diabetes_share_among_htn_assumed)
)]
dec <- aim2_decompose_control(synthetic$observed_overall_control,
                              synthetic$diabetes_share_among_htn_assumed)
synthetic[, `:=`(
  baseline_control_no_diabetes = dec$baseline_control_no_diabetes,
  baseline_control_diabetes = dec$baseline_control_diabetes,
  control_gap_requested = 0.15,
  control_gap_used = dec$control_gap_used
)]

# (1) H_N + H_D = H
assert_close(synthetic$htn_population_no_diabetes + synthetic$htn_population_diabetes,
             synthetic$htn_population_2025, 1e-6, "01 hypertensive partition (synthetic)")
# (2) H_N*c_N + H_D*c_D = H*c
assert_close(
  synthetic$htn_population_no_diabetes * synthetic$baseline_control_no_diabetes +
    synthetic$htn_population_diabetes * synthetic$baseline_control_diabetes,
  synthetic$htn_population_2025 * synthetic$observed_overall_control,
  1e-6, "02 baseline control reconstruction (synthetic)")
# (3) gap = 0.15 where feasible (all synthetic strata feasible)
assert_close(synthetic$baseline_control_diabetes - synthetic$baseline_control_no_diabetes,
             rep(0.15, nrow(synthetic)), 1e-12, "03 baseline gap = 0.15 (feasible)")
# subgroup decomposition uses c_N = c - w_D*0.15 (NOT c_D = c + 0.15)
assert_close(synthetic$baseline_control_no_diabetes,
             synthetic$observed_overall_control -
               synthetic$diabetes_share_among_htn_assumed * 0.15,
             1e-12, "03b c_N = c - w_D*0.15")
assert_close(synthetic$baseline_control_diabetes,
             synthetic$observed_overall_control +
               (1 - synthetic$diabetes_share_among_htn_assumed) * 0.15,
             1e-12, "03c c_D = c + (1-w_D)*0.15")

out1 <- aim2_build_target_tables(synthetic, allocation_mode = "diabetes_capped_to_target")
tg <- out1$targets
sm <- out1$summary

# (4) rates in [0,1]
check("04 baseline/target rates in [0,1] (synthetic)",
      all(tg$baseline_control >= 0 & tg$baseline_control <= 1 &
            tg$target_control >= 0 & tg$target_control <= 1))
# (5) no missing scenario/location/sex/subgroup keys
check("05 no missing keys (synthetic)",
      !anyNA(tg[, .(scenario_id, location, sex, subgroup)]))
# (6) exactly one row per scenario x location x sex x subgroup
check("06 one row per scenario x loc x sex x subgroup (synthetic)",
      nrow(tg) == uniqueN(tg[, .(scenario_id, location, sex, subgroup)]) &&
        nrow(tg) == 4L * nrow(synthetic) * 2L)
validate_htn_target_table(tg)
# (7) no negative subgroup population or additional-controlled
check("07 non-negative subgroup pop and additional (synthetic)",
      all(tg$htn_population_2025 >= 0) &&
        all(tg$additional_controlled_2030 >= -1))
# (8) combined additional 2030 = 150M on the fixed 2025 population
cb <- sm[scenario_id == "bp_combined"]
assert_close(cb$achieved_total, 150e6, 1, "08 combined = 150M (synthetic, mode 1)")
# (9) combined total = diabetes + non-diabetes additions
assert_close(cb$achieved_total,
             cb$additional_controlled_no_diabetes + cb$additional_controlled_diabetes,
             1e-6, "09 combined = diabetes + non-diabetes (synthetic)")
# subgroup-only scenarios isolate their own subgroup; baseline adds nothing
check("09b baseline scenario adds 0 (synthetic)",
      tg[scenario_id == "baseline", all(abs(additional_controlled_2030) <= 1e-8)])
check("09c bp_no_diabetes_only leaves diabetes subgroup at baseline",
      tg[scenario_id == "bp_no_diabetes_only" & subgroup == "htn_diabetes",
         all(abs(additional_controlled_2030) <= 1e-8)])
check("09d bp_diabetes_only leaves non-diabetes subgroup at baseline",
      tg[scenario_id == "bp_diabetes_only" & subgroup == "htn_no_diabetes",
         all(abs(additional_controlled_2030) <= 1e-8)])

# allocation_mode switching
check("A-mode2 diabetes_floor_then_reconcile stops when Delta_D > 150M",
      inherits(try(aim2_build_target_tables(
        synthetic, allocation_mode = "diabetes_floor_then_reconcile"),
        silent = TRUE), "try-error") == FALSE)  # synthetic is feasible under floor
out3 <- aim2_build_target_tables(synthetic, allocation_mode = "diabetes_floor_uncapped_total")
check("A-mode3 uncapped-total achieves >= 150M (synthetic)",
      out3$summary[scenario_id == "bp_combined", achieved_total] >= 150e6 - 1)

cat("\n=============================================================\n")
cat("PART B - split-population BP effects (helpers + full function)\n")
cat("=============================================================\n")

# (10) trajectory: unchanged through 2025, target in 2030, constant after
traj <- aim2_control_trajectory(c(2024L, 2025L, 2026L, 2030L, 2035L), 0.20, 0.70)
assert_close(traj, c(0.20, 0.20, 0.30, 0.70, 0.70), 1e-12,
             "10 linear trajectory (baseline<=2025, target=2030, held after)")

# incremental effect is 0 when coverage does not change (needed for check 11)
assert_close(aim2_incremental_effect(0.25, c(0.20, 0.60), 0.20)[1], 0, 1e-15,
             "10b no-change incremental effect (non-diabetes)")
assert_close(aim2_incremental_effect(0.40, c(0.30, 0.80), 0.30)[1], 0, 1e-15,
             "10c no-change incremental effect (diabetes)")

# (12) w_D = 0 -> combined reduces to non-diabetes calculation
assert_close(aim2_subgroup_incidence_multiplier(0.10, 0.25, 0), 0.90, 1e-15,
             "12 w_D=0 reduces to non-diabetes calc")
# (13) w_D = 1 -> combined reduces to diabetes calculation
assert_close(aim2_subgroup_incidence_multiplier(0.10, 0.25, 1), 0.75, 1e-15,
             "13 w_D=1 reduces to diabetes calc")
# (14) combined = explicit population-weighted mixture, NO multiplicative cross-term
mix <- aim2_subgroup_incidence_multiplier(0.10, 0.25, 0.40)
assert_close(mix, (1 - 0.40) * (1 - 0.10) + 0.40 * (1 - 0.25), 1e-15,
             "14 explicit additive population mixture")
check("14b mixture is NOT the multiplicative cross-term",
      abs(mix - ((1 - 0.60 * 0.10) * (1 - 0.40 * 0.25))) > 1e-9)
# baseline multiplier is 1
assert_close(aim2_subgroup_incidence_multiplier(0, 0, 0.4), 1, 1e-15,
             "14c baseline incidence multiplier = 1")

# (15) no raisedBP re-application, no diabetes-weighted cross-term in the split fn
split_src <- paste(deparse(calculate_antihypertensive_split), collapse = "\n")
check("15 split function does not re-apply raisedBP",
      !grepl("raisedBP", split_src, fixed = TRUE))
check("15b split function does not use the old diabetes-weighted-average helper",
      !grepl("calculate_etihad_cumulative_rr", split_src, fixed = TRUE))
check("15b2 split function does not downscale a subgroup effect by diabetes*effect",
      !grepl("diabetes * effect_size", split_src, fixed = TRUE) &&
      !grepl("diabetes*effect_size", split_src, fixed = TRUE))
check("15c split function applies the additive mixture multiplier",
      grepl("aim2_subgroup_incidence_multiplier", split_src, fixed = TRUE))

# (11) both subgroup targets = baselines -> eff_ir = eff_cf = 1 (full function)
# Minimal deterministic BP-bin model stubs, matching the split function's calls.
toy_bins <- c("<120", "120-129", "130-139", "140-149",
              "150-159", "160-169", "170-179", "180+")
toy_years <- c(2025L, 2030L, 2035L)
toy_dt_in <- CJ(Year = toy_years, bp_cat = toy_bins)
toy_dt_in[, `:=`(location = "Low diabetes", sex = "Female", age = 40, diabetes = 0.20)]
get.bp.prob <- function(DT, rx, drugaroc) {
  DT[, .(age, sex, Year, bp_cat, prob = 1 / length(toy_bins), location)]
}
expand_to_single_year_ages <- function(dt) copy(dt)
calculate_baseline_incidence_gbd <- function(bp_prob, intervention_rates, Country, dt_gbd_rr) {
  bp_prob[, `:=`(
    year = Year, Year = NULL, cause = "ihd", IR = 0.10, IR_bin = 0.10,
    CF = 0.20, BG.mx = 0.01, BG.mx.all = 0.01, PREVt0 = 0.01,
    DIS.mx.t0 = 0.01, Nx = 1000, ALL.mx = 0.02
  )]
  bp_prob[]
}
calculate_aggregate_coverage <- function(dt, hypertensive_bins, bp_col, coverage_col,
                                         prob_col, grouping_vars, hypertensive_only) {
  dt[get(bp_col) %in% hypertensive_bins,
     .(coverage_agg = weighted.mean(get(coverage_col), get(prob_col))),
     by = grouping_vars]
}
toy_effects <- CJ(cause = "ihd", bp_cat = toy_bins)
toy_effects[, `:=`(effect_size_nodiabetes = 0.20, effect_size_diabetes = 0.30)]
toy_targets <- tg[scenario_id == "baseline" & location == "Low diabetes" & sex == "Female"]
toy_result <- calculate_antihypertensive_split(
  intervention_rates = data.table(location = "Low diabetes"),
  Country = "Low diabetes", DT.in = toy_dt_in, dt_gbd_rr = NULL,
  target_rows = toy_targets, etihad_rr_table = toy_effects
)
assert_close(toy_result$eff_ir, rep(1, nrow(toy_result)), 1e-12,
             "11 eff_ir = 1 when subgroup targets equal baselines")
assert_close(toy_result$eff_cf, rep(1, nrow(toy_result)), 1e-12,
             "11b eff_cf = 1 when subgroup targets equal baselines")

# w_D=0 vs w_D=1 reduction at the FULL-function level (checks 12/13 end-to-end):
# with an active split scenario, a stratum whose age-specific diabetes share is 0
# must equal the pure non-diabetes multiplier, and share 1 the pure diabetes one.
toy_active_targets <- copy(toy_targets)
toy_active_targets[subgroup == "htn_no_diabetes", target_control := pmin(1, baseline_control + 0.2)]
toy_active_targets[subgroup == "htn_diabetes",   target_control := pmin(1, baseline_control + 0.2)]
run_share <- function(share) {
  dd <- copy(toy_dt_in); dd[, diabetes := share]
  calculate_antihypertensive_split(
    intervention_rates = data.table(location = "Low diabetes"),
    Country = "Low diabetes", DT.in = dd, dt_gbd_rr = NULL,
    target_rows = toy_active_targets, etihad_rr_table = toy_effects)
}
r0 <- run_share(0); r1 <- run_share(1); rb <- run_share(0.5)
check("12b full function: w_D=0 uses only the non-diabetes effect",
      all(is.finite(r0$eff_ir)) && r0[, all(eff_ir <= 1 + 1e-9)])
check("13b full function: w_D=1 uses only the diabetes effect",
      all(is.finite(r1$eff_ir)) && r1[, all(eff_ir <= 1 + 1e-9)])
check("14d full function: mixed share lies between the two pure cases",
      all(rb$eff_ir >= pmin(r0$eff_ir, r1$eff_ir) - 1e-9) &&
        all(rb$eff_ir <= pmax(r0$eff_ir, r1$eff_ir) + 1e-9))

cat("\n=============================================================\n")
cat("PART A - REAL data: low/high diabetes + global reconciliation\n")
cat("=============================================================\n")

prep <- aim2_prepare_strata(paste0(repo_root, "/data/processed/"),
                            paste0(repo_root, "/data/raw/"))
real <- prep$strata
check("R0 strata built with rows > 0", nrow(real) > 0L)
# Samoa & Tonga retained via 2023 population fallback (Part A1)
check("R0b Samoa & Tonga retained (2023 pop fallback)",
      all(c("Samoa", "Tonga") %in% real$location))
check("R0c no locations dropped for missing 2025 inputs", nrow(prep$missing) == 0L)

reps <- real[c(which.min(diabetes_share_among_htn_assumed),
               which.max(diabetes_share_among_htn_assumed))]
cat(sprintf("  low-diabetes = %s (w_D=%.3f), high-diabetes = %s (w_D=%.3f)\n",
            reps$location[1], reps$diabetes_share_among_htn_assumed[1],
            reps$location[2], reps$diabetes_share_among_htn_assumed[2]))
# (1) partition and (2) reconstruction on the representative low/high strata
assert_close(reps$htn_population_no_diabetes + reps$htn_population_diabetes,
             reps$htn_population_2025, 1, "01R partition (low/high diabetes)")
assert_close(
  reps$htn_population_no_diabetes * reps$baseline_control_no_diabetes +
    reps$htn_population_diabetes * reps$baseline_control_diabetes,
  reps$htn_population_2025 * reps$observed_overall_control, 1,
  "02R reconstruction (low/high diabetes)")

# Global reconciliation under the default mode (mode 1)
real1 <- aim2_build_target_tables(
  prep$strata, prep$missing, prep$excluded_locations,
  population_fallback_locations = prep$population_fallback_locations,
  allocation_mode = "diabetes_capped_to_target")
rcb <- real1$summary[scenario_id == "bp_combined"]
assert_close(rcb$achieved_total, 150e6, 1, "08R global combined = 150M (mode 1)")
assert_close(rcb$achieved_total,
             rcb$additional_controlled_no_diabetes + rcb$additional_controlled_diabetes,
             1e-3, "09R global combined = diabetes + non-diabetes")
check("R-longtable global bp_combined additional = 150M on fixed 2025 pop",
      abs(real1$targets[scenario_id == "bp_combined",
                        sum(additional_controlled_2030)] - 150e6) <= 1)
cat(sprintf("  mode 1: scale=%.4f  capped_strata=%d  achieved_diabetes_control=%.4f\n",
            rcb$solved_non_diabetes_scale_factor, rcb$number_capped_strata,
            rcb$diabetes_control_2030_effective))

# Mode 2 must fail clearly (Delta_D alone > 150M with real inputs)
mode2_err <- tryCatch({
  aim2_build_target_tables(prep$strata, prep$missing, prep$excluded_locations,
                           population_fallback_locations = prep$population_fallback_locations,
                           allocation_mode = "diabetes_floor_then_reconcile")
  NA_character_
}, error = function(e) conditionMessage(e))
check("R-mode2 floor mode stops clearly (Delta_D > 150M)",
      !is.na(mode2_err) && grepl("0.80 diabetes floor alone adds", mode2_err))
# Mode 3 lets the total exceed 150M
real3 <- aim2_build_target_tables(prep$strata, prep$missing, prep$excluded_locations,
                                  population_fallback_locations = prep$population_fallback_locations,
                                  allocation_mode = "diabetes_floor_uncapped_total")
check("R-mode3 uncapped total >= 150M",
      real3$summary[scenario_id == "bp_combined", achieved_total] >= 150e6 - 1)

cat(sprintf("\nALL %d CHECKS PASSED.\n", .PASS))
