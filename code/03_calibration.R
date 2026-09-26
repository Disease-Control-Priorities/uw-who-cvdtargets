#===============================================================================
# 03_calibration.R  --  Stage 03: calibrate + finalize transition probabilities
#-------------------------------------------------------------------------------
# Consumes the stage-02 transition-probability inputs (tps_inpt_part*.rds) and
# the project's GBD 2023 calibration targets, and writes the FINAL, calibrated
# and secular-trend-projected annual transition probabilities to
#
#     data/processed/adjusted_searo_part*.rds
#
# one row per  location x year x sex x modeled cause x single age 20:95
# (age 95 = the open-ended 95+ group). These files are used directly by stages
# 04 (interventions), 05 (baseline) and 06 (scenarios); no later stage re-applies
# a calibration factor or a secular trend.
#
# WHAT THIS REPLACES
# ------------------
# The legacy chain 03_clean_inputs.R -> 031_calibration.R -> 032_adjustments.R,
# plus the run_adjustment_model / run_bgmx_trend / run_CF_trend blocks that used
# to live in 05_build_baseline.R. Those blocks are removed from stage 05 so each
# baseline adjustment is applied EXACTLY ONCE, here.
#
# ONE consolidated calibration (was a two-pass 031 fine-grid then 032 wide-grid):
#   * 031 used a cohort projection (age -> age+1) that matches the main model in
#     06_run_scenarios_multiple.R; 032 used a period projection (same age year to
#     year), which does not. This stage keeps the COHORT projection consistent
#     with 06, and searches ONE multiplier grid spanning the union of the two
#     legacy ranges. The calibrated fit is therefore never worse than the
#     uncalibrated baseline (the identity multiplier (1,1) is in the grid) and
#     targets the same GBD Deaths + Prevalence objective the project always used.
#     It is NOT bit-identical to the legacy two-pass result; downstream reports
#     should be re-knit. See dev/refactor_cvd_calibration_prompt.md.
#
# METHOD (per location x sex x cause x GBD 5-year age band)
#   * Project the well-sick-dead cohort over CAL_YEAR_START..CAL_YEAR_END for
#     every candidate (IRmult, CFmult) on the grid, applied globally.
#   * Aggregate simulated Deaths (dead) and Prevalence (sick) to GBD age bands
#     and score against GBD with error = W_DEATHS*RMSE_deaths + W_PREV*RMSE_prev.
#   * Pick the (IRmult, CFmult) minimising that error for each band; the identity
#     (1,1) is a candidate so a worse-fitting multiplier is never selected.
#   * Bake the winning multipliers into IR/CF for ALL years (historical + future)
#     -- exactly as the legacy factors were applied to every year.
#
# SECULAR TRENDS (baseline projection assumptions, applied to year > CAL/hist end)
#   * BG.mx     x (1 + percent_diff)               [tps_bgmx_forecasted.rds]
#   * BG.mx.all x (1 + percent_diff)               [tps_bgmx_all_forecasted.rds]
#   * CF        x (1 + percent_diff * cf_share)    [tps_bgmx_cvd_forecasted.rds]
#     with cf_share = 0.8 when run_CF_trend_80 (20% of the historical CF decline
#     attributed to past HTN-control gains), else 1. run_CF_trend_ihme swaps in
#     an alternative CVD forecast (tps_bgmx_cvd_ihme.rds), keyed by cause x year.
#   percent_diff is CUMULATIVE from the 2019 base year, so it is applied once per
#   projected row (never compounded) and never to the calibration years.
#
# PROBABILITY GUARD (enforced on the final rows)
#   0 <= IR, CF, BG.mx <= 1 and IR + BG.mx <= 1 and CF + BG.mx <= 1. BG.mx is
#   preserved; the disease TP is capped into the remaining headroom. Rows where
#   even BG.mx alone left no headroom are counted for audit (should be none).
#
# INPUT / OUTPUT CONTRACT
#   in : data/processed/tps_inpt_part*.rds  (stage 02): age, sex, location, year,
#        cause (full GBD name), BG.mx.all, ALL.mx, BG.mx, PREVt0, DIS.mx.t0, Nx,
#        IR, CF; years YEARS_HIST; ages 20:95; four CVD causes.
#        data/raw/GBD/temp_1baseline_rates_gbd23.rds (GBD targets; read-only).
#        data/processed/tps_bgmx_*_forecasted.rds (secular-trend forecasts).
#   out: data/processed/adjusted_searo_part*.rds  (SAME 13-column schema as
#        tps_inpt, full cause names, years YEARS_TP) + calibration_factors.csv,
#        calibration_diagnostics.csv (audit, no "adjusted" in the name so the
#        globbing in 04/05 never mixes them in).
#===============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(foreach)
  library(doParallel)
  library(parallel)
})

message("\n== Stage 03: calibrate + finalize transition probabilities ==")

## ---- 0. Settings from 00_run_model.R (single source of truth) --------------
## Stage 03 checks for these rather than defining competing local values.
.needed <- c("wd_data", "wd_raw", "MODEL_CAUSES", "AGES", "SEXES",
             "YEARS_HIST", "YEARS_TP", "CAL_YEAR_START", "CAL_YEAR_END",
             "CAL_IR_GRID", "CAL_CF_GRID", "W_DEATHS", "W_PREV", "CAL_TP_CAP",
             "TP_EPS", "N_OUT_CHUNKS", "tps_inpt_stub", "calibrated_stub",
             "gbd_target_file", "run_bgmx_trend", "run_CF_trend",
             "run_CF_trend_80", "run_CF_trend_ihme")
.missing <- .needed[!vapply(.needed, exists, logical(1))]
if (length(.missing)) {
  stop("03_calibration.R must be sourced after 00_run_model.R. Missing settings: ",
       paste(.missing, collapse = ", "), call. = FALSE)
}
RUN_PAR   <- if (exists("run_calibration_par")) isTRUE(run_calibration_par) else TRUE
MAX_CORES <- if (exists("cal_max_cores")) cal_max_cores else 10L
HIST_END  <- max(YEARS_HIST)          # last calibrated/observed year (2019)

## ---- helper: single age -> GBD 5-year band label ---------------------------
## Matches create_gbd_age_group() in 05_build_baseline.R and the GBD age_name
## labels stripped of " years": "20-24", ..., "90-94", "95+".
cal_age_group <- function(age) {
  as.character(cut(age,
                   breaks = c(seq(20, 95, by = 5), Inf),
                   labels = c(paste0(seq(20, 90, by = 5), "-", seq(24, 94, by = 5)), "95+"),
                   right = FALSE, include.lowest = TRUE))
}

#===============================================================================
# 1. LOAD STAGE-02 TRANSITION-PROBABILITY INPUTS (this stage never writes them)
#===============================================================================

tps_files <- list.files(wd_data, pattern = tps_inpt_stub, full.names = TRUE)
if (!length(tps_files)) {
  stop("Stage 03: no ", tps_inpt_stub, " files in ", wd_data,
       " -- run stage 02 first.", call. = FALSE)
}
b_rates <- rbindlist(lapply(tps_files, function(f) { d <- readRDS(f); setDT(d); d }),
                     use.names = TRUE, fill = TRUE)

req_cols <- c("age", "sex", "location", "year", "cause", "BG.mx.all", "ALL.mx",
              "BG.mx", "PREVt0", "DIS.mx.t0", "Nx", "IR", "CF")
miss_cols <- setdiff(req_cols, names(b_rates))
if (length(miss_cols)) {
  stop("Stage 03: tps_inpt is missing columns: ", paste(miss_cols, collapse = ", "),
       call. = FALSE)
}
b_rates <- b_rates[, ..req_cols]
b_rates[, cause := trimws(as.character(cause))]

## Fail clearly on the wrong scope rather than calibrating garbage.
if (!setequal(unique(b_rates$cause), MODEL_CAUSES)) {
  stop("Stage 03: tps_inpt causes (", paste(sort(unique(b_rates$cause)), collapse = ", "),
       ") do not match the four modeled causes.", call. = FALSE)
}
if (!all(range(b_rates$age) == c(min(AGES), max(AGES)))) {
  stop("Stage 03: tps_inpt age range is ", paste(range(b_rates$age), collapse = "-"),
       ", expected ", min(AGES), "-", max(AGES), ".", call. = FALSE)
}
if (!all(YEARS_HIST %in% unique(b_rates$year))) {
  stop("Stage 03: tps_inpt is missing historical years ",
       paste(setdiff(YEARS_HIST, unique(b_rates$year)), collapse = ", "), call. = FALSE)
}
if (anyNA(b_rates[, ..req_cols])) stop("Stage 03: tps_inpt contains NA values.", call. = FALSE)

## Defensive clamps on the raw baseline probabilities (as in the legacy stage).
b_rates[CF >= 1, CF := 0.99]; b_rates[IR >= 1, IR := 0.99]
b_rates[CF < 0,  CF := 0];    b_rates[IR < 0,  IR := 0]

## Optional pilot subset for testing one/few countries end to end before the full
## calibration (mirrors who_cvd.pilot_country in 06). Default NULL = all countries.
cal_pilot <- getOption("who_cvd.cal_pilot", NULL)
if (!is.null(cal_pilot)) {
  if (!all(cal_pilot %in% unique(b_rates$location))) {
    stop("who_cvd.cal_pilot names locations not in tps_inpt: ",
         paste(setdiff(cal_pilot, unique(b_rates$location)), collapse = ", "), call. = FALSE)
  }
  b_rates <- b_rates[location %in% cal_pilot]
  message("  [pilot] restricting stage 03 to: ", paste(cal_pilot, collapse = ", "))
}

locs <- sort(unique(b_rates$location))
cat(sprintf("  Loaded tps_inpt: %d rows | %d locations | causes: %s | years %d-%d\n",
            nrow(b_rates), length(locs), paste(MODEL_CAUSES, collapse = ", "),
            min(YEARS_HIST), HIST_END))

#===============================================================================
# 2. GBD CALIBRATION TARGETS  (Deaths + Prevalence, Number, by GBD age band)
#===============================================================================

if (!file.exists(gbd_target_file)) {
  stop("Stage 03: GBD target file not found: ", gbd_target_file, call. = FALSE)
}
gbd <- readRDS(gbd_target_file); setDT(gbd)
gbd <- gbd[metric_name == "Number" &
             measure_name %in% c("Deaths", "Prevalence") &
             cause_name %in% MODEL_CAUSES &
             location_name %in% locs &
             year >= CAL_YEAR_START & year <= CAL_YEAR_END,
           .(location = location_name, sex = sex_name, cause = cause_name,
             age_group = sub(" years$", "", age_name), year, measure = measure_name,
             val)]
gbd <- dcast(gbd, location + sex + cause + age_group + year ~ measure,
             value.var = "val", fun.aggregate = sum)
if (!"Deaths" %in% names(gbd))     gbd[, Deaths := 0]
if (!"Prevalence" %in% names(gbd)) gbd[, Prevalence := 0]
targets <- gbd[, .(location, sex, cause, age_group, year,
                   gbdDeaths = fifelse(is.na(Deaths), 0, Deaths),
                   gbdPrev   = fifelse(is.na(Prevalence), 0, Prevalence))]
missing_target_locs <- setdiff(locs, unique(targets$location))
if (length(missing_target_locs)) {
  stop("Stage 03: no GBD calibration target for: ",
       paste(missing_target_locs, collapse = ", "), call. = FALSE)
}
rm(gbd)

#===============================================================================
# 3. CALIBRATION ENGINE  (cohort well-sick-dead projection, consistent with 06)
#===============================================================================

## Initialise cohort states on the calibration window. States are set for the
## start year (all ages) and for age 20 (all years, the incoming cohort each
## year, taken from the year-specific Nx already present in tps_inpt).
init_cohort <- function(dt, y0) {
  dt[, c("sick", "dead", "well", "pop", "all.mx") := NA_real_]
  dt[year == y0 | age == 20L, `:=`(
    sick   = Nx * PREVt0,
    dead   = Nx * DIS.mx.t0,
    well   = Nx * (1 - (PREVt0 + ALL.mx)),
    pop    = Nx,
    all.mx = Nx * ALL.mx
  )]
  dt[]
}

## One year of cohort ageing (age -> age+1), identical in structure to the
## projection loop in 06_run_scenarios_multiple.R. Deaths across causes deplete
## the shared population via BG.mx.all. Robust merge-update (no positional
## alignment); age 20 keeps its re-initialised incoming-cohort value.
run_cohort <- function(dt, y0, y1) {
  setorder(dt, sex, location, cause, age, year)
  for (i in seq_len(y1 - y0)) {
    yr <- y0 + i
    b2 <- dt[year >= yr - 1L & year <= yr]
    b2[, `:=`(
      sick2 = shift(sick) * (1 - (CF + BG.mx)) + shift(well) * IR,
      dead2 = shift(sick) * CF,
      pop2  = shift(pop)  - shift(all.mx)
    ), by = .(sex, location, cause, age)]
    b2[sick2 < 0, sick2 := 0]; b2[dead2 < 0, dead2 := 0]; b2[pop2 < 0, pop2 := 0]
    b2[, all.mx2 := sum(dead2, na.rm = TRUE), by = .(sex, location, year, age)]
    b2[, all.mx2 := all.mx2 + pop2 * BG.mx.all]
    b2[all.mx2 < 0, all.mx2 := 0]
    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]
    b2 <- b2[year == yr & age < 95L,
             .(location, sex, cause, age = age + 1L, year = yr,
               sick2, dead2, well2, pop2, all.mx2)]
    dt[b2, on = .(location, sex, cause, age, year),
       `:=`(sick = i.sick2, dead = i.dead2, well = i.well2,
            pop = i.pop2, all.mx = i.all.mx2)]
  }
  dt[]
}

## Score one global multiplier: project, aggregate to GBD age bands, and return
## the per-band error (W_DEATHS*RMSE_deaths + W_PREV*RMSE_prev over the window).
score_multiplier <- function(base_win, tgt, ir_mult, cf_mult) {
  dt <- copy(base_win)
  dt[, IR := pmin(IR * ir_mult, CAL_TP_CAP)]
  dt[, CF := pmin(CF * cf_mult, CAL_TP_CAP)]
  dt <- init_cohort(dt, CAL_YEAR_START)
  dt <- run_cohort(dt, CAL_YEAR_START, CAL_YEAR_END)
  agg <- dt[, .(Prevalence = sum(sick, na.rm = TRUE),
                Deaths     = sum(dead, na.rm = TRUE)),
            by = .(location, sex, cause, age_group, year)]
  j <- merge(agg, tgt, by = c("location", "sex", "cause", "age_group", "year"),
             all.x = TRUE)
  j[is.na(gbdDeaths), gbdDeaths := 0]; j[is.na(gbdPrev), gbdPrev := 0]
  err <- j[, .(RMSE_deaths = sqrt(mean((gbdDeaths - Deaths)^2,     na.rm = TRUE)),
               RMSE_prev   = sqrt(mean((gbdPrev   - Prevalence)^2, na.rm = TRUE))),
           by = .(location, sex, cause, age_group)]
  err[, `:=`(error = W_DEATHS * RMSE_deaths + W_PREV * RMSE_prev,
             ir_mult = ir_mult, cf_mult = cf_mult)]
  err[]
}

## Calibrate one group of locations: search the full grid, keep the identity
## (1,1) baseline for audit, and return the winning per-band multipliers.
## base_sub / tgt_sub are already restricted to the group's locations so a worker
## receives only its share of the data, not the full country panel.
calibrate_group <- function(base_sub, tgt_sub, grid) {
  base_win <- base_sub[year >= CAL_YEAR_START & year <= CAL_YEAR_END]
  base_win[, age_group := cal_age_group(age)]

  scores <- rbindlist(lapply(seq_len(nrow(grid)), function(k)
    score_multiplier(base_win, tgt_sub, grid$ir_mult[k], grid$cf_mult[k])),
    use.names = TRUE)
  rm(base_win); invisible(gc(FALSE))   # release the projection working set

  key <- c("location", "sex", "cause", "age_group")
  setorder(scores, location, sex, cause, age_group, error)
  best <- scores[, .SD[1L], by = key]
  base <- scores[abs(ir_mult - 1) < 1e-9 & abs(cf_mult - 1) < 1e-9,
                 .(location, sex, cause, age_group, base_error = error)]
  merge(best, base, by = key, all.x = TRUE)
}

#===============================================================================
# 4. RUN CALIBRATION OVER ALL LOCATIONS
#===============================================================================

grid <- CJ(ir_mult = CAL_IR_GRID, cf_mult = CAL_CF_GRID)
if (!any(abs(grid$ir_mult - 1) < 1e-9 & abs(grid$cf_mult - 1) < 1e-9)) {
  stop("Stage 03: the identity multiplier (1,1) must be in the grid so the ",
       "calibrated fit is never worse than baseline.", call. = FALSE)
}
cat(sprintf("  Calibration grid: %d IR x %d CF = %d candidates | window %d-%d | objective %g*Deaths + %g*Prev\n",
            length(CAL_IR_GRID), length(CAL_CF_GRID), nrow(grid),
            CAL_YEAR_START, CAL_YEAR_END, W_DEATHS, W_PREV))

## Use MORE, SMALLER groups than cores so each parallel task holds only a few
## countries' rows (bounds per-worker peak memory; a PSOCK worker that runs out
## of memory dies with "error reading from connection"). Workers process several
## small tasks in sequence, releasing memory between them.
n_groups <- max(1L, min(length(locs), MAX_CORES * 3L))
groups   <- if (n_groups <= 1L) list(locs) else
  split(locs, cut(seq_along(locs), breaks = n_groups, labels = FALSE))
## Pre-split so each worker gets only its group's rows (not the full panel).
base_by_group <- lapply(groups, function(gl) b_rates[location %in% gl])
tgt_by_group  <- lapply(groups, function(gl) targets[location %in% gl])

if (RUN_PAR && n_groups > 1L) {
  n_cores <- max(1L, min(MAX_CORES, n_groups, parallel::detectCores() - 1L))
  cat(sprintf("  Calibrating in parallel on %d cores (%d location groups)...\n",
              n_cores, length(groups)))
  cl <- makeCluster(n_cores)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  registerDoParallel(cl)
  clusterExport(cl, c("score_multiplier", "init_cohort", "run_cohort",
                      "cal_age_group", "calibrate_group", "grid",
                      "CAL_YEAR_START", "CAL_YEAR_END",
                      "CAL_TP_CAP", "W_DEATHS", "W_PREV"), envir = environment())
  factors <- foreach(base_sub = base_by_group, tgt_sub = tgt_by_group,
                     .combine = rbind, .packages = "data.table") %dopar% {
    setDTthreads(1L)
    calibrate_group(base_sub, tgt_sub, grid)
  }
  try(stopCluster(cl), silent = TRUE)
} else {
  cat("  Calibrating sequentially...\n")
  factors <- rbindlist(Map(function(base_sub, tgt_sub, gi) {
    cat(sprintf("    group %d/%d (%d locations)\n", gi, length(groups),
                uniqueN(base_sub$location)))
    calibrate_group(base_sub, tgt_sub, grid)
  }, base_by_group, tgt_by_group, seq_along(groups)), use.names = TRUE)
}
setDT(factors)
rm(base_by_group, tgt_by_group)

#===============================================================================
# 5. BAKE CALIBRATION FACTORS INTO IR/CF FOR EVERY YEAR
#===============================================================================

b_rates[, age_group := cal_age_group(age)]
b_rates <- merge(b_rates, factors[, .(location, sex, cause, age_group, ir_mult, cf_mult)],
                 by = c("location", "sex", "cause", "age_group"), all.x = TRUE)
b_rates[is.na(ir_mult), ir_mult := 1]
b_rates[is.na(cf_mult), cf_mult := 1]
b_rates[, `:=`(IR = IR * ir_mult, CF = CF * cf_mult)]
b_rates[, c("age_group", "ir_mult", "cf_mult") := NULL]

#===============================================================================
# 6. WRITE OUTPUTS  (adjusted_searo_part*.rds, chunked by whole location)
#===============================================================================

setcolorder(b_rates, req_cols)
out_locs <- sort(unique(b_rates$location))
n_chunks <- max(1L, min(N_OUT_CHUNKS, length(out_locs)))
chunk_of <- if (n_chunks <= 1L) list(out_locs) else
  split(out_locs, cut(seq_along(out_locs), breaks = n_chunks, labels = FALSE))

## Clear only this stub's stale chunks so nothing else called "adjusted*" is
## silently mixed in by the downstream globbing.
old <- list.files(wd_data, pattern = paste0("^", calibrated_stub, "[0-9]+\\.rds$"),
                  full.names = TRUE)
if (length(old)) file.remove(old)
for (i in seq_along(chunk_of)) {
  saveRDS(b_rates[location %in% chunk_of[[i]]],
          file = file.path(wd_data, paste0(calibrated_stub, i, ".rds")))
}

fwrite(factors[, .(location, sex, cause, age_group, ir_mult, cf_mult,
                   base_error, cal_error = error)],
       file.path(wd_data, "calibration_factors.csv"))
fwrite(factors[, .(location, sex, cause, age_group,
                   base_error, cal_error = error,
                   pct_improvement = 100 * (base_error - error) / pmax(base_error, 1e-9),
                   RMSE_deaths, RMSE_prev)],
       file.path(wd_data, "calibration_diagnostics.csv"))

cat(sprintf("Wrote %d %s*.rds + calibration_factors.csv + calibration_diagnostics.csv to %s\n",
            length(chunk_of), adjusted_stub, wd_data))
cat("Stage 03 complete: calibrated_searo_part*.rds now hold FINAL calibrated + trended rates.\n")

## Tidy the large temporaries; keep b_rates for the in-memory handoff to 04/05.
rm(targets, factors, grid, groups)
invisible(gc())
