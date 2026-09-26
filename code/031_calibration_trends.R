
# 031_calibration_trends.R: read calibrated historical rates from stage 03,
# apply secular trends, validate, and write adjusted annual rates.
# All inputs are loaded from disk so calibration need not run in this session.
required_settings <- c(
  "wd_data", "calibrated_stub", "adjusted_stub", "N_OUT_CHUNKS",
  "YEARS_HIST", "YEARS_TP", "AGES", "SEXES", "MODEL_CAUSES", "TP_EPS",
  "run_bgmx_trend", "run_CF_trend", "run_CF_trend_80", "run_CF_trend_ihme"
)
missing_settings <- required_settings[!vapply(required_settings, exists, logical(1))]
if (length(missing_settings)) {
  stop("031_calibration_trends.R: missing settings from 00_run_model.R: ",
       paste(missing_settings, collapse = ", "), call. = FALSE)
}
req_cols <- c("age", "sex", "location", "year", "cause", "BG.mx.all", "ALL.mx",
              "BG.mx", "PREVt0", "DIS.mx.t0", "Nx", "IR", "CF")

files <- list.files(
  path       = wd_data,
  pattern    = paste0("^", calibrated_stub, "[0-9]+\\.rds$"),
  full.names = TRUE
)
if (!length(files)) {
  stop("031_calibration_trends.R: no ", calibrated_stub, "*.rds in ", wd_data,
       "; run 03_calibration.R first.", call. = FALSE)
}

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# Bind them all together, matching columns by name and filling missing ones
b_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

rm(dt_list, files)

if (!setequal(names(b_rates), req_cols)) {
  stop("031_calibration_trends.R: calibrated rates have the wrong schema.",
       call. = FALSE)
}
if (!setequal(unique(b_rates$year), YEARS_HIST)) {
  stop("031_calibration_trends.R: calibrated inputs must contain exactly YEARS_HIST; ",
       "check calibrated_searo_part*.rds.", call. = FALSE)
}
locs <- sort(unique(b_rates$location))

HIST_END  <- max(YEARS_HIST)          # last calibrated/observed year (2019)

#===============================================================================
# 7. EXTEND TO THE FULL PROJECTION HORIZON (repeat the last observed year)
#    The 2020-2050 rows start as copies of HIST_END, exactly as the legacy
#    stage 05 did before applying secular trends.
#===============================================================================

proj_years <- setdiff(YEARS_TP, unique(b_rates$year))
proj_years <- proj_years[proj_years > HIST_END]
if (length(proj_years)) {
  rep_row <- b_rates[year == HIST_END]
  b_rates <- rbindlist(c(list(b_rates),
                         lapply(proj_years, function(y) copy(rep_row)[, year := y])),
                       use.names = TRUE)
}
setorder(b_rates, location, sex, cause, year, age)

#===============================================================================
# 7. SECULAR TRENDS  (projected years only; each applied exactly once)
#===============================================================================

## The forecasts are Global (percent_diff cumulative from the 2019 base), keyed
## by age x sex x cause x year with full cause names. Applied only to year > HIST_END
## so the calibration/observed years are never trended, and once per year so a
## cumulative percent change is never compounded.
apply_trend <- function(dt, file, target_col, share = 1) {
  if (!file.exists(file)) stop("Stage 03: trend file not found: ", file, call. = FALSE)
  f <- readRDS(file); setDT(f)
  if (!all(c("age", "sex", "cause", "year", "percent_diff") %in% names(f))) {
    stop("Stage 03: trend file ", basename(file),
         " lacks age/sex/cause/year/percent_diff.", call. = FALSE)
  }
  f[, cause := trimws(as.character(cause))]
  f <- unique(f[year > HIST_END, .(age, sex, cause, year, percent_diff)],
              by = c("age", "sex", "cause", "year"))
  dt <- merge(dt, f, by = c("age", "sex", "cause", "year"), all.x = TRUE)
  dt[year > HIST_END & !is.na(percent_diff),
     (target_col) := get(target_col) * (1 + percent_diff * share)]
  dt[, percent_diff := NULL]
  dt[]
}

if (isTRUE(run_bgmx_trend)) {
  cat("  Applying background-mortality trends (BG.mx, BG.mx.all)...\n")
  b_rates <- apply_trend(b_rates, file.path(wd_data, "tps_bgmx_forecasted.rds"),     "BG.mx")
  b_rates <- apply_trend(b_rates, file.path(wd_data, "tps_bgmx_all_forecasted.rds"), "BG.mx.all")
}

if (isTRUE(run_CF_trend)) {
  cf_share <- if (isTRUE(run_CF_trend_80)) 0.8 else 1
  if (isTRUE(run_CF_trend_ihme)) {
    cat("  Applying CF trend (IHME Foresight CVD forecast, cause x year)...\n")
    f <- readRDS(file.path(wd_data, "tps_bgmx_cvd_ihme.rds")); setDT(f)
    ## IHME file uses short cause codes and (year, cause) only; map to full names.
    short2full <- c(ihd = "Ischemic heart disease", istroke = "Ischemic stroke",
                    hstroke = "Intracerebral hemorrhage", hhd = "Hypertensive heart disease")
    f[, cause := fifelse(cause %in% names(short2full), short2full[cause], cause)]
    f <- unique(f[year > HIST_END, .(cause, year, percent_diff)], by = c("cause", "year"))
    b_rates <- merge(b_rates, f, by = c("cause", "year"), all.x = TRUE)
    b_rates[year > HIST_END & !is.na(percent_diff), CF := CF * (1 + percent_diff * cf_share)]
    b_rates[, percent_diff := NULL]
  } else {
    cat(sprintf("  Applying CF trend (CVD forecast, %.0f%% of secular decline)...\n",
                100 * cf_share))
    b_rates <- apply_trend(b_rates, file.path(wd_data, "tps_bgmx_cvd_forecasted.rds"),
                           "CF", share = cf_share)
  }
}

#===============================================================================
# 8. PROBABILITY GUARD  (0<=IR,CF,BG.mx<=1; IR+BG.mx<=1; CF+BG.mx<=1)
#    BG.mx is preserved; the disease TP is capped into the remaining headroom.
#===============================================================================

b_rates[is.na(IR), IR := 0]; b_rates[is.na(CF), CF := 0]; b_rates[is.na(BG.mx), BG.mx := 0]
b_rates[IR < 0, IR := 0]; b_rates[IR > 1, IR := 1]
b_rates[CF < 0, CF := 0]; b_rates[CF > 1, CF := 1]
b_rates[BG.mx < 0, BG.mx := 0]; b_rates[BG.mx > 1, BG.mx := 1]
b_rates[, headroom := 1 - BG.mx - TP_EPS]
n_ir_cap <- b_rates[headroom >= 0 & IR > headroom, .N]
n_cf_cap <- b_rates[headroom >= 0 & CF > headroom, .N]
b_rates[headroom >= 0 & IR > headroom, IR := headroom]
b_rates[headroom >= 0 & CF > headroom, CF := headroom]
## Rows where BG.mx alone leaves no headroom: shrink BG.mx just enough (flagged).
n_bg_fix <- b_rates[headroom < 0, .N]
b_rates[headroom < 0, `:=`(BG.mx = 1 - TP_EPS, IR = 0, CF = 0)]
b_rates[, headroom := NULL]

#===============================================================================
# 9. VALIDATION
#===============================================================================

cat("\n", strrep("=", 70), "\nSTAGE 03 VALIDATION\n", strrep("=", 70), "\n", sep = "")

expected_rows <- length(locs) * length(SEXES) * length(MODEL_CAUSES) *
  length(AGES) * length(YEARS_TP)
key_cols <- c("location", "sex", "cause", "year", "age")
num_cols <- c("Nx", "ALL.mx", "BG.mx.all", "BG.mx", "PREVt0", "DIS.mx.t0", "IR", "CF")

stopifnot(
  "schema != tps_inpt"        = setequal(names(b_rates), req_cols),
  "duplicate output keys"     = !anyDuplicated(b_rates, by = key_cols),
  "wrong row count"           = nrow(b_rates) == expected_rows,
  "age coverage != 20:95"     = setequal(unique(b_rates$age), AGES),
  "year coverage != YEARS_TP" = setequal(unique(b_rates$year), YEARS_TP),
  "cause set changed"         = setequal(unique(b_rates$cause), MODEL_CAUSES),
  "location set changed"      = setequal(unique(b_rates$location), locs),
  "non-finite numeric field"  = b_rates[, all(is.finite(as.matrix(.SD))), .SDcols = num_cols],
  "negative Nx"               = b_rates[, all(Nx >= 0)],
  "negative mortality"        = b_rates[, all(ALL.mx >= 0 & BG.mx.all >= 0 & DIS.mx.t0 >= 0)],
  "IR outside [0,1]"          = b_rates[, all(IR >= 0 & IR <= 1)],
  "CF outside [0,1]"          = b_rates[, all(CF >= 0 & CF <= 1)],
  "BG.mx outside [0,1]"       = b_rates[, all(BG.mx >= 0 & BG.mx <= 1)],
  "PREVt0 outside [0,1]"      = b_rates[, all(PREVt0 >= 0 & PREVt0 <= 1)],
  "IR + BG.mx > 1"            = b_rates[, all(IR + BG.mx <= 1 + 1e-9)],
  "CF + BG.mx > 1"            = b_rates[, all(CF + BG.mx <= 1 + 1e-9)]
)
cat("All schema / coverage / probability constraints satisfied.\n")
cat(sprintf("Rows: %d (expected %d). Locations %d | causes %d | ages %d-%d | years %d-%d.\n",
            nrow(b_rates), expected_rows, length(locs), length(MODEL_CAUSES),
            min(AGES), max(AGES), min(YEARS_TP), max(YEARS_TP)))
cat(sprintf("Probability guard: IR capped %d rows, CF capped %d rows, BG.mx shrunk %d rows.\n",
            n_ir_cap, n_cf_cap, n_bg_fix))

## Read the stage-03 audit independently; the calibration script may not have
## run in this R session. Its CSV calls the calibrated objective cal_error.
factor_file <- file.path(wd_data, "calibration_factors.csv")
if (!file.exists(factor_file)) {
  stop("031_calibration_trends.R: missing calibration_factors.csv; ",
       "run 03_calibration.R first.", call. = FALSE)
}
factors <- fread(factor_file)
required_factor_cols <- c("base_error", "cal_error", "ir_mult", "cf_mult")
if (!all(required_factor_cols %in% names(factors))) {
  stop("031_calibration_trends.R: calibration_factors.csv is missing: ",
       paste(setdiff(required_factor_cols, names(factors)), collapse = ", "),
       call. = FALSE)
}
if (!setequal(unique(factors$location), locs)) {
  stop("031_calibration_trends.R: calibration factors and calibrated rates ",
       "have different location sets.", call. = FALSE)
}
tot_base <- factors[, sum(base_error, na.rm = TRUE)]
tot_cal  <- factors[, sum(cal_error, na.rm = TRUE)]
cat(sprintf("Calibration objective: baseline %.4g -> calibrated %.4g (%.1f%% reduction across %d bands).\n",
            tot_base, tot_cal, 100 * (tot_base - tot_cal) / max(tot_base, 1e-9), nrow(factors)))
if (tot_cal > tot_base + 1e-6) {
  stop("Stage 03: calibrated objective worse than baseline -- (1,1) not honoured.",
       call. = FALSE)
}
n_bound <- factors[abs(ir_mult - min(CAL_IR_GRID)) < 1e-9 |
                     abs(ir_mult - max(CAL_IR_GRID)) < 1e-9 |
                     abs(cf_mult - min(CAL_CF_GRID)) < 1e-9 |
                     abs(cf_mult - max(CAL_CF_GRID)) < 1e-9, .N]
if (n_bound > 0) {
  cat(sprintf("NOTE: %d of %d bands chose a grid-boundary multiplier; consider widening the grid.\n",
              n_bound, nrow(factors)))
}

#===============================================================================
# 10. WRITE OUTPUTS  (adjusted_searo_part*.rds, chunked by whole location)
#===============================================================================

setcolorder(b_rates, req_cols)
out_locs <- sort(unique(b_rates$location))
n_chunks <- max(1L, min(N_OUT_CHUNKS, length(out_locs)))
chunk_of <- if (n_chunks <= 1L) list(out_locs) else
  split(out_locs, cut(seq_along(out_locs), breaks = n_chunks, labels = FALSE))

## Clear only this stub's stale chunks so nothing else called "adjusted*" is
## silently mixed in by the downstream globbing.
old <- list.files(wd_data, pattern = paste0("^", adjusted_stub, "[0-9]+\\.rds$"),
                  full.names = TRUE)
if (length(old)) file.remove(old)
for (i in seq_along(chunk_of)) {
  saveRDS(b_rates[location %in% chunk_of[[i]]],
          file = file.path(wd_data, paste0(adjusted_stub, i, ".rds")))
}


## Tidy local audit data; keep b_rates for the in-memory handoff to 04/05.
rm(factors)
invisible(gc())
