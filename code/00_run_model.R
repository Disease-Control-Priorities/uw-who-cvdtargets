rm(list=ls()) 

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)   
library(countrycode)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(gmodels)

# For forecasting mortality
library(forecast)

# ---- Repository-relative paths (single source of truth) --------------------
# wd is detected from the repo root so the pipeline runs from any clone. Override
# with options(who_cvd.wd = "...") or the WHO_CVD_WD env var; the final fallback
# is the original author path.
wd <- local({
  o <- getOption("who_cvd.wd", Sys.getenv("WHO_CVD_WD", ""))
  if (nzchar(o)) return(normalizePath(o, winslash = "/", mustWork = FALSE))
  args  <- commandArgs(FALSE)
  fa    <- grep("^--file=", args, value = TRUE)
  start <- if (length(fa))
    dirname(normalizePath(sub("^--file=", "", fa[[1]]), winslash = "/", mustWork = FALSE))
  else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  d <- start
  for (i in 1:8) {
    if (file.exists(file.path(d, "uw-who-cvdtargets.Rproj"))) return(d)
    parent <- dirname(d); if (identical(parent, d)) break; d <- parent
  }
  "C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/github/uw-who-cvdtargets"
})
if (!endsWith(wd, "/")) wd <- paste0(wd, "/")

wd_code <- paste0(wd, "code/")

# Raw data not available on GitHub (read-only inputs, gitignored)
wd_raw <- paste0(wd, "data/raw/")

# Processed data (calibrated/derived inputs AND stage-03 outputs)
wd_data <- paste0(wd, "data/processed/")
wd_outp <- paste0(wd, "output/")

# Repo-relative scratch space used by the stage-02 helpers (021/022/023).
wd_temp <- paste0(wd, "data/temp/")
if (!dir.exists(wd_temp)) {
  dir.create(wd_temp, recursive = TRUE)
}

setwd(wd_code)

#...........................................................
# 0. Functions and parameters-----
#...........................................................

source("01_utils.R")

run_calibration_par <- FALSE # stage 03: run the calibration search in parallel

run_aod_par <- FALSE # 022_get_tps.R: include the dementia arm (off)

# Secular-trend switches, now consumed by 03_calibration.R (moved out of 05):
run_bgmx_trend <- TRUE  # apply BG.mx / BG.mx.all forecast trends (year > 2019)

run_CF_trend   <- TRUE  # apply the CVD case-fatality forecast trend

# Baseline scenario 80% of secular trend. 20% historically explained by
# HTN control improvements

run_CF_trend_80   <- TRUE

run_CF_trend_ihme  <- FALSE  # alternative CF trend (IHME Foresight) vs GBD

# Vestigial: the calibration is now unconditional in 03_calibration.R, so this no
# longer gates the active pipeline. Retained (TRUE) only because the standalone
# 09_uncertainty_psa.R / 10_deterministic_dsa.R still parse it from this file.
run_adjustment_model <- TRUE

# Remove unnecessary dx

dx_include <- c("All causes",
                "Ischemic heart disease",
                "Ischemic stroke",
                "Intracerebral hemorrhage",
                "Hypertensive heart disease")

cause_map <- c(
  ihd      = "Ischemic heart disease",
  istroke  = "Ischemic stroke",
  hstroke  = "Intracerebral hemorrhage",
  hhd      = "Hypertensive heart disease",
  all      = "All causes"
)

# AFTER  – define the vector once, reuse it
cause_cols <- names(cause_map)

#...........................................................
# Calibration + finalization settings (single source of truth) ----
#...........................................................
# Consumed by 03_calibration.R (and available to 04-06). Stage 03 checks for
# these rather than defining competing local values.

# The four modeled CVD causes (full GBD names), derived from cause_map so the
# modeled scope stays in one place. "all"/dementia are not modeled transitions.
MODEL_CAUSES <- unname(cause_map[setdiff(names(cause_map), "all")])
AGES  <- 20:95                 # single years; 95 = the open-ended 95+ group
SEXES <- c("Female", "Male")

YEARS_HIST <- 2000:2019        # historical coverage of tps_inpt (stage 02)
YEARS_TP   <- 2000:2050        # full annual coverage written by stage 03
CAL_YEAR_START <- 2009L        # calibration projection window (within YEARS_HIST)
CAL_YEAR_END   <- 2019L

# Consolidated calibration grid. The identity (1.0, 1.0) is included so the
# calibrated fit is never worse than the uncalibrated baseline. Objective:
# W_DEATHS*RMSE_deaths + W_PREV*RMSE_prev vs GBD Number, per location x sex x
# cause x 5-year age band. The range was WIDENED past the legacy 031/032 caps
# (which saturated ~77% of bands): the diagnostics showed incidence wanting to
# go lower (IR floor) and case-fatality wanting to go higher (CF ceiling), so IR
# is extended down and CF up. Resolution is fine near 1.0 (small adjustments)
# and coarser in the tails (to bound the grid size / runtime).
CAL_IR_GRID <- c(0.40, 0.50, 0.60, 0.70, 0.80, 0.85, 0.90, 0.95,
                 1.00, 1.05, 1.10, 1.20)
CAL_CF_GRID <- c(0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 0.95, 1.00,
                 1.05, 1.10, 1.20, 1.40, 1.60, 1.80)
W_DEATHS <- 2
W_PREV   <- 1
CAL_TP_CAP <- 0.9              # IR/CF cap DURING the calibration projection
TP_EPS     <- 0.005           # headroom preserved by the final probability guard

# GBD calibration targets: the stage-02-derived GBD 2023 cache (Deaths +
# Prevalence, Number, by 5-year band). Written by 021_get_base_rates.R; read-only
# here. (Kept in data/raw/GBD to avoid re-running the heavy stage-02 GBD build.)
gbd_target_file <- paste0(wd_raw, "GBD/temp_1baseline_rates_gbd23.rds")

# File discovery / output chunking (scoped so stale files are never mixed in).
tps_inpt_stub <- "^tps_inpt_part[0-9]+\\.rds$"
calibrated_stub <- "calibrated_searo_part"
adjusted_stub <- "adjusted_searo_part"
N_OUT_CHUNKS  <- 10L

#...........................................................
# 02. Load inputs-----
#...........................................................

source("02_load_inputs.R")

#...........................................................
# 03. Calibrate + finalize transition probabilities -----
#...........................................................
# Consolidates the retired 03_clean_inputs.R -> 031_calibration.R ->
# 032_adjustments.R chain AND the calibration / secular-trend blocks formerly in
# 05_build_baseline.R. Writes the FINAL calibrated + projected annual rates to
# data/processed/adjusted_searo_part*.rds (used directly by 04, 05, 06).

# only if calibration is needed, run the calibration script. Otherwise, skip to the next step.
if(run_calibration_par) {
  source("03_calibration.R")
}

# run trends script to apply secular trends to the calibrated transition probabilities. 
# This is done after calibration to ensure that the final transition probabilities 
# reflect both the calibration and the secular trends.
source("031_calibration_trends.R")

#...........................................................
# 04. define interventions ----
#...........................................................

source("04_define_interventions.R")

#...........................................................
# 05. build baseline ----
#...........................................................

source("05_build_baseline.R")

#...........................................................
# 06. Run model ----
#...........................................................

# Run Aim 1: multiple interventions
source(paste0(wd_code,"06_run_scenarios_multiple.R"))
# Run Aim 2: HTN control scenarios, 150 Million by 2030
#source("06_run_scenarios_targets.R")

#...........................................................
# 07. Run Burden of Disease ----
#...........................................................

source(paste0(wd_code,"07_output_dalys.R"))

#...........................................................
# 08. Run economic value ----
#...........................................................

source(paste0(wd_code,"08_economic_value_calculation.R"))

