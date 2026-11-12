#===============================================================================
# 03_calibration_who.R  --  Stage 03: calibrate historical transition
#                           probabilities against WHO deaths + GBD prevalence
#-------------------------------------------------------------------------------
# The same grid-search calibrator as 03_calibration.R: same cohort projection,
# IR/CF multiplier grid, objective and weights, calibration window, probability
# caps and output contract. The one substantive difference is the DEATH target:
#
#   * calibration_deaths_source == "WHO" (default in 00_run_model.R): WHO GHE 2023
#     death counts (dt_deaths_who_long.rds, written by 020_get_deaths_who_ghe2023.R;
#     the GHE 2021 version is kept as dt_deaths_who_long_2021.rds)
#     wherever present; GBD 2023 death counts for locations without WHO data and
#     for individual missing WHO cells.
#   * calibration_deaths_source == "GBD": GBD 2023 death counts throughout, which
#     reproduces 03_calibration.R.
#
# The prevalence target is always GBD 2023 prevalent cases (Prevalence, Number).
#
# Consumes the stage-02 inputs (tps_inpt_part*.rds) and writes calibrated but
# UNTRENDED annual transition probabilities for YEARS_HIST to
#
#     data/processed/calibrated_searo_part*.rds
#
# one row per  location x year x sex x modeled cause x single age 20:95
# (age 95 = the open-ended 95+ group). This script applies NO secular trend and
# NO final probability guard: 031_calibration_trends.R reads these files,
# extends them to YEARS_TP, applies the trends and the guard, and writes the
# final adjusted_searo_part*.rds used by 04/05/06.
#
# DEATH TARGET ASSEMBLY (WHO mode)
#   * Locations: 020 labels WHO rows with countrycode names; they are recoded to
#     the GBD 2023 names used in tps_inpt with the explicit crosswalk below
#     (checked by ISO3 when countrycode is installed). Every WHO location must
#     map to exactly one model location.
#   * Ages: WHO reports 5-year bands to 80-84 plus an open 85+ band. Each WHO
#     85+ count is split among 85-89, 90-94 and 95+ with the GBD death-COUNT
#     shares of the same location, sex, cause and year (rates are never split);
#     the split is checked to preserve every WHO 85+ total.
#   * Fallback: an observed WHO count (including an observed zero) is never
#     replaced. A missing WHO cell takes the matching GBD death count; no missing
#     value is ever turned into zero.
#   * Validation: unique keys, complete location x sex x cause x band x year
#     coverage, finite nonnegative counts. calibration_death_target_audit.csv
#     counts WHO vs GBD target cells per location x sex x cause x band.
#
# METHOD (per location x sex x cause x GBD 5-year age band)
#   * Project the well-sick-dead cohort over CAL_YEAR_START..CAL_YEAR_END for
#     every candidate (IRmult, CFmult) on the grid, applied globally.
#   * Aggregate simulated Deaths (dead) and Prevalence (sick) to GBD age bands
#     and score against the death target and GBD prevalence with
#     error = W_DEATHS*RMSE_deaths + W_PREV*RMSE_prev.
#   * Pick the (IRmult, CFmult) minimising that error for each band; the identity
#     (1,1) is a candidate so a worse-fitting multiplier is never selected.
#   * Bake the winning multipliers into IR/CF for every YEARS_HIST year; 031
#     carries the calibrated 2019 values into the projected years.
#
# PROBABILITY CONSTRAINTS (as 03_calibration.R)
#   Raw IR/CF are clamped to [0, 0.99] on input, and IR/CF are capped at
#   CAL_TP_CAP inside the calibration projection. The final guard (IR + BG.mx
#   <= 1, CF + BG.mx <= 1) is enforced by 031 after the trends.
#
# INPUT / OUTPUT CONTRACT
#   in : data/processed/tps_inpt_part*.rds  (stage 02): age, sex, location, year,
#        cause (full GBD name), BG.mx.all, ALL.mx, BG.mx, PREVt0, DIS.mx.t0, Nx,
#        IR, CF; years YEARS_HIST; ages 20:95; four CVD causes.
#        gbd_target_file = data/raw/GBD/temp_1baseline_rates_gbd23.rds (GBD
#        Deaths + Prevalence; read-only).
#        who_deaths_target_file = data/processed/dt_deaths_who_long.rds (WHO
#        death counts; read-only; WHO mode only).
#   out: data/processed/calibrated_searo_part*.rds  (SAME 13-column schema as
#        tps_inpt, full cause names, years YEARS_HIST) + calibration_factors.csv,
#        calibration_diagnostics.csv, calibration_death_target_audit.csv, and
#        *_grid_<WHO|GBD>.csv copies of all three (audit files; no "adjusted" in
#        the name so the globbing in 04/05 never mixes them in).
#
# LOCATION SCOPE (options; default = every tps_inpt location)
#   options(who_cvd.cal_locations = "out_model") calibrates only the countries
#   that have model_output_<country>[_<scenario>].rds in output/out_model/ (the
#   countries 06 actually runs); a character vector of model location names also
#   works. who_cvd.cal_pilot does the same for a quick test. Either way ONLY the
#   selected locations are written, so calibrated_searo / calibration_*.csv (and
#   031's adjusted_searo) then cover that set.
#===============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(foreach)
  library(doParallel)
  library(parallel)
})

message("\n== Stage 03: calibrate historical transition probabilities (grid, WHO/GBD deaths) ==")

## ---- 0. Settings from 00_run_model.R (single source of truth) --------------
## Stage 03 checks for these rather than defining competing local values.
.needed <- c("wd_data", "MODEL_CAUSES", "AGES", "SEXES", "YEARS_HIST",
             "CAL_YEAR_START", "CAL_YEAR_END", "CAL_IR_GRID", "CAL_CF_GRID",
             "W_DEATHS", "W_PREV", "CAL_TP_CAP", "N_OUT_CHUNKS", "tps_inpt_stub",
             "calibrated_stub", "gbd_target_file", "calibration_deaths_source")
.missing <- .needed[!vapply(.needed, exists, logical(1))]
if (length(.missing)) {
  stop("03_calibration_who.R must be sourced after 00_run_model.R. Missing settings: ",
       paste(.missing, collapse = ", "), call. = FALSE)
}
RUN_PAR   <- if (exists("run_calibration_par")) isTRUE(run_calibration_par) else TRUE
MAX_CORES <- if (exists("cal_max_cores")) cal_max_cores else 10L
HIST_END  <- max(YEARS_HIST)          # last calibrated/observed year (2019)

DEATHS_SOURCE <- toupper(calibration_deaths_source)
if (!DEATHS_SOURCE %in% c("WHO", "GBD")) {
  stop("Stage 03: calibration_deaths_source must be \"WHO\" or \"GBD\", not \"",
       calibration_deaths_source, "\".", call. = FALSE)
}
if (DEATHS_SOURCE == "WHO" && !exists("who_deaths_target_file")) {
  stop("Stage 03: calibration_deaths_source is WHO but who_deaths_target_file ",
       "is not set (00_run_model.R).", call. = FALSE)
}

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

## Every model location, kept before any pilot subset so the WHO location
## crosswalk is always checked against the full model scope.
all_locs <- sort(unique(as.character(b_rates$location)))

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

## Optional production scope: "out_model" = the countries with scenario output in
## output/out_model/ (Aim 1 model_output_<country>.rds or Aim 2
## model_output_<country>_<scenario_id>.rds), or an explicit vector of names.
cal_locations <- getOption("who_cvd.cal_locations", NULL)
if (!is.null(cal_locations)) {
  if (identical(cal_locations, "out_model")) {
    if (!exists("wd_outp")) {
      stop("who_cvd.cal_locations = \"out_model\" needs wd_outp from 00_run_model.R.",
           call. = FALSE)
    }
    out_model_dir <- file.path(wd_outp, "out_model")
    om <- sub("^model_output_(.+)[.]rds$", "\\1",
              list.files(out_model_dir, pattern = "^model_output_.+[.]rds$"))
    if (!length(om)) {
      stop("who_cvd.cal_locations = \"out_model\": no model_output_*.rds in ",
           out_model_dir, call. = FALSE)
    }
    ## Exact name, else the longest model location followed by "_<scenario>".
    om_loc <- vapply(om, function(x) {
      hit <- all_locs[x == all_locs | startsWith(x, paste0(all_locs, "_"))]
      if (length(hit)) hit[which.max(nchar(hit))] else NA_character_
    }, character(1))
    if (anyNA(om_loc)) {
      stop("output/out_model files with no tps_inpt location: ",
           paste(names(om_loc)[is.na(om_loc)], collapse = ", "), call. = FALSE)
    }
    cal_locations <- sort(unique(unname(om_loc)))
    rm(om, om_loc, out_model_dir)
  }
  if (!all(cal_locations %in% unique(b_rates$location))) {
    stop("who_cvd.cal_locations names locations not in tps_inpt",
         if (!is.null(cal_pilot)) " (after who_cvd.cal_pilot)", ": ",
         paste(setdiff(cal_locations, unique(b_rates$location)), collapse = ", "),
         call. = FALSE)
  }
  b_rates <- b_rates[location %in% cal_locations]
  message("  [scope] calibrating ", length(cal_locations), " of ", length(all_locs),
          " tps_inpt locations; not calibrated or written: ",
          paste(setdiff(all_locs, cal_locations), collapse = ", "))
}

locs <- sort(unique(b_rates$location))
cat(sprintf("  Loaded tps_inpt: %d rows | %d locations | causes: %s | years %d-%d\n",
            nrow(b_rates), length(locs), paste(MODEL_CAUSES, collapse = ", "),
            min(YEARS_HIST), HIST_END))

#===============================================================================
# 2. CALIBRATION TARGETS: deaths (WHO, GBD fallback) + GBD prevalence
#    Number, by location x sex x cause x GBD 5-year band x year
#===============================================================================

cal_years   <- CAL_YEAR_START:CAL_YEAR_END
tgt_keys    <- c("location", "sex", "cause", "age_group", "year")
band_levels <- unique(cal_age_group(AGES))             # "20-24", ..., "95+"
band_lower  <- as.integer(sub("^([0-9]+).*$", "\\1", band_levels))

## The complete key grid every target table must cover exactly once, and a check
## that the model rows in the window fall on that same grid.
target_grid <- CJ(location = locs, sex = SEXES, cause = MODEL_CAUSES,
                  age_group = band_levels, year = cal_years)
model_keys <- unique(b_rates[year >= CAL_YEAR_START & year <= CAL_YEAR_END,
                             .(location = as.character(location), sex = as.character(sex),
                               cause, age_group = cal_age_group(age),
                               year = as.integer(year))])
if (!fsetequal(model_keys, target_grid)) {
  stop("Stage 03: tps_inpt rows in ", CAL_YEAR_START, "-", CAL_YEAR_END,
       " do not form the complete location x sex x cause x band x year grid.",
       call. = FALSE)
}
rm(model_keys)

check_target_grid <- function(dt, what) {
  if (anyDuplicated(dt, by = tgt_keys)) {
    stop("Stage 03: ", what, " has duplicated location x sex x cause x band x year keys.",
         call. = FALSE)
  }
  miss <- fsetdiff(target_grid, dt[, ..tgt_keys])
  if (nrow(miss)) {
    stop("Stage 03: ", what, " is missing ", nrow(miss), " target cells; first: ",
         paste(unlist(miss[1L]), collapse = " | "), call. = FALSE)
  }
  extra <- fsetdiff(dt[, ..tgt_keys], target_grid)
  if (nrow(extra)) {
    stop("Stage 03: ", what, " has ", nrow(extra), " cells outside the model grid; first: ",
         paste(unlist(extra[1L]), collapse = " | "), call. = FALSE)
  }
}
check_counts <- function(x, what, allow_na = FALSE) {
  bad <- if (allow_na) !is.na(x) & (!is.finite(x) | x < 0) else is.na(x) | !is.finite(x) | x < 0
  if (any(bad)) {
    stop("Stage 03: ", what, " has ", sum(bad), " missing, non-finite or negative counts.",
         call. = FALSE)
  }
}

## ---- 2a. GBD 2023 Deaths + Prevalence (Number) -----------------------------
if (!file.exists(gbd_target_file)) {
  stop("Stage 03: GBD target file not found: ", gbd_target_file, call. = FALSE)
}
gbd <- readRDS(gbd_target_file); setDT(gbd)
gbd <- gbd[metric_name == "Number" &
             measure_name %in% c("Deaths", "Prevalence") &
             cause_name %in% MODEL_CAUSES &
             location_name %in% locs &
             sex_name %in% SEXES &
             year >= CAL_YEAR_START & year <= CAL_YEAR_END,
           .(location = as.character(location_name), sex = as.character(sex_name),
             cause = as.character(cause_name),
             age_group = sub(" years$", "", as.character(age_name)),
             year = as.integer(year), measure = as.character(measure_name),
             val = as.numeric(val))]
gbd <- gbd[age_group %in% band_levels]
if (anyDuplicated(gbd, by = c(tgt_keys, "measure"))) {
  stop("Stage 03: GBD targets have duplicated cells for one measure.", call. = FALSE)
}
if (!all(c("Deaths", "Prevalence") %in% unique(gbd$measure))) {
  stop("Stage 03: GBD target file lacks Deaths or Prevalence counts in the window.",
       call. = FALSE)
}
## Keys are unique per measure, so dcast only reshapes (no aggregation). A
## missing cell stays NA and is caught below, never turned into zero.
gbd <- dcast(gbd, location + sex + cause + age_group + year ~ measure, value.var = "val")
setnames(gbd, c("Deaths", "Prevalence"), c("gbdDeaths", "gbdPrev"))
check_target_grid(gbd, "GBD targets")
check_counts(gbd$gbdDeaths, "GBD deaths")
check_counts(gbd$gbdPrev,   "GBD prevalence")

death_detail <- gbd[, .(location, sex, cause, age_group, year, gbdDeaths, gbdPrev,
                        whoDeaths = NA_real_, who_split_85plus = FALSE)]

## ---- 2b. WHO GHE 2023 deaths (Number), harmonised to the model keys --------
## WHO labels (countrycode country.name of the GHE iso3, from 020) that differ
## from the GBD 2023 names in tps_inpt. Every other WHO location name already
## matches exactly. GHE 2023 adds Taiwan (TWN), absent from GHE 2021. Model
## locations without WHO data (Andorra, Dominica, Marshall Islands and other small
## states/territories) fall back to GBD deaths.
who_location_crosswalk <- c(
  "Antigua & Barbuda"        = "Antigua and Barbuda",
  "Bosnia & Herzegovina"     = "Bosnia and Herzegovina",
  "Brunei"                   = "Brunei Darussalam",
  "Cape Verde"               = "Cabo Verde",
  "Congo - Brazzaville"      = "Congo",
  "Congo - Kinshasa"         = "Democratic Republic of the Congo",
  "Côte d’Ivoire"  = "Ivory Coast",
  "Iran"                     = "Iran (Islamic Republic of)",
  "Laos"                     = "Lao People's Democratic Republic",
  "Moldova"                  = "Republic of Moldova",
  "Myanmar (Burma)"          = "Myanmar",
  "North Korea"              = "Democratic People's Republic of Korea",
  "Palestinian Territories"  = "Palestine",
  "Russia"                   = "Russian Federation",
  "São Tomé & Príncipe" = "Sao Tome and Principe",
  "South Korea"              = "Republic of Korea",
  "St. Lucia"                = "Saint Lucia",
  "St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
  "Syria"                    = "Syrian Arab Republic",
  "Taiwan"                   = "Taiwan (Province of China)",
  "Trinidad & Tobago"        = "Trinidad and Tobago",
  "Venezuela"                = "Venezuela (Bolivarian Republic of)",
  "Vietnam"                  = "Viet Nam"
)

who_split_check <- NULL
if (DEATHS_SOURCE == "WHO") {
  if (!file.exists(who_deaths_target_file)) {
    stop("Stage 03: WHO deaths target not found: ", who_deaths_target_file,
         " -- run 020_get_deaths_who_ghe2023.R first.", call. = FALSE)
  }
  who <- readRDS(who_deaths_target_file); setDT(who)
  who_cols <- c("location", "sex", "cause", "age", "year", "measure", "metric", "val")
  if (length(setdiff(who_cols, names(who)))) {
    stop("Stage 03: WHO deaths file is missing columns: ",
         paste(setdiff(who_cols, names(who)), collapse = ", "), call. = FALSE)
  }
  who <- who[measure == "Deaths" & metric == "Number",
             .(location = as.character(location), sex = as.character(sex),
               cause = as.character(cause),
               age_group = sub(" years$", "", as.character(age)),
               year = as.integer(year), whoDeaths = as.numeric(val))]

  ## Locations: recode, then require a one-to-one match onto model locations.
  who_labels <- sort(unique(who$location))
  who_mapped <- who_labels
  hit <- who_mapped %in% names(who_location_crosswalk)
  who_mapped[hit] <- unname(who_location_crosswalk[who_mapped[hit]])
  if (length(setdiff(who_mapped, all_locs))) {
    stop("Stage 03: WHO locations with no model match (add them to ",
         "who_location_crosswalk): ",
         paste(who_labels[!who_mapped %in% all_locs], collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(who_mapped)) {
    stop("Stage 03: several WHO locations map to the same model location: ",
         paste(who_mapped[duplicated(who_mapped)], collapse = ", "), call. = FALSE)
  }
  if (requireNamespace("countrycode", quietly = TRUE)) {
    iso_who   <- countrycode::countrycode(who_labels, "country.name", "iso3c", warn = FALSE)
    iso_model <- countrycode::countrycode(who_mapped, "country.name", "iso3c", warn = FALSE)
    bad_iso <- !is.na(iso_who) & !is.na(iso_model) & iso_who != iso_model
    if (any(bad_iso)) {
      stop("Stage 03: WHO location crosswalk disagrees with ISO3 for: ",
           paste(who_labels[bad_iso], "->", who_mapped[bad_iso], collapse = "; "),
           call. = FALSE)
    }
  }
  who[, location := who_mapped[match(location, who_labels)]]

  ## Sex, cause and age labels. Non-modelled causes ("All causes") are dropped;
  ## any unexpected sex or age label stops rather than silently vanishing.
  if (length(setdiff(unique(who$sex), SEXES))) {
    stop("Stage 03: unexpected WHO sex labels: ",
         paste(setdiff(unique(who$sex), SEXES), collapse = ", "), call. = FALSE)
  }
  who <- who[location %in% locs & cause %in% MODEL_CAUSES &
               year >= CAL_YEAR_START & year <= CAL_YEAR_END]
  who_open_band <- grep("^[0-9]+\\+$", unique(who$age_group), value = TRUE)
  if (length(who_open_band) > 1L) {
    stop("Stage 03: WHO deaths have several open age bands: ",
         paste(who_open_band, collapse = ", "), call. = FALSE)
  }
  ## Model bands at or above the WHO open band's lower bound (85-89, 90-94, 95+)
  ## receive the split; WHO bands below it must be model bands as they are. An
  ## open band that is itself a model band (95+) needs no split.
  if (length(who_open_band) && !who_open_band %in% band_levels) {
    open_lower  <- as.integer(sub("\\+$", "", who_open_band))
    split_bands <- band_levels[band_lower >= open_lower]
    direct_ok   <- band_levels[band_lower <  open_lower]
  } else {
    split_bands <- character(0)
    direct_ok   <- band_levels
  }
  bad_age <- setdiff(unique(who$age_group), c(direct_ok, who_open_band))
  if (length(bad_age)) {
    stop("Stage 03: unexpected WHO age labels: ", paste(bad_age, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(who, by = tgt_keys)) {
    stop("Stage 03: WHO deaths have duplicated location x sex x cause x age x year keys.",
         call. = FALSE)
  }
  check_counts(who$whoDeaths, "WHO deaths", allow_na = TRUE)

  who_direct <- who[age_group %in% direct_ok,
                    .(location, sex, cause, age_group, year, whoDeaths,
                      who_split_85plus = FALSE)]

  ## Split each observed WHO open-band COUNT with the GBD death-count shares of
  ## the same location x sex x cause x year. A missing WHO open-band cell is not
  ## split, so its bands fall back to GBD below.
  who_split <- who_direct[0L]
  if (length(split_bands)) {
    who_open <- who[age_group == who_open_band & !is.na(whoDeaths),
                    .(location, sex, cause, year, who_open = whoDeaths)]
    shares <- gbd[age_group %in% split_bands,
                  .(location, sex, cause, year, age_group, gbdDeaths)]
    shares[, gbd_open := sum(gbdDeaths), by = .(location, sex, cause, year)]
    who_split <- merge(who_open, shares, by = c("location", "sex", "cause", "year"))
    n_bands <- who_split[, .N, by = .(location, sex, cause, year)]
    if (nrow(n_bands) != nrow(who_open) || any(n_bands$N != length(split_bands))) {
      stop("Stage 03: GBD death counts for ", paste(split_bands, collapse = "/"),
           " are incomplete for some WHO ", who_open_band, " cells.", call. = FALSE)
    }
    if (any(who_split$gbd_open <= 0)) {
      stop("Stage 03: GBD ", paste(split_bands, collapse = "/"), " deaths sum to zero for ",
           uniqueN(who_split[gbd_open <= 0], by = c("location", "sex", "cause", "year")),
           " WHO ", who_open_band, " cells; the split shares are undefined.", call. = FALSE)
    }
    who_split[, whoDeaths := who_open * gbdDeaths / gbd_open]
    who_split_check <- who_split[, .(who_open = who_open[1L], split_sum = sum(whoDeaths)),
                                 by = .(location, sex, cause, year)]
    who_split_check[, rel_diff := abs(split_sum - who_open) / pmax(who_open, 1)]
    if (who_split_check[, max(rel_diff)] > 1e-9) {
      stop("Stage 03: the WHO ", who_open_band, " split does not preserve the WHO total ",
           "(max relative difference ", signif(who_split_check[, max(rel_diff)], 3), ").",
           call. = FALSE)
    }
    who_split <- who_split[, .(location, sex, cause, age_group, year, whoDeaths,
                               who_split_85plus = TRUE)]
    rm(who_open, shares, n_bands)
  }
  who_bands <- rbind(who_direct, who_split)
  if (anyDuplicated(who_bands, by = tgt_keys)) {
    stop("Stage 03: WHO death targets have duplicated keys after the ", who_open_band,
         " split.", call. = FALSE)
  }
  if (nrow(who_bands[!death_detail, on = tgt_keys])) {
    stop("Stage 03: WHO death targets fall outside the model key grid.", call. = FALSE)
  }
  death_detail[who_bands, on = tgt_keys,
               `:=`(whoDeaths = i.whoDeaths, who_split_85plus = i.who_split_85plus)]
  rm(who, who_direct, who_split, who_bands)
}

## ---- 2c. Death target: WHO where observed, GBD otherwise ---------------------
death_detail[, death_source := fifelse(is.na(whoDeaths), "GBD", "WHO")]
death_detail[, targetDeaths := fifelse(is.na(whoDeaths), gbdDeaths, whoDeaths)]
check_target_grid(death_detail, "calibration targets")
check_counts(death_detail$targetDeaths, "death targets")
check_counts(death_detail$gbdPrev,      "prevalence targets")
if (death_detail[death_source == "WHO", !identical(targetDeaths, whoDeaths)]) {
  stop("Stage 03: an observed WHO death count was replaced.", call. = FALSE)
}

## Audit: WHO vs GBD death-target cells per location x sex x cause x band.
death_audit <- death_detail[, .(
  n_cells          = .N,
  n_who_cells      = sum(death_source == "WHO"),
  n_gbd_cells      = sum(death_source == "GBD"),
  who_split_85plus = any(who_split_85plus),
  target_deaths    = sum(targetDeaths),
  who_deaths       = if (any(death_source == "WHO")) sum(whoDeaths, na.rm = TRUE) else NA_real_,
  gbd_deaths       = sum(gbdDeaths),
  gbd_prevalence   = sum(gbdPrev)
), by = .(location, sex, cause, age_group)]
death_audit[, death_target := fifelse(n_gbd_cells == 0L, "WHO",
                                      fifelse(n_who_cells == 0L, "GBD", "WHO+GBD"))]
death_audit[, deaths_source_setting := DEATHS_SOURCE]
setcolorder(death_audit, c("location", "sex", "cause", "age_group", "death_target",
                           "n_cells", "n_who_cells", "n_gbd_cells", "who_split_85plus"))
death_audit <- death_audit[order(location, sex, cause, match(age_group, band_levels))]

n_who <- death_detail[death_source == "WHO", .N]
cat(sprintf("  Death targets (%s): %d cells | WHO %d (%.1f%%) | GBD %d | locations: %d WHO-only, %d GBD-only, %d mixed\n",
            DEATHS_SOURCE, nrow(death_detail), n_who, 100 * n_who / nrow(death_detail),
            nrow(death_detail) - n_who,
            death_detail[, all(death_source == "WHO"), by = location][V1 == TRUE, .N],
            death_detail[, all(death_source == "GBD"), by = location][V1 == TRUE, .N],
            death_detail[, uniqueN(death_source) > 1L, by = location][V1 == TRUE, .N]))
if (!is.null(who_split_check)) {
  cat(sprintf("  WHO %s split: %d cells -> %s; max |split sum - WHO total| / max(WHO total, 1) = %.2e\n",
              who_open_band, nrow(who_split_check), paste(split_bands, collapse = ", "),
              who_split_check[, max(rel_diff)]))
}
if (n_who) {
  cmp <- death_detail[death_source == "WHO",
                      .(WHO = sum(targetDeaths), GBD = sum(gbdDeaths)), by = cause]
  cat("  WHO / GBD deaths over the WHO target cells:",
      paste(sprintf("%s %.3f", cmp$cause, cmp$WHO / cmp$GBD), collapse = " | "), "\n")
  rm(cmp)
}
message("Calibration targets: ",
        if (DEATHS_SOURCE == "WHO") "WHO deaths (GBD fallback)" else "GBD deaths",
        " + GBD prevalence; ", nrow(death_detail), " cells (", CAL_YEAR_START, "-",
        CAL_YEAR_END, ")")

## The engine needs only the keys and the two targets.
targets <- death_detail[, .(location, sex, cause, age_group, year, targetDeaths, gbdPrev)]
rm(gbd, death_detail, who_split_check)

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
## Targets were validated as complete in section 2, so a missing target here is
## an error, never a zero.
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
  if (anyNA(j$targetDeaths) || anyNA(j$gbdPrev))
    stop("Missing target during grid calibration scoring")
  err <- j[, .(RMSE_deaths = sqrt(mean((targetDeaths - Deaths)^2, na.rm = TRUE)),
               RMSE_prev   = sqrt(mean((gbdPrev - Prevalence)^2,  na.rm = TRUE))),
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
# 5. BAKE CALIBRATION FACTORS INTO IR/CF FOR EVERY HISTORICAL YEAR
#===============================================================================

b_rates[, age_group := cal_age_group(age)]
b_rates <- merge(b_rates, factors[, .(location, sex, cause, age_group, ir_mult, cf_mult)],
                 by = c("location", "sex", "cause", "age_group"), all.x = TRUE)
b_rates[is.na(ir_mult), ir_mult := 1]
b_rates[is.na(cf_mult), cf_mult := 1]
b_rates[, `:=`(IR = IR * ir_mult, CF = CF * cf_mult)]
b_rates[, c("age_group", "ir_mult", "cf_mult") := NULL]

#===============================================================================
# 6. WRITE OUTPUTS  (calibrated_searo_part*.rds, chunked by whole location)
#===============================================================================

setcolorder(b_rates, req_cols)
out_locs <- sort(unique(b_rates$location))
n_chunks <- max(1L, min(N_OUT_CHUNKS, length(out_locs)))
chunk_of <- if (n_chunks <= 1L) list(out_locs) else
  split(out_locs, cut(seq_along(out_locs), breaks = n_chunks, labels = FALSE))

## Clear only this stub's stale chunks so 031 never globs a leftover chunk.
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
fwrite(death_audit, file.path(wd_data, "calibration_death_target_audit.csv"))

## Source-labelled copies so a WHO run and a GBD sensitivity run can be compared.
for (f in c("calibration_factors", "calibration_diagnostics", "calibration_death_target_audit")) {
  file.copy(file.path(wd_data, paste0(f, ".csv")),
            file.path(wd_data, paste0(f, "_grid_", DEATHS_SOURCE, ".csv")),
            overwrite = TRUE)
}

cat(sprintf("Wrote %d %s*.rds + calibration_factors.csv + calibration_diagnostics.csv + calibration_death_target_audit.csv (+ _grid_%s copies) to %s\n",
            length(chunk_of), calibrated_stub, DEATHS_SOURCE, wd_data))
cat("Stage 03 complete: calibrated_searo_part*.rds hold calibrated, UNTRENDED rates for",
    min(YEARS_HIST), "-", HIST_END, "; 031_calibration_trends.R applies the secular trends.\n")

## Tidy the large temporaries. 031 re-reads the calibrated chunks from disk.
rm(targets, factors, grid, groups, death_audit)
invisible(gc())
