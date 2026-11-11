# 03_calibration_indonesia_random_tp(2).R
# Calibration on single-year ages 20:95, with 95 representing the open 95+ group.
# WHO deaths and GBD prevalence are scored in five-year bands, 2009-2019.
# A common IR/CF pair initializes age-band multipliers. Each candidate is
# evaluated using a complete cohort projection; the saved rates are rechecked.
# Search: uniform random coordinate updates with occasional full draws.
# Input/output file locations and other surrounding pipeline globals follow the
# source script. Only source one of these alternative calibrators per run.
# Set run_adjustment_model <- FALSE downstream to avoid applying legacy factors.
#
# library(data.table)
# library(foreach)
# library(doParallel)
# library(parallel)

#===============================================================================
# 0. TUNABLE PARAMETERS  (all calibration "magic numbers" live here, commented)
#===============================================================================

## --- search SPACE -----------------------------------------------------------
# Symmetric half-width of the multiplicative search range around 1. 0.5 means
# IR/CF may be scaled anywhere in [0.5, 1.5]. This single symmetric range
# REPLACES 031's +/-5% grid and 032's asymmetric 0-20% IR / 0-50% CF grid; it is
# wide enough to subsume both (their combined reach was ~ -55% .. +5%) while
# being explicit and symmetric. Widen only if combos hit the bound.
SEARCH_HALFWIDTH <- 0.50

# Granularity of the multipliers. "combo" = PRIMARY (one IR + one CF per
# location-sex-cause). "age_group" = SENSITIVITY (per 5-year age group).
GRANULARITY <- "age_group"          # "combo" (primary) | "age_group" (sensitivity)

## --- SEARCH: shared initialization then random age-band updates ---------
N_ITER            <- 400L        # candidate evaluations, plus baseline
COMMON_EVALS      <- 40L         # common-pair candidates within N_ITER
ADJACENT_PROB    <- 0.20        # occasional neighboring pair draw
FULL_VECTOR_PROB <- 0.05        # exploratory independent full-vector draws
SMOOTH_LAMBDA    <- 0           # optional log-multiplier age smoothness penalty
CONVERGE_TOL      <- 1e-4
SEED              <- 42L

## --- objective WEIGHTS / numerics -------------------------------------------
W_DEATHS <- 2                   # fatal weight (matches 031's "2*RMSE_deaths ...")
W_PREV   <- 1                   # non-fatal weight ("... + RMSE_prev")
EPS_REL  <- 1e-6                # denominator floor for relative error (no /0)

## --- probability-constraint numerics ----------------------------------------
# Epsilon buffer kept below 1 when capping/renormalising probabilities. 032 uses
# the same 0.005 buffer in its renormalisation; we reuse it for consistency.
TP_EPS <- 0.005

## --- calibration target window ----------------------------------------------
CAL_YEAR_START <- 2009          # GBD comparison start (031 uses year >= 2009)
CAL_YEAR_END   <- 2019          # GBD comparison end   (031 uses year <  2020)

## --- execution --------------------------------------------------------------
# run_calibration_par is defined in 00_run_model.R; default TRUE here.
RUN_PAR   <- if (exists("run_calibration_par")) isTRUE(run_calibration_par) else TRUE
MAX_CORES <- 14                 # cap on workers (031 used 14)
N_OUT_CHUNKS <- 10              # number of calibrated_searo_part*.rds files

#===============================================================================
# 1. LOAD INPUTS (same sources 031 reads -- this script never writes them)
#===============================================================================

## locations (written by 021_get_base_rates_indonesia.R)
locs <- readRDS(paste0(wd, "locs.rds"))
locs <- as.vector(locs$location)

## baseline transition probabilities (tps_inpt_part*.rds from 022)
## schema: age, sex, location, year, cause, ALL.mx, BG.mx.all, BG.mx,
##         PREVt0, DIS.mx.t0, Nx, IR, CF   (cause = full GBD names, ages 20-95,
##         years 2000-2019; contains disease causes only, no "All causes")
tps_files <- list.files(path = wd_data, pattern = "tps_inpt", full.names = TRUE)
b_rates   <- rbindlist(lapply(tps_files, function(f) { dt <- readRDS(f); setDT(dt); dt }),
                       use.names = TRUE, fill = TRUE)

## same defensive clamps 031 applies before calibrating
b_rates[CF >= 1, CF := 0.99]
b_rates[IR >= 1, IR := 0.99]
b_rates[CF < 0,  CF := 0]
b_rates[IR < 0,  IR := 0]

## keep a frozen copy of the INPUT for end-of-run schema / row-count validation
tps_input_cols <- copy(names(b_rates))
tps_input_nrow <- nrow(b_rates)

## incoming age-20 population (UNWPP 2024), exactly as 031 prepares it
pop20 <- fread(paste0(wd_data, "PopulationsAge20_full.csv"))
pop20 <- pop20[location %in% locs]
pop20 <- pop20[year_id >= CAL_YEAR_START & year_id <= 2050]
setnames(pop20, c("year_id", "Nx"), c("year", "Nx20"))

#===============================================================================
# 2. CALIBRATION TARGETS: selected deaths + GBD prevalence
#===============================================================================

# Deaths: WHO (primary) or GBD (sensitivity). Prevalence: always GBD.
# Join on the complete location/sex/cause/age/year key; never fill missing deaths
# with zero, since that would change the objective without warning.
if (!exists("gbd_target_file"))
  gbd_target_file <- file.path(wd_raw, "GBD", "temp_1baseline_rates_gbd23.rds")
if (!exists("who_deaths_target_file"))
  who_deaths_target_file <- file.path(wd_data, "dt_deaths_who_long.rds")
if (!exists("calibration_deaths_source")) calibration_deaths_source <- "WHO"
calibration_deaths_source <- toupper(calibration_deaths_source)
if (!calibration_deaths_source %in% c("WHO", "GBD"))
  stop("calibration_deaths_source must be WHO or GBD")
if (!file.exists(gbd_target_file)) stop("Missing GBD prevalence target: ", gbd_target_file)
gbd <- as.data.table(readRDS(gbd_target_file))
keys <- c("location", "sex", "cause", "age", "year")
normalise_age <- function(x) {
  a <- trimws(sub(" years$", "", as.character(x)))
  a <- gsub("[–—]", "-", a)
  single <- grepl("^[0-9]+$", a)
  lower <- suppressWarnings(as.integer(ifelse(single, a, sub("^([0-9]+).*$", "\\1", a))))
  a[single & lower >= 20L & lower < 95L] <- paste0(5L * (lower[single & lower >= 20L & lower < 95L] %/% 5L), "-",
                                                   5L * (lower[single & lower >= 20L & lower < 95L] %/% 5L) + 4L)
  a[!single & !grepl("^[0-9]+-[0-9]+$|^[0-9]+\\+$", a)] <- NA_character_
  a[!is.na(lower) & lower >= 95L] <- "95+"
  a[!is.na(lower) & lower < 20L] <- NA_character_
  a
}
prev <- gbd[metric_name == "Number" & measure_name == "Prevalence" &
              cause_name %in% unname(cause_map[names(cause_map) != "all"]) &
              location_name %in% locs & year >= CAL_YEAR_START & year <= CAL_YEAR_END,
            .(location = as.character(location_name), sex = as.character(sex_name),
              cause = as.character(cause_name), age = normalise_age(age_name),
              year = as.integer(year), gbdPrev = as.numeric(val))]
prev <- prev[!is.na(age) & age %in% unique(normalise_age(20:95)) & sex %in% c("Male", "Female")]
if (anyNA(prev$gbdPrev) || any(prev$gbdPrev < 0) || anyDuplicated(prev, by = keys))
  stop("GBD prevalence contains missing, negative or duplicated calibration cells")
if (!nrow(prev)) stop("No GBD prevalence targets in calibration window")

if (calibration_deaths_source == "WHO") {
  if (!file.exists(who_deaths_target_file))
    stop("WHO deaths target not found: ", who_deaths_target_file,
         "; run the WHO-deaths cleaning step first")
  death <- as.data.table(readRDS(who_deaths_target_file))
  pick <- function(candidates, label) {
    found <- intersect(candidates, names(death))
    if (!length(found)) stop("WHO deaths file needs a ", label,
                             " column; available: ", paste(names(death), collapse = ", "))
    found[1L]
  }
  measure_col <- intersect(c("measure_name", "measure"), names(death))
  if (length(measure_col)) death <- death[tolower(as.character(get(measure_col[1L]))) == "deaths"]
  metric_col <- intersect(c("metric_name", "metric"), names(death))
  if (length(metric_col)) death <- death[tolower(as.character(get(metric_col[1L]))) == "number"]
  loc_col <- pick(c("location", "location_name"), "location")
  sex_col <- pick(c("sex", "sex_name"), "sex")
  cause_col <- pick(c("cause", "cause_name"), "cause")
  age_col <- pick(c("age_group", "age_name", "age", "age.group"), "age")
  year_col <- pick(c("year", "year_id"), "year")
  value_col <- pick(c("deaths", "Deaths", "death_count", "deaths_who", "val", "value"), "death count")
  who_single_year <- all(grepl("^[0-9]+$", as.character(death[[age_col]])))
  death <- death[, .(location = as.character(get(loc_col)), sex = as.character(get(sex_col)),
                     cause = as.character(get(cause_col)), age = normalise_age(get(age_col)),
                     year = as.integer(get(year_col)), targetDeaths = as.numeric(get(value_col)))]
  death[sex %in% c("male", "MALE"), sex := "Male"]
  death[sex %in% c("female", "FEMALE"), sex := "Female"]
  death[cause %in% names(cause_map), cause := unname(cause_map[cause])]
  # WHO location labels are harmonized after this RDS is saved in stage 02.
  who_to_gbd_loc <- c(
    "Antigua & Barbuda" = "Antigua and Barbuda",
    "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
    "Brunei" = "Brunei Darussalam",
    "Côte d’Ivoire" = "Ivory Coast",
    "Congo - Kinshasa" = "Democratic Republic of the Congo",
    "Congo - Brazzaville" = "Congo",
    "Cape Verde" = "Cabo Verde",
    "Iran" = "Iran (Islamic Republic of)",
    "South Korea" = "Republic of Korea",
    "Laos" = "Lao People's Democratic Republic",
    "St. Lucia" = "Saint Lucia",
    "Moldova" = "Republic of Moldova",
    "Myanmar (Burma)" = "Myanmar",
    "North Korea" = "Democratic People's Republic of Korea",
    "Palestinian Territories" = "Palestine",
    "Russia" = "Russian Federation",
    "São Tomé & Príncipe" = "Sao Tome and Principe",
    "Syria" = "Syrian Arab Republic",
    "Trinidad & Tobago" = "Trinidad and Tobago",
    "St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    "Venezuela" = "Venezuela (Bolivarian Republic of)",
    "Vietnam" = "Viet Nam"
  )
  death[location %in% names(who_to_gbd_loc),
        location := unname(who_to_gbd_loc[location])]
} else {
  death <- gbd[metric_name == "Number" & measure_name == "Deaths",
               .(location = as.character(location_name), sex = as.character(sex_name),
                 cause = as.character(cause_name), age = normalise_age(age_name),
                 year = as.integer(year), targetDeaths = as.numeric(val))]
}
# WHO reports deaths for an open 85+ band. Allocate that count to the three
# model/GBD bands using GBD Number-of-Deaths shares for the SAME location, sex,
# cause and year. Never treat the 85+ aggregate as an 85-89 count.
if (calibration_deaths_source == "WHO") {
  older <- death[age == "85+" & location %in% locs &
                   cause %in% unname(cause_map[names(cause_map) != "all"]) &
                   sex %in% c("Male", "Female") &
                   year >= CAL_YEAR_START & year <= CAL_YEAR_END]
  group_keys <- c("location", "sex", "cause", "year")
  if (nrow(older)) {
    if (anyDuplicated(older, by = group_keys))
      stop("Duplicate WHO 85+ death counts for a location/sex/cause/year")
    gbd_older <- gbd[metric_name == "Number" & measure_name == "Deaths" &
                       location_name %in% locs &
                       cause_name %in% unname(cause_map[names(cause_map) != "all"]) &
                       year >= CAL_YEAR_START & year <= CAL_YEAR_END,
                     .(location = as.character(location_name), sex = as.character(sex_name),
                       cause = as.character(cause_name), year = as.integer(year),
                       age = normalise_age(age_name), gbd_count = as.numeric(val))]
    gbd_older <- gbd_older[age %in% c("85-89", "90-94", "95+") &
                             sex %in% c("Male", "Female")]
    if (anyNA(gbd_older$gbd_count) || any(gbd_older$gbd_count < 0) ||
        anyDuplicated(gbd_older, by = c(group_keys, "age")))
      stop("GBD older-age deaths contain missing, negative or duplicate counts")
    alloc <- merge(older[, .(location, sex, cause, year,
                              who_85_count = targetDeaths)],
                   gbd_older, by = group_keys, all.x = TRUE,
                   allow.cartesian = TRUE, sort = FALSE)
    coverage <- alloc[, .(n_bands = .N, n_unique = uniqueN(age),
                          missing_gbd = anyNA(gbd_count),
                          gbd_total = sum(gbd_count)), by = group_keys]
    if (coverage[, any(n_bands != 3L | n_unique != 3L | missing_gbd)])
      stop("WHO 85+ split requires GBD death counts for 85-89, 90-94 and 95+ ",
           "in every location/sex/cause/year")
    alloc[coverage, on = group_keys, gbd_total := i.gbd_total]
    if (alloc[, any(who_85_count > 0 & gbd_total <= 0)])
      stop("Cannot distribute positive WHO 85+ deaths with zero GBD 85+ deaths")
    alloc[, targetDeaths := fifelse(gbd_total > 0,
                                   who_85_count * gbd_count / gbd_total, 0)]
    check <- alloc[, .(who_count = first(who_85_count),
                       allocated = sum(targetDeaths)), by = group_keys]
    if (check[, any(abs(who_count - allocated) >
                     1e-8 * pmax(1, abs(who_count)))])
      stop("WHO 85+ death totals are not preserved after GBD-share allocation")
    death <- rbindlist(list(death[age != "85+"],
                            alloc[, .(location, sex, cause, age, year,
                                      targetDeaths)]), use.names = TRUE)
    message("Distributed ", nrow(older),
            " WHO 85+ death cells across GBD 85-89/90-94/95+ bands")
  }
}
death <- death[location %in% locs & cause %in% unname(cause_map[names(cause_map) != "all"]) &
                 sex %in% c("Male", "Female") & year >= CAL_YEAR_START &
                 year <= CAL_YEAR_END & !is.na(age) & age %in% unique(prev$age)]
if (anyNA(death$targetDeaths) || any(death$targetDeaths < 0))
  stop(calibration_deaths_source, " deaths contain missing or negative counts")
# Single-year WHO values can be summed to five-year bands; duplicate aggregate
# bands are ambiguous and must be resolved in the WHO cleaning step.
if (anyDuplicated(death, by = keys)) {
  if (calibration_deaths_source == "WHO" &&
      isTRUE(who_single_year)) {
    death <- death[, .(targetDeaths = sum(targetDeaths)), by = keys]
  } else stop(calibration_deaths_source, " deaths have duplicate age-band keys; check age aggregation and other dimensions")
}
# Country-level fallback only. Retain WHO for every country with any WHO
# calibration-window deaths; incomplete WHO country panels remain an error.
if (calibration_deaths_source == "WHO") {
  fallback_locs <- setdiff(unique(prev$location), unique(death$location))
  if (length(fallback_locs)) {
    gbd_fallback <- gbd[metric_name == "Number" & measure_name == "Deaths" &
                          location_name %in% fallback_locs &
                          cause_name %in% unname(cause_map[names(cause_map) != "all"]) &
                          year >= CAL_YEAR_START & year <= CAL_YEAR_END,
                        .(location = as.character(location_name),
                          sex = as.character(sex_name),
                          cause = as.character(cause_name),
                          age = normalise_age(age_name),
                          year = as.integer(year), targetDeaths = as.numeric(val))]
    gbd_fallback <- gbd_fallback[sex %in% c("Male", "Female") &
                                   !is.na(age) & age %in% unique(prev$age)]
    if (anyNA(gbd_fallback$targetDeaths) || any(gbd_fallback$targetDeaths < 0) ||
        anyDuplicated(gbd_fallback, by = keys))
      stop("GBD fallback deaths contain missing, negative or duplicate cells")
    death <- rbindlist(list(death, gbd_fallback), use.names = TRUE)
    message("GBD deaths used for ", length(fallback_locs),
            " countries without WHO calibration data: ",
            paste(sort(fallback_locs), collapse = ", "))
  }
}
targets <- merge(prev, death, by = keys, all.x = TRUE, sort = FALSE)
if (anyNA(targets$targetDeaths)) {
  missing <- targets[is.na(targetDeaths), ..keys]
  stop("Missing ", calibration_deaths_source, " deaths for ", nrow(missing),
       " GBD prevalence cells. First missing key: ",
       paste(unlist(missing[1L]), collapse = " | "))
}
if (anyDuplicated(targets, by = keys)) stop("Duplicate calibration target keys")
expected <- unique(b_rates[year >= CAL_YEAR_START & year <= CAL_YEAR_END,
                           .(location = as.character(location), sex = as.character(sex),
                             cause = as.character(cause), age = normalise_age(age),
                             year = as.integer(year))])
missing_model_keys <- fsetdiff(expected, targets[, ..keys])
if (nrow(missing_model_keys))
  stop("Calibration targets do not cover ", nrow(missing_model_keys),
       " model cells; first missing: ", paste(unlist(missing_model_keys[1L]), collapse = " | "))
message("Calibration targets: ", calibration_deaths_source, " deaths + GBD prevalence; ",
        nrow(targets), " cells (", CAL_YEAR_START, "-", CAL_YEAR_END, ")")
rm(gbd, prev, death)
age_lut <- unique(data.table(age = normalise_age(20:95),
                             model_age = as.character(gbd_band_label(20:95))))
if (anyDuplicated(age_lut, by = "age"))
      stop("gbd_band_label maps a target age band to multiple model labels")
targets <- merge(targets, age_lut, by = "age", all.x = TRUE, sort = FALSE)
if (anyNA(targets$model_age)) stop("Unmapped calibration age band")
targets[, `:=`(age = model_age, model_age = NULL)]
setkey(targets, location, sex, cause)

#===============================================================================
# 3. HELPER FUNCTIONS
#===============================================================================

## ----------------------------------------------------------------------------
## Map single-year ages 20:95+ to the GBD five-year labels.
## gbd_band_label comes from the existing pipeline utility.
## ----------------------------------------------------------------------------
make_age_match <- function() {
      am <- data.table(age = 20:95)
      am[, age.group := gbd_band_label(age)]
      if (anyNA(am$age.group) || !any(nchar(am$age.group[am$age == 95L]) > 0L))
            stop("Cannot map age 95+ to a GBD age group")
      am
}

## ----------------------------------------------------------------------------
## Enforce the probability / row constraints on a TP table (in place).
## PREFERENCE ORDER (per task spec):
##   (1) NA -> 0, clamp IR,CF,BG.mx into [0,1];
##   (2) PRIMARY: preserve BG.mx, cap the disease TP so IR+BG.mx<=1, CF+BG.mx<=1
##       i.e. IR,CF <= 1 - BG.mx - TP_EPS;
##   (3) FALLBACK (only when BG.mx itself leaves no room, BG.mx >= 1 - TP_EPS):
##       proportionally renormalise the disease TP AND BG.mx by their shares,
##       the share-shrink from 032_adjustments_indonesia.R lines 46-55. Rows
##       whose BG.mx was modified are flagged in `bg_modified` for audit.
## Adds an integer `bg_modified` column (0/1) used only for diagnostics.
## ----------------------------------------------------------------------------
enforce_tp_constraints <- function(dt, tp_eps = TP_EPS) {
      ## (1) NA -> 0 and basic [0,1] clamp -----------------------------------
      dt[is.na(IR),    IR := 0]
      dt[is.na(CF),    CF := 0]
      dt[is.na(BG.mx), BG.mx := 0]
      dt[IR < 0, IR := 0]; dt[IR > 1, IR := 1]
      dt[CF < 0, CF := 0]; dt[CF > 1, CF := 1]
      dt[BG.mx < 0, BG.mx := 0]

      if (!("bg_modified" %in% names(dt))) dt[, bg_modified := 0L]

      ## headroom for the disease TP given FIXED background mortality
      dt[, headroom := 1 - BG.mx - tp_eps]

      ## (2) PRIMARY: cap IR/CF into the headroom, leaving BG.mx untouched ----
      dt[headroom >= 0 & IR > headroom, IR := headroom]
      dt[headroom >= 0 & CF > headroom, CF := headroom]

      ## (3) FALLBACK: BG.mx alone leaves no room (headroom < 0). Must shrink
      ##     BG.mx. Proportional renormalisation a la 032 lines 46-55.
      ##     IR side:
      dt[headroom < 0 & (IR + BG.mx) > 1, `:=`(
            IR_new   = IR    / (IR + BG.mx) - tp_eps,
            BGmx_new = BG.mx / (IR + BG.mx) - tp_eps,
            bg_modified = 1L
      )]
      dt[!is.na(IR_new), `:=`(IR = pmax(IR_new, 0), BG.mx = pmax(BGmx_new, 0))]
      dt[, c("IR_new", "BGmx_new") := NULL]
      ##     CF side (uses possibly-updated BG.mx; for normal rows CF+BG.mx<=1
      ##     already holds after the primary cap, so this only fires in fallback)
      dt[(CF + BG.mx) > 1, `:=`(
            CF_new   = CF    / (CF + BG.mx) - tp_eps,
            BGmx_new = BG.mx / (CF + BG.mx) - tp_eps,
            bg_modified = 1L
      )]
      dt[!is.na(CF_new), `:=`(CF = pmax(CF_new, 0), BG.mx = pmax(BGmx_new, 0))]
      dt[, c("CF_new", "BGmx_new") := NULL]

      dt[, headroom := NULL]
      dt[]
}

## ----------------------------------------------------------------------------
## Build the multiplier table (age.group -> ir_mult, cf_mult) from a flat
## parameter vector, given the granularity. For "combo" the same scalar pair is
## broadcast to every age group; for "age_group" each group gets its own pair.
## ----------------------------------------------------------------------------
build_mtab <- function(par, age_groups, granularity) {
      if (granularity == "age_group") {
            n <- length(age_groups)
            data.table(age.group = age_groups,
                       ir_mult = par[1:n],
                       cf_mult = par[(n + 1):(2 * n)])
      } else {
            data.table(age.group = age_groups,
                       ir_mult = par[1],
                       cf_mult = par[2])
      }
}

## ----------------------------------------------------------------------------
## Apply the multipliers to a combo's TP rows (all years), then enforce the
## probability constraints. Returns a NEW data.table (baseline is never mutated).
## Age groups absent from mtab keep their baseline IR/CF (mult defaults to 1).
## ----------------------------------------------------------------------------
apply_multipliers <- function(combo_rates, mtab, age_match) {
      cr <- copy(combo_rates)
      cr <- merge(cr, age_match, by = "age",       all.x = TRUE)
      cr <- merge(cr, mtab,      by = "age.group", all.x = TRUE)
      cr[is.na(ir_mult), ir_mult := 1]
      cr[is.na(cf_mult), cf_mult := 1]
      cr[, IR := IR * ir_mult]
      cr[, CF := CF * cf_mult]
      cr[, c("age.group", "ir_mult", "cf_mult") := NULL]
      enforce_tp_constraints(cr)
      cr
}

## ----------------------------------------------------------------------------
## Project an age-20 entry cohort over 20:95+, pooling survivors in 95+.
## Candidate and saved rate tables use the same constraints and projection.
## ----------------------------------------------------------------------------
project_combo <- function(cr, pop_combo, y0 = CAL_YEAR_START, y1 = CAL_YEAR_END) {
      # One record per single-year age 20:94 and one open-ended age 95+.
      br <- copy(cr[year >= y0 & year <= y1 & age >= 20L & age <= 95L])
      if (nrow(br) != (y1 - y0 + 1L) * 76L ||
          anyDuplicated(br[, .(year, age)]) ||
          !setequal(unique(br$age), 20:95))
            stop("project_combo requires a complete, unique age 20:95+ grid in every year")
      entrants <- unique(pop_combo[age == 20L & year >= y0 & year <= y1,
                                  .(year, Nx20)])
      if (nrow(entrants) != y1 - y0 + 1L ||
          anyDuplicated(entrants$year) || anyNA(entrants$Nx20))
            stop("Missing or duplicated age-20 UNWPP entrants for calibration years")
      br[entrants, on = .(year), Nx20 := i.Nx20]
      br[age == 20L & year > y0, Nx := Nx20]
      br[, Nx20 := NULL]
      if (anyNA(br[year == y0 | age == 20L, Nx]))
            stop("Missing initial population or age-20 entrants")

      br[year == y0 | age == 20L, `:=`(
            sick = Nx * PREVt0,
            dead = Nx * DIS.mx.t0,
            well = Nx * (1 - (PREVt0 + ALL.mx)),
            pop = Nx,
            all.mx = Nx * ALL.mx
      )]
      # Keep the original projection's protective 0.9 disease-transition cap.
      br[CF > 0.9, CF := 0.9]
      br[IR > 0.9, IR := 0.9]

      for (yr in seq.int(y0 + 1L, y1)) {
            previous <- br[year == yr - 1L,
                           .(age = pmin(age + 1L, 95L), sick, well, pop, all.mx)]
            rates <- br[year == yr, .(age, IR, CF, BG.mx, BG.mx.all)]
            step <- merge(previous, rates, by = "age", all.x = TRUE, sort = FALSE)
            if (nrow(step) != 76L || anyNA(step$IR) || anyNA(step$CF) ||
                anyNA(step$BG.mx) || anyNA(step$BG.mx.all))
                  stop("Missing destination-age transition probabilities")
            step[, `:=`(
                  sick_contribution = pmax(sick * (1 - CF - BG.mx) + well * IR, 0),
                  dead_contribution = pmax(sick * CF, 0),
                  pop_contribution = pmax(pop - all.mx, 0)
            )]
            # Both 94-year-olds and prior 95+ survivors enter the new 95+ cell.
            upd <- step[, .(
                  sick2 = sum(sick_contribution),
                  dead2 = sum(dead_contribution),
                  pop2 = sum(pop_contribution),
                  bg_rate = BG.mx.all[1L]
            ), by = age]
            upd[, `:=`(
                  year = yr,
                  allmx2 = pmax(dead2 + pop2 * bg_rate, 0)
            )]
            upd[, well2 := pmax(pop2 - allmx2 - sick2, 0)]
            br[upd, on = .(year, age), `:=`(
                  sick = i.sick2, dead = i.dead2, well = i.well2,
                  pop = i.pop2, all.mx = i.allmx2
            )]
      }
      if (anyNA(br[, .(sick, dead, well, pop, all.mx)]))
            stop("Projection left missing health states")
      br[, .(location, sex, cause, year, age, sick, dead)]
}

## ----------------------------------------------------------------------------
## Aggregate a single-age projection to 5-year age groups (model Prevalence =
## sum of sick, model Deaths = sum of dead) and join GBD targets.
## ----------------------------------------------------------------------------
proj_vs_targets <- function(proj, combo_targets, age_match) {
      m <- merge(proj, age_match, by = "age", all.x = TRUE)
      ms <- m[, .(Prevalence = sum(sick, na.rm = TRUE),
                  Deaths     = sum(dead, na.rm = TRUE)),
              by = .(location, sex, cause, year, age.group)]
      setnames(ms, "age.group", "age")
      j <- merge(combo_targets, ms,
                 by = c("location", "sex", "cause", "year", "age"), all.x = TRUE)
      j[is.na(Deaths),     Deaths := 0]
      j[is.na(Prevalence), Prevalence := 0]
      j
}

## ----------------------------------------------------------------------------
## Sum of age-band weighted absolute RMSEs (same target measure as grid).
## ----------------------------------------------------------------------------
combo_error <- function(proj, combo_targets, age_match,
                        w_deaths = W_DEATHS, w_prev = W_PREV, eps = EPS_REL) {
      j <- proj_vs_targets(proj, combo_targets, age_match)
      if (anyNA(j$targetDeaths) || anyNA(j$gbdPrev))
            stop("Missing target during stochastic calibration scoring")
      band <- j[, .(RMSE_deaths = sqrt(mean((Deaths - targetDeaths)^2)),
                    RMSE_prev = sqrt(mean((Prevalence - gbdPrev)^2))), by = age]
      sum(w_deaths * band$RMSE_deaths + w_prev * band$RMSE_prev)
}

## ----------------------------------------------------------------------------
## ABSOLUTE RMSE diagnostics per location-sex-cause-age.group (for comparison
## with the grid search (2*RMSE_deaths + RMSE_prev in count units).
## ----------------------------------------------------------------------------
combo_diag <- function(proj, combo_targets, age_match) {
      j <- proj_vs_targets(proj, combo_targets, age_match)
      j[, .(
            RMSE_deaths = sqrt(mean((Deaths     - targetDeaths)^2, na.rm = TRUE)),
            RMSE_prev   = sqrt(mean((Prevalence - gbdPrev)^2,   na.rm = TRUE))
      ), by = .(location, sex, cause, age)]
}

## ----------------------------------------------------------------------------
## Optional squared differences between adjacent age-band log multipliers.
## A zero default keeps calibration driven by observed fit; when enabled,
## the penalized objective is compared to the baseline (whose penalty is zero).
multiplier_smoothness <- function(par, n_g, granularity, lambda) {
      if (granularity != "age_group" || n_g < 2L || lambda <= 0) return(0)
      lambda * (sum(diff(log(par[seq_len(n_g)]))^2) +
                sum(diff(log(par[n_g + seq_len(n_g)]))^2))
}

## ----------------------------------------------------------------------------
## Randomized coordinate search. Common-pair uniform draws initialize a
## whole-vector incumbent; most later draws replace one IR/CF band pair, with
## occasional full-vector draws for independent exploration.
calibrate_one_combo_random <- function(combo_rates, pop_combo, combo_targets, age_match,
                                       granularity, hw, n_iter, converge_tol,
                                       w_deaths, w_prev, eps, seed) {
      set.seed(seed)
      lo <- 1 - hw; hi <- 1 + hw
      age_groups <- unique(age_match[order(age), age.group])
      age_groups <- age_groups[age_groups %in% combo_targets$age]
      n_g <- length(age_groups)
      n_par <- if (granularity == "age_group") 2L * n_g else 2L
      if (!n_g) stop("No calibration age groups")
      eval_par <- function(par) {
            cr2 <- apply_multipliers(combo_rates,
                                     build_mtab(par, age_groups, granularity),
                                     age_match)
            fit <- combo_error(project_combo(cr2, pop_combo), combo_targets,
                               age_match, w_deaths, w_prev, eps)
            if (!is.finite(fit)) stop("Non-finite calibration objective")
            list(fit = fit,
                 total = fit + multiplier_smoothness(par, n_g, granularity,
                                                       SMOOTH_LAMBDA))
      }
      best_par <- rep(1, n_par)
      score <- eval_par(best_par)
      best_fit <- base_err <- score$fit
      best_total <- score$total
      n_eval <- 1L

      n_common <- if (granularity == "age_group") min(COMMON_EVALS, n_iter) else 0L
      if (n_common > 0L) for (it in seq_len(n_common)) {
            pair <- runif(2L, lo, hi)
            cand <- c(rep(pair[1L], n_g), rep(pair[2L], n_g))
            result <- eval_par(cand); n_eval <- n_eval + 1L
            if (result$total < best_total) {
                  best_par <- cand
                  best_fit <- result$fit
                  best_total <- result$total
            }
            if (best_fit <= converge_tol) break
      }
      order <- integer(0)
      position <- 1L
      remaining <- n_iter - n_common
      if (best_fit > converge_tol && remaining > 0L) for (it in seq_len(remaining)) {
            if (granularity == "age_group" && position > length(order)) {
                  order <- sample.int(n_g)
                  position <- 1L
            }
            if (granularity == "combo" || runif(1L) < FULL_VECTOR_PROB) {
                  cand <- runif(n_par, lo, hi)
            } else {
                  g <- order[position]; position <- position + 1L
                  groups <- g
                  if (n_g > 1L && runif(1L) < ADJACENT_PROB) {
                        neighbor <- if (g == n_g) g - 1L else g + 1L
                        groups <- c(g, neighbor)
                  }
                  idx <- c(groups, n_g + groups)
                  cand <- best_par
                  cand[idx] <- runif(length(idx), lo, hi)
            }
            result <- eval_par(cand); n_eval <- n_eval + 1L
            if (result$total < best_total) {
                  best_par <- cand
                  best_fit <- result$fit
                  best_total <- result$total
            }
            if (best_fit <= converge_tol) break
      }
      list(mtab = build_mtab(best_par, age_groups, granularity),
           best_err = best_fit, penalized_err = best_total, base_err = base_err,
           n_eval = n_eval, n_par = n_par,
           hit_bound = any(best_par <= lo + 1e-9 | best_par >= hi - 1e-9))
}

## ----------------------------------------------------------------------------
## Drive one combo end-to-end: calibrate, bake multipliers into ALL years, and
## assemble its calibrated rows, factor records, and baseline-vs-calibrated
## diagnostics. Returns a list combined across combos after the parallel loop.
## ----------------------------------------------------------------------------
run_combo <- function(ci, combos, b_rates, pop20, targets, age_match) {
      loc <- combos$location[ci]; sx <- combos$sex[ci]; cse <- combos$cause[ci]

      cr <- b_rates[location == loc & sex == sx & cause == cse]
      pc <- pop20[location == loc & sex == sx]
      ct <- targets[location == loc & sex == sx & cause == cse]

      ## baseline (mult = 1) rows + projection, for diagnostics & as fallback
      base_rows <- enforce_tp_constraints(copy(cr))

      if (nrow(ct) == 0) {
            ## no GBD target for this combo -> keep baseline unchanged
            base_rows[, bg_modified := NULL]
            return(list(
                  rows = base_rows,
                  factors = data.table(location = loc, sex = sx, cause = cse,
                                       age.group = NA_character_, ir_mult = 1, cf_mult = 1,
                                       granularity = GRANULARITY),
                  diag = data.table(location = loc, sex = sx, cause = cse,
                                    age = NA_character_,
                                    RMSE_deaths_base = NA_real_, RMSE_prev_base = NA_real_,
                                    RMSE_deaths_cal = NA_real_,  RMSE_prev_cal = NA_real_),
                  err = data.table(location = loc, sex = sx, cause = cse,
                                   base_err = NA_real_, cal_err = NA_real_,
                                   n_eval = 0L, n_par = 0L, hit_bound = FALSE,
                                   bg_modified_rows = 0L)
            ))
      }

      fit <- calibrate_one_combo_random(cr, pc, ct, age_match,
                                        GRANULARITY, SEARCH_HALFWIDTH, N_ITER,
                                        CONVERGE_TOL, W_DEATHS, W_PREV, EPS_REL,
                                        SEED + ci * 10000L)

      cal_rows  <- apply_multipliers(cr, fit$mtab, age_match)
      bg_mod_n  <- sum(cal_rows$bg_modified)
      cal_rows[, bg_modified := NULL]

      saved_fit <- combo_error(project_combo(cal_rows, pc), ct, age_match)
      if (!is.finite(saved_fit) ||
          abs(saved_fit - fit$best_err) > 1e-8 * max(1, abs(fit$best_err)) ||
          saved_fit > fit$base_err + 1e-8 * max(1, abs(fit$base_err)))
            stop("Saved transition probabilities differ from the scored solution")

      ## diagnostics: project baseline and calibrated, compute absolute RMSE
      base_diag <- combo_diag(project_combo(base_rows, pc), ct, age_match)
      cal_diag  <- combo_diag(project_combo(cal_rows,  pc), ct, age_match)
      setnames(base_diag, c("RMSE_deaths", "RMSE_prev"),
               c("RMSE_deaths_base", "RMSE_prev_base"))
      setnames(cal_diag,  c("RMSE_deaths", "RMSE_prev"),
               c("RMSE_deaths_cal", "RMSE_prev_cal"))
      diag <- merge(base_diag, cal_diag,
                    by = c("location", "sex", "cause", "age"), all = TRUE)

      factors <- copy(fit$mtab)
      factors[, `:=`(location = loc, sex = sx, cause = cse, granularity = GRANULARITY)]
      setcolorder(factors, c("location", "sex", "cause", "age.group",
                             "ir_mult", "cf_mult", "granularity"))

      err <- data.table(location = loc, sex = sx, cause = cse,
                        base_err = fit$base_err, cal_err = saved_fit,
                        n_eval = fit$n_eval, n_par = fit$n_par,
                        hit_bound = fit$hit_bound, bg_modified_rows = bg_mod_n)

      list(rows = cal_rows, factors = factors, diag = diag, err = err)
}

#===============================================================================
# 4. RUN CALIBRATION OVER ALL location-sex-cause COMBOS
#    Parallelised over COMBOS (not locations): Indonesia is a single location,
#    so location-level parallelism would use one core; combo-level uses all 12
#    (6 causes x 2 sexes) independent units.
#===============================================================================

age_match <- make_age_match()
combos    <- unique(b_rates[, .(location, sex, cause)])
n_combos  <- nrow(combos)

cat(sprintf("Random-search TP calibration: %d location-sex-cause combos | granularity = %s\n",
            n_combos, GRANULARITY))
cat(sprintf("Search range per multiplier: [%.2f, %.2f] | up to %d focused candidates/combo\n",
            1 - SEARCH_HALFWIDTH, 1 + SEARCH_HALFWIDTH, N_ITER))

worker_exports <- c(
      "combos", "b_rates", "pop20", "targets", "age_match",
      "make_age_match", "enforce_tp_constraints",
      "build_mtab", "apply_multipliers", "project_combo", "proj_vs_targets",
      "combo_error", "combo_diag", "multiplier_smoothness", "calibrate_one_combo_random", "run_combo",
      "GRANULARITY", "SEARCH_HALFWIDTH", "N_ITER", "COMMON_EVALS",
      "ADJACENT_PROB", "FULL_VECTOR_PROB", "SMOOTH_LAMBDA", "CONVERGE_TOL",
      "W_DEATHS", "W_PREV", "EPS_REL",
      "TP_EPS", "CAL_YEAR_START", "CAL_YEAR_END", "SEED"
)

if (RUN_PAR && n_combos > 1) {
      n_cores <- max(1L, min(MAX_CORES, n_combos, parallel::detectCores() - 1L))
      cat(sprintf("Running in parallel on %d cores...\n", n_cores))
      cl <- makeCluster(n_cores)
      registerDoParallel(cl)
      results <- foreach(ci = seq_len(n_combos),
                         .packages = c("data.table"),
                         .export   = worker_exports) %dopar% {
            setDTthreads(1)
            run_combo(ci, combos, b_rates, pop20, targets, age_match)
      }
      stopCluster(cl)
} else {
      cat("Running sequentially...\n")
      results <- lapply(seq_len(n_combos), function(ci) {
            res <- run_combo(ci, combos, b_rates, pop20, targets, age_match)
            cat(sprintf("  [%d/%d] %s | %s | %s : err %.3g -> %.3g\n",
                        ci, n_combos, combos$location[ci], combos$sex[ci], combos$cause[ci],
                        res$err$base_err, res$err$cal_err))
            res
      })
}

## --- consolidate ------------------------------------------------------------
calibrated   <- rbindlist(lapply(results, `[[`, "rows"),    use.names = TRUE, fill = TRUE)
factors_out  <- rbindlist(lapply(results, `[[`, "factors"), use.names = TRUE, fill = TRUE)
diag_out     <- rbindlist(lapply(results, `[[`, "diag"),    use.names = TRUE, fill = TRUE)
err_out      <- rbindlist(lapply(results, `[[`, "err"),     use.names = TRUE, fill = TRUE)

## restore the exact INPUT column set/order (drop-in schema match with 031)
setcolorder(calibrated, intersect(tps_input_cols, names(calibrated)))

#===============================================================================
# 5. WRITE OUTPUTS  (calibrated_searo_part*.rds; 031 applies trends)
#===============================================================================

n     <- nrow(calibrated)
chunk <- ceiling(n / N_OUT_CHUNKS)
old_calibrated <- list.files(wd_data,
                             pattern = "^calibrated_searo_part[0-9]+\\.rds$",
                             full.names = TRUE)
if (length(old_calibrated)) file.remove(old_calibrated)
for (i in 1:N_OUT_CHUNKS) {
      start <- (i - 1) * chunk + 1
      end   <- min(i * chunk, n)
      if (start > n) {                       # write empty tail chunk for parity
            saveRDS(calibrated[0], file = paste0(wd_data, "calibrated_searo_part", i, ".rds"))
            next
      }
      saveRDS(calibrated[start:end],
              file = paste0(wd_data, "calibrated_searo_part", i, ".rds"))
}

## calibrated multipliers (analogous to 032's adjustments2023_age.csv)
fwrite(factors_out, paste0(wd_data, "calibration_factors_random_tp.csv"))

## fit diagnostics: baseline-vs-calibrated absolute RMSE per combo x age.group,
## plus the per-combo weighted error and its % improvement over baseline.
err_pct <- copy(err_out[, .(location, sex, cause, base_err, cal_err,
                            n_eval, n_par, hit_bound, bg_modified_rows)])
err_pct[, pct_improvement := 100 * (base_err - cal_err) / pmax(base_err, EPS_REL)]
diag_full <- merge(diag_out, err_pct, by = c("location", "sex", "cause"), all.x = TRUE)
fwrite(diag_full, paste0(wd_data, "calibration_diagnostics_random_tp.csv"))
file.copy(file.path(wd_data, "calibration_diagnostics_random_tp.csv"),
          file.path(wd_data, paste0("calibration_diagnostics_random_tp_", calibration_deaths_source, ".csv")),
          overwrite = TRUE)
file.copy(file.path(wd_data, "calibration_factors_random_tp.csv"),
          file.path(wd_data, paste0("calibration_factors_random_tp_", calibration_deaths_source, ".csv")),
          overwrite = TRUE)

#===============================================================================
# 6. VALIDATION  (assert constraints; print baseline-vs-calibrated fit summary)
#===============================================================================

cat("\n", strrep("=", 70), "\nVALIDATION\n", strrep("=", 70), "\n", sep = "")

stopifnot(
      "IR contains NA"            = !anyNA(calibrated$IR),
      "CF contains NA"            = !anyNA(calibrated$CF),
      "BG.mx contains NA"         = !anyNA(calibrated$BG.mx),
      "IR outside [0,1]"          = calibrated[, all(IR >= 0 & IR <= 1)],
      "CF outside [0,1]"          = calibrated[, all(CF >= 0 & CF <= 1)],
      "IR + BG.mx > 1"            = calibrated[, all(IR + BG.mx <= 1 + 1e-9)],
      "CF + BG.mx > 1"            = calibrated[, all(CF + BG.mx <= 1 + 1e-9)],
      "row count != input"        = nrow(calibrated) == tps_input_nrow,
      "schema != input"           = setequal(names(calibrated), tps_input_cols)
)
cat("All probability/row constraints satisfied.\n")
cat(sprintf("Rows: %d (matches input: %d). Schema matches input: TRUE.\n",
            nrow(calibrated), tps_input_nrow))

bg_rows_total <- sum(err_out$bg_modified_rows, na.rm = TRUE)
if (bg_rows_total > 0) {
      cat(sprintf("NOTE: BG.mx was renormalised (fallback) on %d rows where ",
                  bg_rows_total),
          "BG.mx alone left no room for the disease TP. See bg_modified_rows ",
          "in calibration_diagnostics_random_tp.csv.\n", sep = "")
} else {
      cat("BG.mx preserved on ALL rows (no fallback renormalisation needed).\n")
}

## weighted-error improvement (search objective) -----------------------------
tot_base_err <- sum(err_out$base_err, na.rm = TRUE)
tot_cal_err  <- sum(err_out$cal_err,  na.rm = TRUE)
cat(sprintf("\nWeighted relative error (search objective), summed over combos:\n"))
cat(sprintf("  baseline = %.4g   calibrated = %.4g   reduction = %.1f%%\n",
            tot_base_err, tot_cal_err,
            100 * (tot_base_err - tot_cal_err) / max(tot_base_err, EPS_REL)))

## absolute RMSE improvement (comparable to 031's units) ---------------------
abs_summary <- diag_out[, .(
      RMSE_deaths_base = mean(RMSE_deaths_base, na.rm = TRUE),
      RMSE_deaths_cal  = mean(RMSE_deaths_cal,  na.rm = TRUE),
      RMSE_prev_base   = mean(RMSE_prev_base,   na.rm = TRUE),
      RMSE_prev_cal    = mean(RMSE_prev_cal,    na.rm = TRUE)
)]
cat("\nMean absolute RMSE across combo x age.group cells (counts):\n")
cat(sprintf("  Deaths     : baseline = %.1f -> calibrated = %.1f\n",
            abs_summary$RMSE_deaths_base, abs_summary$RMSE_deaths_cal))
cat(sprintf("  Prevalence : baseline = %.1f -> calibrated = %.1f\n",
            abs_summary$RMSE_prev_base, abs_summary$RMSE_prev_cal))

## per-combo table for quick scan -------------------------------------------
cat("\nPer-combo weighted error (baseline -> calibrated):\n")
print(err_out[order(cause, sex),
              .(location, sex, cause,
                base_err = round(base_err, 3),
                cal_err  = round(cal_err, 3),
                n_eval, hit_bound, bg_modified_rows)])

if (any(err_out$hit_bound, na.rm = TRUE)) {
      cat("\nWARNING: some combos hit the search bound -- consider widening ",
          "SEARCH_HALFWIDTH.\n", sep = "")
}

cat("\nWrote:\n")
cat(sprintf("  %scalibrated_searo_part{1..%d}.rds\n", wd_data, N_OUT_CHUNKS))
cat(sprintf("  %scalibration_factors_random_tp.csv\n", wd_data))
cat(sprintf("  %scalibration_diagnostics_random_tp.csv\n", wd_data))
cat("\nReminder: set  run_adjustment_model <- FALSE  before sourcing ",
    "05_build_baseline_indonesia.R to avoid re-applying 032's old factors.\n", sep = "")

#===============================================================================
# 7. OPTIONAL: baseline-vs-calibrated comparison plot
#     The transparent script produces NO plots; this block is purely optional
#     and the pipeline does NOT depend on it. It only runs if ggplot2 is
#     available and MAKE_PLOTS is TRUE, so sourcing this file never fails for
#     lack of a plotting package. Mirrors diagnostic output 4 (baseline vs
#     calibrated absolute RMSE for Deaths and Prevalence, per combo).
#===============================================================================

MAKE_PLOTS <- if (exists("make_calibration_plots")) isTRUE(make_calibration_plots) else FALSE

if (MAKE_PLOTS && requireNamespace("ggplot2", quietly = TRUE)) {
      pdat <- melt(
            diag_out[, .(location, sex, cause, age,
                         Deaths_base = RMSE_deaths_base, Deaths_cal = RMSE_deaths_cal,
                         Prev_base   = RMSE_prev_base,   Prev_cal   = RMSE_prev_cal)],
            id.vars      = c("location", "sex", "cause", "age"),
            variable.name = "metric", value.name = "rmse"
      )
      pdat[, c("measure", "stage") := tstrsplit(metric, "_", fixed = TRUE)]
      p <- ggplot2::ggplot(
            pdat, ggplot2::aes(x = age, y = rmse, colour = stage, group = stage)) +
            ggplot2::geom_line() +
            ggplot2::facet_grid(measure ~ cause + sex, scales = "free_y") +
            ggplot2::labs(
                  title    = "Random-search calibration: baseline vs calibrated RMSE",
                  subtitle = sprintf("Indonesia | granularity = %s | %d candidates/combo",
                                     GRANULARITY, N_ITER),
                  x = "Age group", y = "Absolute RMSE (counts)", colour = "") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      ggplot2::ggsave(paste0(wd_data, "calibration_fit_random_tp.png"),
                      p, width = 14, height = 7, dpi = 150)
      cat(sprintf("  %scalibration_fit_random_tp.png (optional plot)\n", wd_data))
} else if (MAKE_PLOTS) {
      cat("NOTE: make_calibration_plots is TRUE but ggplot2 is not installed; ",
          "skipping the optional comparison plot.\n", sep = "")
}
