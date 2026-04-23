# 08_economic_value_calculation_v4.R
# VSL / VSLY analysis: monetary value of deaths averted under each intervention
# versus baseline.
#
# Method:
#   - Value of Statistical Life (VSL) is transferred from a U.S. reference
#     using income adjustment (Robinson & Hammitt 2011; Robinson et al. 2019).
#   - Constant Value of a Statistical Life Year (VSLY) is derived as:
#       VSLY = VSL / LE_avg_adult
#     where LE_avg_adult is the UNDISCOUNTED remaining life expectancy at the
#     average age of the adult population in each country-year-scenario.
#   - Monetary VSLY value is computed as:
#       VSLY_value = VSLY × Σ_a [ deaths_averted(a) × LE(a) ]
#     i.e. each averted death is credited with UNDISCOUNTED remaining life
#     expectancy at the age of death, aggregated using the age distribution of
#     deaths averted.
#   - Because the VSLY denominator uses remaining life expectancy at the
#     average adult age, while the VSLY numerator uses remaining life
#     expectancy at the age of death, VSLY totals are not mechanically equal
#     to VSL totals.
#   - GNI per capita is kept as observed from World Bank data and projected
#     forward beyond the last observed year using SSP2 annual GDP growth rates
#     (IIASA SSP 3.1).
#
# ── Methodological notes / departures from Robinson et al. (2019) ────────────
#
# [1] INCOME ELASTICITY — PRIMARY ESTIMATE:
#     Robinson et al. (2019) recommend the DIFFERENTIAL elasticity (0.8 for
#     countries at or above US income; 1.2 for countries below) as the primary
#     reference-case estimate. This is coded as `e1_2` throughout. Uniform
#     elasticities of 1.0 (`e1_0`) and 1.5 (`e1_5`) are treated as sensitivity
#     bounds, not primary estimates. Summary tables therefore use `e1_2` as the
#     headline figure, with `e1_0` and `e1_5` columns retained in dt_final for
#     sensitivity reporting.
#
# [2] GNI vs GDP FOR FORWARD PROJECTION (known limitation):
#     The VSL transfer uses World Bank GNI per capita (PPP) as the income
#     measure, consistent with Robinson et al. (2019). However, SSP scenario
#     projections (IIASA SSP 3.1) provide GDP per capita, not GNI per capita.
#     Forward projection therefore applies SSP2 GDP growth rates to a GNI base.
#     For most countries these series track closely, but the approach can
#     introduce error in remittance-dependent economies or countries with large
#     net foreign income flows (e.g. Philippines, Mexico, India). This
#     limitation should be acknowledged in any published methods section.
#     A future improvement would be to use SSP GNI projections if they become
#     available, or to apply a country-specific GNI/GDP ratio adjustment.
#
# Inputs:
#   - output/out_model/model_output_<country>_<target_col>.rds
#       (from 06_run_scenarios_multiple.R)
#   - data/raw/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv
#       (World Bank GNI per capita, PPP)
#   - data/raw/1721734326790-ssp_basic_drivers_release_3.1_full.xlsx
#       (IIASA SSP 3.1 — used for forward GNI projection)
#   - data/raw/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx
#       (remaining life expectancy by age)
#   - data/processed/Country_groupings_extended.csv
#       (iso3, location, region)
#
# Outputs:
#   - output/08_vsl_results.rds / .csv       — one row per country × year × scenario
#   - output/08_vsl_summary_table.rds / .csv — region × intervention summary (VSL)
#   - output/08_vsly_summary_table.rds / .csv — region × intervention summary (VSLY)
#   - output/08_vsl_vsly_summary_table_appended.rds / .csv — both stacked

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(countrycode)
  library(stringr)
})

# 0) Paths ----
DIR_MODEL    <- file.path(wd, "output", "out_model")
DIR_OUT      <- file.path(wd, "output")
GNI_FILE     <- file.path(wd, "data", "raw",
                          "API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv")
SSP_FILE     <- file.path(wd, "data", "raw",
                          "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
LT_FILE      <- file.path(wd, "data", "raw",
                          "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx")
COUNTRY_FILE <- file.path(wd, "data", "processed",
                          "Country_groupings_extended.csv")

OUT_FILE     <- file.path(DIR_OUT, "08_vsl_results.rds")
OUT_CSV      <- file.path(DIR_OUT, "08_vsl_results.csv")
OUT_SUMM_VSL       <- file.path(DIR_OUT, "08_vsl_summary_table.rds")
OUT_SUMM_VSLY      <- file.path(DIR_OUT, "08_vsly_summary_table.rds")
OUT_SUMM_APP       <- file.path(DIR_OUT, "08_vsl_vsly_summary_table_appended.rds")
# Explicit primary-elasticity copies (e1_2 = Robinson et al. 2019 reference case).
# Consumed downstream by scenarios/scenarios_aim1/aim1_report.Rmd to build the
# slide-deck artefacts for aim1_executive_slides.Rmd.
OUT_SUMM_VSL_PRIM  <- file.path(DIR_OUT, "08_vsl_summary_table_e1_2_primary.rds")
OUT_SUMM_VSLY_PRIM <- file.path(DIR_OUT, "08_vsly_summary_table_e1_2_primary.rds")

# 1) Parameters ----
#
# VSL reference (Robinson & Hammitt 2011):
#   VSL_USA = US_VSL_RATIO × GNI_pc_USA
#
# Income elasticity cases (see methodological note [1] above):
#   e1_2  — PRIMARY: differential 0.8 for countries at/above USA income,
#            1.2 for countries below (Robinson et al. 2019 reference case)
#   e1_0  — SENSITIVITY LOW:  uniform 1.0
#   e1_5  — SENSITIVITY HIGH: uniform 1.5
#
# VSLY (Robinson et al. 2019 reference case):
# VSLY:
#   VSLY = VSL / LE_avg_adult
#   where LE_avg_adult is undiscounted remaining life expectancy at the
#   average adult age.
#
# VSLY monetary value (per country-year-scenario):
#   VSLY_value = VSLY × Σ_a [ deaths_averted(a) × LE(a) ]
#   where LE(a) is undiscounted remaining life expectancy at the age of death.
#
# Discounting:
#   Time-series (calendar-year) discount factors bring annual benefit streams
#   to BASE_YEAR present value:
#      disc_rX = 1 / (1 + rX)^(year - BASE_YEAR)
#   These calendar-year discount factors are then applied to VSL, VSLY, and
#   GNI-based outputs in section 10.
# Minimum VSL floor: VSL / GNI_pc >= VSL_RATIO_FLOOR

US_VSL_RATIO    <- 160   # VSL / GNI_pc reference ratio for USA
VSL_ELAST_HIC   <- 0.8   # elasticity for countries at or above USA income
VSL_ELAST_LMIC  <- 1.2   # elasticity for countries below USA income
VSL_ELAST_LOW   <- 1.0   # sensitivity-low (uniform)
VSL_ELAST_HIGH  <- 1.5   # sensitivity-high (uniform)
VSL_RATIO_FLOOR <- 20    # minimum VSL-to-GNI ratio
ADULT_MIN_AGE   <- 20L
MAX_MODEL_AGE   <- 95L

DISC_RATES <- c(r1 = 0.01, r3 = 0.03, r5 = 0.05)
BASE_YEAR  <- 2026L
SUMMARY_YEARS <- c(2026L, 2030L, 2040L, 2050L)

# Within-lifetime discount rate used to annuitise remaining life expectancy.
# Set equal to r3 (3%) per Robinson et al. (2019) convention. This is distinct
# from the calendar-year discount factors disc_rX defined later.
R_VSLY <- unname(DISC_RATES["r3"])

# Closed-form annuity: present value of 1 unit per year for `le` years,
# discounted at within-lifetime rate `r`.
#   disc_life_years(LE, r) = (1 - (1 + r)^(-LE)) / r
# Returns NA for non-positive or missing LE. Gracefully handles r == 0 by
# returning LE itself (undiscounted case).
disc_life_years <- function(le, r) {
  le <- as.numeric(le)
  if (r == 0) {
    fifelse(is.na(le) | le <= 0, NA_real_, le)
  } else {
    fifelse(is.na(le) | le <= 0, NA_real_, (1 - (1 + r)^(-le)) / r)
  }
}

# 2) Load model outputs ----
model_files <- list.files(
  DIR_MODEL,
  pattern = "^model_output_.*\\.rds$",
  full.names = TRUE
)

if (length(model_files) == 0) {
  stop("No model output files found in: ", DIR_MODEL,
       "\nRun 06_run_scenarios_multiple.R first.")
}

cat("Loading", length(model_files), "model output files...\n")
dt_model <- rbindlist(lapply(model_files, readRDS), fill = TRUE)
cat("Rows loaded:", nrow(dt_model), "\n")

req_cols <- c("location", "year", "scenario", "htn_target_scenario",
              "age", "sex", "dead", "pop")
miss_cols <- setdiff(req_cols, names(dt_model))
if (length(miss_cols) > 0) {
  stop("Model output is missing required columns: ", paste(miss_cols, collapse = ", "))
}

# 3) Aggregate deaths and define population for income denominators ----
# Deaths are aggregated from the model by country-year-scenario.
#
# The model also provides scenario-specific population counts, but for this
# economic valuation script the `population` variable used in downstream income
# denominators is replaced with UN WPP 2024 population counts by country-year.
# This provides a common, externally sourced population base for calculating
# average adult age, life expectancy summaries, and total national income
# denominators across scenarios.

dt_deaths <- dt_model[
  , .(deaths = sum(dead, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

dt_pop_unique <- unique(
  dt_model[, .(location, year, scenario, htn_target_scenario, age, sex, pop)]
)

dt_pop_total <- dt_pop_unique[
  , .(population = sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

dt_deaths <- dt_pop_total[
  dt_deaths,
  on = .(location, year, scenario, htn_target_scenario)
]


# Despite interventions potentially afecting population size,
# we will use to compute national total income an average LE and VSLY 
# based on the population counts from the UN WPP 2024 revision,
#which are not scenario-specific. This is because the WPP 
# population counts are more robust and consistent for this purpose,
# while the model's scenario-specific population counts may be subject to greater 
#uncertainty and variability due to intervention effects. Using WPP population 
#allows us to compute a stable average LE and VSLY for each country-year, 
#which can then be applied across scenarios for a more consistent economic 
# valuation of deaths averted.

dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data,"PopulationsSingleAge0050.rds")))

dt_pop_unwpp[age>=95, age:= 95]

setnames(dt_pop_unwpp, c("year_id"), c("year"))

dt_pop_unwpp <- dt_pop_unwpp[, .(Nx = sum(Nx)), by = .(location, year)]

# merge to deaths data
dt_deaths <- dt_pop_unwpp[
  dt_deaths,
  on = .(location, year)] 

# replace poulation = Nx and drop population

dt_deaths[, population := Nx]
dt_deaths[, Nx := NULL]

# 4) Deaths averted relative to baseline ----
dt_baseline <- dt_deaths[
  scenario == "baseline",
  .(location, year, htn_target_scenario, deaths_baseline = deaths)
]

dt_compare <- dt_deaths[scenario != "baseline"]
dt_compare <- dt_baseline[
  dt_compare,
  on = .(location, year, htn_target_scenario)
]
setnames(dt_compare, "deaths", "deaths_intervention")
dt_compare[, deaths_averted := deaths_baseline - deaths_intervention]

# 5) Country mapping ----
country_grp <- fread(COUNTRY_FILE)

required_country_cols <- c("iso3", "location", "region")
missing_country_cols <- setdiff(required_country_cols, names(country_grp))
if (length(missing_country_cols) > 0) {
  stop("Country mapping file is missing required columns: ",
       paste(missing_country_cols, collapse = ", "))
}

dt_compare <- country_grp[dt_compare, on = .(location)]

missing_iso3 <- is.na(dt_compare$iso3)
if (any(missing_iso3)) {
  dt_compare[missing_iso3,
             iso3 := countrycode(location, origin = "country.name", destination = "iso3c",
                                 warn = FALSE)
  ]
}
setnames(dt_compare, "region", "who_region")

# 6) GNI per capita with SSP2 forward projection ----
#
# Strategy:
#   - Keep observed World Bank GNI per capita PPP values unchanged.
#   - For years beyond the last observed GNI year per country, project forward
#     using SSP2 annual GDP per capita growth rates (IIASA SSP 3.1).
#
# Key idea: growth_t = ssp_interp(t) / ssp_interp(t-1) - 1
#   gni(y0 + k) = gni(y0) * prod_{j=1}^{k} (1 + growth_{y0+j})
#
# This preserves the last observed GNI level and applies only SSP-implied
# growth, without incorrectly using ratios of GNI levels to GDP levels.
#
# NOTE: SSP projections provide GDP, not GNI. See methodological note [2]
# in the file header for a discussion of this known limitation.

gni_raw   <- fread(GNI_FILE, skip = 4, header = TRUE)
year_cols <- grep("^[0-9]{4}$", names(gni_raw), value = TRUE)

gni <- melt(
  gni_raw,
  id.vars      = "Country Code",
  measure.vars = year_cols,
  variable.name = "year",
  value.name    = "gni_pc_ppp"
)
setnames(gni, "Country Code", "iso3")
gni[, year := as.integer(as.character(year))]
gni <- gni[!is.na(gni_pc_ppp) & year >= 2000 & year <= 2050]

# --- SSP2 GDP per capita for growth rates ---
if (!exists("ssp_gdp")) {
  ssp_gdp <- as.data.table(read_excel(SSP_FILE, sheet = "data"))
}

ssp_pc <- ssp_gdp[
  Scenario == "SSP2" & Variable == "GDP|PPP [per capita]"
]

if (nrow(ssp_pc) == 0) {
  stop(
    "SSP data filtered to 0 rows. Check that Scenario='SSP2' and ",
    "Variable='GDP|PPP [per capita]' exist in the SSP file."
  )
}

ssp_yr_cols <- grep("^[0-9]{4}$", names(ssp_pc), value = TRUE)

ssp_pc_long <- melt(
  ssp_pc,
  id.vars      = "Region",
  measure.vars = ssp_yr_cols,
  variable.name = "year",
  value.name    = "ssp_gdp_pc"
)
setnames(ssp_pc_long, "Region", "location")
ssp_pc_long[, year := as.integer(as.character(year))]

ssp_pc_long[
  ,
  iso3 := countrycode(location, origin = "country.name", destination = "iso3c")
]
ssp_pc_long <- ssp_pc_long[!is.na(iso3) & !is.na(ssp_gdp_pc) & ssp_gdp_pc > 0]

model_years <- sort(unique(dt_compare$year))
iso3_list   <- sort(unique(dt_compare[!is.na(iso3), iso3]))

# Interpolate SSP GDP pc to annual series (log-linear between 5-year nodes)
ssp_annual <- ssp_pc_long[iso3 %in% iso3_list, {
  ord  <- order(year)
  yrs  <- year[ord]
  vals <- ssp_gdp_pc[ord]
  
  log_interp <- approx(
    x    = yrs,
    y    = log(vals),
    xout = model_years,
    rule = 2
  )$y
  
  data.table(
    year       = model_years,
    ssp_interp = exp(log_interp)
  )
}, by = iso3]

# Fix SSP interpolation: hold 2025 value flat for years < 2025 to avoid
# back-projecting income levels that contradict observed GNI data.
ssp_25 <- ssp_annual[year == 2025, .(iso3, ssp_interp_2025 = ssp_interp)]
ssp_annual <- merge(ssp_annual, ssp_25, by = "iso3", all.x = TRUE)
ssp_annual[year < 2025, ssp_interp := ssp_interp_2025]
ssp_annual[, ssp_interp_2025 := NULL]

setorder(ssp_annual, iso3, year)
ssp_annual[, ssp_growth := ssp_interp / shift(ssp_interp) - 1, by = iso3]

n_ssp_countries <- uniqueN(ssp_annual$iso3)
cat("SSP2 annual growth rates available for", n_ssp_countries,
    "of", length(iso3_list), "countries.\n")

# Last observed GNI year and value per country
gni_last <- gni[iso3 %in% iso3_list, {
  idx <- which.max(year)
  .(last_year = year[idx], gni_last = gni_pc_ppp[idx])
}, by = iso3]

# Full iso3-year grid
gni_grid <- CJ(iso3 = iso3_list, year = model_years)
gni_grid <- gni[gni_grid, on = .(iso3, year)]
gni_grid <- ssp_annual[gni_grid, on = .(iso3, year)]
gni_grid <- gni_last[gni_grid, on = .(iso3)]
setorder(gni_grid, iso3, year)

# Project GNI forward recursively after last observed year only
gni_grid[
  ,
  gni_pc_proj := {
    out <- gni_pc_ppp
    
    if (!is.na(last_year[1]) && !is.na(gni_last[1])) {
      future_idx <- which(year > last_year[1])
      
      if (length(future_idx) > 0) {
        for (j in future_idx) {
          if (year[j] == last_year[1] + 1) {
            if (!is.na(ssp_growth[j]))
              out[j] <- gni_last[1] * (1 + ssp_growth[j])
          } else {
            if (!is.na(out[j - 1]) && !is.na(ssp_growth[j]))
              out[j] <- out[j - 1] * (1 + ssp_growth[j])
          }
        }
      }
    }
    out
  },
  by = iso3
]

gni_grid[, gni_pc_ppp_final := fifelse(!is.na(gni_pc_ppp), gni_pc_ppp, gni_pc_proj)]

n_miss_gni <- sum(is.na(gni_grid$gni_pc_ppp_final))
if (n_miss_gni > 0) {
  warning(
    n_miss_gni,
    " (iso3, year) rows still missing GNI after SSP projection. ",
    "These will produce NA VSL/VSLY values."
  )
}

gni_grid <- gni_grid[, .(iso3, year, gni_pc_ppp = gni_pc_ppp_final)]

dt_compare <- gni_grid[dt_compare, on = .(iso3, year)]

# US GNI per year (VSL transfer reference income)
us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp)]
if (nrow(us_gni) == 0) {
  stop("No USA GNI values found. Check iso3='USA' is present in the GNI file.")
}
dt_compare <- us_gni[dt_compare, on = .(year)]

# 7) Life expectancy at average adult age ----
#
# Robinson et al. (2019) reference case: derive a constant VSLY from the
# population-average VSL divided by remaining life expectancy at the average
# age of the adult population. LE is undiscounted (no annuity formula).

# We compute remaining life expectancy at the average age of the adult
# population for each country-year-scenario. This quantity, `le_avg_adult`,
# is used as the denominator for the constant VSLY:
#
#   VSLY = VSL / LE_avg_adult
#
# Steps:
#   1. Compute avg_adult_age = Σ(pop × age) / Σ(pop) for adult ages only.
#   2. Map avg_adult_age to the WPP 5-year lower-bound age group.
#   3. Join to WPP life tables to obtain le_avg_adult.
#
# We also compute `le_avg_adult_disc` as an auxiliary discounted version of
# remaining life expectancy, but it is not used in the active VSLY formula in
# this version of the script.


if (!file.exists(LT_FILE) && !exists("lt_interp")) {
  stop("Life expectancy input not found. Expected file: ", LT_FILE,
       "\nOr provide object `lt_interp` in the environment.")
}

if (!exists("lt_interp")) {
  cat("Loading WPP life expectancy data...\n")
  lt <- as.data.table(read_excel(
    LT_FILE,
    sheet = "Medium variant",
    range = "A17:DH22967"
  ))
  
  setnames(
    lt,
    c("Region, subregion, country or area *", "Notes", "Location code",
      "ISO3 Alpha-code", "ISO2 Alpha-code", "SDMX code**", "Type",
      "Parent code", "Year"),
    c("location", "Notes", "Locationcode", "ISO3", "ISO2", "SDMX",
      "Type", "Parencode", "year"),
    skip_absent = TRUE
  )
  
  lt_interp <- melt(
    lt,
    id.vars      = colnames(lt)[1:11],
    variable.name = "age",
    value.name    = "le"
  )
  lt_interp <- lt_interp[year >= 2025, .(location, age, year, le)]
  lt_interp[, age := as.numeric(str_extract(age, "\\d+"))]
  lt_interp[, le  := as.numeric(le)]
  
  loc_fix_lt <- c(
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Côte d'Ivoire"                   = "Ivory Coast",
    "China, Taiwan Province of China" = "Taiwan (Province of China)",
    "United Republic of Tanzania"     = "Tanzania",
    "Türkiye"                         = "Turkey",
    "United States of America"        = "United States",
    "Dem. People's Republic of Korea" = "Democratic People's Republic of Korea",
    "Micronesia (Fed. States of)"     = "Micronesia (Federated States of)",
    "State of Palestine"              = "Palestine"
  )
  lt_interp[location %in% names(loc_fix_lt), location := loc_fix_lt[location]]
  rm(lt)
}

# Average adult age weighted by scenario-specific population
dt_pop_age <- dt_pop_unique[
  , .(pop = sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario, age)
]

dt_avg_adult <- dt_pop_age[age >= ADULT_MIN_AGE,
                           .(
                             adult_population = sum(pop, na.rm = TRUE),
                             avg_adult_age    = sum(pop * age, na.rm = TRUE) / sum(pop, na.rm = TRUE)
                           ),
                           by = .(location, year, scenario, htn_target_scenario)
]

# Map average adult age to WPP 5-year lower-bound age group
dt_avg_adult[, age_ref_5y := pmin(MAX_MODEL_AGE,
                                  (as.integer(floor(avg_adult_age)) %/% 5L) * 5L)]

dt_le_lookup <- copy(dt_avg_adult)[, .(
  location, year, scenario, htn_target_scenario,
  age = age_ref_5y, adult_population, avg_adult_age
)]

setkey(lt_interp,    location, age, year)
setkey(dt_le_lookup, location, age, year)
dt_le_lookup <- lt_interp[dt_le_lookup, on = .(location, age, year), roll = "nearest"]

n_miss_le <- sum(is.na(dt_le_lookup$le))
if (n_miss_le > 0) {
  warning(n_miss_le, " rows missing remaining life expectancy after lt_interp join.",
          " Check location-name alignment between model output and WPP.")
}

setnames(dt_le_lookup, "age", "age_ref_5y")
setnames(dt_le_lookup, "le",  "le_avg_adult")

dt_compare <- dt_le_lookup[
  dt_compare,
  on = .(location, year, scenario, htn_target_scenario)
]

# Discounted LE at the AVERAGE ADULT AGE (denominator for VSLY).
# Annuity at R_VSLY. See disc_life_years() helper defined in section 1.
dt_compare[, le_avg_adult_disc := disc_life_years(le_avg_adult, R_VSLY)]

# 7b) Age-specific life-years gained at age of death (Robinson et al. 2019) ----

#
# To estimate VSLY-based monetary values, we calculate life-years gained using
# the age distribution of deaths averted and remaining life expectancy at the
# age of death.
#
# Pipeline:
#   1. Aggregate baseline and intervention deaths by location, year, scenario,
#      htn_target_scenario, and age.
#   2. Compute age-specific deaths averted = baseline - intervention.
#   3. Merge age-specific remaining life expectancy from WPP life tables.
#   4. Aggregate to country-year-scenario:
#         life_years_gained_undisc = Σ_a deaths_averted(a) × LE(a)
#         life_years_gained_disc   = Σ_a deaths_averted(a) × LE_disc(a)
#
# In the active implementation of this script, VSLY monetary values use
# `life_years_gained_undisc`. The discounted version is retained as an
# auxiliary diagnostic quantity.

# Age-specific deaths (all scenarios)
dt_deaths_age <- dt_model[
  , .(deaths = sum(dead, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario, age)
]

# Baseline deaths by age
dt_baseline_age <- dt_deaths_age[
  scenario == "baseline",
  .(location, year, age, htn_target_scenario, deaths_baseline = deaths)
]

# Intervention scenarios only; compute age-specific deaths averted
dt_deaths_age <- dt_deaths_age[scenario != "baseline"]
dt_deaths_age <- dt_baseline_age[
  dt_deaths_age,
  on = .(location, year, age, htn_target_scenario)
]
setnames(dt_deaths_age, "deaths", "deaths_intervention_age")
dt_deaths_age[, deaths_averted_age := deaths_baseline - deaths_intervention_age]

# Map single-year age to WPP 5-year lower-bound age group (same scheme as
# section 7 for internal consistency).
dt_deaths_age[, age_ref_5y := pmin(MAX_MODEL_AGE,
                                   (as.integer(floor(age)) %/% 5L) * 5L)]

# Age-specific LE from WPP lt_interp (rolling nearest-year join).
# Use a renamed copy so the `age` column in lt_interp can match age_ref_5y
# without colliding with the single-year `age` already in dt_deaths_age.
lt_age <- lt_interp[, .(location, year, age_ref_5y = age, le_age = le)]
setkey(lt_age,        location, age_ref_5y, year)
setkey(dt_deaths_age, location, age_ref_5y, year)
dt_deaths_age <- lt_age[
  dt_deaths_age,
  on = .(location, age_ref_5y, year),
  roll = "nearest"
]

n_miss_le_age <- sum(is.na(dt_deaths_age$le_age))
if (n_miss_le_age > 0) {
  warning(n_miss_le_age,
          " age-specific rows missing remaining life expectancy after ",
          "lt_interp join. Check location-name alignment.")
}

# Discounted LE at age of death (annuity at R_VSLY)
dt_deaths_age[, le_age_disc := disc_life_years(le_age, R_VSLY)]

# Aggregate life-years gained to country-year-scenario grain
dt_ly_gained <- dt_deaths_age[
  , .(
    life_years_gained_undisc = sum(deaths_averted_age * le_age,      na.rm = TRUE),
    life_years_gained_disc   = sum(deaths_averted_age * le_age_disc, na.rm = TRUE),
    # Population-weighted average age-of-death (informational only)
    avg_age_of_death_averted = sum(age * deaths_averted_age, na.rm = TRUE) /
      sum(deaths_averted_age,        na.rm = TRUE)
  ),
  by = .(location, year, scenario, htn_target_scenario)
]

dt_compare <- dt_ly_gained[
  dt_compare,
  on = .(location, year, scenario, htn_target_scenario)
]

# 8) VSL transfer ─----
# VSL_country = US_VSL_RATIO × GNI_pc_USA × (GNI_pc_country / GNI_pc_USA)^elasticity
# Floor: VSL >= VSL_RATIO_FLOOR × GNI_pc_country

dt_compare[, vsl_e1_0 := US_VSL_RATIO * gni_pc_usa *
             (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LOW]

dt_compare[, vsl_e1_5 := US_VSL_RATIO * gni_pc_usa *
             (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIGH]

dt_compare[, vsl_e1_2 := fifelse(
  gni_pc_ppp >= gni_pc_usa,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIC,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LMIC
)]

dt_compare[, vsl_e1_0 := pmax(vsl_e1_0, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_2 := pmax(vsl_e1_2, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_5 := pmax(vsl_e1_5, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]

# 9) VSLY (Robinson et al. 2019 reference case) ----
#
# (constant VSLY approach)
#
# In this implementation, constant VSLY is derived from VSL using
# UNDISCOUNTED remaining life expectancy at the average adult age:
#
#   VSLY = VSL / LE_avg_adult
#
# VSLY monetary values are then calculated using UNDISCOUNTED age-specific
# life-years gained:
#
#   VSLY_value = VSLY × life_years_gained_undisc
#              = VSLY × Σ_a [ deaths_averted(a) × LE(a) ]
#
# This means the denominator is based on remaining life expectancy at the
# average adult age, while the numerator is based on remaining life expectancy
# at the ages of deaths averted. As a result, VSLY totals differ from VSL
# totals whenever the age pattern of deaths averted differs from the average
# adult age profile.

dt_compare[le_avg_adult_disc > 0, `:=`(
  vsly_e1_0 = vsl_e1_0 / le_avg_adult,
  vsly_e1_2 = vsl_e1_2 / le_avg_adult,
  vsly_e1_5 = vsl_e1_5 / le_avg_adult
)]


# Monetary values of deaths averted — VSL (unchanged)
dt_compare[, `:=`(
  economic_value_e1_0 = vsl_e1_0 * deaths_averted,
  economic_value_e1_2 = vsl_e1_2 * deaths_averted,
  economic_value_e1_5 = vsl_e1_5 * deaths_averted
)]

# Monetary values of deaths averted — VSLY.
# life_years_gained_disc is ALREADY within-lifetime-discounted at R_VSLY,
# so do NOT apply calendar-year disc_r3 yet — that happens in section 10.
# dt_compare[, `:=`(
#   vsly_value_e1_0 = vsly_e1_0 * life_years_gained_disc,
#   vsly_value_e1_2 = vsly_e1_2 * life_years_gained_disc,
#   vsly_value_e1_5 = vsly_e1_5 * life_years_gained_disc
# )]

dt_compare[, `:=`(
  vsly_value_e1_0 = vsly_e1_0 * life_years_gained_undisc,
  vsly_value_e1_2 = vsly_e1_2 * life_years_gained_undisc,
  vsly_value_e1_5 = vsly_e1_5 * life_years_gained_undisc
)]

# 10) Time-series (calendar-year) discount factors ----

#
# These bring annual streams of GNI and monetary benefits from calendar year
# `year` to BASE_YEAR present value:
#
#   disc_rX = 1 / (1 + rX)^(year - BASE_YEAR)
#
# These discount factors are applied to VSL-based values, VSLY-based values,
# and GNI-based denominators. Although discounted life-expectancy quantities
# are also computed earlier in the script, the active VSLY implementation in
# this version uses undiscounted life-years gained and then applies only the
# calendar-year discounting at this stage.
#
# Applied to GNI (for income-share denominators) and to economic values.

dt_compare[, `:=`(
  disc_r1 = 1 / (1 + DISC_RATES["r1"])^(year - BASE_YEAR),
  disc_r3 = 1 / (1 + DISC_RATES["r3"])^(year - BASE_YEAR),
  disc_r5 = 1 / (1 + DISC_RATES["r5"])^(year - BASE_YEAR)
)]

dt_compare[, `:=`(
  gni_pc_disc_r3              = gni_pc_ppp            * disc_r3,
  # PRIMARY estimate columns (e1_2: Robinson et al. 2019 reference-case elasticity)
  economic_value_e1_2_disc_r3 = economic_value_e1_2   * disc_r3,
  vsly_value_e1_2_disc_r3     = vsly_value_e1_2       * disc_r3,
  # SENSITIVITY HIGH columns (e1_5: uniform elasticity, retained for reporting)
  economic_value_e1_5_disc_r3 = economic_value_e1_5   * disc_r3,
  vsly_value_e1_5_disc_r3     = vsly_value_e1_5       * disc_r3
)]

# 11) Final dataset ----
dt_final <- dt_compare[, .(
  location,
  iso3,
  year,
  scenario,
  htn_target_scenario,
  who_region,
  deaths_baseline,
  deaths_intervention,
  deaths_averted,
  avg_age_of_death_averted,
  population,
  adult_population,
  avg_adult_age,
  age_ref_5y,
  le_avg_adult,
  le_avg_adult_disc,
  life_years_gained_undisc,
  life_years_gained_disc,
  gni_pc_ppp,
  gni_pc_usa,
  vsl_e1_0,
  vsl_e1_2,
  vsl_e1_5,
  vsly_e1_0,
  vsly_e1_2,
  vsly_e1_5,
  economic_value_e1_0,
  economic_value_e1_2,
  economic_value_e1_5,
  vsly_value_e1_0,
  vsly_value_e1_2,
  vsly_value_e1_5,
  disc_r1,
  disc_r3,
  disc_r5,
  gni_pc_disc_r3,
  # Primary discounted values (e1_2 — Robinson et al. 2019 reference case)
  economic_value_e1_2_disc_r3,
  vsly_value_e1_2_disc_r3,
  # Sensitivity discounted values (e1_5 — high sensitivity bound)
  economic_value_e1_5_disc_r3,
  vsly_value_e1_5_disc_r3
)]

setorder(dt_final, location, year, scenario)

# Update WHO region
# ISO3 codes and WHO region ──────────────────────────────────────────────
country_grp <- fread(file.path(wd, "data", "raw",
                               "who-regions.csv"))

setnames(
  country_grp,
  old = c("Entity", "Code", "World regions according to WHO"),
  new = c("location", "iso3", "region_who")
)

country_grp[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
country_grp[, Year := NULL]

# main merge
dt_final <- country_grp[dt_final, on = .(location)]

# fill missing iso3 using countrycode
missing_iso3 <- is.na(dt_final$iso3)
if (any(missing_iso3)) {
  dt_final[missing_iso3,
           iso3 := countrycode(
             location,
             origin = "country.name",
             destination = "iso3c",
             warn = FALSE
           )
  ]
}

# manual fixes
fix_country_grp <- data.table(
  location = c(
    "Brunei Darussalam",
    "Cabo Verde",
    "Democratic People's Republic of Korea",
    "Democratic Republic of the Congo",
    "Iran (Islamic Republic of)",
    "Ivory Coast",
    "Lao People's Democratic Republic",
    "Micronesia (Federated States of)",
    "Palestine",
    "Republic of Korea",
    "Republic of Moldova",
    "Russian Federation",
    "Saint Vincent and the Grenadines",
    "Syrian Arab Republic",
    "Taiwan (Province of China)",
    "Timor-Leste",
    "Venezuela (Bolivarian Republic of)",
    "Viet Nam"
  ),
  iso3 = c(
    "BRN","CPV","PRK","COD","IRN","CIV","LAO","FSM","PSE",
    "KOR","MDA","RUS","VCT","SYR","TWN","TLS","VEN","VNM"
  ),
  who_region = c(
    "WPR","AFR","SEAR","AFR","EMR","AFR","WPR","WPR","EMR",
    "WPR","EUR","EUR","AMR","EMR","WPR","SEAR","AMR","WPR"
  )
)

# patch missing values from manual table
dt_final[fix_country_grp, on = .(location),
         `:=`(
           iso3       = fcoalesce(iso3, i.iso3),
           region_who = fcoalesce(region_who, i.who_region)
         )
]

dt_final[, region_who := fcase(
  region_who == "AFR",  "Africa",
  region_who == "EMR",  "Eastern Mediterranean",
  region_who == "EUR",  "Europe",
  region_who == "AMR",  "Americas",
  region_who == "SEAR", "South-East Asia",
  region_who == "WPR",  "Western Pacific",
  default = region_who
)]

## Keep only encoded region who
dt_final[, who_region := region_who]

dt_final[, region_who := NULL]
# 12) Save main results ----
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
saveRDS(dt_final, OUT_FILE)
fwrite(dt_final, OUT_CSV)

cat("Saved:", OUT_FILE, "\n")
cat("Saved:", OUT_CSV, "\n")
cat("Rows:", nrow(dt_final), "| Columns:", ncol(dt_final), "\n")
cat("Scenarios:", paste(unique(dt_final$scenario), collapse = ", "), "\n")
cat("Years:", min(dt_final$year), "–", max(dt_final$year), "\n")
cat("Countries:", length(unique(dt_final$location)), "\n")

# --- Sanity check: VSLY vs VSL totals (primary, discounted, 2026–2050) -------
# Diagnostic check:
# Under this implementation, VSLY totals should differ from VSL totals because
# VSLY is based on age-specific life-years gained, not simply deaths averted.
# A ratio of exactly 1.00 would suggest that the same life expectancy measure
# is being used in both the VSLY denominator and numerator, recreating the
# algebraic identity VSLY × LY = VSL × deaths.

diag_vsl  <- sum(dt_final$economic_value_e1_2_disc_r3, na.rm = TRUE)
diag_vsly <- sum(dt_final$vsly_value_e1_2_disc_r3,     na.rm = TRUE)
cat(sprintf(
  "Sanity check (primary, e1_2, r=3%%, cumulative): VSL = %.3e | VSLY = %.3e | VSLY/VSL = %.3f\n",
  diag_vsl, diag_vsly, diag_vsly / diag_vsl
))
if (abs(diag_vsly / diag_vsl - 1) < 1e-4) {
  warning("VSLY/VSL ratio is ~1.000 — check that age-specific LE is being used in ",
          "life_years_gained_disc and that disc_life_years() is applied correctly.")
}

# 13) Summary reporting tables -----
#
# Rows    : intervention scenario × WHO region  (+ "World" totals)
# Columns : 2026 | 2030 | 2040 | 2050 | cumulative 2026–2050
# Values  : discounted at r = 3%, presented as absolute (USD PPP) and as share
#           of discounted GNI (for cross-country comparability).
#
# PRIMARY metric: e1_2 (differential elasticity — Robinson et al. 2019
#   reference case). This is the headline figure for publication.
# SENSITIVITY metric: e1_5 (uniform elasticity = 1.5 — high bound).
#   Included as a separate set of summary tables for sensitivity reporting.
#
# Outputs:
#   1) VSL summary  (primary):   economic_value_e1_2_disc_r3
#   2) VSLY summary (primary):   vsly_value_e1_2_disc_r3
#   3) VSL summary  (sensitivity e1_5): economic_value_e1_5_disc_r3
#   4) VSLY summary (sensitivity e1_5): vsly_value_e1_5_disc_r3
#   5) Appended: all four stacked with valuation_type and elasticity_case cols

make_summary_table <- function(dt, value_col, value_prefix) {
  
  # Verify column exists before proceeding
  if (!value_col %in% names(dt)) {
    stop("Column '", value_col, "' not found in dt_final. ",
         "Check that the discount step created it correctly.")
  }
  if (!"gni_pc_disc_r3" %in% names(dt)) {
    stop("Column 'gni_pc_disc_r3' not found in dt_final. ",
         "Check that the discount step created it correctly.")
  }
  if (!"who_region" %in% names(dt)) {
    stop("Column 'who_region' not found in dt_final.")
  }
  
  # Single-year slice: region level
  make_slice_region <- function(yr) {
    sub <- dt[year == yr, .(
      metric_value = sum(get(value_col), na.rm = TRUE),
      total_income = sum(population * gni_pc_disc_r3, na.rm = TRUE)
    ), by = .(who_region, scenario)]
    sub[, share_income := metric_value / total_income]
    sub[, total_income := NULL]
    setnames(sub,
             c("metric_value", "share_income"),
             c(paste0(value_prefix, "_", yr), paste0("share_", yr))
    )
    sub
  }
  
  # Single-year slice: world total
  make_slice_world <- function(yr) {
    sub <- dt[year == yr, .(
      metric_value = sum(get(value_col), na.rm = TRUE),
      total_income = sum(population * gni_pc_disc_r3, na.rm = TRUE)
    ), by = .(scenario)]
    sub[, `:=`(who_region = "World", share_income = metric_value / total_income)]
    sub[, total_income := NULL]
    setnames(sub,
             c("metric_value", "share_income"),
             c(paste0(value_prefix, "_", yr), paste0("share_", yr))
    )
    sub
  }
  
  # Merge multiple year-slices side by side
  merge_slices <- function(lst) {
    Reduce(
      function(a, b) merge(a, b, by = c("who_region", "scenario"), all = TRUE),
      lst
    )
  }
  
  # Cumulative totals over BASE_YEAR:2050 — region level
  make_total_region <- function() {
    sub <- dt[year >= BASE_YEAR & year <= 2050, .(
      metric_total = sum(get(value_col), na.rm = TRUE),
      income_total = sum(population * gni_pc_disc_r3, na.rm = TRUE)
    ), by = .(who_region, scenario)]
    sub[, share_total := metric_total / income_total]
    sub[, income_total := NULL]
    setnames(sub, "metric_total", paste0(value_prefix, "_total"))
    sub
  }
  
  # Cumulative totals over BASE_YEAR:2050 — world total
  make_total_world <- function() {
    sub <- dt[year >= BASE_YEAR & year <= 2050, .(
      metric_total = sum(get(value_col), na.rm = TRUE),
      income_total = sum(population * gni_pc_disc_r3, na.rm = TRUE)
    ), by = .(scenario)]
    sub[, `:=`(who_region = "World", share_total = metric_total / income_total)]
    sub[, income_total := NULL]
    setnames(sub, "metric_total", paste0(value_prefix, "_total"))
    sub
  }
  
  dt_reg_ann   <- merge_slices(lapply(SUMMARY_YEARS, make_slice_region))
  dt_world_ann <- merge_slices(lapply(SUMMARY_YEARS, make_slice_world))
  dt_reg_tot   <- make_total_region()
  dt_world_tot <- make_total_world()
  
  dt_summary_region <- merge(dt_reg_ann, dt_reg_tot,
                             by = c("who_region", "scenario"), all = TRUE)
  dt_summary_world  <- merge(dt_world_ann, dt_world_tot,
                             by = c("who_region", "scenario"), all = TRUE)
  
  dt_summary <- rbind(dt_summary_region, dt_summary_world, fill = TRUE)
  setorder(dt_summary, scenario, who_region)
  dt_summary
}

# PRIMARY summary tables (e1_2 — Robinson et al. 2019 reference-case elasticity)
dt_summary_vsl_e1_2 <- make_summary_table(
  dt           = dt_final,
  value_col    = "economic_value_e1_2_disc_r3",
  value_prefix = "vsl"
)

dt_summary_vsly_e1_2 <- make_summary_table(
  dt           = dt_final,
  value_col    = "vsly_value_e1_2_disc_r3",
  value_prefix = "vsly"
)

# SENSITIVITY summary tables (e1_5 — uniform elasticity high bound)
dt_summary_vsl_e1_5 <- make_summary_table(
  dt           = dt_final,
  value_col    = "economic_value_e1_5_disc_r3",
  value_prefix = "vsl"
)

dt_summary_vsly_e1_5 <- make_summary_table(
  dt           = dt_final,
  value_col    = "vsly_value_e1_5_disc_r3",
  value_prefix = "vsly"
)

# 14) Standardise column names and stack ----
# Rename value columns to generic "metric_*" / "share_*" so all four tables
# share the same schema and can be rbind-ed cleanly.
# An `elasticity_case` column records the source elasticity for each block.

rename_to_metric <- function(dt, old_prefix) {
  old_val <- grep(paste0("^", old_prefix, "_"), names(dt), value = TRUE)
  new_val <- sub(paste0("^", old_prefix, "_"), "metric_", old_val)
  setnames(dt, old_val, new_val)
  dt
}

dt_summary_vsl_e1_2  <- rename_to_metric(dt_summary_vsl_e1_2,  "vsl")
dt_summary_vsly_e1_2 <- rename_to_metric(dt_summary_vsly_e1_2, "vsly")
dt_summary_vsl_e1_5  <- rename_to_metric(dt_summary_vsl_e1_5,  "vsl")
dt_summary_vsly_e1_5 <- rename_to_metric(dt_summary_vsly_e1_5, "vsly")

dt_summary_appended <- rbindlist(
  list(
    copy(dt_summary_vsl_e1_2) [, `:=`(valuation_type = "VSL",  elasticity_case = "e1_2_primary")],
    copy(dt_summary_vsly_e1_2)[, `:=`(valuation_type = "VSLY", elasticity_case = "e1_2_primary")],
    copy(dt_summary_vsl_e1_5) [, `:=`(valuation_type = "VSL",  elasticity_case = "e1_5_sensitivity")],
    copy(dt_summary_vsly_e1_5)[, `:=`(valuation_type = "VSLY", elasticity_case = "e1_5_sensitivity")]
  ),
  fill = TRUE, use.names = TRUE
)

setcolorder(dt_summary_appended,
            c("valuation_type", "elasticity_case", "who_region", "scenario",
              setdiff(names(dt_summary_appended),
                      c("valuation_type", "elasticity_case", "who_region", "scenario")))
)
setorder(dt_summary_appended, valuation_type, elasticity_case, scenario, who_region)

# 15) Save summary tables ----
saveRDS(dt_summary_vsl_e1_2,  OUT_SUMM_VSL)
saveRDS(dt_summary_vsly_e1_2, OUT_SUMM_VSLY)
saveRDS(dt_summary_appended,  OUT_SUMM_APP)

# Explicit primary (e1_2) copies for the Aim 1 slide-deck pipeline
saveRDS(dt_summary_vsl_e1_2,  OUT_SUMM_VSL_PRIM)
saveRDS(dt_summary_vsly_e1_2, OUT_SUMM_VSLY_PRIM)

# Primary tables (e1_2)
fwrite(dt_summary_vsl_e1_2,  file.path(DIR_OUT, "08_vsl_summary_table_e1_2_primary.csv"))
fwrite(dt_summary_vsly_e1_2, file.path(DIR_OUT, "08_vsly_summary_table_e1_2_primary.csv"))

# Sensitivity tables (e1_5)
fwrite(dt_summary_vsl_e1_5,  file.path(DIR_OUT, "08_vsl_summary_table_e1_5_sensitivity.csv"))
fwrite(dt_summary_vsly_e1_5, file.path(DIR_OUT, "08_vsly_summary_table_e1_5_sensitivity.csv"))

# Full appended table (all four blocks)
fwrite(dt_summary_appended, file.path(DIR_OUT, "08_vsl_vsly_summary_table_appended.csv"))

cat("Saved primary VSL summary (e1_2): ",
    "08_vsl_summary_table_e1_2_primary.csv\n")
cat("Saved primary VSLY summary (e1_2):",
    "08_vsly_summary_table_e1_2_primary.csv\n")
cat("Saved sensitivity VSL summary (e1_5): ",
    "08_vsl_summary_table_e1_5_sensitivity.csv\n")
cat("Saved sensitivity VSLY summary (e1_5):",
    "08_vsly_summary_table_e1_5_sensitivity.csv\n")
cat("Saved appended summary table:     ",
    "08_vsl_vsly_summary_table_appended.csv\n")
cat("Rows primary VSL:", nrow(dt_summary_vsl_e1_2),
    "| Rows primary VSLY:", nrow(dt_summary_vsly_e1_2),
    "| Rows appended:", nrow(dt_summary_appended), "\n")
cat("Regions:", paste(sort(unique(dt_summary_vsly_e1_2$who_region)), collapse = ", "), "\n")


# ── Note ─────────────────────────────────────────────────────────────────────
# The dot plot of economic value as a share of regional income is no longer
# produced here. It is built downstream in
#   scenarios/scenarios_aim1/aim1_report.Rmd
# from `08_vsl_vsly_summary_table_appended.rds`, and saved as a slide
# artefact for `aim1_executive_slides.Rmd`. This keeps the executive-deck
# pipeline (scalars, tables, plots) in a single place.

