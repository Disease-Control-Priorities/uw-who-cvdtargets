
# 08_economic_value_calculation.R
# VSLY analysis: monetary value of deaths averted under each intervention vs baseline.
#
# Method: income-adjusted Value of Statistical Life (VSL) following
#   Robinson & Hammitt (2011) and Bolongaita et al. (2024)
#   https://www.nature.com/articles/s41591-024-03248-4
#   Converted to VSLY using population-weighted remaining life expectancy (WPP 2024).
#
# Inputs:
#   output/out_model/model_output_<country>_<target_col>.rds
#   data/raw/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv   (World Bank GNI per capita PPP)
#   data/raw/1721734326790-ssp_basic_drivers_release_3.1_full.xlsx (IIASA SSP 3.1)
#   data/raw/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx
#   data/processed/Country_groupings_extended.csv
#
# Outputs:
#   output/08_vsly_results.rds / .csv          — one row per country × year × scenario
#   output/08_vsly_summary_table.rds / .csv    — region × intervention summary

# ── 0) Paths ──────────────────────────────────────────────────────────────────

DIR_MODEL    <- file.path(wd, "output", "out_model")
DIR_OUT      <- file.path(wd, "output")
GNI_FILE     <- file.path(wd, "data", "raw",
                           "API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv")
SSP_FILE     <- file.path(wd, "data", "raw",
                           "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx")
COUNTRY_FILE <- file.path(wd, "data", "processed",
                           "Country_groupings_extended.csv")
OUT_FILE     <- file.path(DIR_OUT, "08_vsly_results.rds")
OUT_SUMM     <- file.path(DIR_OUT, "08_vsly_summary_table.rds")

# ── 1) Parameters ──────────────────────────────────────────────────────────────
#
# VSL reference (Robinson & Hammitt 2011):
#   VSL_USA = US_VSL_RATIO × GNI_pc_USA
#
# Income elasticity cases:
#   e1_2  — differential baseline: 0.8 for countries at/above USA income,
#            1.2 for countries below USA income
#   e1_0  — sensitivity: uniform 1.0
#   e1_5  — sensitivity: uniform 1.5  ← used in summary table per task specification
#
# Minimum VSL floor: VSL / GNI_pc >= VSL_RATIO_FLOOR (prevents implausibly low values)
#
# Discounting:
#   discount_factor = 1 / (1 + r)^(year − BASE_YEAR)
#   BASE_YEAR = 2026 (first full intervention year)
#   VSLY annuity factor (discounts remaining life-years within a single death event):
#     a(LE, r) = (1 − (1+r)^(−LE)) / r
#     When r → 0: a(LE, 0) = LE  (matches undiscounted VSLY = VSL / LE)

US_VSL_RATIO    <- 160    # VSL / GNI_pc reference ratio for the USA
VSL_ELAST_HIC   <- 0.8   # income elasticity for countries at or above USA income
VSL_ELAST_LMIC  <- 1.2   # income elasticity for countries below USA income
VSL_ELAST_LOW   <- 1.0   # sensitivity low: uniform elasticity
VSL_ELAST_HIGH  <- 1.5   # sensitivity high: uniform elasticity (used in summary)
VSL_RATIO_FLOOR <- 20    # minimum VSL-to-GNI_pc ratio

DISC_RATES <- c(r1 = 0.01, r3 = 0.03, r5 = 0.05)  # annual discount rates
BASE_YEAR  <- 2026L                                   # first intervention year

# ── 1b) Life expectancy data (lt_interp) ──────────────────────────────────────
#
# lt_interp contains remaining life expectancy (LE) by location, age, and year
# from WPP 2024 (Medium variant).  If 07_output_dalys.R ran first, the object
# is already in the environment; otherwise it is recomputed here.
#
# lt_interp structure after processing:
#   location (character), age (numeric, WPP 5-year lower bound: 0,1,5,10,...,100),
#   year (integer, 5-year intervals: 2025, 2030, ...), le (numeric, remaining years)

if (!exists("lt_interp")) {
  lt <- as.data.table(read_excel(
    paste0(wd_raw, "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"),
    sheet = "Medium variant", range = "A17:DH22967"))

  setnames(lt,
    c("Region, subregion, country or area *", "Notes", "Location code",
      "ISO3 Alpha-code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code", "Year"),
    c("location", "Notes", "Locationcode", "ISO3", "ISO2", "SDMX", "Type", "Parencode", "year"))

  lt_interp <- melt(lt, id.vars = colnames(lt)[1:11],
                    value.name = "le", variable.name = "age")
  lt_interp <- lt_interp[year >= 2025, c("location", "age", "year", "le"), with = FALSE]
  lt_interp[, age := as.numeric(as.character(str_extract_all(age, "\\d+")))]
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

# ── 2) Load model outputs ──────────────────────────────────────────────────────
#
# 06_run_scenarios_multiple.R saves one file per (country, htn_target_scenario).
# Columns: scenario, age, cause, sex, year, well, sick, newcases, dead, pop,
#          all.mx, intervention, location, eff_ir, eff_cf, htn_target_scenario

model_files <- list.files(DIR_MODEL, pattern = "^model_output_.*\\.rds$",
                          full.names = TRUE)
if (length(model_files) == 0)
  stop("No model output files found in: ", DIR_MODEL,
       "\nRun 06_run_scenarios_multiple.R first.")

cat("Loading", length(model_files), "model output files...\n")
dt_model <- rbindlist(lapply(model_files, readRDS), fill = TRUE)
cat("Rows loaded:", nrow(dt_model), "\n")

# ── 3) Deaths and scenario-specific population ─────────────────────────────────
#
# Intervention scenarios change total population counts over the model horizon,
# so we use the scenario-specific population rather than the baseline only.
#
# `pop` in dt_model is replicated across causes for the same (location, year, age, sex).
# We deduplicate on (location, year, scenario, htn_target_scenario, age, sex)
# before summing to avoid over-counting.

dt_deaths <- dt_model[,
  .(deaths = sum(dead, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

dt_pop_unique <- unique(
  dt_model[, .(location, year, scenario, htn_target_scenario, age, sex, pop)]
)

dt_pop_total <- dt_pop_unique[,
  .(population = sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

dt_deaths <- dt_pop_total[dt_deaths, on = .(location, year, scenario, htn_target_scenario)]

# ── 3b) Population-weighted remaining life expectancy ──────────────────────────
#
# For each (location, year, scenario, htn_target_scenario):
#   LE_pw = Σ_a [ pop_a × le_a ] / Σ_a [ pop_a ]
#
# where pop_a sums over sex first, and le_a comes from lt_interp.
#
# The model uses single-year ages (20–95); lt_interp provides LE at the lower
# bound of WPP 5-year groups (20, 25, 30, ..., 95).  We map each single-year
# model age to its 5-year group lower bound: age_lt5 = floor(age / 5) × 5.
#
# lt_interp covers 5-year calendar intervals (2025, 2030, ...).  We use a
# rolling nearest-year join to assign LE to each annual model year.

# Aggregate pop over sex, then bin to 5-year WPP age groups
dt_pop_age <- dt_pop_unique[,
  .(pop = sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario, age)
]
dt_pop_age[, age_lt5 := (as.integer(age) %/% 5L) * 5L]

dt_pop_lt5 <- dt_pop_age[,
  .(pop = sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario, age_lt5)
]
setnames(dt_pop_lt5, "age_lt5", "age")

# Rolling nearest-year join: for each (location, age, model_year), find the
# closest lt_interp year.  This handles annual model years vs 5-yr WPP intervals.
setkey(lt_interp, location, age, year)
setkey(dt_pop_lt5, location, age, year)
dt_pop_lt5 <- lt_interp[dt_pop_lt5, on = .(location, age, year), roll = "nearest"]

n_miss_le <- sum(is.na(dt_pop_lt5$le))
if (n_miss_le > 0)
  warning(n_miss_le, " (location, age, year) rows missing LE after lt_interp join.",
          " Check that location names align between model output and lt_interp.")

# Population-weighted LE aggregated to country × year × scenario
dt_le_pw <- dt_pop_lt5[!is.na(le),
  .(le_pop_weighted = sum(pop * le, na.rm = TRUE) / sum(pop, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

# ── 4) Deaths averted vs baseline ─────────────────────────────────────────────
#
# deaths_averted = deaths_baseline − deaths_intervention
# Positive value = deaths prevented by the intervention relative to no-intervention.

dt_baseline <- dt_deaths[
  scenario == "baseline",
  .(location, year, htn_target_scenario, deaths_baseline = deaths)
]

dt_compare <- dt_deaths[scenario != "baseline"]
dt_compare  <- dt_baseline[dt_compare, on = .(location, year, htn_target_scenario)]
setnames(dt_compare, "deaths", "deaths_intervention")
dt_compare[, deaths_averted := deaths_baseline - deaths_intervention]

# Merge population-weighted LE into comparison table
dt_compare <- dt_le_pw[dt_compare, on = .(location, year, scenario, htn_target_scenario)]

n_miss_lepw <- sum(is.na(dt_compare$le_pop_weighted))
if (n_miss_lepw > 0)
  warning(n_miss_lepw, " rows missing population-weighted LE in dt_compare.")

# ── 5) ISO3 codes and WHO region ──────────────────────────────────────────────

country_grp <- fread(COUNTRY_FILE)

dt_compare <- country_grp[dt_compare, on = .(location), nomatch = NA]

missing_iso3 <- is.na(dt_compare$iso3)
if (any(missing_iso3)) {
  dt_compare[missing_iso3,
    iso3 := countrycode(location, origin = "country.name",
                        destination = "iso3c", warn = FALSE)
  ]
}

setnames(dt_compare, "region", "who_region")

# ── 6) GNI per capita with SSP forward projection ─────────────────────────────
#
# Observed values: World Bank GNI per capita, PPP (constant 2017 international USD).
# The World Bank file typically ends 1–3 years before the present.  For model years
# beyond the last observed year we extend using SSP2 GDP|PPP per capita growth rates
# from IIASA SSP Release 3.1 (Scenario "SSP2", Variable "GDP|PPP [per capita]").
#
# Projection formula for year y > y0  (y0 = last observed GNI year per country):
#   gni(y) = gni(y0) × [ ssp_interp(y) / ssp_interp(y0) ]
#
# ssp_interp(y) is derived by log-linear interpolation between the 5-year SSP
# anchor points.  Using the ratio ssp(y)/ssp(y0) preserves the observed GNI level
# while applying SSP-implied annual growth rates.  The ratio is unit-free, so the
# currency difference between World Bank (2017 PPP USD) and SSP (2005 PPP USD)
# does not affect the projection — only the growth rates carry over.
#
# Observed GNI values are kept unchanged where available.

gni_raw  <- fread(GNI_FILE, skip = 4, header = TRUE)
year_cols <- grep("^[0-9]{4}$", names(gni_raw), value = TRUE)

gni <- melt(gni_raw,
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
  ssp_gdp <- as.data.table(
    read_excel(SSP_FILE, sheet = "data"))
}

ssp_pc      <- ssp_gdp[Scenario == "SSP2" & Variable == "GDP|PPP [per capita]"]
ssp_yr_cols <- grep("^[0-9]{4}$", names(ssp_pc), value = TRUE)

ssp_pc_long <- melt(ssp_pc,
  id.vars      = "Region",
  measure.vars = ssp_yr_cols,
  variable.name = "ssp_year",
  value.name    = "ssp_gdp_pc"
)
setnames(ssp_pc_long, "Region", "iso3")
ssp_pc_long[, ssp_year := as.integer(as.character(ssp_year))]
ssp_pc_long <- ssp_pc_long[!is.na(ssp_gdp_pc) & ssp_gdp_pc > 0]

if (nrow(ssp_pc_long) == 0)
  stop("SSP data filtered to 0 rows. Check that Scenario='SSP2' and ",
       "Variable='GDP|PPP [per capita]' exist in the SSP file.")

model_years <- sort(unique(dt_compare$year))
iso3_list   <- unique(dt_compare[!is.na(iso3), iso3])

# Interpolate SSP to annual: log-linear between 5-year anchor points.
# rule=2 in approx() holds constant beyond the SSP range (flat extrapolation).
ssp_annual <- ssp_pc_long[iso3 %in% iso3_list, {
  ord       <- order(ssp_year)
  ssp_yrs_s <- ssp_year[ord]
  ssp_val_s <- ssp_gdp_pc[ord]
  log_i     <- approx(ssp_yrs_s, log(ssp_val_s), xout = model_years, rule = 2)$y
  data.table(year = model_years, ssp_interp = exp(log_i))
}, by = iso3]

n_ssp_countries <- uniqueN(ssp_annual$iso3)
cat("SSP2 growth rates available for", n_ssp_countries,
    "of", length(iso3_list), "countries.\n")

# Last observed GNI year and value per country
gni_last <- gni[iso3 %in% iso3_list, {
  idx <- which.max(year)
  .(last_year = year[idx], gni_last = gni_pc_ppp[idx])
}, by = iso3]

# SSP value at each country's last observed year (denominator for growth ratio)
ssp_at_last <- ssp_annual[gni_last, on = .(iso3, year = last_year)]
setnames(ssp_at_last, c("year", "ssp_interp"), c("last_year", "ssp_at_last"))

# Build full (iso3 × model_year) grid; start with exact observed GNI
gni_grid <- CJ(iso3 = iso3_list, year = model_years)
gni_grid <- gni[year %in% model_years][gni_grid, on = .(iso3, year)]  # exact match

# Merge SSP interpolated values and last-obs reference
gni_grid <- ssp_annual[gni_grid,     on = .(iso3, year)]
gni_grid <- ssp_at_last[gni_grid,    on = .(iso3)]

# For years where observed GNI is missing: project forward using SSP ratio
# gni(y) = gni(y0) × ssp_interp(y) / ssp_interp(y0)
gni_grid[is.na(gni_pc_ppp) & !is.na(ssp_at_last) & ssp_at_last > 0,
  gni_pc_ppp := gni_last * (ssp_interp / ssp_at_last)
]

n_miss_gni <- sum(is.na(gni_grid$gni_pc_ppp))
if (n_miss_gni > 0)
  warning(n_miss_gni, " (iso3, year) rows still missing GNI after SSP projection. ",
          "These will produce NA VSL/VSLY values.")

gni_grid <- gni_grid[, .(iso3, year, gni_pc_ppp)]

dt_compare <- gni_grid[dt_compare, on = .(iso3, year)]

# US GNI per year (VSL transfer reference income)
us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp)]
if (nrow(us_gni) == 0)
  stop("No USA GNI values found. Check iso3='USA' is present in the GNI file.")
dt_compare <- us_gni[dt_compare, on = .(year)]

# ── 7) VSL calculation ─────────────────────────────────────────────────────────
#
# Income-transfer method:
#   VSL_country = US_VSL_RATIO × GNI_pc_USA × (GNI_pc_country / GNI_pc_USA)^elasticity
#
# Simplified (factoring out GNI_pc_USA):
#   VSL_country = US_VSL_RATIO × GNI_pc_country × (GNI_pc_country / GNI_pc_USA)^(e−1)
#
# Floor: VSL_country ≥ VSL_RATIO_FLOOR × GNI_pc_country
#   → prevents implausibly low VSL in very low-income countries.

dt_compare[, vsl_e1_0 := US_VSL_RATIO * gni_pc_usa *
             (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LOW]

dt_compare[, vsl_e1_5 := US_VSL_RATIO * gni_pc_usa *
             (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIGH]

dt_compare[, vsl_e1_2 := ifelse(
  gni_pc_ppp >= gni_pc_usa,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIC,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LMIC
)]

dt_compare[, vsl_e1_0 := pmax(vsl_e1_0, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_2 := pmax(vsl_e1_2, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_5 := pmax(vsl_e1_5, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]

# VSL economic value of deaths averted (retained for backward compatibility)
dt_compare[, economic_value_e1_0 := vsl_e1_0 * deaths_averted]
dt_compare[, economic_value_e1_2 := vsl_e1_2 * deaths_averted]
dt_compare[, economic_value_e1_5 := vsl_e1_5 * deaths_averted]

# ── 7b) VSLY calculation ───────────────────────────────────────────────────────
#
# VSLY = Value of a Statistical Life-Year, derived from VSL and remaining LE.
#
# Undiscounted VSLY (uniform value across all remaining life-years):
#   vsly_undiscounted = VSL / LE_pop_weighted
#
# Discounted VSLY at rate r (annuity formula — each future life-year is
# discounted relative to the present):
#   annuity_factor(LE, r) = [ 1 − (1+r)^(−LE) ] / r
#   vsly_r = VSL / annuity_factor(LE, r)
#   When r → 0: annuity_factor → LE, so vsly_r → vsly_undiscounted.
#
# Economic value of deaths averted:
#   vsly_value         = deaths_averted × vsly_undiscounted
#   vsly_value_r3      = deaths_averted × vsly_r3   (annuity at 3%)
#
# The time-series discount factor (Section 8) brings these annual values to
# 2026 present value and is applied separately.

# Undiscounted VSLY (all three elasticity cases)
dt_compare[le_pop_weighted > 0, `:=`(
  vsly_e1_0 = vsl_e1_0 / le_pop_weighted,
  vsly_e1_2 = vsl_e1_2 / le_pop_weighted,
  vsly_e1_5 = vsl_e1_5 / le_pop_weighted
)]

# Discounted VSLY at 3% (annuity equivalent)
r3 <- DISC_RATES["r3"]
dt_compare[le_pop_weighted > 0,
  annuity_r3 := (1 - (1 + r3)^(-le_pop_weighted)) / r3
]
dt_compare[le_pop_weighted > 0, `:=`(
  vsly_r3_e1_0 = vsl_e1_0 / annuity_r3,
  vsly_r3_e1_2 = vsl_e1_2 / annuity_r3,
  vsly_r3_e1_5 = vsl_e1_5 / annuity_r3
)]

# VSLY economic value of deaths averted
dt_compare[, `:=`(
  vsly_value_e1_0    = deaths_averted * vsly_e1_0,
  vsly_value_e1_2    = deaths_averted * vsly_e1_2,
  vsly_value_e1_5    = deaths_averted * vsly_e1_5,
  vsly_value_r3_e1_0 = deaths_averted * vsly_r3_e1_0,
  vsly_value_r3_e1_2 = deaths_averted * vsly_r3_e1_2,
  vsly_value_r3_e1_5 = deaths_averted * vsly_r3_e1_5
)]

# ── 8) Discount factors ────────────────────────────────────────────────────────
#
# Time-series discounting brings the annual stream of benefits back to BASE_YEAR.
#   disc_rX = 1 / (1 + rX)^(year − BASE_YEAR)
#
# Discounted GNI and VSL columns express values in BASE_YEAR constant PPP terms
# for comparability across the 2026–2050 horizon.

dt_compare[, `:=`(
  disc_r1 = 1 / (1 + DISC_RATES["r1"])^(year - BASE_YEAR),
  disc_r3 = 1 / (1 + DISC_RATES["r3"])^(year - BASE_YEAR),
  disc_r5 = 1 / (1 + DISC_RATES["r5"])^(year - BASE_YEAR)
)]

dt_compare[, `:=`(
  gni_pc_disc_r3           = gni_pc_ppp         * disc_r3,
  vsl_e1_5_disc_r3         = vsl_e1_5           * disc_r3,
  # Undiscounted VSLY × time-discount (life-years valued equally within each death)
  vsly_value_e1_5_disc_r3  = vsly_value_e1_5    * disc_r3,
  # Annuity VSLY × time-discount (life-years discounted within + across time)
  vsly_value_r3_e1_5_disc  = vsly_value_r3_e1_5 * disc_r3
)]

# ── 9) Final dataset ───────────────────────────────────────────────────────────

dt_final <- dt_compare[, .(
  location, iso3, year, scenario, htn_target_scenario, who_region,
  deaths_baseline, deaths_intervention, deaths_averted,
  population, le_pop_weighted,
  gni_pc_ppp, gni_pc_usa,
  vsl_e1_0, vsl_e1_2, vsl_e1_5,
  vsly_e1_0,    vsly_e1_2,    vsly_e1_5,
  vsly_r3_e1_0, vsly_r3_e1_2, vsly_r3_e1_5,
  disc_r1, disc_r3, disc_r5,
  gni_pc_disc_r3,
  vsl_e1_5_disc_r3,
  economic_value_e1_0, economic_value_e1_2, economic_value_e1_5,
  vsly_value_e1_0,    vsly_value_e1_2,    vsly_value_e1_5,
  vsly_value_r3_e1_0, vsly_value_r3_e1_2, vsly_value_r3_e1_5,
  vsly_value_e1_5_disc_r3,
  vsly_value_r3_e1_5_disc
)]

setorder(dt_final, location, year, scenario)

if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
saveRDS(dt_final, OUT_FILE)
fwrite(dt_final, file.path(DIR_OUT, "08_vsly_results.csv"))

cat("Saved:", OUT_FILE, "\n")
cat("Rows:", nrow(dt_final),
    "| Columns:", ncol(dt_final),
    "| Countries:", length(unique(dt_final$location)), "\n")
cat("Scenarios:", paste(unique(dt_final$scenario), collapse = ", "), "\n")
cat("Years:", min(dt_final$year), "–", max(dt_final$year), "\n")

# ── 10) Summary reporting table ────────────────────────────────────────────────
#
# Rows    : intervention scenario × WHO region  (+ "World" totals)
# Columns : 2026 | 2030 | 2040 | 2050 | total 2026–2050
# Metric  : discounted VSLY under r = 3%, elasticity = 1.5
#           vsly_value_r3_e1_5_disc = deaths_averted × vsly_r3_e1_5 × disc_r3
#
# "Share of income" for year t:
#   share_t = Σ(vsly_value_r3_e1_5_disc_t) / Σ(population_t × gni_pc_disc_r3_t)
#   Both numerator and denominator are discounted to BASE_YEAR.
#   The disc_r3 factors cancel for a single year slice, so the ratio equals
#   Σ(VSLY value_t) / Σ(GDP_t) regardless of discount rate — useful for
#   annual comparability.  For the cumulative column, discounting affects the
#   time-weighting of each year's contribution.

SUMMARY_YEARS <- c(2026L, 2030L, 2040L, 2050L)

# Helper: aggregate one year slice to (who_region, scenario) level
make_slice <- function(dt, yr) {
  sub <- dt[year == yr, .(
    vsly_value   = sum(vsly_value_r3_e1_5_disc, na.rm = TRUE),
    total_income = sum(population * gni_pc_disc_r3,  na.rm = TRUE)
  ), by = .(who_region, scenario)]
  sub[, share_income := vsly_value / total_income]
  sub[, total_income := NULL]
  setnames(sub,
    c("vsly_value",   "share_income"),
    c(paste0("vsly_",   yr), paste0("share_", yr))
  )
  sub
}

# Helper: aggregate one year slice to World level (no region grouping)
make_slice_world <- function(dt, yr) {
  sub <- dt[year == yr, .(
    vsly_value   = sum(vsly_value_r3_e1_5_disc, na.rm = TRUE),
    total_income = sum(population * gni_pc_disc_r3,  na.rm = TRUE)
  ), by = .(scenario)]
  sub[, `:=`(who_region = "World", share_income = vsly_value / total_income)]
  sub[, total_income := NULL]
  setnames(sub,
    c("vsly_value",   "share_income"),
    c(paste0("vsly_",   yr), paste0("share_", yr))
  )
  sub
}

# Annual slices for regions and World
slices_region <- lapply(SUMMARY_YEARS, make_slice,       dt = dt_final)
slices_world  <- lapply(SUMMARY_YEARS, make_slice_world, dt = dt_final)

# Merge annual slices across years
merge_slices <- function(lst) {
  Reduce(function(a, b) merge(a, b, by = c("who_region", "scenario"), all = TRUE), lst)
}

dt_reg_ann   <- merge_slices(slices_region)
dt_world_ann <- merge_slices(slices_world)

# Cumulative totals 2026–2050
make_total <- function(dt) {
  sub <- dt[year >= BASE_YEAR & year <= 2050, .(
    vsly_total   = sum(vsly_value_r3_e1_5_disc, na.rm = TRUE),
    income_total = sum(population * gni_pc_disc_r3,  na.rm = TRUE)
  ), by = .(who_region, scenario)]
  sub[, share_total := vsly_total / income_total]
  sub[, income_total := NULL]
  sub
}
make_total_world <- function(dt) {
  sub <- dt[year >= BASE_YEAR & year <= 2050, .(
    vsly_total   = sum(vsly_value_r3_e1_5_disc, na.rm = TRUE),
    income_total = sum(population * gni_pc_disc_r3,  na.rm = TRUE)
  ), by = .(scenario)]
  sub[, `:=`(who_region = "World", share_total = vsly_total / income_total)]
  sub[, income_total := NULL]
  sub
}

dt_reg_tot   <- make_total(dt_final)
dt_world_tot <- make_total_world(dt_final)

# Combine annual + cumulative columns
dt_summary_region <- merge(dt_reg_ann, dt_reg_tot,
                           by = c("who_region", "scenario"), all = TRUE)
dt_summary_world  <- merge(dt_world_ann, dt_world_tot,
                           by = c("who_region", "scenario"), all = TRUE)

dt_summary <- rbind(dt_summary_region, dt_summary_world, fill = TRUE)
setorder(dt_summary, scenario, who_region)

saveRDS(dt_summary, OUT_SUMM)
fwrite(dt_summary, file.path(DIR_OUT, "08_vsly_summary_table.csv"))

cat("Saved summary table:", OUT_SUMM, "\n")
cat("Rows:", nrow(dt_summary),
    "| Regions:", paste(unique(dt_summary$who_region), collapse = ", "), "\n")
