
# 08_economic_value_calculation.R
# VSL analysis: monetary value of deaths averted under each intervention vs baseline.
#
# Method: income-adjusted Value of Statistical Life (VSL) following
#   Robinson & Hammitt (2011) and Bolongaita et al. (2024)
#   https://www.nature.com/articles/s41591-024-03248-4
#
# Inputs:
#   - output/out_model/model_output_<country>_<target_col>.rds
#       (from 06_run_scenarios_multiple.R)
#   - data/raw/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv  (World Bank GNI)
#   - data/processed/Country_groupings_extended.csv            (iso3, location, region)
#
# Output:
#   output/08_vsl_results.rds  — one row per country × year × scenario

# ── 0) Paths ──────────────────────────────────────────────────────────────

# Resolve project root from this script's location
DIR_MODEL    <- file.path(wd, "output", "out_model")
DIR_OUT      <- file.path(wd, "output")
GNI_FILE     <- file.path(wd, "data", "raw",
                           "API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv")
COUNTRY_FILE <- file.path(wd, "data", "processed",
                           "Country_groupings_extended.csv")
OUT_FILE     <- file.path(DIR_OUT, "08_vsl_results.rds")

# ── 1) VSL parameters ─────────────────────────────────────────────────────
#
# Reference: VSL-to-income ratio for the USA = 160 (Robinson & Hammitt 2011)
#   VSL_USA = US_VSL_RATIO * GNI_pc_USA
#
# Income elasticity cases (applied when transferring VSL across countries):
#   e1_2  — differential baseline: 0.8 for countries at or above USA income,
#            1.2 for countries below USA income
#   e1_0  — sensitivity low : uniform 1.0 for all countries
#   e1_5  — sensitivity high: uniform 1.5 for all countries
#
# Minimum VSL ratio enforced at 20× income (prevents implausibly low values
# in very low-income countries).

US_VSL_RATIO    <- 160   # VSL / GNI_pc reference for USA
VSL_ELAST_HIC   <- 0.8   # elasticity for countries at or above USA income
VSL_ELAST_LMIC  <- 1.2   # elasticity for countries below USA income
VSL_ELAST_LOW   <- 1.0   # sensitivity-low (uniform)
VSL_ELAST_HIGH  <- 1.5   # sensitivity-high (uniform)
VSL_RATIO_FLOOR <- 20    # minimum VSL-to-GNI ratio (floors very-low-income VSL)


## Remaining LIFE EXPECTANCy to compute VSLY


lt <- as.data.table(read_excel(paste0(wd_raw,"WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"), 
                               sheet = "Medium variant", range = "A17:DH22967"))

setnames(lt,c("Region, subregion, country or area *","Notes","Location code","ISO3 Alpha-code","ISO2 Alpha-code","SDMX code**","Type","Parent code","Year"),
         c("location","Notes","Locationcode","ISO3","ISO2","SDMX","Type","Parencode","year"))

lt_interp <- melt(lt,id.vars = colnames(lt)[1:11],value.name = "le",variable.name = "age")

# Filter the data.table
lt_interp <- lt_interp[year >= 2025,c("location","age","year","le"),with = F]

lt_interp[,age := as.numeric(as.character(str_extract_all(age, "\\d+")))]

lt_interp[,le:=as.numeric(le)]

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

lt_interp[
  location %in% names(loc_fix_lt),
  location := loc_fix_lt[location]
]

# Cleaning
rm(lt)

## GNI Growth

# For extrapolating GNI growth rates, we can use World Bank data on GNI per capita.
# and for future (after last observation), implicit growth rates Socioeconomic Projections of the Shared Socioeconomic Pathways (SSPs), hosted by IIASA
# Release 3.1, July 2024

ssp_gdp <- as.data.table(read_excel(paste0(wd_raw,"1721734326790-ssp_basic_drivers_release_3.1_full.xlsx"), 
                                          sheet = "data"))

# Filter scenario SSPs to SSP2 (middle-of-the-road) and years 2010–2050.
# and variable "GDP per capita, PPP (constant 2017 international $)"
ssp_gdp <- ssp_gdp[Scenario=="SSP2" & Variable=="GDP|PPP [per capita]",]

# ── 2) Load model outputs ──────────────────────────────────────────────────
#
# 06_run_scenarios_multiple saves one file per (country, htn_target_scenario):
#   model_output_<country>_<target_col>.rds
# Each file contains rbindlist output from run_multiple_scenarios(), with an
# added column htn_target_scenario = target_col.
#
# Columns: scenario, age, cause, sex, year, well, sick, newcases,
#          dead, pop, all.mx, intervention, location, eff_ir, eff_cf,
#          htn_target_scenario

model_files <- list.files(DIR_MODEL,
                          pattern  = "^model_output_.*\\.rds$",
                          full.names = TRUE)

if (length(model_files) == 0) {
  stop("No model output files found in: ", DIR_MODEL,
       "\nRun 06_run_scenarios_multiple.R first.")
}

cat("Loading", length(model_files), "model output files...\n")
dt_model <- rbindlist(lapply(model_files, readRDS), fill = TRUE)
cat("Rows loaded:", nrow(dt_model), "\n")

# ── 3) Aggregate deaths and population to country × year × scenario ────────
#
# Deaths: sum `dead` across all age groups, sexes, and causes.
# Population: `pop` in the model is repeated for each cause, so we extract
#   unique (location, year, age, sex) rows from baseline to avoid over-counting.
#   We use the baseline scenario for population since intervention does not
#   change total population counts in this model.

dt_deaths <- dt_model[,
  .(deaths = sum(dead, na.rm = TRUE)),
  by = .(location, year, scenario, htn_target_scenario)
]

# Population: one row per (location, year, age, sex) from baseline only
dt_pop <- unique(
  dt_model[scenario == "baseline", .(location, year, age, sex, pop)]
)
dt_pop_total <- dt_pop[,
  .(population = sum(pop, na.rm = TRUE)),
  by = .(location, year)
]

# Merge population into deaths table
dt_deaths <- dt_pop_total[dt_deaths, on = .(location, year)]

# ── 4) Compute deaths averted vs baseline ─────────────────────────────────
#
# deaths_averted = deaths_baseline − deaths_intervention
# A positive value means the intervention prevented deaths relative to
# the counterfactual (no intervention).

dt_baseline <- dt_deaths[
  scenario == "baseline",
  .(location, year, htn_target_scenario, deaths_baseline = deaths)
]

dt_compare <- dt_deaths[scenario != "baseline"]

# Join baseline deaths onto each non-baseline scenario row
dt_compare <- dt_baseline[
  dt_compare,
  on = .(location, year, htn_target_scenario)
]

setnames(dt_compare, "deaths", "deaths_intervention")
dt_compare[, deaths_averted := deaths_baseline - deaths_intervention]

# ── 5) Add ISO3 codes ──────────────────────────────────────────────────────
#
# Country_groupings_extended.csv maps iso3 ↔ location (GBD name) ↔ region.
# We join by location name; any unmatched location falls back to countrycode().

country_grp <- fread(COUNTRY_FILE)
# Columns: iso3, location, region

# Primary merge on GBD location name
dt_compare <- country_grp[
  dt_compare,
  on = .(location),
  nomatch = NA
]

# Fallback: derive iso3 from location name via countrycode for unmatched rows
missing_iso3 <- is.na(dt_compare$iso3)
if (any(missing_iso3)) {
  dt_compare[missing_iso3,
    iso3 := countrycode(location, origin = "country.name", destination = "iso3c",
                        warn = FALSE)
  ]
}

setnames(dt_compare, "region", "who_region")

# ── 6) Load and process GNI data ──────────────────────────────────────────
#
# Source: World Bank GNI per capita, PPP (constant 2017 international $)
# File has four header rows; actual data starts at row 5.
# Columns: Country Name, Country Code, Indicator Name, Indicator Code,
#          <year columns 1960…latest>

gni_raw <- fread(GNI_FILE, skip = 4, header = TRUE)

# Drop non-year columns; keep Country Code
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

# Drop rows with no GNI value; keep years within model horizon (2017–2050)
gni <- gni[!is.na(gni_pc_ppp) & year >= 2000 & year <= 2050]

# Imputation rule: for any country × model-year combination where GNI is
# missing, use the value from the nearest available year for that country.
# Ties are broken by preferring the earlier year (last-observation carry-forward
# for future years, first-observation carry-back for pre-data years).

model_years <- unique(dt_compare$year)
iso3_list   <- unique(dt_compare[!is.na(iso3), iso3])

# Build a full grid of (iso3, model_year) and fill with nearest GNI.
# data.table rolling joins require the left table to be keyed on the join columns.
setkey(gni, iso3, year)
gni_grid <- CJ(iso3 = iso3_list, year = model_years)  # CJ produces a keyed table

# roll = "nearest": for each grid row, use the GNI from the closest available year
gni_grid <- gni[gni_grid, on = .(iso3, year), roll = "nearest"]

# Rename for clarity
setnames(gni_grid, "gni_pc_ppp", "gni_pc_ppp_imputed")

# Merge imputed GNI into comparison table
dt_compare <- gni_grid[
  dt_compare,
  on = .(iso3, year)
]
setnames(dt_compare, "gni_pc_ppp_imputed", "gni_pc_ppp")

# ── 7) VSL calculation ─────────────────────────────────────────────────────
#
# Formula (income-transfer method):
#   VSL_country = US_VSL * (GNI_pc_country / GNI_pc_USA)^elasticity
#   US_VSL      = US_VSL_RATIO * GNI_pc_USA
#
# Simplified:
#   VSL_country = US_VSL_RATIO * GNI_pc_USA * (GNI_pc_country / GNI_pc_USA)^elasticity
#               = US_VSL_RATIO * GNI_pc_country * (GNI_pc_country / GNI_pc_USA)^(elasticity - 1)
#
# Minimum VSL floor: VSL / GNI_pc >= VSL_RATIO_FLOOR (= 20)
# → floor applied as: VSL = max(VSL, VSL_RATIO_FLOOR * GNI_pc)
#
# Three elasticity cases computed as separate columns:
#   vsl_e1_0 — uniform elasticity = 1.0
#   vsl_e1_2 — differential: 0.8 for HIC (at or above USA), 1.2 for LMIC
#   vsl_e1_5 — uniform elasticity = 1.5

# Attach US GNI per year (reference income for VSL transfer)
us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp_imputed)]
dt_compare <- us_gni[dt_compare, on = .(year)]

# Compute VSL under each elasticity case
dt_compare[, vsl_e1_0 := US_VSL_RATIO * gni_pc_usa *
                          (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LOW]

dt_compare[, vsl_e1_5 := US_VSL_RATIO * gni_pc_usa *
                          (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIGH]

# Differential baseline: elasticity depends on whether country income >= USA
dt_compare[, vsl_e1_2 := ifelse(
  gni_pc_ppp >= gni_pc_usa,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_HIC,
  US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^VSL_ELAST_LMIC
)]

# Enforce VSL floor: VSL >= VSL_RATIO_FLOOR * GNI_pc
dt_compare[, vsl_e1_0 := pmax(vsl_e1_0, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_2 := pmax(vsl_e1_2, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
dt_compare[, vsl_e1_5 := pmax(vsl_e1_5, VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]

# Economic value = VSL × deaths_averted (in constant 2017 USD PPP)
dt_compare[, economic_value_e1_0 := vsl_e1_0 * deaths_averted]
dt_compare[, economic_value_e1_2 := vsl_e1_2 * deaths_averted]
dt_compare[, economic_value_e1_5 := vsl_e1_5 * deaths_averted]

# ── 8) Final column selection and ordering ─────────────────────────────────

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
  population,
  gni_pc_ppp,
  vsl_e1_0,
  vsl_e1_2,
  vsl_e1_5,
  economic_value_e1_0,
  economic_value_e1_2,
  economic_value_e1_5
)]

setorder(dt_final, location, year, scenario)

# ── 9) Save ───────────────────────────────────────────────────────────────

if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
saveRDS(dt_final, OUT_FILE)
cat("Saved:", OUT_FILE, "\n")
cat("Rows:", nrow(dt_final), "| Columns:", ncol(dt_final), "\n")
cat("Scenarios:", paste(unique(dt_final$scenario), collapse = ", "), "\n")
cat("Years:", min(dt_final$year), "–", max(dt_final$year), "\n")
cat("Countries:", length(unique(dt_final$location)), "\n")

# Fwrite to check in excel if needed
fwrite(dt_final, file.path(DIR_OUT, "08_vsl_results.csv"))

