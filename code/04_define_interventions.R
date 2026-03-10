# Required inputs----


#...........................................................
# HTN Targets Aim 2----
#...........................................................


#............................................................................
#  Here create the needed improvement rate to reach 150 more under control by 2030,
# and apply it to the baseline rates to create the "improvement" scenario for aim2
#...........................................................................

# Use data.in, weight by pop20 year==2025
# The control rate in 2025 is reach_base data.in$htncov2 weighted by pop20 year==2025
# The target is 150 million more under control by 2030, which is
# 150 million / global pop in 2025* htncov2 = x% increase in control rate
# htncov2 varies by sex and location, so we calculate the needed increase in
# control rate for each location and sex

target_additional_control <- 150e6

# Control rates from NCD Risk Factor Collaboration (NCD-RisC) data, 
# which is the basis for our baseline control rates (htncov2)
data.in<-fread(paste0(wd_data,"bp_data6.csv"))%>%rename(location = location_gbd)%>%select(-Year, -Country)

# NCD risk htn prevalence
dt_htn_prev <- fread(paste0(wd_raw,"NCD-RisC_Lancet_2021_Hypertension_age_specific_estimates_by_country.csv"))

# Select 2019 and required columns
dt_htn_prev <- dt_htn_prev[Year == 2019,c("Country", "Sex", "Age", "Prevalence of hypertension"),with=F]

setnames(dt_htn_prev,old=c("Country", "Sex", "Age", "Prevalence of hypertension"),
                     new=c("location","sex","age","htn_prev"))

# re encode sex to Male and Female
dt_htn_prev[,sex := ifelse(sex == "Men","Male","Female")]

# re encode locations to match names
# dt_htn_prev[location == "United States", location := "United States of America"]
# dt_htn_prev[location == "Viet Nam", location := "vietnam"]
# dt_htn_prev[location == "The Gambia", location := "Gambia"]

dt_htn_prev[, location := fcase(
  location == "Viet Nam", "Vietnam",
  location == "United States of America", "United States",
  location == "Lao PDR", "Laos",
  location == "Congo, Dem. Rep.", "Democratic Republic of the Congo",
  location == "Cabo Verde", "Cape Verde",
  location == "Gambia", "The Gambia",
  location == "Bahamas", "The Bahamas",
  location == "Micronesia (Federated States of)", "Federated States of Micronesia",
  location == "Syrian Arab Republic", "Syria",
  location == "Palestine, State of", "Palestine",
  location == "North Macedonia", "Macedonia",
  location == "Taiwan", "Taiwan (Province of China)",
  location == "Brunei Darussalam", "Brunei",
  location == "DR Congo", "Democratic Republic of the Congo",
  location == "Guinea Bissau", "Guinea-Bissau",
  location == "Macedonia (TFYR)", "Macedonia",
  location == "Occupied Palestinian Territory", "Palestine",
  default = location
)]

# create htn prev for 20-30 and 70-85+ by taking the average of adjacent age groups (30-34 for 20-24 and 25-29, and 75-79 for 80-84 and 85+ )

# add missing ages by copying adjacent groups
add_low  <- dt_htn_prev[age == "30-34", .(location, sex, htn_prev)][, age := "20-24"]
add_low2 <- dt_htn_prev[age == "30-34", .(location, sex, htn_prev)][, age := "25-29"]

add_high  <- dt_htn_prev[age == "75-79", .(location, sex, htn_prev)][, age := "80-84"]
add_high2 <- dt_htn_prev[age == "75-79", .(location, sex, htn_prev)][, age := "85plus"]

# bind + de-duplicate (in case you run it twice)
dt_htn_prev<- unique(
  rbind(dt_htn_prev, add_low, add_low2, add_high, add_high2),
  by = c("location","sex","age")
)

# merge data in with data prev
data.in <- merge(data.in,dt_htn_prev,by.x=c("location","sex","Age.group"),
                 by.y=c("location","sex","age"),all.x = TRUE)

# data.in$salt[data.in$location=="China"]<-4.83*2.54
# length(unique(data.in$location))

# 2025 hypertension control and population by location-sex
pop2095 <- as.data.table(readRDS(paste0(wd_data,"PopulationsSingleAge0050.rds")))
setnames(pop2095, "year_id", "year")

# for some reason in UNWPP 2025 Samoa and American Samoa have no data, so taking 2023 and appending to 2025
pop2095_samoa <- pop2095[location %in% c("Samoa", "American Samoa") & year == 2023 & age >= 20,]

pop2095_samoa <- pop2095_samoa[,
                   .(pop20 = sum(Nx, na.rm = TRUE)),
                   by = .(location, sex)]

# pop age>=20 in 2025, by location-sex
pop2095 <- pop2095[age >= 20 & year == 2025,
                   .(pop20 = sum(Nx, na.rm = TRUE)),
                   by = .(location, sex)]

pop2095 <- rbind(pop2095, pop2095_samoa)
# unique location-sex HTN inputs (htncov2 is among raisedBP)
data.in_htn <- unique(
  data.in[, .(location, sex, htncov2, raisedBP,htn_prev,diabetes)],
  by = c("location", "sex")
)

aim2_base_2025 <- merge(
  data.in_htn,
  pop2095,
  by = c("location", "sex"),
  all.x = TRUE
)[!is.na(pop20)]

# ---- Baseline global controlled (among raisedBP) in 2025
global_raisedBP_pop_2025 <- aim2_base_2025[, sum(pop20 * htn_prev, na.rm = TRUE)]
global_controlled_2025   <- aim2_base_2025[, sum(pop20 * htn_prev * htncov2, na.rm = TRUE)]

global_control_rate_2025 <- global_controlled_2025 / global_raisedBP_pop_2025

# ---- Target controlled count and target control rate (still among raisedBP)
target_controlled_2030   <- global_controlled_2025 + target_additional_control
target_control_rate_2030 <- target_controlled_2030 / global_raisedBP_pop_2025

# multiplicative scale factor on htncov2
aim2_scale_factor <- target_control_rate_2030 / global_control_rate_2025

# ---- Apply proportional increase to each location-sex htncov2 (cap at 1)
aim2_locsex <- copy(aim2_base_2025)

aim2_locsex[, `:=`(
  htncov2_2025 = htncov2,
  htncov2_2030 = pmin(1, htncov2 * aim2_scale_factor),
  
  # annual increment over 5 years (2025 -> 2030)
  annual_increment = (pmin(1, htncov2 * aim2_scale_factor) - htncov2) / 5
)]

# ---- Controlled counts MUST include htn_prev
aim2_locsex[, controlled_2025 := pop20 * htn_prev * htncov2_2025]
aim2_locsex[, controlled_2030 := pop20 * htn_prev * htncov2_2030]

aim2_achieved_additional_control <- aim2_locsex[
  , sum(controlled_2030 - controlled_2025, na.rm = TRUE)
]

aim2_loc <- aim2_locsex[, {
  
  pop_total <- sum(pop20, na.rm = TRUE)
  
  raisedBP_pop_total <- sum(pop20 * htn_prev, na.rm = TRUE)
  
  controlled_2025_total <- sum(controlled_2025, na.rm = TRUE)
  controlled_2030_total <- sum(controlled_2030, na.rm = TRUE)
  
  list(
    # keep original column names
    htncov2 = controlled_2025_total / raisedBP_pop_total,
    raisedBP = raisedBP_pop_total / pop_total,
    pop20 = pop_total,
    htncov2_2025 = controlled_2025_total / raisedBP_pop_total,
    htncov2_2030 = controlled_2030_total / raisedBP_pop_total,
    annual_increment = (controlled_2030_total - controlled_2025_total) /
      raisedBP_pop_total / 5,
    controlled_2025 = controlled_2025_total,
    controlled_2030 = controlled_2030_total
  )
  
}, by = location]

# save .csv for input in running the model
fwrite(aim2_loc, paste0(wd_data,"htn_control_targets_by_loc.csv"))

#clean up environment
rm(aim2_loc,aim2_base_2025, aim2_locsex, data.in_htn,pop2095_samoa,pop2095, target_additional_control, global_raisedBP_pop_2025, global_controlled_2025,
   global_control_rate_2025, target_controlled_2030, target_control_rate_2030, aim2_scale_factor)



## Temporary code to test the impact of the proportional increase in control rates on the number of people controlled globally in 2030, to confirm it matches the target of 150 million additional controlled
## Import targets excel ----
aim2_loc <- as.data.table(
  read_excel("C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/Scenarios.xlsx",
             sheet = "Sheet1",range = "A6:O197")
)

# keep target control rates baseline htncov2, _aspirational,_ambitious, and _progress
aim2_loc <- aim2_loc[, .(location,htncov2, htncov2_aspirational, htncov2_ambitious, htncov2_progress,htn_ctrl_diabetes)]

# save .csv for input in running the model
fwrite(aim2_loc, paste0(wd_data,"htn_control_targets_by_loc.csv"))


#...........................................................
# STATINS AIM 2: Country-level baseline/target table ----
#...........................................................
#
# Goal: one row per country with:
#   - 2025 baseline statin coverage (from statin_data.rds)
#   - 2030 target coverage (fixed at 80%)
#   - population age 40+ (UNWPP 2024)
#   - diabetes prevalence and count among 40+ (from Scenarios.xlsx or bp_data6 fallback)
#   - primary prevention eligible count (pop without CVD x AF, x prop_athero for istroke)
#   - secondary prevention eligible count (prevalent IHD + atherosclerotic istroke, age 40+)
#   - iso3 code (from covfxn2.csv)
#
# Sources:
#   Pop 40+        : PopulationsSingleAge0050.rds (UNWPP 2024)
#   Diabetes prev  : Scenarios.xlsx Sheet1 A6:O197 (grep for "diab" column);
#                    fallback -> data.in$diabetes from bp_data6.csv (mean over age-sex strata)
#   Statin baseline: data/processed/Statins/statin_data.rds (statins_current column)
#   AF for statins : data/processed/af_statins.rds (location x cause x af_statins)
#   Secondary prev : adjusted*.rds files (PREVt0 x UNWPP Nx, cause = ihd or istroke, age >= 40)
#   Primary prev   : (1 - PREVt0) x AF x Nx; ischemic stroke scaled by prop_athero_stroke
#   ISO3 map       : data/processed/covfxn2.csv


# -- 1. Population age 40+ in 2025, by location ------------------------------
# Source: UNWPP 2024 single-year age (same file used earlier for HTN section)
# Note: pop2095 was removed above; reload here under a new object name

pop_raw_s <- as.data.table(readRDS(paste0(wd_data, "PopulationsSingleAge0050.rds")))
if ("year_id" %in% names(pop_raw_s)) setnames(pop_raw_s, "year_id", "year")

# Samoa / American Samoa: no 2025 data in UNWPP -> fall back to 2023 (matches HTN logic above)
pop_samoa_s <- pop_raw_s[
  location %in% c("Samoa", "American Samoa") & year == 2023 & age >= 40,
  .(pop_40plus_2025 = sum(Nx, na.rm = TRUE)),
  by = .(location)
]

pop40_2025_s <- pop_raw_s[
  age >= 40 & year == 2025,
  .(pop_40plus_2025 = sum(Nx, na.rm = TRUE)),
  by = .(location)
]
pop40_2025_s <- rbind(pop40_2025_s[!location %in% c("Samoa", "American Samoa")], pop_samoa_s)

# Also build 5-year-age x sex population (needed to merge with b_rates strata for prev counts)
# b_rates stores age as the lower bound of each 5-yr bin (40, 45, ..., 95)
pop_samoa_5yr_s <- pop_raw_s[
  location %in% c("Samoa", "American Samoa") & year == 2023 & age >= 40
]
pop_5yr_2025_s <- rbind(
  pop_raw_s[age >= 40 & year == 2025],
  pop_samoa_5yr_s
)
pop_5yr_2025_s[, age5 := pmin(5L * (age %/% 5L), 95L)]  # collapse single years -> 5-yr bins, cap at 95
pop_5yr_2025_s <- pop_5yr_2025_s[, .(Nx_unwpp = sum(Nx, na.rm = TRUE)), by = .(location, sex, age5)]
setnames(pop_5yr_2025_s, "age5", "age")

rm(pop_raw_s, pop_samoa_s, pop_samoa_5yr_s)


# -- 2. ISO3 lookup ----------------------------------------------------------
# Source: covfxn2.csv -- has iso3 and location (GBD naming convention)
# Unmatched locations will simply have NA iso3 in the output
iso3_map_s <- unique(
  fread(paste0(wd_data, "covfxn2.csv"))[, .(location, iso3)],
  by = "location"
)


# -- 3. Diabetes prevalence by location --------------------------------------
# Primary source: Scenarios.xlsx Sheet1 A6:O197 (same file as aim2_loc above)
# We reload with all 15 columns and search for any column whose name contains "diab".
# If none found, fall back to data.in$diabetes (bp_data6.csv, averaged over age-sex strata).
#
# NOTE: inspect the printed column names below to confirm which column is selected.

aim2_loc_full_s <- as.data.table(
  read_excel("C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/Scenarios.xlsx",
             sheet = "Sheet1", range = "A6:O197")
)

cat("=== Columns in Scenarios.xlsx Sheet1 (A6:O197) ===\n")
print(names(aim2_loc_full_s))

diab_cols_s <- grep("diab", names(aim2_loc_full_s), ignore.case = TRUE, value = TRUE)

if (length(diab_cols_s) > 0) {
  # Use the first matching column (e.g. "diabetes_prev", "diab_2025", etc.)
  diab_col_use_s <- diab_cols_s[1]
  cat("Using diabetes prevalence column from Scenarios.xlsx:", diab_col_use_s, "\n")
  aim2_diab_s <- aim2_loc_full_s[
    , .(location, diabetes_prev_2025 = as.numeric(get(diab_col_use_s)))
  ]
} else {
  # Fallback: data.in (bp_data6.csv) has diabetes by location x age x sex.
  # Average over all age-sex strata within each location as an approximation.
  # For a more accurate estimate, weight by age-sex population (not done here).
  warning("No 'diab*' column found in Scenarios.xlsx -- falling back to data.in$diabetes (bp_data6.csv).")
  diab_col_use_s <- "data.in$diabetes (bp_data6.csv)"
  aim2_diab_s <- data.in[
    , .(diabetes_prev_2025 = mean(diabetes, na.rm = TRUE)),
    by = location
  ]
}

rm(aim2_loc_full_s, diab_cols_s)


# -- 4. Baseline statin coverage in 2025 -------------------------------------
# Source: statin_data.rds -- column statins_current holds the 2025 observed coverage
# (constant across all years; equals the baseline pp_cov from FDC_coverage_data_statints_pp.csv)
# NA statins_current means no coverage data for that location -> treated as 0 in the model

dt_statin_s <- readRDS(file = paste0(wd_data, "Statins/statin_data.rds"))

statin_base_s <- unique(
  dt_statin_s[, .(location, statins_baseline_2025 = statins_current)],
  by = "location"
)

rm(dt_statin_s)


# -- 5. Attributable fractions for statins by location x cause ---------------
# Source: af_statins.rds -- columns: location, cause, af_statins
#   cause values used here: "ihd" and "istroke"
# Global defaults (GBD 2021 High Fasting Plasma Glucose AFs) used where data are missing:
#   af_ihd     = 0.1497
#   af_istroke = 0.1161

af_ihd_def_s     <- 0.1497
af_istroke_def_s <- 0.1161

dt_af_s <- readRDS(file = paste0(wd_data, "af_statins.rds"))

# Pivot to wide (one row per location)
dt_af_wide_s <- dcast(
  dt_af_s[cause %in% c("ihd", "istroke")],
  location ~ cause,
  value.var = "af_statins"
)
# Rename so cause levels become column names af_ihd / af_istroke
if ("ihd"     %in% names(dt_af_wide_s)) setnames(dt_af_wide_s, "ihd",     "af_ihd")
if ("istroke" %in% names(dt_af_wide_s)) setnames(dt_af_wide_s, "istroke", "af_istroke")
if (!"af_ihd"     %in% names(dt_af_wide_s)) dt_af_wide_s[, af_ihd     := af_ihd_def_s]
if (!"af_istroke" %in% names(dt_af_wide_s)) dt_af_wide_s[, af_istroke := af_istroke_def_s]

dt_af_wide_s[is.na(af_ihd),     af_ihd     := af_ihd_def_s]
dt_af_wide_s[is.na(af_istroke), af_istroke := af_istroke_def_s]

rm(dt_af_s)


# -- 6. Secondary & primary prevention counts (from calibrated baseline rates) -
#
# Load adjusted*.rds files as b_rates_s (same pattern as 05_build_baseline.R).
# Keep only the columns needed to reduce memory.
#
# PREVt0 = calibrated initial prevalence (proportion in "sick" state at model start).
#          Constant across years in the output files; we extract from the earliest year.
#
# SECONDARY prevention eligible:
#   People who already HAVE IHD or atherosclerotic ischemic stroke (age >= 40).
#   Count = PREVt0 x Nx_unwpp, where Nx_unwpp is UNWPP 2025 population.
#   For ischemic stroke: only the atherosclerotic fraction (prop_athero_stroke) is counted.
#
# PRIMARY prevention eligible:
#   People WITHOUT existing CVD who are at risk (dyslipidaemia-driven risk via AF).
#   Count = (1 - PREVt0) x AF x Nx_unwpp
#   For ischemic stroke: further scaled by prop_athero_stroke.
#
# NOTE: Primary and secondary counts are reported separately; their sum
#       (total_eligible_2025) assumes non-overlapping groups (standard simplification).

prop_athero_stroke_s <- 0.60   # share of ischemic strokes that are atherosclerotic

files_adj_s   <- list.files(path = wd_data, pattern = "adjusted", full.names = TRUE)
dt_list_adj_s <- lapply(files_adj_s, function(f) {
  dt <- readRDS(f)
  setDT(dt)
  needed <- intersect(names(dt), c("location", "year", "age", "sex", "cause", "PREVt0"))
  dt[, .SD, .SDcols = needed]
})
b_rates_s <- rbindlist(dt_list_adj_s, use.names = TRUE, fill = TRUE)
rm(dt_list_adj_s, files_adj_s)

# Harmonise location names (same renames as 05_build_baseline.R)
b_rates_s[location == "United States of America",           location := "United States"]
b_rates_s[location == "Bolivia (Plurinational State of)",   location := "Bolivia"]
b_rates_s[location == "United Republic of Tanzania",        location := "Tanzania"]

# Extract PREVt0 from the earliest available year (expected: 2017)
# PREVt0 does not vary by year; using the first year avoids duplicating rows.
if ("year" %in% names(b_rates_s)) {
  earliest_yr_s <- min(b_rates_s$year, na.rm = TRUE)
  b_prev_s <- b_rates_s[year == earliest_yr_s & age >= 40 & cause %in% c("ihd", "istroke"),
                         .(location, sex, age, cause, PREVt0)]
} else {
  earliest_yr_s <- NA_integer_
  b_prev_s <- b_rates_s[age >= 40 & cause %in% c("ihd", "istroke"),
                         .(location, sex, age, cause, PREVt0)]
}
rm(b_rates_s)

# Merge PREVt0 strata with UNWPP 2025 population (5-yr bins)
# This gives us 2025-population-weighted prevalence counts
b_prev_s <- merge(
  b_prev_s,
  pop_5yr_2025_s,
  by    = c("location", "sex", "age"),
  all.x = TRUE
)
# Strata with no UNWPP population match contribute 0 to counts
b_prev_s[is.na(Nx_unwpp), Nx_unwpp := 0]

# Merge location-level AFs (does not vary by age/sex within a location)
b_prev_s <- merge(b_prev_s, dt_af_wide_s, by = "location", all.x = TRUE)
b_prev_s[is.na(af_ihd),     af_ihd     := af_ihd_def_s]
b_prev_s[is.na(af_istroke), af_istroke := af_istroke_def_s]

# -- 6a. Secondary prevention counts (prevalent cases, age >= 40) ------------
sec_prev_s <- b_prev_s[, .(
  sec_prev_ihd_2025     = sum(ifelse(cause == "ihd",
                                     PREVt0 * Nx_unwpp,
                                     0), na.rm = TRUE),
  sec_prev_istroke_2025 = sum(ifelse(cause == "istroke",
                                     prop_athero_stroke_s * PREVt0 * Nx_unwpp,
                                     0), na.rm = TRUE)
), by = .(location)]

sec_prev_s[, sec_prev_total_2025 := sec_prev_ihd_2025 + sec_prev_istroke_2025]

# -- 6b. Primary prevention counts (at-risk without existing disease, age >= 40) -
prim_prev_s <- b_prev_s[, .(
  prim_prev_ihd_2025     = sum(ifelse(cause == "ihd",
                                      (1 - PREVt0) * af_ihd * Nx_unwpp,
                                      0), na.rm = TRUE),
  prim_prev_istroke_2025 = sum(ifelse(cause == "istroke",
                                      (1 - PREVt0) * prop_athero_stroke_s * af_istroke * Nx_unwpp,
                                      0), na.rm = TRUE)
), by = .(location)]

prim_prev_s[, prim_prev_total_2025 := prim_prev_ihd_2025 + prim_prev_istroke_2025]

rm(b_prev_s, dt_af_wide_s, pop_5yr_2025_s)


# -- 7. Assemble final country-level table -----------------------------------

statins_targets_s <- merge(pop40_2025_s,     aim2_diab_s,   by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, iso3_map_s,    by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, statin_base_s, by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, sec_prev_s,    by = "location", all.x = TRUE)
statins_targets_s <- merge(statins_targets_s, prim_prev_s,   by = "location", all.x = TRUE)

# Derived fields
statins_targets_s[, `:=`(
  year                     = 2025L,
  diabetes_pop_40plus_2025 = pop_40plus_2025 * diabetes_prev_2025,
  total_eligible_2025      = sec_prev_total_2025 + prim_prev_total_2025,
  statins_target_2030      = 0.80
)]


# -- 8. Column ordering ------------------------------------------------------
col_order_s <- c(
  "location", "iso3", "year",
  "pop_40plus_2025",
  "diabetes_prev_2025", "diabetes_pop_40plus_2025",
  "statins_baseline_2025", "statins_target_2030",
  "prim_prev_ihd_2025", "prim_prev_istroke_2025", "prim_prev_total_2025",
  "sec_prev_ihd_2025",  "sec_prev_istroke_2025",  "sec_prev_total_2025",
  "total_eligible_2025"
)
setcolorder(statins_targets_s, col_order_s[col_order_s %in% names(statins_targets_s)])


# -- 9. Sanity checks --------------------------------------------------------
cat("=== statins_control_targets_by_loc: Diagnostics ===\n")
cat("  Unique countries:               ", uniqueN(statins_targets_s$location),                          "\n")
cat("  Missing statins baseline:       ", sum(is.na(statins_targets_s$statins_baseline_2025)), "\n")
cat("  Missing diabetes prevalence:    ", sum(is.na(statins_targets_s$diabetes_prev_2025)),   "\n")
cat("  Missing pop 40+:                ", sum(is.na(statins_targets_s$pop_40plus_2025)),       "\n")
cat("  Missing secondary prev total:   ", sum(is.na(statins_targets_s$sec_prev_total_2025)),  "\n")
cat("  Missing primary prev total:     ", sum(is.na(statins_targets_s$prim_prev_total_2025)), "\n")
cat("  Rows with iso3 matched:         ", sum(!is.na(statins_targets_s$iso3)),                "\n")
cat("  Diabetes source used:           ", diab_col_use_s,                                     "\n")


# -- 10. Write CSV -----------------------------------------------------------
# Path consistent with project convention: fwrite to wd_data (data/processed/)
fwrite(statins_targets_s, paste0(wd_data, "statins_control_targets_by_loc.csv"))
cat("Written: statins_control_targets_by_loc.csv\n")


# -- 11. Clean up ------------------------------------------------------------
rm(statins_targets_s, pop40_2025_s, aim2_diab_s, iso3_map_s, statin_base_s,
   sec_prev_s, prim_prev_s, prop_athero_stroke_s,
   af_ihd_def_s, af_istroke_def_s, diab_col_use_s,
   col_order_s, earliest_yr_s)
