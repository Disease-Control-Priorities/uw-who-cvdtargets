#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the WHO CVD deaths 2000 - 2023, which will be used to 
# calculate the baseline rates for each country and year instead of GBD 2023. 
# The data was provided from the WHO.
# The output of this file is a rds file with death counts for each country and year.

# Load IHD and IHD from excel book csv file provided by WHO. 
# The data is in the "WHO_CVD_GHE2021.csv" file in the raw data folder.
# 
# dt_deaths_who_cvd <- fread(paste0(wd_raw,"WHO/", "CVD_GHE2021.csv"))
# 
# # now load stroke data
# dt_deaths_who_stroke <- fread(paste0(wd_raw,"WHO/", "Stroke_GHE2021.csv"))
# 
# # Append the two datasets together, keeping only the relevant columns and renaming them for consistency
# dt_deaths_who <- rbind(dt_deaths_who_cvd,dt_deaths_who_stroke, fill = TRUE)
# 
# # clean up the data by removing unnecessary files
# rm(dt_deaths_who_cvd,dt_deaths_who_stroke)
# 
# # Stroke is aggregated. So delete and keep broken down into ischemic and hemorrhagic stroke in the WHO data,
# dt_deaths_who <- dt_deaths_who[!causename %in% c("Cardiovascular diseases","Stroke"),]

dt_deaths_who <- readRDS(paste0(wd_raw,"WHO/","ghe2023_cvd_fin_186.rds"))

setDT(dt_deaths_who)

# Keep columns needed for deaths and rates
dt_deaths_who <- dt_deaths_who[, .(
  iso3, sex, year, ghecause, causename,
  dths, dths.low, dths.up, pop, age
)]

# Rename to match GBD-style columns
setnames(
  dt_deaths_who,
  old = c("iso3", "ghecause", "causename", "dths",
          "dths.low", "dths.up", "sex"),
  new = c("location_id", "cause_id", "cause", "val",
          "lower", "upper", "sex_id")
)

# Remove both-sex category and regional/income aggregates
invalid_locs <- c(
  "1_Afr", "1_LI", "2_Amr", "2_LMI", "3_Sear", "3_UMI",
  "4_Eur", "4_HI", "5_Emr", "6_Wpr", "World"
)

dt_deaths_who <- dt_deaths_who[
  sex_id %in% c(1L, 2L) & !location_id %chin% invalid_locs
]

dt_deaths_who[, sex := fifelse(sex_id == 1L, "Male", "Female")]

dt_deaths_who[, location := countrycode(
  location_id, origin = "iso3c", destination = "country.name"
)]

# Harmonize adult age groups


# Age 999
# groups <- c("location_id", "sex_id", "year", "cause_id")
# dt_deaths_who[, `:=`(
#   unknown_deaths = sum(val[age == 999]),
#   known_deaths   = sum(val[age != 999])
# ), by = groups]
# 
# # Cannot allocate when no known-age deaths provide weights
# if (dt_deaths_who[
#   unknown_deaths > 0 & (is.na(known_deaths) | known_deaths <= 0),
#   .N
# ] > 0L) {
#   stop("Some groups have unknown-age deaths but no valid known-age total.")
# }
# 
# # Allocate unknown-age deaths proportionally
# dt_deaths_who[
#   age != 999 & known_deaths > 0,
#   val := val + unknown_deaths * (val / known_deaths)
# ]
# 
# # Remove the redistributed rows and temporary columns
# dt_deaths_who <- dt_deaths_who[age != 999]
# dt_deaths_who[, c("unknown_deaths", "known_deaths") := NULL]

# omit age 999

dt_deaths_who <- dt_deaths_who[age != 999]
age_map <- c(
  "20" = "20-24 years",
  "25" = "25-29 years",
  "30" = "30-34 years",
  "35" = "35-39 years",
  "40" = "40-44 years",
  "45" = "45-49 years",
  "50" = "50-54 years",
  "55" = "55-59 years",
  "60" = "60-64 years",
  "65" = "65-69 years",
  "70" = "70-74 years",
  "75" = "75-79 years",
  "80" = "80-84 years",
  "85" = "85+ years"
)

dt_deaths_who <- dt_deaths_who[
  age %in% as.numeric(names(age_map))
]

dt_deaths_who[, age := unname(age_map[as.character(age)])]


# Keep and harmonize selected causes
cause_map_who <- c(
  "All Causes"                 = "All causes",
  "Haemorrhagic stroke"         = "Intracerebral hemorrhage",
  "Hypertensive heart disease"  = "Hypertensive heart disease",
  "Ischaemic heart disease"     = "Ischemic heart disease",
  "Ischaemic stroke"            = "Ischemic stroke",
  "Rheumatic heart disease"     = "Rheumatic heart disease"
)

dt_deaths_who <- dt_deaths_who[cause %chin% names(cause_map_who)]
dt_deaths_who[, cause := unname(cause_map_who[cause])]

# Calculate death rates and bounds per 100,000 population
# Assumes deaths and population are counts in compatible units
dt_deaths_who[, `:=`(
  rate       = fifelse(pop > 0, val   / pop, NA_real_),
  rate_lower = fifelse(pop > 0, lower / pop, NA_real_),
  rate_upper = fifelse(pop > 0, upper / pop, NA_real_)
)]

dt_deaths_who_long <- melt(
  dt_deaths_who,
  id.vars = c("year", "cause", "age", "sex", "location"),
  measure.vars = c("val", "rate"),
  variable.name = "metric",
  value.name = "val",
  variable.factor = FALSE
)

dt_deaths_who_long[, metric := fifelse(
  metric == "val", "Number", "Rate"
)]

dt_deaths_who_long[, measure := "Deaths"]

# Exact columns and order shown in your screenshot
dt_deaths_who_long <- dt_deaths_who_long[, .(
  year, cause, age, sex, location, metric, val, measure
)]

# Keep only up to 2021
dt_deaths_who_long <- dt_deaths_who_long[year <= 2021,]
# save the cleaned WHO deaths data as rds for later use
saveRDS(dt_deaths_who_long, paste0(wd_data, "dt_deaths_who_long.rds"))

# Cleaning space
rm(dt_deaths_who, dt_deaths_who_long)

# # Validation: compare former WHO GHE 2021 data with the new GHE 2023 data
# # Compar GHE 23 vs formeR GHE 21 data
# 
# who_deaths_2021 <- readRDS(paste0(wd_data, "dt_deaths_who_long.rds"))
# who_deaths_2023 <- readRDS(paste0(wd_data, "dt_deaths_who_long_2023.rds"))
# 
# who_deaths_2021 <- who_deaths_2021[year <= 2021]
# who_deaths_2023 <- who_deaths_2023[year <= 2021]
# 
# setDT(who_deaths_2021)
# setDT(who_deaths_2023)
# 
# keys <- c("year", "cause", "age", "sex", "location", "metric", "measure")
# 
# # Check that each combination identifies one row
# stopifnot(
#   uniqueN(who_deaths_2021, by = keys) == nrow(who_deaths_2021),
#   uniqueN(who_deaths_2023, by = keys) == nrow(who_deaths_2023)
# )
# 
# # Full join: retain combinations present in either file
# comparison <- merge(
#   who_deaths_2021[, c(
#     .SD, list(present_2021 = TRUE)
#   )],
#   who_deaths_2023[, c(
#     .SD, list(present_2023 = TRUE)
#   )],
#   by = keys,
#   all = TRUE,
#   suffixes = c("_2021", "_2023")
# )
# 
# comparison[, status := fcase(
#   is.na(present_2021), "Only in 2023",
#   is.na(present_2023), "Only in 2021",
#   default = "Both"
# )]
# 
# # Signed differences: 2023 minus 2021
# comparison[, `:=`(
#   difference = val_2023 - val_2021,
#   pct_difference = fifelse(
#     !is.na(val_2021) & val_2021 != 0,
#     100 * (val_2023 - val_2021) / val_2021,
#     NA_real_
#   )
# )]
# 
# comparison[, c("present_2021", "present_2023") := NULL]
# 
# # Coverage comparison
# comparison[, .N, by = .(metric, status)]
# 
# # Summarize matched, nonmissing values separately for Number and Rate
# comparison_summary <- comparison[
#   status == "Both" & !is.na(val_2021) & !is.na(val_2023),
#   .(
#     n = .N,
#     mean_difference = mean(difference),
#     mean_absolute_difference = mean(abs(difference)),
#     median_pct_difference = median(pct_difference, na.rm = TRUE),
#     max_absolute_difference = max(abs(difference))
#   ),
#   by = .(metric, cause)
# ]
# 
# print(comparison_summary)
# 
# # Inspect largest absolute differences within each metric
# largest_differences <- comparison[
#   status == "Both" & !is.na(difference)
# ][order(-abs(difference)), head(.SD, 20L), by = metric]
# 
# View(largest_differences)
# View(comparison)

