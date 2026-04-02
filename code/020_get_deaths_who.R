#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the WHO CVD deaths 2000 - 2023, which will be used to 
# calculate the baseline rates for each country and year instead of GBD 2023. 
# The data was provided from the WHO.
# The output of this file is a rds file with death counts for each country and year.

# Load IHD and IHD from excel book csv file provided by WHO. 
# The data is in the "WHO_CVD_GHE2021.csv" file in the raw data folder.

dt_deaths_who_cvd <- fread(paste0(wd_raw,"WHO/", "CVD_GHE2021.csv"))

# now load stroke data
dt_deaths_who_stroke <- fread(paste0(wd_raw,"WHO/", "Stroke_GHE2021.csv"))

# Append the two datasets together, keeping only the relevant columns and renaming them for consistency
dt_deaths_who <- rbind(dt_deaths_who_cvd,dt_deaths_who_stroke, fill = TRUE)

# clean up the data by removing unnecessary files
rm(dt_deaths_who_cvd,dt_deaths_who_stroke)

# Stroke is aggregated. So delete and keep broken down into ischemic and hemorrhagic stroke in the WHO data,
dt_deaths_who <- dt_deaths_who[!causename %in% c("Cardiovascular diseases","Stroke"),]

# rename to match dt GBD 2023 data
setnames(dt_deaths_who,
         old = c("iso3",     "ghecause",  "causename", "dths", "sex"),
         new = c("location_id", "cause_id",  "cause",     "val","sex_id"))

# Encode sex
dt_deaths_who <- dt_deaths_who[sex_id!=3,] # remove both sex category
dt_deaths_who[,sex := ifelse(sex_id==1,"Male","Female")]

# assign location name to iso3 location id
invalid_locs <- c(
  "1_Afr","1_LI","2_Amr","2_LMI","3_Sear","3_UMI",
  "4_Eur","4_HI","5_Emr","6_Wpr","World"
)

dt_deaths_who <- dt_deaths_who[!(location_id %in% invalid_locs),]

dt_deaths_who[, location := countrycode(location_id, "iso3c", "country.name")]

# drop pop column not needed for this analysis
dt_deaths_who[, pop := NULL]

# Recode WHO age groups to match dt$age
age_map <- c(
  "20-24" = "20-24 years",
  "25-29" = "25-29 years",
  "30-34" = "30-34 years",
  "35-39" = "35-39 years",
  "40-44" = "40-44 years",
  "45-49" = "45-49 years",
  "50-54" = "50-54 years",
  "55-59" = "55-59 years",
  "60-64" = "60-64 years",
  "65-69" = "65-69 years",
  "70-74" = "70-74 years",
  "75-79" = "75-79 years",
  "80-84" = "80-84 years",
  "85+"   = "85+ years"
)

# Keep only 20+ groups that exist in the mapping
dt_deaths_who <- dt_deaths_who[age %in% names(age_map)]

# Harmonize age labels
dt_deaths_who[, age := age_map[age]]

# Harmonize cause names in dt_deaths_who to match dt$cause
cause_map <- c(
  "All Causes" = "All causes",
  "Haemorrhagic stroke" = "Intracerebral hemorrhage",
  "Hypertensive heart disease" = "Hypertensive heart disease",
  "Ischaemic heart disease" = "Ischemic heart disease",
  "Ischaemic stroke" = "Ischemic stroke"
)

dt_deaths_who[, cause := cause_map[cause]]

# optional check for anything unmapped
dt_deaths_who[is.na(cause), unique(cause)]

# Melt to long format
dt_deaths_who_long <- melt(
  dt_deaths_who,
  id.vars = c("location_id", "sex_id", "year", "cause_id",
              "cause", "age", "sex", "location"),
  measure.vars = c("val", "rate"),
  variable.name = "metric",
  value.name = "val"
)

dt_deaths_who_long[, metric := fifelse(
  metric == "val", "Number", "Rate"
)]

# create measure==Deaths
dt_deaths_who_long[, measure := "Deaths"]

# remove unnecessary columns to merge with GBD
dt_deaths_who_long[, c("cause_id", "sex_id","location_id") := NULL]

# save the cleaned WHO deaths data as rds for later use
saveRDS(dt_deaths_who_long, paste0(wd_data, "dt_deaths_who_long.rds"))

# Cleaning space
rm(dt_deaths_who, dt_deaths_who_long)