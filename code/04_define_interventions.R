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
  data.in[, .(location, sex, htncov2, raisedBP,htn_prev)],
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

aim2_loc <- as.data.table(
  read_excel("C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/Scenarios.xlsx",
             sheet = "Sheet1",range = "A6:M197")
)

# keep target control rates baseline htncov2, _aspirational,_ambitious, and _progress
aim2_loc <- aim2_loc[, .(location,htncov2, htncov2_aspirational, htncov2_ambitious, htncov2_progress)]

# save .csv for input in running the model
fwrite(aim2_loc, paste0(wd_data,"htn_control_targets_by_loc.csv"))

