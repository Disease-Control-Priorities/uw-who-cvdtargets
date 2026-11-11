# Required inputs----

#...........................................................
#add covid mx data ----
#...........................................................

setwd(wd_raw)
load(paste0(wd_data,"wpp.adj.Rda"))

wpp.adj<-wpp.adj%>%
  mutate(location_name = ifelse(location_name=="North Korea", "Democratic People's Republic of Korea", location_name))
#Covid mx ~= excess mortality

# check for names as GBD and UWPP 2024
locs_wpp.adj <- unique(wpp.adj$location_name)

#baseline rates calculated in file calibration:

# Stage 03 (03_calibration.R) writes the FINAL calibrated + secular-trend-
# projected rates here. Scope the pattern to the current chunks so no stale
# "adjusted*" file is silently mixed in.
files <- list.files(
  path       = wd_data,
  pattern    = "^adjusted_searo_part[0-9]+\\.rds$",
  full.names = TRUE
)

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# Bind them all together, matching columns by name and filling missing ones
b_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

rm(dt_list, files)

locs_b_rates <- unique(b_rates$location)
b_rates[location=="United States of America",location:="United States"]
b_rates[location=="Bolivia (Plurinational State of)",location:="Bolivia"]
b_rates[location=="United Republic of Tanzania",location:="Tanzania"]

b_rates <- b_rates[!is.na(location),]
.drop_pd <- intersect(c("percent_lag","percent_diff"), names(b_rates))
if (length(.drop_pd)) b_rates[, (.drop_pd) := NULL]

# covid.mx is merged once, below (see the "Covid 2020/2021" block). The former
# early left_join of wpp.adj covid.mx here only fed the covid-rebalance block
# (removed): now that stage 03 delivers the 2020-2050 rows, that block would fire
# spuriously on real covid values and double-touch BG.mx / IR / CF.

# Update to UNWPP 2024
dt_pop_unwpp <- as.data.table(readRDS(paste0(wd_data,"PopulationsSingleAge0050.rds")))

dt_pop_unwpp[age>=95, age:= 95]

setnames(dt_pop_unwpp, c("year_id"), c("year"))

dt_pop_unwpp <- dt_pop_unwpp[, .(Nx = sum(Nx)), by = .(location, year, sex, age)]

b_rates <- merge(b_rates,
                 dt_pop_unwpp[, .(location,year,age,sex,Nx2=Nx)],
                 by = c("location", "year", "age","sex"),
                 all.x = TRUE
)

# replace Nx with Nx2
b_rates <- b_rates[, Nx := ifelse(is.na(Nx2), Nx, Nx2)]

b_rates[,Nx2 := NULL]

locs <- unique(b_rates$location)

#...........................................................
# Population data from UNWPP
pop20 <- read.csv(paste0(wd_data,"PopulationsAge20_2050.csv"), stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
  select(-c(Nx2))

#...........................................................
# Blood Pressure data ----
# blood pressure data calculated in file: "Blood pressure.R"
#...........................................................

data.in<-fread(paste0(wd_data,"bp_data6.csv"))%>%rename(location = location_gbd)%>%select(-Year, -Country)

data.in$salt[data.in$location=="China"]<-4.83*2.54
length(unique(data.in$location))

#...........................................................
# HTN add scale-up data ----
#...........................................................

inc <- read.csv(paste0(wd_data,"covfxn2.csv"), stringsAsFactors = F)%>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)

bpcats<-c("<120", "120-129", "130-139", 
          "140-149", "150-159", "160-169", 
          "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)

data.in <- as.data.table(data.in)
# Fixes location names

name_map <- c(
  "Brunei"                            = "Brunei Darussalam",
  "Cape Verde"                        = "Cabo Verde",
  "Cote d'Ivoire"                     = "Ivory Coast",
  "Czech Republic"                    = "Czechia",
  "Federated States of Micronesia"    = "Micronesia (Federated States of)",
  "Iran"                              = "Iran (Islamic Republic of)",
  "Laos"                              = "Lao People's Democratic Republic",
  "Macedonia"                         = "North Macedonia",
  "Moldova"                           = "Republic of Moldova",
  "South Korea"                       = "Republic of Korea",
  "Swaziland"                         = "Eswatini",
  "Syria"                             = "Syrian Arab Republic",
  "The Bahamas"                       = "Bahamas",
  "The Gambia"                        = "Gambia",
  "Venezuela"                         = "Venezuela (Bolivarian Republic of)",
  "Vietnam"                           = "Viet Nam",
  "North Korea"                       = "Democratic People's Republic of Korea"
)

# 3. update your data.in in place, using fcoalesce() so that
#    any location not in name_map stays unchanged
data.in[, location := fcoalesce(name_map[location], location)]

inc <- as.data.table(inc)
inc[, location := fcoalesce(name_map[location], location)]

unique(data.in$location)
any(is.na(data.in))

locs_data.in <- unique(data.in$location)

# NOTE: the covid TP-rebalance block that used to sit here was removed with the
# refactor. It ran before the year-2020/2021 rows existed (covid.mx was all NA),
# so it was a no-op that got overwritten anyway; once stage 03 delivers those
# rows it would instead fire on real covid values and re-touch BG.mx/IR/CF. The
# state-transition loop in 06 already accounts for covid.mx per year.

#...........................................................
###fxn ----
#...........................................................

repYear<-function(row){
  2017+floor((row-1)/224)
}

data.in<-data.table(data.in%>%select(-age)%>%rename(age=Age.group))
b_rates[, newcases:=0]

# Years 2020-2050 now arrive already extended and secular-trend-projected from
# stage 03 (03_calibration.R); the former "repeat year 2019 to 2020-2050" block
# was removed so the projection horizon is materialised exactly once.

# rename causes to match abbreviated names
b_rates[,cause:=ifelse(cause=="Ischemic heart disease", "ihd",
                       ifelse(cause=="Ischemic stroke", "istroke",
                              ifelse(cause=="Intracerebral hemorrhage", "hstroke",
                                     ifelse(cause=="Hypertensive heart disease", "hhd",
                                            ifelse(cause=="Alzheimer's disease and other dementias", "aod",
                                                   cause)))))]

# #...........................................................
# # Calibration adjustments ----
# #...........................................................
# The IR/CF calibration is now baked into adjusted_searo_part*.rds by stage 03
# (03_calibration.R). The former run_adjustment_model block that re-multiplied
# IR/CF by adjustments2023_age.csv here was removed so the calibration is applied
# exactly once (no double adjustment across stages).

# #...........................................................
# # UNWPP 2024 Pop ----
# #...........................................................
# Adjust pop 20 to unwpp

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
  select(-c(Nx2))

# #...........................................................
# # Covid 2020/2021 ----
# #...........................................................

if ("covid.mx" %in% names(b_rates)) b_rates[,covid.mx:=NULL]
b_rates <- merge(b_rates,wpp.adj[,c("location_name","year","sex","age","covid.mx"),with=F],
                 by.x=c("location","year","sex","age"),
                 by.y=c("location_name","year","sex","age"),all.x=T)

b_rates[is.na(covid.mx), covid.mx:=0]
b_rates[covid.mx>=1, covid.mx:=0.9]

# For running capacity, preserve only year >= 2017
b_rates <- b_rates[year>=2017,]
#...........................................................
# Mortality downward trends ----
#...........................................................
# The secular BG.mx / BG.mx.all / CF trends are now applied once, in stage 03
# (03_calibration.R), using the same run_bgmx_trend / run_CF_trend /
# run_CF_trend_80 / run_CF_trend_ihme switches and the same tps_bgmx_*_forecasted
# files. The former trend blocks were removed here so each trend is applied
# exactly once and the calibration/observed years are never trended twice.

# Clean up environment
rm(dt_pop_unwpp, wpp.adj, pop20)
