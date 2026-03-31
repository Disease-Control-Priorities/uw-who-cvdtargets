#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the GBD 2019 data and the GBD 2019 population estimates.
# The output of this file is a data.csv file with the baseline rates for each country and year.
# The output data set includes the following columns: sex	age	location	year	ALL.mx	BG.mx.all	cause	BG.mx	PREVt0	DIS.mx.t0	Nx


# tracking locations during all steps
locs <- readRDS(file = paste0(wd,"locs.rds"))

locs <- as.vector(locs$location)
#data permalink

# dt <- rbindlist(lapply(sprintf(paste0(wd_raw,"GBD/","IHME-GBD_2021_DATA-fc833c60-%d.csv"), 1:21), fread),
#                 use.names = TRUE, fill = TRUE)

# path <- paste0(wd_raw,"GBD/GBD2023/")
# 
# # List all CSV files
# files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
# 
# # Read and combine using rbindlist
# dt <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
# 
# dt<-data.table(dt)
# 
# dt[, upper:=NULL]
# dt[, lower:=NULL]
# 
# unique(dt$year)
# unique(dt$location_name)
# unique(dt$cause_name)
# unique(dt$age_name)
# 
# # Clean some known patterns (optional but helps)
# dt[, location_clean := gsub("^(Republic|Kingdom|Commonwealth|Democratic Republic|Principality|Federated States|State|United States|Islamic Republic|Portuguese|Union) of ", "", location_name)]
# dt[, location_clean := gsub(" of the", "", location_clean)]
# dt[, location_clean := trimws(location_clean)]
# 
# rename_map <- c(
#   "Global" = "Global",          # No match in locs; set to NA or remove
#   "Moldova" = "Republic of Moldova",
#   "Korea" = "Republic of Korea",
#   "America" = "United States of America",
#   "the Congo" = "Congo",
#   "Côte d'Ivoire" = "Ivory Coast",
#   "the Niger" = "Niger",
#   "People's Republic of Bangladesh" = "Bangladesh",
#   "Independent State of Samoa" = "Samoa",
#   "Portuguese Republic" = "Portugal",
#   "Argentine Republic" = "Argentina",
#   "Micronesia" = "Micronesia (Federated States of)",
#   "Hellenic Republic" = "Greece",
#   "the Philippines" = "Philippines",
#   "People's Republic of China" = "China",
#   "Federal Republic of Nigeria" = "Nigeria",
#   "the Gambia" = "Gambia",
#   "French Republic" = "France",
#   "the Marshall Islands" = "Marshall Islands",
#   "Togolese Republic" = "Togo",
#   "Federal Democratic Republic of Ethiopia" = "Ethiopia",
#   "Lebanese Republic" = "Lebanon",
#   "Swiss Confederation" = "Switzerland",
#   "Hashemite Kingdom of Jordan" = "Jordan",
#   "Federal Democratic Republic of Nepal" = "Nepal",
#   "Sultanate of Oman" = "Oman",
#   "Plurinational State of Bolivia" = "Bolivia (Plurinational State of)",
#   "Bolivarian Republic of Venezuela" = "Venezuela (Bolivarian Republic of)",
#   "Eastern Republic of Uruguay" = "Uruguay",
#   "Socialist Republic of Viet Nam" = "Viet Nam",
#   "Federal Republic of Somalia" = "Somalia",
#   "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
#   "the Bahamas" = "Bahamas",
#   "Federal Republic of Germany" = "Germany",
#   "Democratic Socialist Republic of Sri Lanka" = "Sri Lanka",
#   "People's Democratic Republic of Algeria" = "Algeria",
#   "Gabonese Republic" = "Gabon",
#   "Slovak Republic" = "Slovakia",
#   "United Mexican States" = "Mexico",
#   "Independent State of Papua New Guinea" = "Papua New Guinea",
#   "Czech Republic" = "Czechia",
#   "Federative Republic of Brazil" = "Brazil",
#   "the Union of Myanmar" = "Myanmar",
#   "Kyrgyz Republic" = "Kyrgyzstan",
#   "Arab Republic of Egypt" = "Egypt",
#   "the Comoros" = "Comoros",
#   "Iran" = "Iran (Islamic Republic of)",
#   "Grand Duchy of Luxembourg" = "Luxembourg",
#   "the Netherlands" = "Netherlands",
#   "Türkiye" = "Turkey",
#   "Taiwan" = "Taiwan (Province of China)"
# )
# 
# # Apply the renaming
# dt[, location_clean := fifelse(location_clean %in% names(rename_map),
#                                rename_map[location_clean],
#                                location_clean)]
# 
# dt[, location_name := location_clean]
# 
# dt[, c("location_clean") := NULL]
# 
# # end name cleaning here
# 

dt <- readRDS(file = paste0(wd_raw,"GBD/","temp_1baseline_rates_gbd23.rds"))

dt <- data.table(dt)

# Filter the data to include only the specified causes
dt <- dt[cause_name %in% dx_include,]

setnames(dt,c("sex_name","age_name","cause_name","measure_name","metric_name","location_name")
         ,c("sex","age","cause","measure","metric","location"))

dt <- dt[location!="Global",]
dt[, upper:=NULL]
dt[, lower:=NULL]

unique(dt$cause)

#Get average annual rates of change by cohort
#as a function#

year1 <- 1995
year2 <- 2000

dt <- dt[, c("measure","location","sex","age","cause","metric","year","val"),with=FALSE]

# Fix countries names
dt[location == "Türkiye", location := "Turkey"]
dt[location == "Côte d'Ivoire", location := "Ivory Coast"]

dt <- dt[age!="75-84 years",]

dt <- as.data.frame(dt)

#...........................................................
# Getting rates for diseases ----
#...........................................................

#dt_ori <- copy(dt)

get.new.rates<-function(dt, year1, year2, cause_map = cause_map){
  
  all_long <- unname(cause_map["all"])           # "All causes"
  spec_long <- setdiff(cause_map, all_long)      # every specific cause
  
    #year1<-1995
    #year2<-2000
  prevyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year1)%>%
                  select(-c(measure, metric, year))%>%
                    rename(prev14 = val))
  
  prevyear1 <- unique(prevyear1,by=c("location","sex","age","cause"))
  
  prevrtyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year1)%>%
    select(-c(measure, metric, year))%>%
    rename(prevrt14 = val))
  
  prevrtyear1 <- unique(prevrtyear1,by=c("location","sex","age","cause"))
  
  deathyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year1)%>%
    select(-c(measure, metric, year))%>%
    rename(death14 = val))
  
  deathyear1 <- unique(deathyear1,by=c("location","sex","age","cause"))
  
  deathrtyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year1)%>%
    select(-c(measure, metric, year))%>%
    rename(deathrt14 = val))
  
  deathrtyear1 <- unique(deathrtyear1,by=c("location","sex","age","cause"))
  
  prevyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year2)%>%
    select(-c(measure, metric, year))%>%
    rename(prev19 = val))
  
  prevyear2 <- unique(prevyear2,by=c("location","sex","age","cause"))
  
  prevrtyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year2)%>%
    select(-c(measure, metric, year))%>%
    rename(prevrt19 = val))
  
  prevrtyear2 <- unique(prevrtyear2,by=c("location","sex","age","cause"))
  
  deathyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year2)%>%
    select(-c(measure, metric, year))%>%
    rename(death19 = val))
  
  deathyear2 <- unique(deathyear2,by=c("location","sex","age","cause"))
  
  deathrtyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year2)%>%
    select(-c(measure, metric, year))%>%
    rename(deathrt19 = val))
  
  deathrtyear2 <- unique(deathrtyear2,by=c("location","sex","age","cause"))
  
  mymerge<-function(x,y){merge.data.table(x,y, by=c("location", "age", "sex", "cause"))}
  
  dt14<-Reduce(mymerge, list(prevyear1,prevrtyear1,deathyear1,deathrtyear1))
  dt19<-Reduce(mymerge, list(prevyear2,prevrtyear2,deathyear2,deathrtyear2))
  
  dt14[, pop14:=death14/deathrt14*100000]
  dt14<-dt14%>%mutate(age2 = ifelse(age=="20-24 years", 1, NA),
                      age2 = ifelse(age=="25-29 years", 2, age2),
                      age2 = ifelse(age=="30-34 years", 3, age2),
                      age2 = ifelse(age=="35-39 years", 4, age2),
                      age2 = ifelse(age=="40-44 years", 5, age2),
                      age2 = ifelse(age=="45-49 years", 6, age2),
                      age2 = ifelse(age=="50-54 years", 7, age2),
                      age2 = ifelse(age=="55-59 years", 8, age2),
                      age2 = ifelse(age=="60-64 years", 9, age2),
                      age2 = ifelse(age=="65-69 years", 10, age2),
                      age2 = ifelse(age=="70-74 years", 11, age2),
                      age2 = ifelse(age=="75-79 years", 12, age2),
                      age2 = ifelse(age=="80-84 years", 13, age2),
                      age2 = ifelse(age=="85-89 years", 14, age2),
                      age2 = ifelse(age=="90-94 years", 15, age2),
                      age2 = ifelse(age=="95+ years", 16, age2))
  
  dt19[, pop19:=death19/deathrt19*100000]
  dt19<-dt19%>%mutate(age2 = ifelse(age=="20-24 years", 0, NA),
                      age2 = ifelse(age=="25-29 years", 1, age2),
                      age2 = ifelse(age=="30-34 years", 2, age2),
                      age2 = ifelse(age=="35-39 years", 3, age2),
                      age2 = ifelse(age=="40-44 years", 4, age2),
                      age2 = ifelse(age=="45-49 years", 5, age2),
                      age2 = ifelse(age=="50-54 years", 6, age2),
                      age2 = ifelse(age=="55-59 years", 7, age2),
                      age2 = ifelse(age=="60-64 years", 8, age2),
                      age2 = ifelse(age=="65-69 years", 9, age2),
                      age2 = ifelse(age=="70-74 years", 10, age2),
                      age2 = ifelse(age=="75-79 years", 11, age2),
                      age2 = ifelse(age=="80-84 years", 12, age2),
                      age2 = ifelse(age=="85-89 years", 13, age2),
                      age2 = ifelse(age=="90-94 years", 14, age2),
                      age2 = ifelse(age=="95+ years", 15, age2))
  
  dt19[, age:=NULL]
  
  dt<-merge(dt14, dt19, by=c("age2", "location", "sex", "cause"))
  
  #allcause<-dt[cause=="All causes"]
  allcause <- dt[cause == all_long]
  
  setnames(allcause, c("death14", "deathrt14", "death19", "deathrt19"),
           c("alldeath14", "alldeathrt14", "alldeath19", "alldeathrt19"))
  allcause[,c("cause", "prev14", "prevrt14", "prev19", "prevrt19",
              "pop14", "pop19"):=NULL]
  
  dt<-merge(dt, allcause, by=c("age2", "sex", "location", "age"))
  
  #dt<-dt[cause!="All causes"]
  dt <- dt[cause != all_long]
  

  dt[, othermx14:=alldeath14-death14]
  dt[, othermx19:=alldeath19-death19]
  dt[, othermxrt14:=alldeathrt14 - deathrt14]
  dt[, othermxrt19:=alldeathrt19 - deathrt19]
  
  dt[, well14:=pop14-prev14-alldeath14]
  dt[,well19:=pop19-prev19-alldeath19]
  
  dt[, wellAARC:=log((well19/pop19)/(well14/pop14))/5]
  dt[, sickAARC:=log(prevrt19/prevrt14)/5]
  dt[, deadAARC:=log(deathrt19/deathrt14)/5]
  dt[, deadotherAARC:=log(othermxrt19/othermxrt14)/5]
  
  dt[sickAARC<0, sickAARC:=0]
  dt[sickAARC>1, sickAARC:=0.99]
  dt[deadAARC<0, deadAARC:=0]
  dt[deadAARC>1, deadAARC:=0.99]
  dt[deadotherAARC<0, deadotherAARC:=0]
  dt[deadotherAARC>1, deadotherAARC:=0.99]
  
  
  dt[,age2:=1]
  
  rows<-as.numeric(nrow(dt))
  
  reprow<-function(row){
    floor((row-1)/rows)
  }
  
  DT<-dt[rep(seq(1,nrow(dt)), 5)][, age2:=age2+reprow(.I)]
  
  DT[age2==1, Well:=well14]
  DT[age2==1, Sick:=prev14]
  DT[age2==1, Dead:=death14]
  DT[age2==1, DeadOther:=othermx14]
  
  for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Well2:=shift(Well)*(1+wellAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Well2")]
  DT[age2==i, Well:=DT2[,Well2]]
  }
  
  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, Sick2:=shift(Sick)*(1+sickAARC), by=.(age, sex, location, cause)]
    DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Sick2")]
    DT[age2==i, Sick:=DT2[,Sick2]]
  }
  
  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, Dead2:=shift(Dead)*(1+deadAARC), by=.(age, sex, location, cause)]
    DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Dead2")]
    DT[age2==i, Dead:=DT2[,Dead2]]
  }
  
  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, DeadOther2:=shift(DeadOther)*(1+deadotherAARC), by=.(age, sex, location, cause)]
    DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "DeadOther2")]
    DT[age2==i, DeadOther:=DT2[,DeadOther2]]
  }
  
  DT[, IR:=(Sick-(shift(Sick)-Dead))/shift(Well),  by=.(age, sex, location, cause)]
  DT[, CF:=Dead/shift(Sick),  by=.(age, sex, location, cause)]
  
  DT[ , avgIR:=mean(na.omit(IR)), by=.(age, sex, location, cause)]
  DT[ , avgCF:=mean(na.omit(CF)), by=.(age, sex, location, cause)]
  DT[ , midptage:=as.numeric(substr(age,1,2))+2]
  
  DT_final<-unique(DT[,c("midptage", "age", "sex", "location", "cause", "avgIR", "avgCF")])
  DT_final[, year:=year2]
  
  DT_final[avgIR<0 | is.na(avgIR), avgIR:=0]
  DT_final[avgCF>1, avgCF:=0.9]
  DT_final[avgIR>1, avgIR:=0.9]
}
#end of function

newrates <- get.new.rates(dt, 1995,2000, cause_map = cause_map)
unique(newrates$cause)

for(i in 1:19){
  cat("Year: ", 1995+i, "\n")
  DT_final<-  get.new.rates(dt, 1995+i, 2000+i,cause_map = cause_map)
  newrates<-rbindlist(list(DT_final, newrates), use.names = T)
}

DT_final<-newrates #store for debugging
any(is.na(DT_final))

over95<-DT_final[age=="90-94 years"]
over95[, age:="95+ years"]
over95[, midptage:=97]

new<-rbindlist(list(over95, DT_final))
new<-new[order(location, sex, midptage)]
unique(new$midptage)
setnames(new, c("avgIR", "avgCF"), c("IR", "CF"))

get.data <- function(sx, cse, var, dfin, yr, country){
  x        <- c(seq(22,92,5), 95)
  y        <- dfin %>% filter(sex==sx & cause==cse) %>% pull(var)
  d        <- approx(x,y, xout=20:95, rule=2, method="linear")
  df       <- data.table(age = d$x, dname = d$y, cause = cse, sex = sx, location = country, year = yr)
  setnames(df, "dname", var)
  df
}

#...........................................................
# Rates by single age ----
#...........................................................

# Modify since there are no cases of aod for 20-40

get.single.age.rates <- function(data, yr, country, cause_map) {
  
  data <- data[year == yr & location == country]
  
  spec_long <- setdiff(cause_map, cause_map["all"])   # ignore "All causes"
  sexes     <- c("Female", "Male")
  
  ## helper to pull one variable for one (sex, cause) pair
  pull_var <- function(var) {
    rbindlist(lapply(spec_long, function(cse) {
      rbindlist(lapply(sexes, function(sx)
        get.data(sx, cse, var, data, yr, country)))
    }))
  }
  
  IRs <- pull_var("IR")
  CFs <- pull_var("CF")
  
  merge(IRs, CFs,
        by = c("age", "sex", "cause", "location", "year"))
}



#...........................................................
# Computing TPS by single age ----
#...........................................................

#locs<-unique(dt$location)
#locs <- c("India","Bangladesh")

# Load baseline_rates
#baseline_rates <- as.data.table(readRDS(file = paste0(wd_data,"baseline_rates.rds")))

#baseline rates calculated in file:

files <- list.files(
  path       = wd_data, 
  pattern    = "baseline", 
  full.names = TRUE
)

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# Bind them all together, matching columns by name and filling missing ones
baseline_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)


# path to create folder
#folder <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/tps/"
folder <- paste0(wd_temp,"tps/")

# if it already exists, delete it (recursively)
if (dir.exists(folder)) {
  unlink(folder, recursive = TRUE)
}

# now create a fresh one
dir.create(folder)


if(run_aod_par == TRUE){
  
  # Patch. Rates for AOD not computed (rare incidence/prevalence) 
  new_aod <- new[age %in% c("20-24 years","25-29 years","30-34 years","35-39 years") & cause=="Ischemic heart disease",]
  
  new_aod[, cause := "Alzheimer's disease and other dementias"]
  new_aod[, IR := 0]
  new_aod[, CF := 0]
  
  new <- rbindlist(list(new, new_aod), use.names = T)
  
  new <- new[order(new$location,new$cause,new$year,new$sex,new$midptage,new$age),]
  
}

## 1.  Loop over locations
years <- 2000:2019           # vector of years you want

for (loc in locs) {
  
  cat("Processing location:", loc, "\n")
  
  ## 1A.  Build the full 2000-2019 table of single-age rates
  ##       (IR + CF) for this location
  newrates <- rbindlist(
    lapply(years, function(yr)
      get.single.age.rates(new, yr, loc, cause_map)),
    use.names = TRUE
  )
  
  ## sanity check
  cat("   any NA in newrates? ", anyNA(newrates), "\n")
  
  ## 1B.  Merge with baseline_rates already on disk / in memory
  other    <- baseline_rates[location == loc]   # or readRDS(…)
  dataout  <- merge(other, newrates,
                    by = c("age", "sex", "location", "year", "cause"))
  
  cat("   any NA after merge? ", anyNA(dataout), "\n")
  
  ## 1C.  Save
  saveRDS(dataout,
          file = file.path(folder,
                           paste0("tps_", loc, ".rds")))
}


#...........................................................
# Consolidate and split the tps files ----
#...........................................................

folder <- paste0(wd_temp,"tps/")

# List all .rds files in the folder
files <- list.files(
  #path       = "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/tps/", 
  path       = folder,
  pattern    = "\\.rds$", 
  full.names = TRUE
)

# Read each one 
dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

# Bind them all together, matching columns by name and filling missing ones
tps_inpt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# compute number of rows and chunk size
n     <- nrow(tps_inpt)
chunk <- ceiling(n / 5)

# loop over the three parts
for (i in 1:5) {
  start <- (i - 1) * chunk + 1
  end   <- min(i * chunk, n)
  
  part <- tps_inpt[start:end]
  
  saveRDS(
    part,
    file = paste0(wd_data, "tps_inpt_part", i, ".rds")
  )
}

rm(dt_list)

#...........................................................
# Cleaning up the workspace ----
#...........................................................

rm(list = ls()[sapply(ls(), function(x) is.data.frame(get(x)))])
rm(is,i,locs)

