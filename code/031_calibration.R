#...........................................................
# Documentation ----
#...........................................................

#run_calibration_par <- TRUE # set to TRUE to run parallel calibration
#data required:
  #incoming population (if not using fertility)
  #transition probabilities
  #initial state populations

setwd(wd_temp)

# Load locations file from file 1.get_base_rates.R
#locs<-c("Bangladesh","India")

locs<- readRDS(paste0(wd,"locs.rds"))
#locs<- as.vector(locs$locs)
locs<- as.vector(locs$location)

#baseline rates calculated in file:

files <- list.files(
  path       = wd_data, 
  pattern    = "tps_inpt", 
  full.names = TRUE
)

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  
  dt
})

# Bind them all together, matching columns by name and filling missing ones
b_rates <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

rm(dt_list)

b_rates[CF>=1, CF:=0.99]
b_rates[IR>=1, IR:=0.99]
b_rates[CF<0, CF:=0]
b_rates[IR<0, IR:=0]

# pop20 <- fread("PopulationsAge20_full.csv")%>%filter(location %in% locs)
# pop20 <- pop20[year_id>=2009 & year_id<=2040]

# Updating UNWPP2024

pop20 <- fread(paste0(wd_data,"PopulationsAge20_full.csv")) 

pop20[location=="Turkiye", location:="Türkiye"]
# China, Taiwan Province of China
pop20[location=="China, Taiwan Province of China", location:="Taiwan (Province of China)"]
#State of Palestine
pop20[location=="State of Palestine", location:="Palestine"]

# Dem. People's Republic of Korea "Democratic People's Republic of Korea"
pop20[location=="Dem. People's Republic of Korea", location:="Democratic People's Republic of Korea"]

#Micronesia (Fed. States of)
pop20[location=="Micronesia (Fed. States of)", location:="Micronesia (Federated States of)"]

pop20 <- pop20[location %in% locs]

pop20 <- pop20[year_id>=2009 & year_id<=2050]

setnames(pop20, c("year_id", "Nx"), c("year", "Nx20"))

locs20 <- unique(pop20$location)

missing_locs <- setdiff(locs, locs20)

missing_locs <- locs[! locs %in% locs20]

#...........................................................
#Run model to calibrate TPs 2009-2019----
#...........................................................

#IRadjust<-1
#CFadjust<-1
#any(is.na(base_rates))

state.transition<-function(b_rates, pop20,  IRadjust, CFadjust){ 
      
      #base_rates<-merge(b_rates, pop20[year<=2019], by=c("year", "location", "sex", "age"), all=TRUE)
      base_rates<-merge(b_rates, pop20[year<=2019], by=c("year", "location", "sex", "age"), all.x=TRUE)
      base_rates[age==20 & year>2009, Nx:=Nx20]
      base_rates[, Nx20:=NULL]
      
      ## calculate initial states for the incoming year 2000 and all years for age 20 population
      base_rates[year==2009 | age==20, sick:=Nx*PREVt0]
      base_rates[year==2009 | age==20, dead:=Nx*DIS.mx.t0]
      base_rates[year==2009 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
      
      base_rates[age==20 | year==2009, pop:=Nx]
      base_rates[age==20 | year==2009, all.mx:=Nx*ALL.mx]
      
      base_rates[, IR:=IR*IRadjust]
      base_rates[, CF:=CF*CFadjust]
      
      base_rates[,IRadjust:=IRadjust]
      base_rates[,CFadjust:=CFadjust]
      
      base_rates[CF>0.9, CF:=0.9]
      base_rates[IR>0.9, IR:=0.9]
      
      #STATE TRANSITIONS#
      for(i in 1:11){
            
            b2<-base_rates[year<=2009+i & year>=2009+i-1]
            b2[,age2:=age+1]
            
            #sick
            b2[, sick2:=shift(sick)*(1-(CF+BG.mx)) + shift(well)*IR, by=.(sex, location, cause, age)]
            #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
            b2[sick2<0, sick2:=0] #prevent possible negatives
            
            #dead
            b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age)]
            #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
            b2[dead2<0, sick2:=0] #prevent possible negatives
            
            #pop
            b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age)]
            #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
            b2[pop2<0, pop2:=0] #prevent possible negatives
            
            #all dead
            b2[, all.mx2:=sum(dead2), by=.(sex, location, year, age)]
            b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)]
            b2[all.mx2<0, all.mx2:=0]
            
            #well
            b2[, well2:=pop2-all.mx2-sick2]
            b2[well2<0, well2:=0] #prevent possible negatives
            
            #re-combined into original data.table
            b2<-b2[year==2009+i & age2<96, c("age2", "sick2", "dead2", "well2", "pop2", "all.mx2", "sex", "location", "cause")]
            setnames(b2, "age2", "age")
            base_rates[year==2009+i & age>20, sick:=b2[,sick2]]
            base_rates[year==2009+i & age>20, dead:=b2[,dead2]]
            base_rates[year==2009+i & age>20, well:=b2[,well2]]
            base_rates[year==2009+i & age>20, pop:=b2[,pop2]]
            base_rates[year==2009+i & age>20, all.mx:=b2[,all.mx2]]
            
      }
      
      base_rates%>%select(year, location, sex, age, cause, IR, CF, well, sick, dead, pop, all.mx, IRadjust, CFadjust)
}


#...........................................................
#Run model 121 times ----
#...........................................................#

# start parallel
#.................................................
# 1. split your 203 locations into 14 groups
groups <- split(locs, cut(seq_along(locs), breaks = 30, labels = FALSE))

# 2. prepare output folder (do this *before* launching the workers!)
#save_path <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/calibration_parallel"
save_path <- paste0(wd_temp,"calibration_parallel")
  
dir.create(save_path, recursive = TRUE, showWarnings = FALSE)

# Check locations in each group
locs_b_rates <- unique(b_rates$location)
locs_pop20 <- unique(pop20$location)

missing_locs <- setdiff(locs_b_rates,locs_pop20)

if(run_calibration_par == TRUE){

  # 3. spin up a 6-core cluster
  cl <- makeCluster(14)
  registerDoParallel(cl)
  
  # 4. export objects & functions so each worker can see them
  exports <- c("b_rates", "pop20", "state.transition", "groups", "save_path")
  
  # 5. run the parallel loop, writing .rds files only
  invisible(
    foreach(gidx = seq_along(groups),
            .export   = exports,
            .packages = c("dplyr","data.table")) %dopar% {
              
              my_locs <- groups[[gidx]]
              b_sub    <- b_rates %>% filter(location %in% my_locs)
              
               for(i in -5:5) {
                 for(j in -5:5) {
              
              # for(i in -5:5) {
              #   for(j in -5:5) {
                  df <- state.transition(b_sub, pop20, 1 + i/100, 1 + j/100) %>%
                    filter(year >= 2009)
                  
                  fname <- sprintf("group%02d_i%+d_j%+d.rds", gidx, i, j)
                  saveRDS(df, file.path(save_path, fname))
                }
              }
              
              # return NULL so nothing big is sent back
              NULL
            }
  )
  
  # 6. clean up
  stopCluster(cl)
  
}

# end parallel
#.................................................

# # Testing China
# 
# my_locs <- c("China")
# 
# b_sub    <- b_rates %>% filter(location %in% my_locs)
# 
# temp <- c()
# 
# for(i in -1:1) {
#   for(j in -1:1) {
#     cat("Running for i=", i, " and j=", j, "\n")
#     df <- state.transition(b_sub, pop20, 1 + i/100, 1 + j/100) %>%
#       filter(year >= 2009)
#     df$i <- i
#     df$j <- j
#     temp <- rbind(temp, df)
#   }
# }


# Consolidate out.df file


#...........................................................
# Error minimization ----
#Minimize root mean squared error compared to GBD 2009-2019
#Weight fatal estimates 2x non-fatal estimates
#...........................................................

# gbd <- rbindlist(lapply(sprintf(paste0(wd_raw,"GBD/","IHME-GBD_2021_DATA-fc833c60-%d.csv"), 1:21), fread),
#                 use.names = TRUE, fill = TRUE)

gbd <- readRDS(file = paste0(wd_raw,"GBD/","temp_1baseline_rates_gbd23.rds"))
gbd <- data.table(gbd)
gbd[, upper:=NULL]
gbd[, lower:=NULL]

# # Remove unnecessary dx
# dx_include <- c("All causes","Ischemic heart disease",
#                 "Ischemic stroke","Intracerebral hemorrhage",
#                 "Alzheimer's disease and other dementias","Hypertensive heart disease")                            

# Filter the data to include only the specified causes
gbd <- gbd[cause_name %in% dx_include,]

unique(gbd$year)
unique(gbd$location_name)
unique(gbd$cause_name)
unique(gbd$age_name)

setnames(gbd,c("sex_name","age_name","cause_name","measure_name","metric_name","location_name")
         ,c("sex","age","cause","measure","metric","location"))

# 1) filter, 2) reshape (wide), 3) drop the metric column
gbd <- dcast(
  gbd[
    metric == "Number" &
      measure != "Incidence" &
      location %in% locs &
      year >= 2009
  ],
  # use all other columns except 'measure' and 'val' as id-vars
  ... ~ measure,
  value.var = "val"
)[, metric := NULL]

# gbd <- as.data.frame(gbd)
# gbd <- gbd%>%
#        filter(metric=="Number" & measure!="Incidence" & location %in% locs & year>=2009)%>%
#        spread(measure, val)%>%
#        select(-metric)

unique(gbd$location)
unique(gbd$age)
unique(gbd$cause)

age_match<-data.frame(age=20:95)%>%
      mutate(age.group = ifelse(age<25, "20-24 years", NA),
             age.group = ifelse(age>=25 & age<30, "25-29 years", age.group),
             age.group = ifelse(age>=30 & age<35, "30-34 years", age.group),
             age.group = ifelse(age>=35 & age<40, "35-39 years", age.group),
             age.group = ifelse(age>=40 & age<45, "40-44 years", age.group),
             age.group = ifelse(age>=45 & age<50, "45-49 years", age.group),
             age.group = ifelse(age>=50 & age<55, "50-54 years", age.group),
             age.group = ifelse(age>=55 & age<60, "55-59 years", age.group),
             age.group = ifelse(age>=60 & age<65, "60-64 years", age.group),
             age.group = ifelse(age>=65 & age<70, "65-69 years", age.group),
             age.group = ifelse(age>=70 & age<75, "70-74 years", age.group),
             age.group = ifelse(age>=75 & age<80, "75-79 years", age.group),
             age.group = ifelse(age>=80 & age<85, "80-84 years", age.group),
             age.group = ifelse(age>=85 & age<90, "85-89 years", age.group),
             age.group = ifelse(age>=90 & age<95, "90-94 years", age.group),
             age.group = ifelse(age==95, "95+ years", age.group))

#............................................................. 
# Run Calibration by location groups----
#...........................................................

# path to create save_folder
#save_folder <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/calibration_adjusted_searo/"

save_folder <- paste0(wd_temp, "calibration_adjusted_searo/")
  
# if it already exists, delete it (recursively)
if (dir.exists(save_folder)) {
  unlink(save_folder, recursive = F)
}

# now create a fresh one
dir.create(save_folder)

#calibration_path <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/processed/calibration_parallel"

calibration_path <- paste0(wd_temp,"calibration_parallel")

# pre‐compute the renamed GBD table once
gbd2 <- data.table(gbd %>% rename(gbdDeaths = Deaths, gbdPrev = Prevalence))

for(grp in sprintf("%02d", 1:30)) {
  pat <- paste0("group", grp)
  files <- list.files(
    path       = calibration_path,
    pattern    = pat,
    full.names = TRUE
  )
  if(length(files) == 0) {
    warning("No files found for pattern ", pat)
    next
  }
  
  # 1) read and rbind
  dt_list <- lapply(files, function(f) {
    dt <- readRDS(f)
    setDT(dt)
    dt
  })
  out.df <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  
  # 2) apply your CF/IR filter
  out.df <- out.df[CF > 0 & CF <= 0.9 & IR > 0 & IR <= 0.9]
  
  # 3) build the intervention label
  dt_all <- as.data.table(out.df)
  dt_all[, intervention := paste0("IR", IRadjust, "CF", CFadjust)]
  
  # 4) summarize by age.group via age_match
  dt_sum <- dt_all %>%
    left_join(age_match, by = "age") %>%    
    group_by(cause, sex, year, intervention, location, age.group) %>%
    summarise(
      Prevalence = sum(sick,   na.rm = TRUE),
      Deaths     = sum(dead,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(age = age.group) %>%
    as.data.table()
  
  # 5) join to GBD, compute RMSEs
  data2 <- dt_sum[year < 2020] %>%
    left_join(
      gbd2[year < 2020 & cause != "All causes"],
      by = c("year","sex","cause","location","age")
    ) %>%
    group_by(year, intervention, sex, cause, location, age) %>%
    summarise(
      Deaths     = mean(Deaths,   na.rm = TRUE),
      Prevalence = mean(Prevalence, na.rm = TRUE),
      gbdDeaths  = sum(gbdDeaths,  na.rm = TRUE),
      gbdPrev    = sum(gbdPrev,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.table()
  
  data2[, derror := (gbdDeaths - Deaths)^2 ]
  data2[, perror := (gbdPrev   - Prevalence)^2 ]
  
  data_adj <- data2 %>%
    group_by(location, sex, cause, intervention, age) %>%
    summarise(
      RMSE_deaths = sqrt(mean(derror, na.rm = TRUE)),
      RMSE_prev   = sqrt(mean(perror, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    as.data.table()
  
  # 6) pick the best (min error) per cell
  data_adj[, error := 2 * RMSE_deaths + RMSE_prev ]
  best <- data_adj[, .SD[which.min(error)], by = .(location, sex, cause, age)]
  
  # 7) extract IRadjust & CFadjust from the intervention string
  best[, IRadjust := as.numeric(sub(".*IR(.*)CF.*", "\\1", intervention))]
  best[, CFadjust := as.numeric(sub(".*CF(.*)",      "\\1", intervention))]
  
  # 8) select and save
  adjustments <- best[, .(sex, location, cause, IRadjust, CFadjust, age)]
  out_file <- file.path(save_folder, paste0("adjusted_", pat, ".rds"))
  saveRDS(adjustments, out_file)
  
  message("Saved adjustments for ", pat, " to:\n  ", out_file)
}

#...........................................................
# Save final adjustments -----
#...........................................................

# Load adjustments
# List all .rds files in the folder
files <- list.files(
  path       = save_folder, 
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
adjustments <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# merge adjustments with age_match
b_rates<-left_join(b_rates, age_match)%>%
  left_join(adjustments%>%rename(age.group = age))%>%
  mutate(CF = CF*CFadjust,
         IR = IR*IRadjust)%>%
  select(-age.group, -IRadjust, -CFadjust)

b_rates <-b_rates%>%
  mutate(CF = ifelse(is.na(CF), 0, CF),
         IR = ifelse(is.na(IR) > 0.9, 0, IR))
# compute number of rows and chunk size
n     <- nrow(b_rates)
chunk <- ceiling(n / 10)

# loop over the three parts
for (i in 1:10) {
  start <- (i - 1) * chunk + 1
  end   <- min(i * chunk, n)
  
  part <- b_rates[start:end]
  
  saveRDS(
    part,
    file = paste0(wd_data, "adjusted_searo_part", i, ".rds")
  )
}
