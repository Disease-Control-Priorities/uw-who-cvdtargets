#..................................................................
#
# adjustments----
#..................................................................

# conditional to run inputs for adjustments inputs

#run_adjustments_inputs <- FALSE

setwd(wd)
#Load data

if(run_adjustments_inputs) {

  pop20 <- fread(paste0(wd_data,"PopulationsAge20_full.csv"))
  pop20<-pop20[year_id>=2010 & year_id<=2050]
  
  #baseline rates calculated in file:
  #b_rates<-fread("../2. get AARcs/baseline_rates_new_new2019.csv")
  
  #baseline rates calculated in file calibration:
  
  files <- list.files(
    path       = wd_data, 
    pattern    = "adjusted", 
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
  
  b_rates[location=="United States of America",location:="United States"]
  b_rates[location=="Bolivia (Plurinational State of)",location:="Bolivia"]
  b_rates[location=="United Republic of Tanzania",location:="Tanzania"]
  
  b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                     by=c("location", "year", "sex", "age"))%>%
    mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
    select(-c(Nx2))
  
  #can't have >1 for TP rows
  b_rates<-b_rates%>%mutate(test = ifelse(IR+BG.mx>1,1,0))%>%
    mutate(IR2 = ifelse(test==1, (IR/(IR+BG.mx))-0.005, IR),
           BG.mx2 = ifelse(test==1, (BG.mx/(IR+BG.mx))-0.005, BG.mx))%>%
    mutate(test2=ifelse(IR2+BG.mx2>1,1,0))%>%
    mutate(test=ifelse(CF+BG.mx2>1,1,0))%>%
    mutate(CF2 = ifelse(test==1, (CF/(CF+BG.mx2))-0.005, CF),
           BG.mx3 = ifelse(test==1, BG.mx2/(CF+BG.mx2)-0.005, BG.mx2))%>%
    mutate(test3 = ifelse(CF2+BG.mx3>1,1,0))%>%
    select(-c(IR,CF,BG.mx,BG.mx2, test, test2, test3))%>%
    rename(IR = IR2, CF = CF2, BG.mx = BG.mx3)
  
  # keep from 2010 to 2022
  b_rates <- b_rates[year >= 2010 & year <= 2022]
  
  rep_rates<-b_rates[year==2010]
  
  #repeat year 2017 to 2019
  repYear2010<-function(row){
    #2010+floor((row-1)/118560)
    2010+floor((row-1)/nrow(rep_rates))
  }
  
  b_rates<-rep_rates[rep(seq(1,nrow(rep_rates)),11)][, year:=repYear2010(.I)]
  
  check_rep_rates <- b_rates[,list(N=.N),by=list(location,year)]
  
  setnames(pop20, c("year_id", "Nx"), c("year", "Nx20"))
  
  # countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global", 
  #                                                                               location!="American Samoa",location!="Andorra",location!= "Bermuda",
  #                                                                               location!= "Dominica", location!="Greenland", location!="Marshall Islands",
  #                                                                               location!="Northern Mariana Islands", location!="Palestine",
  #                                                                               location!="Taiwan (Province of China)", location!="Guam", location!="Puerto Rico",
  #                                                                               location!="South Sudan", location!="Virgin Islands, U.S.")%>%pull(location)
  # 
  # b_rates<-b_rates[location %in% countrylist]
  # pop20<-pop20[location %in% countrylist]
  
  #baseline function
  
  ################## run baseline projections ################################
  
  #Remove locations that are not in the rates data
  #missing_locs <- setdiff(unique(b_rates$location), unique(pop20$location)) 
  missing_locs <- setdiff(unique(pop20$location),unique(b_rates$location)) 
  pop20 <- pop20[!location %in% missing_locs, ]
  
  # remove tonga and tokeloau (not population after 2023 in UNWPP data)
  
  pop20 <- pop20[!(location %in% c("Tonga", "Tokelau")), ]
  b_rates <- b_rates[!(location %in% c("Tonga", "Tokelau")), ]
  
  
  #as a function#
  state.transition<-function(b_rates, pop20, IRadjust, CFadjust){ 
    
    base_rates<-merge(b_rates, pop20, by=c("year", "location", "sex", "age"), all=TRUE)
    base_rates[age==20 & year>2010, Nx:=Nx20]
    base_rates[, Nx20:=NULL]
    
    ## calculate initial states for the incoming year 2000 and all years for age 20 population
    base_rates[year==2010 | age==20, sick:=Nx*PREVt0]
    base_rates[year==2010 | age==20, dead:=Nx*DIS.mx.t0]
    base_rates[year==2010 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
    
    #base_rates<-base_rates[location %in% countrylist]
    base_rates[age==20 | year==2010, pop:=Nx]
    base_rates[age==20 | year==2010, all.mx:=Nx*ALL.mx]
    
    base_rates[, IR:=IR*IRadjust]
    base_rates[, CF:=CF*CFadjust]
    
    base_rates[,IRadjust:=IRadjust]
    base_rates[,CFadjust:=CFadjust]
    
    base_rates[CF>0.99, CF:=0.99]
    base_rates[IR>0.99, IR:=0.99]
    #STATE TRANSITIONS#
    for(i in 1:10){
      
      cat(paste0("Year: ", 2010+i, "\n"))
      
      b2<-base_rates[year<=2010+i & year>=2010+i-1]
      b2[,age2:=age+1]
      
      #sick
      b2[, sick2:=shift(sick)*(1-(CF+BG.mx)) + shift(well)*IR, by=.(sex, location, cause, age)]
      #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
      b2[sick2<0, sick2:=0]
      
      #dead
      b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age)]
      #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
      b2[dead2<0, sick2:=0]
      
      #pop
      b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age)]
      #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
      b2[pop2<0, pop2:=0] #prevent negatives
      
      #all dead
      b2[, all.mx2:=sum(dead2), by=.(sex, location, year, age)]
      b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)]
      b2[all.mx2<0, all.mx2:=0]
      
      #well
      b2[, well2:=pop2-all.mx2-sick2]
      b2[well2<0, well2:=0] #prevent negatives
      
      #re-combined into original data.table
      b2<-b2[year==2010+i & age2<96, c("age2", "sick2", "dead2", "well2", "pop2", "all.mx2", "sex", "location", "cause")]
      setnames(b2, "age2", "age")
      base_rates[year==2010+i & age>20, sick:=b2[,sick2]]
      base_rates[year==2010+i & age>20, dead:=b2[,dead2]]
      base_rates[year==2010+i & age>20, well:=b2[,well2]]
      base_rates[year==2010+i & age>20, pop:=b2[,pop2]]
      base_rates[year==2010+i & age>20, all.mx:=b2[,all.mx2]]
      
    }
    
    base_rates
  }
  
  
  # #Run reps for adjustments file
  # Sys.time()
  # out.df<-state.transition(b_rates, pop20, 1.05, 1.05)
  # 
  # for(i in 0:6){
  #   for(j in 0:6){
  #     cat(paste0("IR: ", i, " CF: ", j, "\n"))
  #     out.df2<-state.transition(b_rates, pop20, 1-(0.05*i), 1-(0.05*j))
  #     out.df<-rbindlist(list(out.df, out.df2))
  #   }
  # }
  # 
  # Sys.time()
  
  # use all available threads
  setDTthreads(1)
  #setDTthreads(0)
  
  # assume b_rates and pop20 are already loaded data.tables in your workspace:
  #   b_rates: columns year, location, sex, age, PREVt0, DIS.mx.t0, ALL.mx, IR, CF, BG.mx, BG.mx.all
  #   pop20:   columns year, location, sex, age=20, Nx20
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 1. Build immutable base template 
  # ────────────────────────────────────────────────────────────────────────────────
  
  base0 <- merge(
    b_rates,
    pop20,
    by = c("year", "location", "sex", "age"),
    all = TRUE
  )[, `:=`(
    # overwrite Nx for age 20 cohorts after 2010, then drop Nx20
    Nx = fifelse(age == 20 & year > 2010, Nx20, Nx),
    Nx20 = NULL
  )]
  
  # initialize states for year 2010 & all age-20 rows
  base0[year == 2010 | age == 20, `:=`(
    sick   = Nx * PREVt0,
    dead   = Nx * DIS.mx.t0,
    well   = Nx * (1 - (PREVt0 + ALL.mx)),
    pop    = Nx,
    all.mx = Nx * ALL.mx
  )]
  
  # key for fast grouped operations
  setkey(base0, sex, location, cause, age, year)
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 2. Yearly update function 
  # ────────────────────────────────────────────────────────────────────────────────
  
  update_year <- function(dt) {
    # compute “previous-year” values
    dt[, `:=`(
      sick_p  = shift(sick),
      well_p  = shift(well),
      pop_p   = shift(pop),
      allmx_p = shift(all.mx)
    ), by = .(sex, location, cause, age)]
    
    # only update for years > 2010
    dt[year > 2010, `:=`(
      sick = pmax(sick_p * (1 - (CF + BG.mx)) + well_p * IR, 0),
      dead = pmax(sick_p * CF, 0),
      pop  = pmax(pop_p - allmx_p, 0)
    )]
    
    # recompute all-cause mortality and well
    dt[, all.mx := dead + pop * BG.mx.all]
    dt[, well   := pmax(pop - sick - all.mx, 0)]
    
    # clean up helper columns
    dt[, c("sick_p", "well_p", "pop_p", "allmx_p") := NULL]
    
    invisible(dt)
  }
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 3. Forward runner 
  # ────────────────────────────────────────────────────────────────────────────────
  
  run_forward <- function(dt, n_years = 10L) {
    for (i in seq_len(n_years)) {
      update_year(dt)
    }
    dt
  }
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 4. Scenario function 
  # ────────────────────────────────────────────────────────────────────────────────
  
  state.transition.fast <- function(IRmult, CFmult, base_template) {
    # copy the base template
    dt <- copy(base_template)
    
    # apply multipliers & cap at 0.99
    dt[, `:=`(
      IR       = pmin(IR * IRmult, 0.99),
      CF       = pmin(CF * CFmult, 0.99),
      IRadjust = IRmult,
      CFadjust = CFmult
    )]
    
    # run the 10-year projection in-place
    run_forward(dt, n_years = 10L)
  }
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 5. Run all scenarios 
  # ────────────────────────────────────────────────────────────────────────────────
  
  # define IR and CF grids: from 1.00 down to 0.65 in steps of 0.05
  grid <- CJ(
    IRmult = 1 - 0.05 * 0:4,
    CFmult = 1 - 0.05 * 0:10
  )
  
  # # define IR and CF grids: from 1.00 down to 0.65 in steps of 0.05
  # grid <- CJ(
  #   IRmult = 1 - 0.05 * 0:7,
  #   CFmult = 1 - 0.05 * 0:7
  # )
  
  # optional timing
  start_time <- Sys.time()
  
  # run each scenario (parallelizable with future.apply or BiocParallel if desired)
  out_list <- lapply(seq_len(nrow(grid)), function(i) {
    with(grid[i], state.transition.fast(IRmult, CFmult, base0))
  })
  
  # bind once
  out.df <- rbindlist(out_list, idcol = "scenario_id")
  
  end_time <- Sys.time()
  message("Completed ", nrow(grid), " scenarios in ",
          round(difftime(end_time, start_time, units = "secs"), 1), " seconds.")
  
  # ────────────────────────────────────────────────────────────────────────────────
  # 6. Save or return -
  # ────────────────────────────────────────────────────────────────────────────────
  
  out.df <- out.df[!is.na(cause),c("scenario_id","year","location","sex","age","cause","IR","CF",
                                   "sick", "dead", "all.mx", "IRadjust", "CFadjust"),with=F]
  
  saveRDS(out.df, file = paste0(wd_temp,"out_df_adjusted.rds"))
  
  ##Load GBD data##----
  
  # tracking locations during all steps
  locs <- readRDS(file = paste0(wd,"locs.rds"))
  
  locs <- as.vector(locs$location)
  #data permalink
  #https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/463a40833819742df724b697ba2cc03f
  
  # dt<-bind_rows(read.csv("IHME-GBD_2019_DATA-d0d7a8c8-1.csv", stringsAsFactors = F),
  #               read.csv("IHME-GBD_2019_DATA-d0d7a8c8-2.csv", stringsAsFactors = F))%>%
  #       filter(location!="Global")
  
  #gbd <- rbindlist(lapply(sprintf(paste0(wd_raw,"GBD/","IHME-GBD_2021_DATA-fc833c60-%d.csv"), 1:21), fread),
  #                   use.names = TRUE, fill = TRUE)
  gbd <- readRDS(file = paste0(wd_raw,"GBD/","temp_1baseline_rates_gbd23.rds"))
  gbd <- data.table(gbd)

  
  # Remove unnecessary dx
  dx_include <- c("All causes","Ischemic heart disease",
                  "Ischemic stroke","Intracerebral hemorrhage",
                  "Alzheimer's disease and other dementias","Hypertensive heart disease")                            
  
  # Filter the data to include only the specified causes
  gbd <- gbd[cause_name %in% dx_include,]
  
  setnames(gbd,c("sex_name","age_name","cause_name","measure_name","metric_name","location_name")
           ,c("sex","age","cause","measure","metric","location"))
  
  gbd <- gbd[location!="Global",]
  gbd[, upper:=NULL]
  gbd[, lower:=NULL]
  
  gbd <- gbd[, c("measure","location","sex","age","cause","metric","year","val"),with=FALSE]
  
  # Fix countries names
  gbd[location == "Türkiye", location := "Turkey"]
  gbd[location == "Côte d'Ivoire", location := "Ivory Coast"]
  
  # select 2010:2020
  gbd <- gbd[year >= 2010 & year <= 2020,]
  
  gbd <- gbd[metric=="Number",]
  
  gbd$metric <- NULL
  
  gbd <- reshape(gbd,idvar = c("location","year","age","sex","cause"),
                 timevar="measure",direction = "wide")
  
  setnames(gbd, c("val.Deaths", "val.Prevalence"), c("gbdDeaths", "gbdPrev"))
  
  # asignend 0 to missing alzheime r's disease and other dementias
  gbd[is.na(gbdDeaths) & cause == "Alzheimer's disease and other dementias", gbdDeaths := 0]
  gbd[is.na(gbdPrev) & cause == "Alzheimer's disease and other dementias", gbdPrev := 0]

  # Apply mapping
  gbd[cause=="Alzheimer's disease and other dementias", cause:="aod"]
  gbd[cause=="Hypertensive heart disease", cause:="hhd"]
  gbd[cause=="Ischemic heart disease", cause:="ihd"]
  gbd[cause=="Ischemic stroke", cause:="istroke"]
  gbd[cause=="Intracerebral hemorrhage", cause:="hstroke"]
  gbd[cause=="All causes", cause:="all"]
  
  saveRDS(gbd, file = paste0(wd_temp,"gbd2021_adj.rds"))
  
  # cleaning
  rm(gbd,out.df)
  
  # Combine model insample results and GBD data----
  
  dt  <- readRDS(paste0(wd_temp,"out_df_adjusted.rds"))
  
  dt[cause=="Alzheimer's disease and other dementias", cause:="aod"]
  dt[cause=="Hypertensive heart disease", cause:="hhd"]
  dt[cause=="Ischemic heart disease", cause:="ihd"]
  dt[cause=="Ischemic stroke", cause:="istroke"]
  dt[cause=="Intracerebral hemorrhage", cause:="hstroke"]
  dt[cause=="All causes", cause:="all"]
  
  
  
  #setnames(dt, c("sick", "dead"), c("Prevalence", "Deaths"))
  
  # create GBD age groups
  
  gbd_breaks <- c(seq(20, 95, by = 5), Inf)
  gbd_labels <- c(
    paste0(seq(20, 90, by = 5), "-", seq(24, 94, by = 5)),
    "95+"
  )
  
  # 2) (Optionally) wrap in a helper
  create_gbd_age_group <- function(age) {
    cut(
      age,
      breaks        = gbd_breaks,
      labels        = gbd_labels,
      right         = FALSE,      # [20,25), [25,30), …, [95,Inf)
      include.lowest = TRUE
    )
  }
  
  # # 3) Apply to your data.table
  dt[, age_group := create_gbd_age_group(age)]
  
  # group by age group,location,year,sex,cause,scenario_id
  dt <- dt[
    , .(
      Deaths      = sum(dead),
      Prevalence  = sum(sick),
      All         = sum(all.mx)
    ),
    by = .(age_group, location, year, sex, cause, scenario_id)
  ]
  
  saveRDS(dt,paste0(wd_temp,"out_df_adjusted_grouped.rds"))
  
}

dt  <- readRDS(paste0(wd_temp,"out_df_adjusted_grouped.rds"))

gbd <- readRDS(paste0(wd_temp,"gbd2021_adj.rds"))

gbd[,age_group := gsub(" years", "", age)]

gbd$age <- NULL
dt[,age_group := as.character(age_group)]

# join with gbd data
dt <- merge(dt,gbd,by=c("age_group", "sex", "cause", "location", "year"),all.x=T)

dt[is.na(gbdDeaths),gbdDeaths := 0]
dt[is.na(gbdPrev),gbdPrev := 0]

###### root mean square error ########
data.adj <- dt[  ,  .(
  RMSE_deaths = sqrt(mean((gbdDeaths   - Deaths    )^2)),
  RMSE_prev   = sqrt(mean((gbdPrev      - Prevalence)^2))
),
by = .(location, sex,age_group, cause, scenario_id)
][ , error := 2 * RMSE_deaths + RMSE_prev]

test<-data.adj[ , .SD[which.min(error)], by=.(location, sex,age_group, cause)]

# define IR and CF grids: from 1.00 down to 0.65 in steps of 0.05
# grid <- CJ(
#   IRadjust = 1 - 0.05 * 0:7,
#   CFadjust = 1 - 0.05 * 0:7
# )

grid <- CJ(
  IRadjust  = 1 - 0.05 * 0:4,
  CFadjust  = 1 - 0.05 * 0:10
)

# add a sequence ID
grid[, scenario_id := .I]

# (optionally) move scenario_id to the front
setcolorder(grid, c("scenario_id", "IRadjust", "CFadjust"))


adjustments <- merge(test,grid,all.x=T)


##write out adjustments

write.csv(adjustments, paste0(wd,"adjustments2023_age.csv"), row.names = F)
