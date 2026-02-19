rm(list=ls()) 

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd(paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/GitHub/UW-RTSL-100MLives/"))

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)   
library(countrycode)
library(stringr)
library(parallel)
library(doParallel)
library(foreach)
library(gmodels)

# For forecasting mortality
library(forecast)

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/Github/UW-RTSL-100MLives/"

wd_code <- paste0(wd,"code/")

# Raw data not available on GitHub
wd_raw <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/data/raw/")

# Processed data (from base rates and tps)
wd_data <- paste0(wd,"data/processed/")
wd_outp <- paste0(wd,"output/")

# Create a temporary directory for the processing data change to wd in final version
wd_temp <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/data/","temp/")
if (!dir.exists(wd_temp)) {
  dir.create(wd_temp, recursive = TRUE)
}

setwd(wd_code)

#...........................................................
# 0. Functions and parameters-----
#...........................................................

source("functions_review_6_100.R")
#source("functions_review_6_dm.R")
#source("functions_review_6.R")

run_calibration_par <- TRUE # set to TRUE to run parallel calibration

run_adjustment_model <- TRUE # set to TRUE to run adjustment model

run_aod_par <- FALSE # set to TRUE to run model with dementia

run_adjustments_inputs <- TRUE

run_bgmx_trend <- TRUE

run_CF_trend   <- TRUE

# Baseline scenario 80% of secular trend. 20% historically explained by
# HTN control improvements

run_CF_trend_80   <- TRUE

run_CF_trend_ihme  <- FALSE



# Remove unnecessary dx
# dx_include <- c("All causes","Ischemic heart disease",
#                 "Ischemic stroke","Intracerebral hemorrhage",
#                 "Alzheimer's disease and other dementias","Hypertensive heart disease")

dx_include <- c("All causes",
                "Ischemic heart disease",
                "Ischemic stroke",
                "Intracerebral hemorrhage",
                "Hypertensive heart disease")

cause_map <- c(
  ihd      = "Ischemic heart disease",
  istroke  = "Ischemic stroke",
  hstroke  = "Intracerebral hemorrhage",
  hhd      = "Hypertensive heart disease",
  all      = "All causes"
)

# AFTER  – define the vector once, reuse it
cause_cols <- names(cause_map)

#...........................................................
# 1. Base rates (State and Risks)-----
#...........................................................

source("1.get_base_population_100.R")
source("1.get_base_rates_100.R")

#...........................................................
# 2. Transition Probabilities-----
#...........................................................

source("2.get_tps_100.R")
source("2.get_tps_bgmx_100")

#...........................................................
# 3. Risk Factors-----
#...........................................................

source("3.risk_factors_100.R")

#...........................................................
# 4. Interventions-----
#...........................................................

#source("4.interventions_100.R")
# load tfa2 statins 2
source("4.interventions_100_tfa2_statins2.R")


#...........................................................
# 5. Calibration-----
#...........................................................

source("5.calibration_100.R")

#...........................................................
# 6. Adjustments-----
#...........................................................

source("6.adjustments_100.R")

#...........................................................
# 7. Model-----
#...........................................................

source("7.model_100_multiplicative_func_new3.R")


