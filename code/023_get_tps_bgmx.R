#...........................................................
# Documentation ----
#...........................................................

# The input of this file is the GBD 2019 dmortality ata and the GBD 2019 population estimates.
# The output of this file is a data.csv file with the baseline rates for each country and year.
# The output data set includes the following columns: sex	age	location	year	ALL.mx	BG.mx.all	cause	BG.mx	PREVt0	DIS.mx.t0	Nx


#baseline rates calculated in file calibration:

files <- list.files(
  path       = wd_data, 
  pattern    = "tps", 
  full.names = TRUE
)

dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  setDT(dt)  # convert to data.table by reference if it isn't already
  dt
})

dt_bgmx <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

# Keep only backgroun mortality and demographics

dt_bgmx <- dt_bgmx[location!="Global",c("location","year","age","sex","cause",
                       "BG.mx", "Nx","BG.mx.all", "ALL.mx","DIS.mx.t0"),with=F]

# Testing: global average

dt <- dt_bgmx[,list(BG.mx=weighted.mean(BG.mx,Nx),
                    BG.mx.all=weighted.mean(BG.mx.all,Nx)),by=list(year,age,sex,cause)]

dt[,location:="Global"]

library(StMoMo)
library(demography)
library(forecast)   # for auto.arima + forecast

# ============================================================
# Adult mortality forecast (ages 20–95+) by location/sex/cause
# Train: 2000–2015  |  Validate (RMSE): 2016–2019  |  Project: 2016–2050
# Input dt columns: location, sex, cause, year, age, BG.mx  (rates)
# ============================================================
# ---------- Utilities ----------
# Build an ages x years rate matrix with proper dimnames
make_rate_matrix <- function(d, ages, years) {
  # ensure full grid (mean in case of duplicates)
  m <- tapply(d$BG.mx, list(d$age, d$year), mean)
  # coerce to requested rectangular grid
  row_missing <- setdiff(ages, as.numeric(rownames(m)))
  col_missing <- setdiff(years, as.numeric(colnames(m)))
  if (length(row_missing)) {
    m <- rbind(m, matrix(NA_real_, nrow = length(row_missing), ncol = ncol(m),
                         dimnames = list(as.character(row_missing), colnames(m))))
  }
  if (length(col_missing)) {
    m <- cbind(m, matrix(NA_real_, nrow = nrow(m), ncol = length(col_missing),
                         dimnames = list(rownames(m), as.character(col_missing))))
  }
  m <- m[order(as.numeric(rownames(m))), order(as.numeric(colnames(m))), drop = FALSE]
  # guard against zeros/negatives
  m[m <= 0 | is.na(m)] <- NA_real_
  m
}

# Classical LC on log-rates via SVD
lc_fit_on_rates <- function(mx_mat) {
  logm <- log(mx_mat)
  ax   <- rowMeans(logm, na.rm = TRUE)
  # center by ax, then SVD on complete cases across years
  M <- sweep(logm, 1, ax, "-")
  # drop columns with any NA to compute SVD robustly
  good_cols <- which(colSums(!is.na(M)) == nrow(M))
  if (!length(good_cols)) stop("No complete years to run SVD.")
  Ms <- M[, good_cols, drop = FALSE]
  sv <- svd(Ms)
  bx <- sv$u[, 1]
  kt_partial <- sv$d[1] * sv$v[, 1]
  # Expand kt to all years using projection
  kt <- rep(NA_real_, ncol(M))
  kt[good_cols] <- kt_partial
  # normalize so sum(bx)=1
  s <- sum(bx)
  bx <- bx / s
  kt <- kt * s
  list(ax = ax, bx = bx, kt = kt, good_cols = good_cols)
}

# Forecast kt with ARIMA and reconstruct full log m(x,t)
lc_forecast <- function(ax, bx, kt, years_train, last_proj_year) {
  # Fit ARIMA on kt where it’s observed
  obs_idx <- which(!is.na(kt))
  kt_obs  <- kt[obs_idx]
  yrs_obs <- years_train[obs_idx]
  # fit.kt  <- auto.arima(kt_obs)
  # h_years <- last_proj_year - max(years_train)
  # kt_fc   <- forecast(fit.kt, h = h_years)
  # kt_all  <- c(kt_obs, as.numeric(kt_fc$mean))
  # Fit ETS with damped trend on kt
  fit.kt  <- forecast::ets(kt_obs, model = "AAN", damped = TRUE)
  h_years <- last_proj_year - max(years_train)
  kt_fc   <- forecast::forecast(fit.kt, h = h_years)
  kt_all  <- c(kt_obs, as.numeric(kt_fc$mean))
  
  years_all <- c(yrs_obs, max(years_train) + seq_len(h_years))
  
  # Reconstruct log m for observed + forecast years
  logm_all <- ax + bx %o% kt_all
  dimnames(logm_all) <- list(age = names(ax), year = as.character(years_all))
  
  list(logm_all = logm_all, years_all = years_all, kt_fit = kt_obs, kt_fc = kt_fc)
}

# ---------- Per-group wrapper ----------
fit_forecast_group <- function(dg) {
  # TRAIN / TEST windows
  train_years <- 2000:2015
  test_years  <- 2016:2019
  proj_until  <- 2050
  
  # ages present in training (and within 20–95)
  ages_all <- sort(unique(dg$age))
  ages_use <- ages_all[ages_all >= 20 & ages_all <= 95]
  
  # Build matrices
  mx_train <- make_rate_matrix(dg[year %in% train_years], ages_use, train_years)
  mx_test  <- make_rate_matrix(dg[year %in% test_years],  ages_use, test_years)
  
  # Drop ages with any NA in training (need complete across train years for SVD)
  keep_age <- which(rowSums(is.na(mx_train)) == 0)
  if (!length(keep_age)) return(list(rmse = NA_real_, proj = data.table()))
  mx_train <- mx_train[keep_age, , drop = FALSE]
  mx_test  <- mx_test[rownames(mx_train), , drop = FALSE]
  
  # Fit LC on training
  lc <- lc_fit_on_rates(mx_train)
  
  # Forecast to 2050
  yrs_train <- as.integer(colnames(mx_train))
  fc <- lc_forecast(lc$ax, lc$bx, lc$kt, yrs_train, proj_until)
  
  # ----- RMSE on 2016–2019 (log scale) -----
  # Predicted for test years = using kt fitted/projection aligned to test years
  # We’ll rebuild log m for ALL available years (fitted+forecast) then pick 2016–2019
  # First, reconstruct fitted for train (using kt where observed)
  logm_train_fit <- lc$ax + lc$bx %o% (lc$kt[match(yrs_train, yrs_train)])
  dimnames(logm_train_fit) <- list(age = rownames(mx_train), year = as.character(yrs_train))
  
  # Combine fitted train and forecast years in one matrix
  # fc$logm_all already has dimnames and includes only years with kt series used in ARIMA fit + forecasts.
  # We only need test years 2016–2019:
  need_cols <- as.character(test_years)
  # Build a helper matrix with those years, using fc where available
  # If any requested test year not in fc$logm_all (rare), skip RMSE for that year
  has_cols <- need_cols[need_cols %in% colnames(fc$logm_all)]
  if (length(has_cols)) {
    logm_hat_test <- fc$logm_all[rownames(mx_test), has_cols, drop = FALSE]
    # align observed
    obs_test <- log(mx_test[, has_cols, drop = FALSE])
    rmse <- sqrt(mean((obs_test - logm_hat_test)^2, na.rm = TRUE))
  } else {
    rmse <- NA_real_
  }
  
  # ----- Projections 2016–2050 -----
  proj_years <- (max(train_years) + 1L):proj_until
  mat_proj <- exp(fc$logm_all[, as.character(proj_years), drop = FALSE])
  
  proj_dt <- as.data.table(as.table(mat_proj))
  setnames(proj_dt, c("age", "year", "BG.mx_proj"))
  proj_dt[, age  := as.numeric(as.character(age))]
  proj_dt[, year := as.integer(as.character(year))]
  setorder(proj_dt, age, year)
  
  list(rmse = rmse, proj = proj_dt)
}

# ---------- run bgmx ----------
# Expecting 'dt' in environment with needed columns
stopifnot(all(c("location","sex","cause","year","age","BG.mx") %in% names(dt)))
dt <- as.data.table(dt)
dt[, `:=`(year = as.integer(year), age = as.integer(age))]
setkey(dt, location, sex, cause, year, age)

res <- dt[, {
  out <- fit_forecast_group(.SD)
  .(rmse = out$rmse, proj = list(out$proj))
}, by = .(location, sex, cause)]

# ---------- Outputs -
# 1) projections long table (2016–2050)
projections <- res[, rbindlist(proj), by = .(location, sex, cause)]
# columns: location, sex, cause, age, year, BG.mx_proj

# 2) RMSE per group (2016–2019)
rmse_by_group <- res[, .(location, sex, cause, rmse)]

# (optional) attach rmse to each projected row
projections_with_rmse <- res[, {
  p <- rbindlist(proj)
  p[, rmse := rmse[1L]]
  p
}, by = .(location, sex, cause)]


# consolidate one table

setnames(projections_with_rmse,c("BG.mx_proj"),c("BG.mx"))
dt <- rbind(dt,projections_with_rmse[year>2019,],fill=T)

# ggplot time series of bg.mx by age, wrapped by cause for Female

g <- ggplot(dt[sex=="Female" & age==80,], aes(x = year, y = BG.mx, color = factor(age), group = age)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ cause, scales = "free_y") +
  scale_y_continuous(trans = "log10") +   # mortality rates often log-scaled
  labs(
    title = "Projected Background Mortality Rates (Female, 2016–2050)",
    x = "Year",
    y = "Mortality rate (BG.mx, log scale)",
    color = "Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
g  
  

# save projections
dt$rmse <- NULL

# Compute the percentage decline from base year
dt_2019 <- dt[year==2019,]

dt_2019$year <- NULL

setnames(dt_2019,c("BG.mx"),c("BG.mx_2019"))
dt <- merge(dt,dt_2019,all.x = T)

# Assuming dt is your data.table and BG.mx is the column of interest
# Compute lagged percent difference vs previous year
dt[, percent_lag := (BG.mx - shift(BG.mx, 1L, type = "lag")) / shift(BG.mx, 1L, type = "lag"), by = .(sex, cause, location)]
dt[, percent_diff := (BG.mx - BG.mx_2019) / BG.mx_2019]

dt[,c("BG.mx_2019","location","BG.mx"):=NULL]

saveRDS(dt, file = paste0(wd_data,"tps_bgmx_forecasted.rds"))

## run all dead envelope------

dt <- dt_bgmx[,list(BG.mx=weighted.mean(BG.mx.all,Nx)),by=list(year,age,sex,cause)]

dt[,location:="Global"]


stopifnot(all(c("location","sex","cause","year","age","BG.mx") %in% names(dt)))

dt <- as.data.table(dt)
dt[, `:=`(year = as.integer(year), age = as.integer(age))]
setkey(dt, location, sex, cause, year, age)

res <- dt[, {
  out <- fit_forecast_group(.SD)
  .(rmse = out$rmse, proj = list(out$proj))
}, by = .(location, sex, cause)]

# ---------- Outputs 
# 1) projections long table (2016–2050)
projections <- res[, rbindlist(proj), by = .(location, sex, cause)]
# columns: location, sex, cause, age, year, BG.mx_proj

# 2) RMSE per group (2016–2019)
rmse_by_group <- res[, .(location, sex, cause, rmse)]

# (optional) attach rmse to each projected row
projections_with_rmse <- res[, {
  p <- rbindlist(proj)
  p[, rmse := rmse[1L]]
  p
}, by = .(location, sex, cause)]


# consolidate one table

setnames(projections_with_rmse,c("BG.mx_proj"),c("BG.mx"))
dt <- rbind(dt,projections_with_rmse[year>2019,],fill=T)

# ggplot time series of bg.mx by age, wrapped by cause for Female

g <- ggplot(dt[sex=="Female" & age==80,], aes(x = year, y = BG.mx, color = factor(age), group = age)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ cause, scales = "free_y") +
  scale_y_continuous(trans = "log10") +   # mortality rates often log-scaled
  labs(
    title = "Projected Background Mortality Rates (Female, 2016–2050)",
    x = "Year",
    y = "Mortality rate (BG.mx, log scale)",
    color = "Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
g  


# save projections
dt$rmse <- NULL

# Compute the percentage decline from base year
dt_2019 <- dt[year==2019,]

dt_2019$year <- NULL

setnames(dt_2019,c("BG.mx"),c("BG.mx_2019"))
dt <- merge(dt,dt_2019,all.x = T)

# Assuming dt is your data.table and BG.mx is the column of interest
# Compute lagged percent difference vs previous year
dt[, percent_lag := (BG.mx - shift(BG.mx, 1L, type = "lag")) / shift(BG.mx, 1L, type = "lag"), by = .(sex, cause, location)]
dt[, percent_diff := (BG.mx - BG.mx_2019) / BG.mx_2019]

dt[,c("BG.mx_2019","location","BG.mx"):=NULL]

saveRDS(dt, file = paste0(wd_data,"tps_bgmx_all_forecasted.rds"))

## run all CVD trend------

# dt <- dt_bgmx[,list(ALL.mx=weighted.mean(ALL.mx,Nx),
#                     BG.mx.all=weighted.mean(BG.mx.all,Nx)),by=list(year,age,sex,cause)]
# 
# dt[,BG.mx:=ALL.mx-BG.mx.all]
#dt[,c("ALL.mx","BG.mx.all"):=NULL]

dt <- dt_bgmx[,list(BG.mx=weighted.mean(DIS.mx.t0,Nx)),by=list(year,age,sex,cause)]

dt[,location:="Global"]

stopifnot(all(c("location","sex","cause","year","age","BG.mx") %in% names(dt)))

dt <- as.data.table(dt)
dt[, `:=`(year = as.integer(year), age = as.integer(age))]
setkey(dt, location, sex, cause, year, age)

res <- dt[, {
  out <- fit_forecast_group(.SD)
  .(rmse = out$rmse, proj = list(out$proj))
}, by = .(location, sex, cause)]

# ---------- Outputs 
# 1) projections long table (2016–2050)
projections <- res[, rbindlist(proj), by = .(location, sex, cause)]
# columns: location, sex, cause, age, year, BG.mx_proj

# 2) RMSE per group (2016–2019)
rmse_by_group <- res[, .(location, sex, cause, rmse)]

# (optional) attach rmse to each projected row
projections_with_rmse <- res[, {
  p <- rbindlist(proj)
  p[, rmse := rmse[1L]]
  p
}, by = .(location, sex, cause)]


# consolidate one table

setnames(projections_with_rmse,c("BG.mx_proj"),c("BG.mx"))
dt <- rbind(dt,projections_with_rmse[year>2019,],fill=T)

# ggplot time series of bg.mx by age, wrapped by cause for Female

g <- ggplot(dt[sex=="Female" & age==70,], aes(x = year, y = BG.mx, color = factor(age), group = age)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ cause, scales = "free_y") +
  scale_y_continuous(trans = "log10") +   # mortality rates often log-scaled
  labs(
    title = "Projected Background Mortality Rates (Female, 2016–2050)",
    x = "Year",
    y = "Mortality rate (BG.mx, log scale)",
    color = "Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
g  


# save projections
dt$rmse <- NULL

# Compute the percentage decline from base year
dt_2019 <- dt[year==2019,]

dt_2019$year <- NULL

setnames(dt_2019,c("BG.mx"),c("BG.mx_2019"))
dt <- merge(dt,dt_2019,all.x = T)

# Assuming dt is your data.table and BG.mx is the column of interest
# Compute lagged percent difference vs previous year
dt[, percent_lag := (BG.mx - shift(BG.mx, 1L, type = "lag")) / shift(BG.mx, 1L, type = "lag"), by = .(sex, cause, location)]
dt[, percent_diff := (BG.mx - BG.mx_2019) / BG.mx_2019]

dt[,c("BG.mx_2019","location","BG.mx"):=NULL]

saveRDS(dt, file = paste0(wd_data,"tps_bgmx_cvd_forecasted.rds"))


## run all CVD IHME Foresight------

# IHME
files <- list.files(path = "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/raw/GBD", 
                    pattern = "gbd2023_foresight_asr_.*20\\+\\.csv$", 
                    full.names = TRUE)

# Read and bind them
dt_gbd_fore <- rbindlist(lapply(files, fread), fill = TRUE, idcol = "source")

setnames(dt_gbd_fore, 
         tolower(gsub(" ", "_", names(dt_gbd_fore))))

dt_gbd_fore <- dt_gbd_fore[year>=2000 & scenario %in% c("Past","Reference") & !is.na(value),
                           c("year","cause_of_death_or_injury","value"),with=F]

dt_gbd_fore[,source:="GBD Foresight"]

setnames(dt_gbd_fore,c("cause_of_death_or_injury","value"),c("cause","ASMR"))

dt_gbd_fore[, cause := fcase(
  cause == "Ischemic heart disease", "ihd",
  cause == "Ischemic stroke", "istroke",
  cause == "Intracerebral hemorrhage", "hstroke",
  cause == "Hypertensive heart disease", "hhd",
  cause == "Alzheimer's disease and other dementias", "aod",
  default = cause
)]

dt_2019 <- dt_gbd_fore[year==2019,]
dt_2019[,year:=NULL]

setnames(dt_2019,"ASMR","ASMRB")

dt <- merge(dt_gbd_fore,dt_2019,all.x = T)

dt[, percent_diff := (ASMR - ASMRB) / ASMRB]

dt <- dt[,c("year","cause","percent_diff"), with= F]

saveRDS(dt, file = paste0(wd_data,"tps_bgmx_cvd_ihme.rds"))




