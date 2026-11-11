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




## Coherent all-cause / modeled-cause / other-cause forecast -----------------
# Li et al. (2019), Insurance: Mathematics and Economics 86:122-133,
# doi:10.1016/j.insmatheco.2019.02.011, sections 3.2 and 4.1: a Lee-Carter base
# forecast for every series in the hierarchy, then MinT reconciliation by sex
# and age on the rate scale. This is a separate forecast; the four trend files
# above are unchanged.
#
# The modeled causes are the four CVD causes in cause_map (00_run_model.R);
# dementia is not part of this project. Hierarchy at every sex/age/year:
#   ALL.mx = ihd + istroke + hstroke + hhd + BG.mx.all
# Inputs from tps_inpt (021/022):
#   DIS.mx.t0 = GBD deaths from the cause / population (a population rate,
#               not a case-fatality hazard among prevalent cases)
#   BG.mx     = ALL.mx - DIS.mx.t0 of the cause on that row
# BG.mx.all is built here as ALL.mx - sum(CVD). The BG.mx.all column in
# tps_inpt is not used: 021_get_base_rates.R defines its own cause_map with
# Alzheimer's disease and other dementias, so that column also excludes
# dementia deaths. The difference is reported below.

coherent_cause_map <- c(
  "Ischemic heart disease" = "ihd", "ihd" = "ihd",
  "Ischemic stroke" = "istroke", "istroke" = "istroke",
  "Intracerebral hemorrhage" = "hstroke", "hstroke" = "hstroke",
  "Hypertensive heart disease" = "hhd", "hhd" = "hhd"
)
coherent_cvd_causes <- c("ihd", "istroke", "hstroke", "hhd")
coherent_bottom <- c(coherent_cvd_causes, "BG.mx.all")
coherent_components <- c("ALL.mx", coherent_bottom)
coherent_sexes <- c("Female", "Male")
coherent_years <- 2000:2019
coherent_ages <- 20:95
coherent_until <- 2050L
coherent_training_end <- 2015L
coherent_holdout_years <- (coherent_training_end + 1L):max(coherent_years)
coherent_keys <- c("location", "sex", "year", "age")
# Relative tolerance for the additive identities (some rates are ~1e-7).
coherent_rel_tol <- 1e-10

# Read the calibration parts by name: on a rerun, the pattern = "tps" listing
# at the top of this script also returns the trend files and these outputs.
coherent_files <- list.files(wd_data, pattern = "^tps_inpt_part[0-9]+\\.rds$",
                             full.names = TRUE)
if (!length(coherent_files)) {
  stop("Coherent forecast: no tps_inpt_part*.rds files in ", wd_data)
}
coherent_input <- data.table::rbindlist(lapply(coherent_files, readRDS),
                                        use.names = TRUE, fill = TRUE)
coherent_cols <- c(coherent_keys, "cause", "Nx", "ALL.mx", "DIS.mx.t0",
                   "BG.mx", "BG.mx.all")
coherent_missing_cols <- setdiff(coherent_cols, names(coherent_input))
if (length(coherent_missing_cols)) {
  stop("Coherent forecast: tps_inpt lacks columns ",
       paste(coherent_missing_cols, collapse = ", "))
}
coherent_input <- coherent_input[year %in% coherent_years &
                                   age %in% coherent_ages, ..coherent_cols]
coherent_input[, `:=`(year = as.integer(year), age = as.integer(age),
                      cause = trimws(as.character(cause)))]
coherent_unknown <- setdiff(unique(coherent_input$cause),
                            names(coherent_cause_map))
if (length(coherent_unknown)) {
  stop("Coherent forecast: unexpected causes in tps_inpt (",
       paste(coherent_unknown, collapse = "; "),
       "); the hierarchy expects rows for the four CVD causes only.")
}
coherent_input[, cause := unname(coherent_cause_map[cause])]

# Input checks. Nothing is imputed, and a location with an incomplete grid
# stops the run rather than being dropped (which would shift the Global
# population between years).
coherent_bad_values <- coherent_input[,
  lapply(.SD, function(v) sum(!is.finite(v) | v < 0)),
  .SDcols = c("Nx", "ALL.mx", "DIS.mx.t0", "BG.mx")]
if (any(unlist(coherent_bad_values) > 0L)) {
  stop("Coherent forecast: missing, non-finite or negative inputs (rows): ",
       paste(names(coherent_bad_values), unlist(coherent_bad_values),
             sep = " = ", collapse = ", "))
}
if (!all(coherent_input$sex %in% coherent_sexes)) {
  stop("Coherent forecast: unexpected sex labels ",
       paste(setdiff(unique(coherent_input$sex), coherent_sexes), collapse = ", "))
}
if (anyDuplicated(coherent_input, by = c(coherent_keys, "cause"))) {
  stop("Coherent forecast: duplicate location/sex/year/age/cause rows.")
}
coherent_incomplete <- coherent_input[, .N, by = location][
  N != length(coherent_sexes) * length(coherent_years) *
    length(coherent_ages) * length(coherent_cvd_causes), location]
if (length(coherent_incomplete)) {
  stop("Coherent forecast: incomplete sex/year/age/cause grid for ",
       paste(coherent_incomplete, collapse = ", "))
}
coherent_spread <- coherent_input[, .(ALL = max(ALL.mx) - min(ALL.mx),
                                      Nx = max(Nx) - min(Nx)),
                                  by = coherent_keys][ALL > 0 | Nx > 0]
if (nrow(coherent_spread)) {
  stop("Coherent forecast: Nx or ALL.mx differ across cause rows in ",
       nrow(coherent_spread), " location/sex/year/age cells.")
}
coherent_bg_mismatch <- coherent_input[abs(BG.mx + DIS.mx.t0 - ALL.mx) >
                                         coherent_rel_tol * ALL.mx, .N]
if (coherent_bg_mismatch) {
  stop("Coherent forecast: BG.mx != ALL.mx - DIS.mx.t0 in ", coherent_bg_mismatch,
       " rows, so DIS.mx.t0 is not a population death rate.")
}

coherent_loc <- data.table::dcast(coherent_input,
                                  location + sex + year + age ~ cause,
                                  value.var = "DIS.mx.t0")
coherent_loc <- merge(coherent_loc,
                      coherent_input[cause == coherent_cvd_causes[1L],
                                     c(coherent_keys, "Nx", "ALL.mx", "BG.mx.all"),
                                     with = FALSE],
                      by = coherent_keys)
data.table::setnames(coherent_loc, "BG.mx.all", "BG.mx.all_tps_inpt")
coherent_loc[, BG.mx.all := ALL.mx - (ihd + istroke + hstroke + hhd)]
coherent_negative <- coherent_loc[BG.mx.all < 0]
if (nrow(coherent_negative)) {
  stop("Coherent forecast: the four CVD rates exceed ALL.mx in ",
       nrow(coherent_negative), " cells (first: ",
       paste(unlist(coherent_negative[1L, coherent_keys, with = FALSE]),
             collapse = ", "), ").")
}
coherent_gap <- coherent_loc[, (BG.mx.all - BG.mx.all_tps_inpt) / ALL.mx]
if (any(abs(coherent_gap) > coherent_rel_tol)) {
  message("Coherent forecast: tps_inpt BG.mx.all differs from ALL.mx - sum(CVD) ",
          "in ", sum(abs(coherent_gap) > coherent_rel_tol), " of ",
          length(coherent_gap), " cells (median ",
          signif(100 * stats::median(coherent_gap), 2), "%, max ",
          signif(100 * max(abs(coherent_gap)), 2), "% of ALL.mx). ",
          "This forecast uses ALL.mx - sum(CVD).")
}
coherent_loc[, BG.mx.all_tps_inpt := NULL]
# Zero-population cells are valid: they add no exposure and no deaths to the
# population-weighted Global rates. They are reported, not imputed or removed.
coherent_zero_nx <- coherent_loc[Nx == 0, .N, by = location][order(-N)]
if (nrow(coherent_zero_nx)) {
  message("Coherent forecast: ", sum(coherent_zero_nx$N),
          " location/sex/year/age cells have Nx = 0 and carry zero weight (",
          paste(coherent_zero_nx$location, coherent_zero_nx$N, collapse = ", "),
          ").")
}

# Global rate = total deaths / total population, so the identity carries over.
coherent_wide <- coherent_loc[, c(list(Nx = sum(Nx)),
                                  lapply(.SD, function(rate) sum(rate * Nx) / sum(Nx))),
                              by = .(sex, year, age), .SDcols = coherent_components]
data.table::setorder(coherent_wide, sex, year, age)
rm(coherent_input, coherent_loc)
if (nrow(coherent_wide) != length(coherent_sexes) * length(coherent_years) *
    length(coherent_ages) || coherent_wide[, any(!(Nx > 0))]) {
  stop("Coherent forecast: Global population is missing or zero in some cells.")
}
coherent_obs_error <- coherent_wide[, max(abs(ALL.mx - rowSums(.SD)) / ALL.mx),
                                    .SDcols = coherent_bottom]
if (!(coherent_obs_error <= coherent_rel_tol)) {
  stop("Coherent forecast: observed Global rates break the all-cause identity ",
       "(max relative error ", signif(coherent_obs_error, 3), ").")
}

# Lee-Carter on log rates, with the same damped-trend ETS for kt as the trend
# files above. No floor: a non-positive rate stops the forecast.
coherent_lc <- function(panel, component, end_year, forecast_end) {
  wide <- data.table::dcast(panel[year <= end_year], age ~ year,
                            value.var = component)
  mat <- as.matrix(wide[, -1L, with = FALSE])
  rownames(mat) <- wide$age
  if (!identical(as.integer(wide$age), coherent_ages) ||
      !identical(colnames(mat), as.character(2000:end_year)) || anyNA(mat)) {
    stop("Coherent forecast: incomplete Lee-Carter matrix for ", component)
  }
  if (any(mat <= 0)) {
    stop("Coherent forecast: ", component, " has ", sum(mat <= 0),
         " non-positive Global rates; the log-rate model is undefined.")
  }
  log_rates <- log(mat)
  ax <- rowMeans(log_rates)
  decomposition <- svd(sweep(log_rates, 1L, ax, "-"), nu = 1L, nv = 1L)
  bx <- decomposition$u[, 1L]
  kt <- decomposition$d[1L] * decomposition$v[, 1L]
  kt_fit <- forecast::ets(kt, model = "AAN", damped = TRUE)
  future_kt <- as.numeric(forecast::forecast(kt_fit,
                                             h = forecast_end - end_year)$mean)
  fitted <- exp(ax + bx %o% kt)
  future <- exp(ax + bx %o% future_kt)
  dimnames(fitted) <- dimnames(mat)
  dimnames(future) <- list(rownames(mat),
                           as.character((end_year + 1L):forecast_end))
  list(fitted = fitted, future = future)
}

coherent_fit <- function(panel, end_year, forecast_end) {
  if (max(panel$year) != end_year) {
    stop("Coherent forecast: training data do not end in ", end_year)
  }
  fits <- lapply(coherent_components, function(component) {
    coherent_lc(panel, component, end_year, forecast_end)
  })
  names(fits) <- coherent_components
  # LC fitted-value residual covariance is a proxy for the unknown covariance
  # of base forecast errors in Li et al.'s MinT formula. It is not a rolling
  # one-step error estimate. Shrink half toward its diagonal: 16-20 years
  # cannot identify a stable unrestricted covariance across all components.
  # No additive jitter: error variances run from ~1e-15 (hhd, age 20) to ~5e-6.
  covariance <- lapply(seq_along(coherent_ages), function(i) {
    observed <- panel[age == coherent_ages[i]][order(year)]
    errors <- sapply(coherent_components, function(component) {
      observed[[component]] - fits[[component]]$fitted[i, ]
    })
    W <- stats::cov(errors)
    W <- 0.5 * W + 0.5 * diag(diag(W), nrow(W))
    if (anyNA(W) || any(diag(W) <= 0)) {
      stop("Coherent forecast: degenerate error covariance at age ",
           coherent_ages[i])
    }
    W
  })
  list(fits = fits, covariance = covariance)
}

# MinT (Li et al. eqs. 16 and 22) in its equivalent constraint form,
# y~ = y^ - W C'(C W C')^-1 C y^, where C y = 0 is the identity
# ALL.mx - sum(bottom) = 0. This avoids inverting W, whose condition number
# reaches ~1e6.
coherent_C <- matrix(c(1, rep(-1, length(coherent_bottom))), nrow = 1L,
                     dimnames = list(NULL, coherent_components))

coherent_reconcile <- function(fit, years, label) {
  data.table::rbindlist(lapply(seq_along(coherent_ages), function(i) {
    base <- do.call(rbind, lapply(fit$fits, function(f) {
      f$future[i, as.character(years)]
    }))
    WC <- fit$covariance[[i]] %*% t(coherent_C)
    reconciled <- base - WC %*% solve(coherent_C %*% WC, coherent_C %*% base)
    bottom <- reconciled[coherent_bottom, , drop = FALSE]
    total <- colSums(bottom)
    if (max(abs(reconciled["ALL.mx", ] - total) / total) > coherent_rel_tol) {
      stop("Coherent forecast: MinT output is not coherent (", label,
           ", age ", coherent_ages[i], ").")
    }
    # Linear MinT does not bound rates below. Stop rather than truncate, which
    # would break the identity.
    if (any(!(bottom >= 0))) {
      negative <- which(!(bottom >= 0), arr.ind = TRUE)[1L, ]
      stop("Coherent forecast: negative reconciled ",
           coherent_bottom[negative[1L]], " rate (", label, ", age ",
           coherent_ages[i], ", ", years[negative[2L]], ").")
    }
    out <- data.table::as.data.table(t(bottom))
    out[, `:=`(year = years, age = coherent_ages[i], ALL.mx = total)]
    out
  }))
}

coherent_by_sex <- lapply(coherent_sexes, function(sx) {
  panel <- coherent_wide[sex == sx]
  # Holdout: fit on 2000-2015 only, forecast and reconcile 2016-2019, and
  # compare cell by cell with the observed 2016-2019 Global rates.
  holdout_fit <- coherent_fit(panel[year <= coherent_training_end],
                              coherent_training_end, max(coherent_holdout_years))
  holdout <- data.table::melt(
    coherent_reconcile(holdout_fit, coherent_holdout_years,
                       paste(sx, "holdout")),
    id.vars = c("year", "age"), variable.name = "component",
    value.name = "reconciled", variable.factor = FALSE)
  base <- data.table::rbindlist(lapply(coherent_components, function(component) {
    m <- holdout_fit$fits[[component]]$future
    data.table::data.table(component = component,
                           age = rep(as.integer(rownames(m)), ncol(m)),
                           year = rep(as.integer(colnames(m)), each = nrow(m)),
                           base = as.vector(m))
  }))
  actual <- data.table::melt(
    panel[year %in% coherent_holdout_years, c("year", "age", coherent_components),
          with = FALSE],
    id.vars = c("year", "age"), variable.name = "component",
    value.name = "observed", variable.factor = FALSE)
  holdout <- merge(merge(holdout, base, by = c("component", "year", "age")),
                   actual, by = c("component", "year", "age"))
  if (nrow(holdout) != length(coherent_components) * length(coherent_ages) *
      length(coherent_holdout_years) || anyNA(holdout)) {
    stop("Coherent forecast: holdout forecasts and observations do not align.")
  }
  diagnostics <- holdout[, .(
    n = .N,
    rmse_rate = sqrt(mean((reconciled - observed)^2)),
    rmse_rate_base = sqrt(mean((base - observed)^2)),
    rmse_log = sqrt(mean((log(reconciled) - log(observed))^2)),
    rmse_log_base = sqrt(mean((log(base) - log(observed))^2))),
    by = component]
  final_fit <- coherent_fit(panel, max(coherent_years), coherent_until)
  projection <- coherent_reconcile(final_fit,
                                   (max(coherent_years) + 1L):coherent_until,
                                   paste(sx, "projection"))
  projection[, sex := sx]
  diagnostics[, sex := sx]
  list(projection = projection, diagnostics = diagnostics)
})

coherent_future <- data.table::rbindlist(
  lapply(coherent_by_sex, `[[`, "projection"), use.names = TRUE)
coherent_future[, source := "reconciled_forecast"]
coherent_history <- coherent_wide[, c("sex", "year", "age", coherent_components),
                                  with = FALSE]
coherent_history[, source := "observed"]
coherent_rates <- data.table::rbindlist(list(coherent_history, coherent_future),
                                        use.names = TRUE)
data.table::setorder(coherent_rates, sex, age, year)
coherent_diagnostics <- data.table::rbindlist(
  lapply(coherent_by_sex, `[[`, "diagnostics"), use.names = TRUE)
data.table::setcolorder(coherent_diagnostics, c("sex", "component"))

# Output checks: complete grid, finite nonnegative rates, and the all-cause
# identity at every sex/age/year, observed and projected.
if (nrow(coherent_rates) != length(coherent_sexes) * length(coherent_ages) *
    length(min(coherent_years):coherent_until) ||
    anyDuplicated(coherent_rates, by = c("sex", "age", "year"))) {
  stop("Coherent forecast: output grid is incomplete or duplicated.")
}
if (coherent_rates[, any(!is.finite(as.matrix(.SD)) | as.matrix(.SD) < 0),
                   .SDcols = coherent_components]) {
  stop("Coherent forecast: negative or non-finite rates in the output.")
}
coherent_identity <- coherent_rates[, abs(ALL.mx - rowSums(.SD)) / ALL.mx,
                                    .SDcols = coherent_bottom]
if (!(max(coherent_identity) <= coherent_rel_tol)) {
  stop("Coherent forecast: all-cause aggregation identity failed ",
       "(max relative error ", signif(max(coherent_identity), 3), ").")
}
message("Coherent forecast: identity holds in all ", nrow(coherent_rates),
        " sex/age/year cells (max relative error ",
        signif(max(coherent_identity), 3), ").")

coherent_long <- data.table::melt(
  coherent_rates, id.vars = c("sex", "age", "year", "source", "ALL.mx",
                              "BG.mx.all"),
  measure.vars = coherent_cvd_causes, variable.name = "cause",
  value.name = "DIS.mx.t0", variable.factor = FALSE)
coherent_long[, BG.mx := ALL.mx - DIS.mx.t0]
if (coherent_long[, any(!(BG.mx >= 0))]) {
  stop("Coherent forecast: negative BG.mx in the output.")
}
coherent_long[, `:=`(
  percent_diff_cvd = (DIS.mx.t0 - DIS.mx.t0[year == 2019L]) /
    DIS.mx.t0[year == 2019L],
  percent_diff_bgmx = (BG.mx - BG.mx[year == 2019L]) / BG.mx[year == 2019L],
  percent_diff_bgmx_all = (BG.mx.all - BG.mx.all[year == 2019L]) /
    BG.mx.all[year == 2019L]),
  by = .(sex, age, cause)]
coherent_long[, location := "Global"]
data.table::setcolorder(coherent_long, c("location", "sex", "age", "year",
                                         "source", "cause", "DIS.mx.t0",
                                         "ALL.mx", "BG.mx", "BG.mx.all"))
data.table::setorder(coherent_long, sex, cause, age, year)
print(coherent_diagnostics)
saveRDS(coherent_long,
        file = file.path(wd_data, "tps_mortality_coherent_forecasted.rds"))
saveRDS(coherent_diagnostics,
        file = file.path(wd_data, "tps_mortality_coherent_validation.rds"))
