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


## Coherent all-cause / modeled CVD / other-cause forecast -------------------
# Li et al. (2019), Insurance: Mathematics and Economics 86:122-133,
# doi:10.1016/j.insmatheco.2019.02.011, sections 3.2 and 4.1.
# At each age/sex/year: ALL.mx = sum(DIS.mx.t0 for the disjoint CVD causes)
#                            + other-cause mortality.
# This is a separate forecast: the existing three trend files above are retained.
# The two background quantities below have explicit definitions:
# BG.mx.all = other-cause mortality after excluding ALL modeled CVD causes;
# BG.mx     = ALL.mx minus the ONE CVD cause on that row.
# Check whether those definitions match the model's original BG.mx fields before
# substituting these projections into any simulation transition probabilities.

coherent_cvd_causes <- c("ihd", "istroke", "hstroke", "hhd")
coherent_years <- 2000:2019
coherent_ages <- 20:95
coherent_until <- 2050L
coherent_training_end <- 2015L
coherent_tolerance <- 1e-8

missing_causes <- setdiff(coherent_cvd_causes, unique(dt_bgmx$cause))
if (length(missing_causes)) {
  stop("Coherent forecast: missing modeled CVD causes: ",
       paste(missing_causes, collapse = ", "))
}
coherent_input <- copy(dt_bgmx[cause %in% coherent_cvd_causes &
                                 year %in% coherent_years & age %in% coherent_ages])
coherent_input[, `:=`(year = as.integer(year), age = as.integer(age))]
group_keys <- c("location", "sex", "year", "age")
if (coherent_input[, anyDuplicated(.SD), .SDcols = c(group_keys, "cause")]) {
  stop("Coherent forecast: duplicate location/sex/year/age/cause input rows.")
}
if (anyNA(coherent_input[, .(Nx, ALL.mx, DIS.mx.t0)]) ||
    coherent_input[, any(!is.finite(Nx) | !is.finite(ALL.mx) |
                           !is.finite(DIS.mx.t0) | Nx <= 0 |
                           ALL.mx < 0 | DIS.mx.t0 < 0)]) {
  stop("Coherent forecast: missing or invalid population or mortality rates.")
}
if (coherent_input[, any(.N != length(coherent_cvd_causes)), by = group_keys][, any(V1)]) {
  stop("Coherent forecast: every location/sex/year/age needs all CVD causes.")
}
if (coherent_input[, any(max(ALL.mx) - min(ALL.mx) > coherent_tolerance ||
                         max(Nx) - min(Nx) > coherent_tolerance),
                   by = group_keys][, any(V1)]) {
  stop("Coherent forecast: ALL.mx and Nx must agree across cause rows.")
}

# Use the same exposure for every cause and for the aggregate. The remainder is
# constructed from ALL.mx, rather than treating BG.mx as an additive component.
coherent_population <- unique(coherent_input[, .(location, sex, year, age,
                                                  Nx, ALL.mx)])
coherent_total <- coherent_population[,
  .(ALL.mx = weighted.mean(ALL.mx, Nx)), by = .(sex, year, age)]
coherent_cvd <- coherent_input[,
  .(DIS.mx.t0 = weighted.mean(DIS.mx.t0, Nx)),
  by = .(sex, year, age, cause)]
coherent_wide <- data.table::dcast(coherent_cvd, sex + year + age ~ cause,
                                  value.var = "DIS.mx.t0")
coherent_wide <- merge(coherent_total, coherent_wide,
                       by = c("sex", "year", "age"), all = TRUE)
if (anyNA(coherent_wide) ||
    coherent_wide[, any(.N != length(coherent_years)), by = .(sex, age)][, any(V1)] ||
    coherent_wide[, any(.N != length(coherent_ages)), by = .(sex, year)][, any(V1)]) {
  stop("Coherent forecast: age 20-95 and year 2000-2019 must be complete for every sex.")
}
coherent_wide[, other := ALL.mx - rowSums(.SD),
              .SDcols = coherent_cvd_causes]
if (coherent_wide[, any(other < -coherent_tolerance)]) {
  stop("Coherent forecast: modeled CVD rates exceed all-cause mortality.")
}
coherent_wide[, other := pmax(other, 0)]

# The input variables must represent comparable population mortality rates.
# In particular, a conditional death hazard among prevalent CVD cases cannot
# be added to a general-population background rate. Reject such inputs rather
# than silently reinterpreting the existing BG.mx model parameter.
coherent_existing_bg <- coherent_input[,
  .(BG.mx_input = weighted.mean(BG.mx, Nx)),
  by = .(sex, year, age, cause)]
coherent_existing_bg <- merge(
  coherent_existing_bg,
  coherent_total, by = c("sex", "year", "age"))
coherent_existing_bg <- merge(
  coherent_existing_bg, coherent_cvd,
  by = c("sex", "year", "age", "cause"))
if (anyNA(coherent_existing_bg$BG.mx_input) ||
    coherent_existing_bg[, any(abs(BG.mx_input + DIS.mx.t0 - ALL.mx) >
                               pmax(coherent_tolerance, 1e-5 * ALL.mx))]) {
  stop("Coherent forecast: BG.mx + DIS.mx.t0 does not equal ALL.mx. ",
       "Check whether DIS.mx.t0 is a conditional disease hazard; ",
       "the Li et al. cause-of-death identity cannot be applied to it.")
}
coherent_existing_all_bg <- coherent_input[cause == coherent_cvd_causes[1L],
  .(BG.mx.all_input = weighted.mean(BG.mx.all, Nx)),
  by = .(sex, year, age)]
coherent_existing_all_bg <- merge(
  coherent_existing_all_bg, coherent_wide[, .(sex, year, age, other)],
  by = c("sex", "year", "age"))
if (anyNA(coherent_existing_all_bg$BG.mx.all_input) ||
    coherent_input[, any(max(BG.mx.all) - min(BG.mx.all) >
                           coherent_tolerance), by = group_keys][, any(V1)] ||
    coherent_existing_all_bg[, any(abs(BG.mx.all_input - other) >
                                    pmax(coherent_tolerance, 1e-5 * other))]) {
  stop("Coherent forecast: BG.mx.all is not the residual after all modeled ",
       "CVD causes. Inspect the mortality-rate definitions first.")
}

# LC log-rate models need positive cells. 1e-10 is a numerical floor for
# zero-valued series, not an imputation for missing data. Keep exact zeros in
# observed outputs and calculate reconciliation errors on the rate scale.
coherent_lc <- function(panel, component, end_year, forecast_end) {
  train <- panel[year <= end_year]
  mat <- as.matrix(data.table::dcast(train, age ~ year,
                                    value.var = component)[, -1L, with = FALSE])
  rownames(mat) <- as.character(coherent_ages)
  if (!identical(colnames(mat), as.character(2000:end_year)) ||
      anyNA(mat) || any(!is.finite(mat))) {
    stop("Coherent forecast: incomplete LC training matrix for ", component)
  }
  log_rates <- log(pmax(mat, 1e-10))
  ax <- rowMeans(log_rates)
  decomposition <- svd(sweep(log_rates, 1L, ax, "-"), nu = 1L, nv = 1L)
  bx <- decomposition$u[, 1L]
  kt <- decomposition$d[1L] * decomposition$v[, 1L]
  fitted <- exp(sweep(bx %o% kt, 1L, ax, "+"))
  if (forecast_end > end_year) {
    kt_fit <- forecast::ets(kt, model = "AAN", damped = TRUE)
    future_kt <- as.numeric(forecast::forecast(
      kt_fit, h = forecast_end - end_year)$mean)
    future <- exp(sweep(bx %o% future_kt, 1L, ax, "+"))
    colnames(future) <- as.character((end_year + 1L):forecast_end)
  } else {
    future <- matrix(numeric(), nrow = length(coherent_ages), ncol = 0L)
  }
  rownames(fitted) <- rownames(future) <- as.character(coherent_ages)
  colnames(fitted) <- as.character(2000:end_year)
  list(fitted = fitted, future = future)
}

coherent_components <- c("ALL.mx", coherent_cvd_causes, "other")
coherent_S <- rbind(rep(1, length(coherent_components) - 1L),
                    diag(length(coherent_components) - 1L))
rownames(coherent_S) <- coherent_components
colnames(coherent_S) <- coherent_components[-1L]

coherent_fit <- function(panel, end_year, forecast_end) {
  fits <- lapply(coherent_components, function(component) {
    coherent_lc(panel, component, end_year, forecast_end)
  })
  names(fits) <- coherent_components
  # LC fitted-value residual covariance is a proxy for the unknown covariance
  # of base forecast errors in Li et al.'s MinT formula. It is not a rolling
  # one-step error estimate. Shrink half toward its diagonal: 16-20 years
  # cannot identify a stable
  # unrestricted covariance across all components at every age.
  covariance <- lapply(seq_along(coherent_ages), function(i) {
    errors <- sapply(coherent_components, function(component) {
      observed <- panel[age == coherent_ages[i] & year <= end_year]
      data.table::setorder(observed, year)
      observed[[component]] - fits[[component]]$fitted[i, ]
    })
    W <- stats::cov(errors)
    W <- 0.5 * W + 0.5 * diag(diag(W), nrow(W))
    diag(W) <- pmax(diag(W), 1e-16) + 1e-12
    W
  })
  names(covariance) <- as.character(coherent_ages)
  list(fits = fits, covariance = covariance)
}

coherent_reconcile <- function(fit, years) {
  result <- vector("list", length(coherent_ages))
  for (i in seq_along(coherent_ages)) {
    precision <- solve(fit$covariance[[i]])
    quadratic <- crossprod(coherent_S, precision %*% coherent_S)
    projection <- solve(quadratic, crossprod(coherent_S, precision))
    base <- sapply(coherent_components, function(component) {
      as.numeric(fit$fits[[component]]$future[i, as.character(years)])
    })
    base <- t(base)
    bottom <- projection %*% base
    # Linear MinT can yield a negative cause rate. In those cells, solve the
    # same weighted least-squares problem subject to nonnegative causes.
    negative <- which(colSums(bottom < 0) > 0L)
    for (k in negative) {
      target <- base[, k]
      initial <- pmax(bottom[, k], 0)
      opt <- stats::optim(initial, function(x) {
        difference <- as.vector(coherent_S %*% x) - target
        as.numeric(crossprod(difference, precision %*% difference)) / 2
      }, gr = function(x) {
        as.vector(crossprod(coherent_S,
                            precision %*% (coherent_S %*% x - target)))
      }, method = "L-BFGS-B", lower = rep(0, length(initial)),
      control = list(maxit = 1000L, factr = 1e7))
      if (opt$convergence != 0L) {
        stop("Coherent forecast: nonnegative reconciliation failed.")
      }
      bottom[, k] <- opt$par
    }
    total <- colSums(bottom)
    result[[i]] <- data.table::as.data.table(
      t(bottom[coherent_cvd_causes, , drop = FALSE]))
    result[[i]][, `:=`(age = coherent_ages[i], year = years,
                      ALL.mx = total, other = bottom["other", ])]
  }
  data.table::rbindlist(result)
}

# Fit on 2000-2015 for a genuine 2016-2019 holdout; refit through 2019 for
# the delivered 2020-2050 forecast. Observations through 2019 stay observed.
coherent_projected <- coherent_wide[, {
  group_panel <- copy(.SD)
  holdout_fit <- coherent_fit(group_panel[year <= coherent_training_end],
                              coherent_training_end, 2019L)
  holdout <- coherent_reconcile(holdout_fit, 2016:2019)
  actual <- group_panel[year %in% 2016:2019]
  data.table::setkey(holdout, year, age)
  data.table::setkey(actual, year, age)
  diagnostics <- data.table::data.table(
    component = coherent_components,
    rmse_rate = vapply(coherent_components, function(component) {
      sqrt(mean((holdout[[component]] - actual[[component]])^2))
    }, numeric(1L)),
    rmse_unreconciled_rate = vapply(coherent_components, function(component) {
      sqrt(mean((as.vector(holdout_fit$fits[[component]]$future) -
                   actual[[component]])^2))
    }, numeric(1L)))
  final_fit <- coherent_fit(group_panel, 2019L, coherent_until)
  projection <- coherent_reconcile(final_fit, 2020:coherent_until)
  .(projection = list(projection), diagnostics = list(diagnostics))
}, by = sex]

coherent_future <- coherent_projected[,
  data.table::rbindlist(projection), by = sex]
coherent_future[, source := "reconciled_forecast"]
coherent_history <- copy(coherent_wide)
coherent_history[, source := "observed"]
coherent_rates <- data.table::rbindlist(
  list(coherent_history, coherent_future), use.names = TRUE, fill = TRUE)
data.table::setorder(coherent_rates, sex, age, year)

coherent_long <- data.table::melt(
  coherent_rates, id.vars = c("sex", "age", "year", "ALL.mx", "other", "source"),
  measure.vars = coherent_cvd_causes, variable.name = "cause",
  value.name = "DIS.mx.t0", variable.factor = FALSE)
coherent_long[, `:=`(BG.mx = pmax(ALL.mx - DIS.mx.t0, 0), BG.mx.all = other)]
coherent_long[, `:=`(percent_diff_cvd = (DIS.mx.t0 -
                       DIS.mx.t0[year == 2019L][1L]) /
                       DIS.mx.t0[year == 2019L][1L],
                     percent_diff_bgmx = (BG.mx - BG.mx[year == 2019L][1L]) /
                       BG.mx[year == 2019L][1L]),
              by = .(sex, age, cause)]
# Identity must be checked at the aggregate level, before the cause-wise melt.
coherent_identity <- coherent_rates[, abs(ALL.mx - other -
                             rowSums(.SD)), .SDcols = coherent_cvd_causes]
if (any(!is.finite(coherent_identity)) ||
    max(coherent_identity) > coherent_tolerance) {
  stop("Coherent forecast: all-cause aggregation identity failed.")
}
coherent_long[, location := "Global"]
coherent_long[!is.finite(percent_diff_cvd), percent_diff_cvd := NA_real_]
coherent_long[!is.finite(percent_diff_bgmx), percent_diff_bgmx := NA_real_]
coherent_diagnostics <- coherent_projected[,
  data.table::rbindlist(diagnostics), by = sex]
saveRDS(coherent_long,
        file = file.path(wd_data, "tps_mortality_coherent_forecasted.rds"))
saveRDS(coherent_diagnostics,
        file = file.path(wd_data, "tps_mortality_coherent_validation.rds"))




