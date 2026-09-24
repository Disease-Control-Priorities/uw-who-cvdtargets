# =============================================================================
# 093_psa_downstream.R -- Aim 1 PSA: 07/08 analogues, reduction, endpoints
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions only.
#
# 07 and 08 are never sourced (both write to output/). Their read-only input
# preparation is reproduced here:
#   * 07: the statements BEFORE the model-output section (disability weights and
#     WPP life expectancy) are evaluated in a sandbox after a write scan.
#   * 08: the constants block (section 1) is evaluated the same way; the GNI/SSP
#     projection, iso3 and WHO-region mapping are PSA-local copies of 08's code.
# Everything else (deaths averted, YLL/YLD/DALY, VSL/VSLY, summaries) is computed
# WITHIN each draw from the PSA model output, then summarised across draws.
#
# Floating-point conventions follow the originals where a discrete threshold
# depends on them (08's average adult age -> 5-year LE lookup):
#   data.table GForce sum() (single j)  -> sequential double accumulation
#   base sum()/colSums()/multi-expression j -> long-double accumulation

#...........................................................
# Labels and reporting constants ----
#...........................................................

PSA_SCENARIOS <- c("baseline", "bp_no_diabetes_only", "bp_diabetes_only",
                   "bp_combined", "statins_only", "all_interventions")
PSA_SCENARIO_LABEL <- c(baseline = "b.a.u",
                        bp_no_diabetes_only = "HTN Control (No Diabetes)",
                        bp_diabetes_only = "HTN Control (Diabetes)",
                        bp_combined = "HTN Control (Combined, 150M)",
                        statins_only = "Improved Statin Uptake",
                        all_interventions = "All Interventions")
PSA_INT_ORDER <- c("HTN Control (No Diabetes)", "HTN Control (Diabetes)",
                   "Improved Statin Uptake", "All Interventions")
PSA_AGE_GROUPS <- c("<70", "70-79", "80-89", "90+")
PSA_REPORT_YEAR0 <- 2025L     # report sums deaths from 2025 (review defect D5 labels)
PSA_INT_YEAR <- 2026L         # 07's int_year
PSA_ASMR_YEAR0 <- 2019L
PSA_CAUSE_MAP <- c(ihd = "Ischemic heart disease", istroke = "Ischemic stroke",
                   hstroke = "Intracerebral hemorrhage",
                   hhd = "Hypertensive heart disease", all = "All causes")

psa_age_group <- function(age) {
  as.character(cut(age, breaks = c(0, 70, 80, 90, Inf), labels = PSA_AGE_GROUPS,
                   right = FALSE, include.lowest = TRUE))
}

#...........................................................
# 07 inputs (disability weights, life expectancy) ----
#...........................................................

psa_load_07_inputs <- function(paths) {
  f <- file.path(paths$wd_code, "07_output_dalys.R")
  exprs <- parse(f, keep.source = FALSE)
  stop_at <- which(vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("out_model_path"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("07: could not find the model-output section.")
  pre <- exprs[seq_len(stop_at - 1L)]
  bad <- psa_scan_writes(pre)
  if (length(bad)) stop("07 input section contains write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$wd_raw <- paths$wd_raw
  env$cause_map <- PSA_CAUSE_MAP
  suppressWarnings(suppressMessages(for (e in pre) eval(e, env)))
  list(dw = copy(env$dw), lt_interp = copy(env$lt_interp), int_year = env$int_year)
}

#...........................................................
# 08 inputs (constants, GNI projection, region mapping) ----
#...........................................................

psa_load_08_constants <- function(paths) {
  f <- file.path(paths$wd_code, "08_economic_value_calculation.R")
  exprs <- parse(f, keep.source = FALSE)
  stop_at <- which(vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("model_files"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("08: could not find the model-output section.")
  pre <- exprs[seq_len(stop_at - 1L)]
  bad <- psa_scan_writes(pre)
  if (length(bad)) stop("08 constants section contains write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$wd <- paths$wd
  suppressMessages(for (e in pre) eval(e, env))
  keep <- c("US_VSL_RATIO", "VSL_ELAST_HIC", "VSL_ELAST_LMIC", "VSL_ELAST_LOW",
            "VSL_ELAST_HIGH", "VSL_RATIO_FLOOR", "ADULT_MIN_AGE", "MAX_MODEL_AGE",
            "DISC_RATES", "BASE_YEAR", "SUMMARY_YEARS", "R_VSLY", "disc_life_years",
            "GNI_FILE", "SSP_FILE", "COUNTRY_FILE")
  miss <- keep[!vapply(keep, exists, logical(1), envir = env, inherits = FALSE)]
  if (length(miss)) stop("08 constants missing: ", paste(miss, collapse = ", "))
  mget(keep, envir = env)
}

# 08 sections 5-6 (iso3 and GNI with SSP2 projection), PSA-local copy.
psa_08_gni <- function(k08, locs, model_years = PSA_YEARS) {
  country_grp <- fread(k08$COUNTRY_FILE)
  loc <- data.table(location = locs)
  loc <- country_grp[loc, on = .(location)]
  miss <- is.na(loc$iso3)
  if (any(miss)) {
    loc[miss, iso3 := countrycode::countrycode(location, origin = "country.name",
                                               destination = "iso3c", warn = FALSE)]
  }
  gni_raw <- fread(k08$GNI_FILE, skip = 4, header = TRUE)
  year_cols <- grep("^[0-9]{4}$", names(gni_raw), value = TRUE)
  gni <- melt(gni_raw, id.vars = "Country Code", measure.vars = year_cols,
              variable.name = "year", value.name = "gni_pc_ppp")
  setnames(gni, "Country Code", "iso3")
  gni[, year := as.integer(as.character(year))]
  gni <- gni[!is.na(gni_pc_ppp) & year >= 2000 & year <= 2050]
  ssp_gdp <- as.data.table(readxl::read_excel(k08$SSP_FILE, sheet = "data"))
  ssp_pc <- ssp_gdp[Scenario == "SSP2" & Variable == "GDP|PPP [per capita]"]
  if (nrow(ssp_pc) == 0) stop("SSP data filtered to 0 rows.")
  ssp_yr_cols <- grep("^[0-9]{4}$", names(ssp_pc), value = TRUE)
  ssp_pc_long <- melt(ssp_pc, id.vars = "Region", measure.vars = ssp_yr_cols,
                      variable.name = "year", value.name = "ssp_gdp_pc")
  setnames(ssp_pc_long, "Region", "location")
  ssp_pc_long[, year := as.integer(as.character(year))]
  ssp_pc_long[, iso3 := suppressWarnings(countrycode::countrycode(
    location, origin = "country.name", destination = "iso3c"))]
  ssp_pc_long <- ssp_pc_long[!is.na(iso3) & !is.na(ssp_gdp_pc) & ssp_gdp_pc > 0]
  iso3_list <- sort(unique(loc[!is.na(iso3), iso3]))
  ssp_annual <- ssp_pc_long[iso3 %in% iso3_list, {
    ord  <- order(year)
    log_interp <- approx(x = year[ord], y = log(ssp_gdp_pc[ord]), xout = model_years, rule = 2)$y
    data.table(year = model_years, ssp_interp = exp(log_interp))
  }, by = iso3]
  ssp_25 <- ssp_annual[year == 2025, .(iso3, ssp_interp_2025 = ssp_interp)]
  ssp_annual <- merge(ssp_annual, ssp_25, by = "iso3", all.x = TRUE)
  ssp_annual[year < 2025, ssp_interp := ssp_interp_2025]
  ssp_annual[, ssp_interp_2025 := NULL]
  setorder(ssp_annual, iso3, year)
  ssp_annual[, ssp_growth := ssp_interp / shift(ssp_interp) - 1, by = iso3]
  gni_last <- gni[iso3 %in% iso3_list, {
    idx <- which.max(year)
    .(last_year = year[idx], gni_last = gni_pc_ppp[idx])
  }, by = iso3]
  gni_grid <- CJ(iso3 = iso3_list, year = model_years)
  gni_grid <- gni[gni_grid, on = .(iso3, year)]
  gni_grid <- ssp_annual[gni_grid, on = .(iso3, year)]
  gni_grid <- gni_last[gni_grid, on = .(iso3)]
  setorder(gni_grid, iso3, year)
  gni_grid[, gni_pc_proj := {
    out <- gni_pc_ppp
    if (!is.na(last_year[1]) && !is.na(gni_last[1])) {
      future_idx <- which(year > last_year[1])
      for (j in future_idx) {
        if (year[j] == last_year[1] + 1) {
          if (!is.na(ssp_growth[j])) out[j] <- gni_last[1] * (1 + ssp_growth[j])
        } else {
          if (!is.na(out[j - 1]) && !is.na(ssp_growth[j])) out[j] <- out[j - 1] * (1 + ssp_growth[j])
        }
      }
    }
    out
  }, by = iso3]
  gni_grid[, gni_pc_ppp_final := fifelse(!is.na(gni_pc_ppp), gni_pc_ppp, gni_pc_proj)]
  gni_grid <- gni_grid[, .(iso3, year, gni_pc_ppp = gni_pc_ppp_final)]
  us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp)]
  if (nrow(us_gni) == 0) stop("No USA GNI values found.")
  list(loc_iso3 = loc[, .(location, iso3)], gni_grid = gni_grid, us_gni = us_gni)
}

# 08 section 11 WHO-region mapping, applied to a one-row-per-location skeleton.
psa_08_regions <- function(paths, loc_iso3) {
  dt_final <- copy(loc_iso3)[, row_id := .I]
  country_grp <- fread(file.path(paths$wd, "data", "raw", "who-regions.csv"))
  setnames(country_grp, old = c("Entity", "Code", "World regions according to WHO"),
           new = c("location", "iso3", "region_who"))
  country_grp[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
  country_grp[, Year := NULL]
  dt_final <- country_grp[dt_final, on = .(location)]
  missing_iso3 <- is.na(dt_final$iso3)
  if (any(missing_iso3)) {
    dt_final[missing_iso3, iso3 := countrycode::countrycode(
      location, origin = "country.name", destination = "iso3c", warn = FALSE)]
  }
  fix_country_grp <- data.table(
    location = c("Brunei Darussalam", "Cabo Verde", "Democratic People's Republic of Korea",
                 "Democratic Republic of the Congo", "Iran (Islamic Republic of)", "Ivory Coast",
                 "Lao People's Democratic Republic", "Micronesia (Federated States of)",
                 "Palestine", "Republic of Korea", "Republic of Moldova", "Russian Federation",
                 "Saint Vincent and the Grenadines", "Syrian Arab Republic",
                 "Taiwan (Province of China)", "Timor-Leste",
                 "Venezuela (Bolivarian Republic of)", "Viet Nam"),
    iso3 = c("BRN", "CPV", "PRK", "COD", "IRN", "CIV", "LAO", "FSM", "PSE",
             "KOR", "MDA", "RUS", "VCT", "SYR", "TWN", "TLS", "VEN", "VNM"),
    who_region = c("WPR", "AFR", "SEAR", "AFR", "EMR", "AFR", "WPR", "WPR", "EMR",
                   "WPR", "EUR", "EUR", "AMR", "EMR", "WPR", "SEAR", "AMR", "WPR"))
  dt_final[fix_country_grp, on = .(location), `:=`(
    iso3 = fcoalesce(iso3, i.iso3), region_who = fcoalesce(region_who, i.who_region))]
  dt_final[, region_who := fcase(
    region_who == "AFR", "Africa", region_who == "EMR", "Eastern Mediterranean",
    region_who == "EUR", "Europe", region_who == "AMR", "Americas",
    region_who == "SEAR", "South-East Asia", region_who == "WPR", "Western Pacific",
    default = region_who)]
  mult <- dt_final[, .N, by = row_id]
  if (any(mult$N != 1L)) stop("08 region mapping would duplicate location rows.")
  dt_final[order(row_id), .(location, who_region = region_who)]
}

#...........................................................
# Report country metadata (aim1_report.Rmd chunk `country-metadata`) ----
#...........................................................

psa_report_country_metadata <- function(paths) {
  countries_dt <- fread(paste0(paths$wd_data, "Country_groupings_extended.csv"))
  who_regions <- fread(paste0(paths$wd_raw, "who-regions.csv"))
  setnames(who_regions, old = c("Entity", "Code", "World regions according to WHO"),
           new = c("location", "iso3", "region_who"))
  who_regions[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
  who_regions[, c("Year", "location") := NULL]
  countries_dt <- merge(countries_dt, who_regions, by = "iso3", all.x = TRUE)
  who_fix <- data.table(
    iso3 = c("BMU", "GRL", "PSE", "SDS", "TWN", "VCT", "VIR"),
    region_who_fix = c("Americas", "Europe", "Eastern Mediterranean", "Africa",
                       "Western Pacific", "Americas", "Americas"))
  countries_dt <- merge(countries_dt, who_fix, by = "iso3", all.x = TRUE)
  countries_dt[, region_who := fcoalesce(region_who, region_who_fix)]
  countries_dt[, region_who_fix := NULL]
  countries_dt[, region := fifelse(!is.na(region_who), region_who, region)]
  countries_dt[, region_who := NULL]
  income_groups <- fread(paste0(paths$wd_raw, "World Bank Income Group 2026.csv"))
  income_groups[location_income == "", location_income := "L"]
  income_groups[, location_income := fcase(
    location_income == "L", "Low income", location_income == "LM", "Lower-middle income",
    location_income == "UM", "Upper-middle income", location_income == "H", "High income",
    default = location_income)]
  countries_dt <- merge(countries_dt, income_groups[, .(iso3, location_income)],
                        by = "iso3", all.x = TRUE)
  unique(countries_dt, by = "location")
}

#...........................................................
# Downstream inputs, assembled once in the master ----
#...........................................................

psa_downstream_inputs <- function(paths, locs, all_locs = locs) {
  i07 <- psa_load_07_inputs(paths)
  k08 <- psa_load_08_constants(paths)
  # 08 builds the GNI grid (and the USA reference) over every model location;
  # per-iso3 values do not depend on which other countries are present.
  g08 <- psa_08_gni(k08, union(all_locs, locs))
  g08$loc_iso3 <- g08$loc_iso3[location %in% locs]
  reg08 <- psa_08_regions(paths, g08$loc_iso3)
  # 08 groups by who_region, so an unmapped location forms its own NA group.
  reg08[is.na(who_region), who_region := PSA_NA_REGION]
  meta <- psa_report_country_metadata(paths)
  pop <- as.data.table(readRDS(paste0(paths$wd_data, "PopulationsSingleAge0050.rds")))
  pop[age >= 95, age := 95]
  setnames(pop, "year_id", "year")
  pop <- pop[, .(Nx = sum(Nx)), by = .(location, year)]
  per <- lapply(setNames(locs, locs), function(Country) {
    psa_country_downstream(Country, i07, k08, g08, reg08, meta, pop)
  })
  who_levels <- sort(unique(reg08$who_region))
  # 08 income denominators: sum(population x gni_pc_disc_r3, na.rm = TRUE) by
  # region-year and for the World (draw-invariant).
  inc <- rbindlist(lapply(per, function(d) data.table(who_region = d$who_region, year = PSA_YEARS,
                                                      income = d$income_08)))
  income_08 <- rbind(inc[, .(total_income = sum(income, na.rm = TRUE)), by = .(who_region, year)],
                     inc[, .(who_region = "World", total_income = sum(income, na.rm = TRUE)), by = year],
                     use.names = TRUE)
  list(i07 = list(int_year = i07$int_year), k08 = k08[setdiff(names(k08), "disc_life_years")],
       per_country = per, who_levels = who_levels, income_08 = income_08, country_meta = meta,
       loc_region = meta[location %in% locs, .(location, region, location_income, iso3)],
       reg08 = reg08, loc_iso3 = g08$loc_iso3)
}

PSA_NA_REGION <- "<NA>"

# Per-country lookup matrices for the 07/08 analogues.
psa_country_downstream <- function(Country, i07, k08, g08, reg08, meta, pop) {
  dw <- i07$dw[location == Country]
  dw_c <- dw$dw[match(PSA_CAUSES, dw$cause)]
  if (anyDuplicated(dw$cause)) stop("07 disability weights duplicate a cause for ", Country)
  lt <- i07$lt_interp[location == Country]
  if (anyDuplicated(lt, by = c("age", "year"))) stop("WPP life table has duplicate keys for ", Country)
  yrs07 <- PSA_INT_YEAR:max(PSA_YEARS)
  le07 <- matrix(NA_real_, length(PSA_AGES), length(yrs07))
  if (nrow(lt)) {
    ix <- lt[age %in% PSA_AGES & year %in% yrs07]
    le07[cbind(match(ix$age, PSA_AGES), match(ix$year, yrs07))] <- ix$le
  }
  # 08: rolling (nearest-year) lookups at 5-year lower-bound ages.
  refs <- seq(0L, 100L)
  le_ref <- matrix(NA_real_, length(refs), length(PSA_YEARS))
  if (nrow(lt)) {
    ly <- sort(unique(lt$year))
    near <- vapply(PSA_YEARS, function(y) ly[which.min(abs(ly - y))], numeric(1))
    for (j in seq_along(PSA_YEARS)) {
      sub <- lt[year == near[j]]
      le_ref[match(sub$age, refs), j] <- sub$le
    }
  }
  age_ref_5y <- pmin(k08$MAX_MODEL_AGE, (as.integer(floor(PSA_AGES)) %/% 5L) * 5L)
  le_age08 <- le_ref[match(age_ref_5y, refs), , drop = FALSE]
  iso <- g08$loc_iso3[location == Country, iso3]
  gni <- g08$gni_grid[iso3 == iso][match(PSA_YEARS, year), gni_pc_ppp]
  if (!length(gni)) gni <- rep(NA_real_, length(PSA_YEARS))
  usg <- g08$us_gni[match(PSA_YEARS, year), gni_pc_usa]
  vsl_e1_2 <- fifelse(gni >= usg,
                      k08$US_VSL_RATIO * usg * (gni / usg)^k08$VSL_ELAST_HIC,
                      k08$US_VSL_RATIO * usg * (gni / usg)^k08$VSL_ELAST_LMIC)
  vsl_e1_5 <- k08$US_VSL_RATIO * usg * (gni / usg)^k08$VSL_ELAST_HIGH
  vsl_e1_2 <- pmax(vsl_e1_2, k08$VSL_RATIO_FLOOR * gni, na.rm = TRUE)
  vsl_e1_5 <- pmax(vsl_e1_5, k08$VSL_RATIO_FLOOR * gni, na.rm = TRUE)
  disc_r3 <- 1 / (1 + k08$DISC_RATES["r3"])^(PSA_YEARS - k08$BASE_YEAR)
  population <- pop[location == Country][match(PSA_YEARS, year), Nx]
  if (!length(population)) population <- rep(NA_real_, length(PSA_YEARS))
  list(Country = Country, dw = dw_c, le07 = le07, le_ref = le_ref, refs = refs,
       le_age08 = le_age08, gni = gni, gni_usa = usg, vsl_e1_2 = vsl_e1_2,
       vsl_e1_5 = vsl_e1_5, disc_r3 = unname(disc_r3), population = population,
       income_08 = population * (gni * unname(disc_r3)),
       who_region = reg08[location == Country, who_region],
       region = meta[location == Country, region],
       location_income = meta[location == Country, location_income])
}

#...........................................................
# Per-country, per-scenario reduction (inside workers) ----
#...........................................................

psa_indicator <- function(groups, levels) {
  m <- matrix(0, length(levels), length(groups))
  m[cbind(match(groups, levels), seq_along(groups))] <- 1
  m
}

psa_reduce_design <- function() {
  st <- psa_strata()
  st[, agegrp := psa_age_group(age)]
  g1 <- CJ(cause = PSA_CAUSES, sex = PSA_SEXES, agegrp = PSA_AGE_GROUPS, sorted = FALSE)
  g1[, key := paste(cause, sex, agegrp, sep = "|")]
  list(strata = st, g1 = g1,
       A1 = psa_indicator(paste(st$cause, st$sex, st$agegrp, sep = "|"), g1$key),
       A2 = psa_indicator(st$cause, PSA_CAUSES),
       A3 = psa_indicator(st$age, PSA_AGES),
       yrs_report = which(PSA_YEARS >= PSA_REPORT_YEAR0),
       yrs_int = which(PSA_YEARS >= PSA_INT_YEAR),
       yrs_asmr = which(PSA_YEARS >= PSA_ASMR_YEAR0))
}

# Sum over strata of an [S, Y, B] array with a row-group indicator A -> [G, Y, B].
psa_group_sum <- function(A, x) {
  d <- dim(x)
  array(A %*% matrix(x, d[1], d[2] * d[3]), c(nrow(A), d[2], d[3]))
}

# Sequential double sum over all strata (08's GForce sum by location-year).
psa_seq_strata_sum <- function(x) {
  d <- dim(x)
  m <- matrix(x, d[1], d[2] * d[3])
  acc <- m[1, ]
  for (i in 2:d[1]) acc <- acc + m[i, ]
  matrix(acc, d[2], d[3])
}

# Per-age sequential double sum over (sex, cause) in 08's row order.
psa_seq_age_sum <- function(x) {
  d <- dim(x)
  a <- array(x, c(length(PSA_AGES), length(PSA_CAUSES), length(PSA_SEXES), d[2], d[3]))
  acc <- a[, 1L, 1L, , ]
  for (s in seq_along(PSA_SEXES)) for (ci in seq_along(PSA_CAUSES)) {
    if (s == 1L && ci == 1L) next
    acc <- acc + a[, ci, s, , ]
  }
  array(acc, c(length(PSA_AGES), d[2], d[3]))
}

# 08's pop per age: sum over sexes of the DISTINCT cause-row values (unique()
# keeps 1-4 rows depending on exact floating-point equality), sequential double.
psa_distinct_pop_age <- function(pop) {
  d <- dim(pop)
  a <- array(pop, c(length(PSA_AGES), length(PSA_CAUSES), length(PSA_SEXES), d[2], d[3]))
  acc <- NULL
  for (s in seq_along(PSA_SEXES)) {
    v <- lapply(seq_along(PSA_CAUSES), function(ci) a[, ci, s, , ])
    keep <- list(TRUE,
                 v[[2]] != v[[1]],
                 v[[3]] != v[[1]] & v[[3]] != v[[2]],
                 v[[4]] != v[[1]] & v[[4]] != v[[2]] & v[[4]] != v[[3]])
    for (ci in seq_along(PSA_CAUSES)) {
      term <- v[[ci]] * keep[[ci]]
      acc <- if (is.null(acc)) term else acc + term
    }
  }
  array(acc, c(length(PSA_AGES), d[2], d[3]))
}

psa_reduce_scenario <- function(res, dsc, design, is_baseline = FALSE) {
  B <- dim(res$dead)[3]
  S <- dim(res$dead)[1]
  yi <- design$yrs_int
  out <- list()
  out$g1 <- psa_group_sum(design$A1, res$dead)                         # [32, Y, B]
  # 07: YLD = sick x DW(location, cause); YLL = dead x LE(location, age, year).
  dwv <- dsc$dw[match(design$strata$cause, PSA_CAUSES)]
  le <- dsc$le07[match(design$strata$age, PSA_AGES), , drop = FALSE]   # [S, 25]
  yld <- res$sick[, yi, , drop = FALSE] * dwv
  yll <- res$dead[, yi, , drop = FALSE] * as.vector(le)
  daly <- yld + yll
  na0 <- function(z) { z[is.na(z)] <- 0; z }
  out$g2 <- array(c(psa_group_sum(design$A2, na0(yll)), psa_group_sum(design$A2, na0(yld)),
                    psa_group_sum(design$A2, na0(daly))),
                  c(length(PSA_CAUSES), length(yi), B, 3L))           # [4, 25, B, 3]
  ya <- design$yrs_asmr
  out$g3 <- array(c(psa_group_sum(design$A3, res$dead[, ya, , drop = FALSE]),
                    psa_group_sum(design$A3, res$well[, ya, , drop = FALSE]),
                    psa_group_sum(design$A3, res$sick[, ya, , drop = FALSE])),
                  c(length(PSA_AGES), length(ya), B, 3L))             # [76, 32, B, 3]
  tot_year <- apply(out$g1, c(2L, 3L), sum)                           # [Y, B]
  out$c2 <- tot_year[design$yrs_report, , drop = FALSE]                # [26, B]
  out$c1 <- rbind(dead_2025_2050 = colSums(out$c2),
                  dead_2026_2050 = colSums(tot_year[yi, , drop = FALSE]),
                  yll = apply(out$g2[, , , 1L, drop = FALSE], 3L, sum),
                  yld = apply(out$g2[, , , 2L, drop = FALSE], 3L, sum),
                  daly = apply(out$g2[, , , 3L, drop = FALSE], 3L, sum))  # [5, B]
  # 08 inputs.
  out$deaths_year <- psa_seq_strata_sum(res$dead)                      # [Y, B]
  out$deaths_age <- psa_seq_age_sum(res$dead)                          # [76, Y, B]
  out$pop_age <- psa_distinct_pop_age(res$pop)                         # [76, Y, B]
  if (is_baseline) {
    ya <- design$yrs_asmr
    cause_age <- function(x) {
      st <- design$strata
      A <- psa_indicator(paste(st$cause, st$age), paste(rep(PSA_CAUSES, each = length(PSA_AGES)),
                                                         rep(PSA_AGES, times = length(PSA_CAUSES))))
      psa_group_sum(A, x[, ya, 1L, drop = FALSE])[, , 1L]
    }
    out$g4 <- list(dead = cause_age(res$dead), well = cause_age(res$well),
                   sick = cause_age(res$sick))
    out$g5 <- cbind(newcases = colSums(res$newcases[, , 1L]), sick = colSums(res$sick[, , 1L]),
                    pop = colSums(res$pop[, , 1L]))
  }
  out
}

# 08 per location-year-draw for one intervention scenario (sections 3-10).
psa_econ_scenario <- function(red, base, dsc, k08, detail = FALSE) {
  Y <- length(PSA_YEARS); B <- ncol(red$deaths_year)
  averted <- base$deaths_year - red$deaths_year                        # [Y, B]
  averted_age <- base$deaths_age - red$deaths_age                      # [76, Y, B]
  la <- array(dsc$le_age08, dim(averted_age))
  prod <- averted_age * la
  prod[is.na(prod)] <- 0
  lyg <- matrix(colSums(matrix(prod, length(PSA_AGES), Y * B)), Y, B)
  pa <- red$pop_age                                                   # [76, Y, B]
  pm <- matrix(pa, length(PSA_AGES), Y * B)
  adult <- PSA_AGES >= k08$ADULT_MIN_AGE
  avg_age <- matrix(colSums(pm[adult, , drop = FALSE] * PSA_AGES[adult]) /
                      colSums(pm[adult, , drop = FALSE]), Y, B)
  age_ref <- matrix(pmin(k08$MAX_MODEL_AGE, (as.integer(floor(avg_age)) %/% 5L) * 5L), Y, B)
  le_avg <- matrix(dsc$le_ref[cbind(match(as.vector(age_ref), dsc$refs),
                                    rep(seq_len(Y), times = B))], Y, B)
  ok <- !is.na(le_avg) & le_avg > 0                                   # le_avg_adult_disc > 0
  vsly_e1_2 <- ifelse(ok, dsc$vsl_e1_2 / le_avg, NA_real_)
  vsly_e1_5 <- ifelse(ok, dsc$vsl_e1_5 / le_avg, NA_real_)
  econ_e1_2 <- dsc$vsl_e1_2 * averted
  econ_e1_5 <- dsc$vsl_e1_5 * averted
  vval_e1_2 <- vsly_e1_2 * lyg
  vval_e1_5 <- vsly_e1_5 * lyg
  disc <- dsc$disc_r3
  out <- list(values = array(c(econ_e1_2 * disc, vval_e1_2 * disc, econ_e1_5 * disc, vval_e1_5 * disc),
                             c(Y, B, 4L)))                            # [Y, B, 4]
  if (detail) {
    out$detail <- list(deaths_baseline = base$deaths_year, deaths_intervention = red$deaths_year,
                       deaths_averted = averted, life_years_gained_undisc = lyg,
                       avg_adult_age = avg_age, age_ref_5y = age_ref, le_avg_adult = le_avg,
                       vsly_e1_2 = vsly_e1_2, economic_value_e1_2 = econ_e1_2,
                       vsly_value_e1_2 = vval_e1_2)
  }
  out
}

#...........................................................
# Worker task: a group of countries for one block of draws ----
#...........................................................

psa_empty_partials <- function(B, n_regions) {
  nS <- length(PSA_SCENARIOS)
  list(g1 = array(0, c(nS, 32L, length(PSA_YEARS), B)),
       g2 = array(0, c(nS, length(PSA_CAUSES), length(PSA_INT_YEAR:max(PSA_YEARS)), B, 3L)),
       g3 = array(0, c(nS, length(PSA_AGES), length(PSA_ASMR_YEAR0:max(PSA_YEARS)), B, 3L)),
       e1 = array(0, c(n_regions + 1L, nS, length(PSA_YEARS), B, 4L)),
       g4 = NULL, g5 = NULL)
}

psa_add_partials <- function(a, b) {
  for (nm in c("g1", "g2", "g3", "e1")) a[[nm]] <- a[[nm]] + b[[nm]]
  if (!is.null(b$g4)) {
    if (is.null(a$g4)) { a$g4 <- b$g4; a$g5 <- b$g5 } else {
      for (v in names(a$g4)) a$g4[[v]] <- a$g4[[v]] + b$g4[[v]]
      a$g5 <- a$g5 + b$g5
    }
  }
  a
}

psa_state_checks <- function(res, rt, Country, sn) {
  prob_ok <- all(is.finite(rt$IR)) && all(is.finite(rt$CF)) &&
    min(rt$IR) >= 0 && max(rt$IR) <= 0.99 && min(rt$CF) >= 0 && max(rt$CF) <= 0.99
  states <- c("well", "sick", "dead", "pop", "allmx", "newcases")
  nonfinite <- sum(vapply(states, function(v) sum(!is.finite(res[[v]])), numeric(1)))
  negative <- sum(vapply(states, function(v) sum(res[[v]] < 0, na.rm = TRUE), numeric(1)))
  data.table(location = Country, scenario = sn, prob_in_range = prob_ok,
             ir_min = min(rt$IR), ir_max = max(rt$IR), cf_min = min(rt$CF), cf_max = max(rt$CF),
             n_nonfinite_states = nonfinite, n_negative_states = negative,
             eff_ir_min = min(rt$eff_ir), eff_ir_max = max(rt$eff_ir),
             bp_draw0_vs_06 = if (is.null(rt$diag$bp_draw0_vs_06)) NA_real_ else rt$diag$bp_draw0_vs_06,
             salt_draw0_vs_ref = if (is.null(rt$diag$salt_draw0_vs_ref)) NA_real_ else rt$diag$salt_draw0_vs_ref,
             salt_na_share_2026plus = if (is.null(rt$diag$salt_na_share_2026plus)) NA_real_ else rt$diag$salt_na_share_2026plus,
             eff_ir_reset_to_1 = if (is.null(rt$diag$eff_ir_reset_to_1)) NA_real_ else rt$diag$eff_ir_reset_to_1)
}

psa_run_country <- function(Country, cin, fns, shared, dsc, draw_ids, E, RR, design,
                            n_regions, region_index, keep_long = FALSE, econ_detail = FALSE,
                            cache_env = NULL, sample_max_draw = 20L) {
  t0 <- proc.time()[["elapsed"]]
  if (!is.null(cache_env) && exists(Country, envir = cache_env, inherits = FALSE)) {
    ctx <- get(Country, envir = cache_env)          # draw-invariant; built once per worker
  } else {
    ctx <- psa_country_context(Country, cin, fns, shared$repYear, shared$dt_gbd_rr,
                               shared$run_args, shared$scenarios, shared$htn_scenario_ids,
                               shared$sheet1_0, shared$rr10_0)
    if (!is.null(cache_env)) assign(Country, ctx, envir = cache_env)
    invisible(gc(verbose = FALSE))     # release the ~1 GB of merge temporaries now
  }
  ctx$draw0_col <- match(0L, draw_ids, nomatch = 0L)
  t_ctx <- proc.time()[["elapsed"]] - t0
  B <- length(draw_ids)
  part <- psa_empty_partials(B, n_regions)
  c1 <- array(NA_real_, c(length(PSA_SCENARIOS), 5L, B))
  c2 <- array(NA_real_, c(length(PSA_SCENARIOS), length(PSA_REPORT_YEAR0:max(PSA_YEARS)), B))
  checks <- list(); long <- list(); econ_det <- list()
  base <- NULL
  for (k in seq_along(PSA_SCENARIOS)) {
    sn <- PSA_SCENARIOS[k]
    iv <- shared$scenarios[[sn]]
    rt <- psa_scenario_rates(ctx, iv, shared$htn_scenario_ids[[sn]], E, RR, fns)
    res <- psa_transitions(ctx, rt$IR, rt$CF)
    checks[[sn]] <- psa_state_checks(res, rt, Country, sn)
    red <- psa_reduce_scenario(res, dsc, design, is_baseline = (sn == "baseline"))
    part$g1[k, , , ] <- red$g1
    part$g2[k, , , , ] <- red$g2
    part$g3[k, , , , ] <- red$g3
    c1[k, , ] <- red$c1
    c2[k, , ] <- red$c2
    if (sn == "baseline") {
      part$g4 <- red$g4; part$g5 <- red$g5
      base <- red
    } else {
      ec <- psa_econ_scenario(red, base, dsc, shared$k08, detail = econ_detail)
      v <- ec$values; v[is.na(v)] <- 0               # 08 sums with na.rm = TRUE
      # Slices are [year, draw, metric] in storage order; add as vectors so a
      # one-draw block (dropped dimension) cannot break conformability.
      if (length(region_index)) {
        part$e1[region_index, k, , , ] <- as.vector(part$e1[region_index, k, , , ]) + as.vector(v)
      }
      part$e1[n_regions + 1L, k, , , ] <- as.vector(part$e1[n_regions + 1L, k, , , ]) + as.vector(v)
      # Draw-0 column only (the deterministic result), for cell-level validation.
      if (econ_detail && isTRUE(ctx$draw0_col > 0)) {
        econ_det[[sn]] <- lapply(ec$detail, function(m) m[, ctx$draw0_col, drop = FALSE])
      }
    }
    if (keep_long) {
      # Full 06-schema outputs are kept only for the first draws (reproducible
      # per-country chunk); everything else is reduced in place.
      kc <- which(draw_ids <= sample_max_draw)
      if (length(kc)) {
        sub <- function(a) if (length(dim(a)) == 3L) a[, , kc, drop = FALSE] else a
        long[[sn]] <- psa_batch_to_long(ctx, lapply(res, sub), lapply(rt[c("eff_ir", "eff_cf")], sub),
                                        sn, psa_intervention_label(iv), draw_ids[kc])
      }
    }
    rm(res, rt, red)
    if (B > 20L) invisible(gc(verbose = FALSE))   # bound per-worker peak memory for large blocks
  }
  list(Country = Country, part = part, c1 = c1, c2 = c2,
       checks = rbindlist(checks), long = if (keep_long) rbindlist(long) else NULL,
       econ_detail = if (econ_detail) econ_det else NULL,
       timing = data.table(location = Country, t_context = t_ctx,
                           t_total = proc.time()[["elapsed"]] - t0, n_draws = B))
}

psa_worker_task <- function(task) {
  fns <- get(".psa_fns", envir = globalenv())
  wk <- get(".psa_worker", envir = globalenv())      # this worker's country inputs
  design <- psa_reduce_design()
  B <- length(task$draw_ids)
  E  <- psa_effect_arrays(task$sheet1_block, task$draw_ids)
  RR <- psa_rr10_array(task$rr10_block, task$draw_ids)
  part <- psa_empty_partials(B, length(wk$who_levels))
  per_loc <- list(); checks <- list(); timing <- list(); long <- list(); econ_det <- list()
  cache_env <- if (isTRUE(task$cache_contexts)) wk$ctx_cache else NULL
  gc(reset = TRUE, verbose = FALSE)
  for (Country in wk$countries) {
    ri <- match(wk$dsc[[Country]]$who_region, wk$who_levels)
    r <- psa_run_country(Country, wk$cin[[Country]], fns, wk$shared, wk$dsc[[Country]],
                         task$draw_ids, E, RR, design, length(wk$who_levels), ri,
                         keep_long = Country %in% task$keep_long,
                         econ_detail = identical(task$econ_detail, "ALL") || Country %in% task$econ_detail,
                         cache_env = cache_env,
                         sample_max_draw = if (is.null(task$sample_max_draw)) 20L else task$sample_max_draw)
    part <- psa_add_partials(part, r$part)
    per_loc[[Country]] <- list(c1 = r$c1, c2 = r$c2)
    checks[[Country]] <- r$checks; timing[[Country]] <- r$timing
    if (!is.null(r$long)) long[[Country]] <- r$long
    if (!is.null(r$econ_detail)) econ_det[[Country]] <- r$econ_detail
  }
  mem <- gc(verbose = FALSE)
  list(part = part, per_loc = per_loc, checks = rbindlist(checks), timing = rbindlist(timing),
       long = long, econ_detail = econ_det, worker_pid = Sys.getpid(),
       max_mem_mb = sum(mem[, ncol(mem)]))
}

#...........................................................
# Block combination, ASMR, endpoints ----
#...........................................................

# Report ASMR weights (aim1_report.Rmd chunk `helpers`; review defect D4: this is
# a decade-step approximation, not the published WHO table; reproduced as is).
psa_who_std <- function() {
  full_who_std <- data.table(
    age = 0:100,
    weight = c(rep(0.013, 10), rep(0.014, 10), rep(0.013, 10), rep(0.012, 10),
               rep(0.011, 10), rep(0.009, 10), rep(0.007, 10), rep(0.005, 10),
               rep(0.003, 10), rep(0.001, 10), 0.001))
  who_std <- full_who_std[age >= 20 & age <= 94]
  who_std <- rbind(who_std, data.table(age = 95, weight = full_who_std[age >= 95, sum(weight)]))
  who_std[, weight := weight / sum(weight)]
  who_std[]
}

# ASMR per scenario-year-draw from global (dead, well, sick) by age. As in the
# report, Nx = sum(well) + sum(sick) over the four cause chains (review defect
# D3: population counted four times); reproduced so point estimates match.
psa_asmr_from_g3 <- function(g3) {
  w <- psa_who_std()$weight
  d <- dim(g3)[1:4]
  dead <- array(g3[, , , , 1L], d)
  Nx <- array(g3[, , , , 2L], d) + array(g3[, , , , 3L], d)
  rate <- dead / Nx
  out <- apply(rate, c(1L, 3L, 4L), function(r) sum(r * w, na.rm = TRUE)) * 1e5
  array(out, d[c(1L, 3L, 4L)])                                        # [scen, year, B]
}

psa_combine_block <- function(results, who_levels, draw_ids, locs) {
  part <- results[[1]]$part
  if (length(results) > 1L) for (r in results[-1]) part <- psa_add_partials(part, r$part)
  per_loc <- do.call(c, lapply(results, `[[`, "per_loc"))
  miss <- setdiff(locs, names(per_loc))
  if (length(miss)) stop("Block is missing countries: ", paste(miss, collapse = ", "))
  per_loc <- per_loc[locs]
  list(draw_ids = draw_ids,
       g1 = part$g1, g2 = part$g2, e1 = part$e1,
       asmr = psa_asmr_from_g3(part$g3),
       g3_draw0 = if (0L %in% draw_ids) part$g3[, , , match(0L, draw_ids), , drop = FALSE] else NULL,
       g4 = part$g4, g5 = part$g5,
       c1 = simplify2array(lapply(per_loc, `[[`, "c1")),     # [scen, 5, B, loc]
       c2 = simplify2array(lapply(per_loc, `[[`, "c2")),     # [scen, 26, B, loc]
       who_levels = who_levels, locs = locs,
       checks = rbindlist(lapply(results, `[[`, "checks")),
       timing = rbindlist(lapply(results, `[[`, "timing")),
       worker_mem_mb = vapply(results, `[[`, numeric(1), "max_mem_mb"),
       long = do.call(c, lapply(results, `[[`, "long")),
       econ_detail = do.call(c, lapply(results, `[[`, "econ_detail")))
}

# Concatenate blocks along the draw dimension.
psa_bind_blocks <- function(blocks) {
  cat_last <- function(xs) {
    d <- dim(xs[[1]]); n <- length(d)
    arr <- array(unlist(xs, use.names = FALSE), c(d[-n], sum(vapply(xs, function(x) dim(x)[n], 1L))))
    arr
  }
  # draw is the 4th of 4 dims (g1), 4th of 5 (g2, e1: move), 3rd of 3 (asmr)
  perm_bind <- function(xs, draw_dim) {
    n <- length(dim(xs[[1]]))
    perm <- c(setdiff(seq_len(n), draw_dim), draw_dim)
    b <- cat_last(lapply(xs, aperm, perm))
    aperm(b, order(perm))
  }
  list(draw_ids = unlist(lapply(blocks, `[[`, "draw_ids")),
       g1 = perm_bind(lapply(blocks, `[[`, "g1"), 4L),
       g2 = perm_bind(lapply(blocks, `[[`, "g2"), 4L),
       e1 = perm_bind(lapply(blocks, `[[`, "e1"), 4L),
       asmr = perm_bind(lapply(blocks, `[[`, "asmr"), 3L),
       c1 = perm_bind(lapply(blocks, `[[`, "c1"), 3L),
       c2 = perm_bind(lapply(blocks, `[[`, "c2"), 3L),
       g3_draw0 = Filter(Negate(is.null), lapply(blocks, `[[`, "g3_draw0")))
}

# Long data.table from an array; dims vary fastest-first as in R's storage.
psa_array_dt <- function(arr, labels, value) {
  stopifnot(length(labels) == length(dim(arr)))
  grid <- do.call(CJ, c(rev(labels), list(sorted = FALSE)))
  setcolorder(grid, names(labels))
  grid[, (value) := as.vector(arr)]
  grid[]
}

#...........................................................
# Draw-level derived endpoints (every estimand formed WITHIN a draw) ----
#...........................................................

psa_build_endpoints <- function(bound, meta) {
  ids <- bound$draw_ids
  design <- psa_reduce_design()
  lab <- function(x) unname(PSA_SCENARIO_LABEL[x])
  g1 <- psa_array_dt(bound$g1, list(scenario = PSA_SCENARIOS, grp = design$g1$key,
                                    year = PSA_YEARS, draw_id = ids), "dead")
  g1[, c("cause", "sex", "age_group") := tstrsplit(grp, "|", fixed = TRUE)]
  g1[, grp := NULL]
  rep_yrs <- g1[year >= PSA_REPORT_YEAR0]
  delayed <- function(by) {
    x <- rep_yrs[, .(deaths = sum(dead)), by = c("draw_id", "scenario", by)]
    b <- x[scenario == "baseline", c("draw_id", by, "deaths"), with = FALSE]
    setnames(b, "deaths", "baseline_deaths")
    x <- b[x[scenario != "baseline"], on = c("draw_id", by)]
    x[, `:=`(deaths_delayed = baseline_deaths - deaths, intervention = lab(scenario))]
    x[]
  }
  out <- list()
  out$annual <- delayed("year")
  setorder(out$annual, draw_id, scenario, year)
  out$annual[, cum_deaths_delayed := cumsum(deaths_delayed), by = .(draw_id, scenario)]
  bau_total <- rep_yrs[scenario == "baseline", .(bau_total_deaths = sum(dead)), by = draw_id]
  out$cumul <- delayed(character())
  out$cumul <- bau_total[out$cumul, on = "draw_id"]
  out$cumul[, `:=`(deaths_delayed_ave = deaths_delayed / 25,
                   pct_of_bau = deaths_delayed / bau_total_deaths * 100)]
  out$cause <- delayed("cause")
  out$age_sex <- delayed(c("age_group", "sex"))
  out$age <- delayed("age_group")
  out$age[, `:=`(deaths_projected = deaths, deaths_delayed_ave = deaths_delayed / 25,
                 deaths_delayed_pct = deaths_delayed / baseline_deaths)]
  out$sex <- delayed("sex")
  # Country, region and income endpoints from per-location totals.
  c1 <- psa_array_dt(bound$c1, list(scenario = PSA_SCENARIOS,
                                    metric = c("dead_2025_2050", "dead_2026_2050", "yll", "yld", "daly"),
                                    draw_id = ids, location = meta$locs), "value")
  c1 <- dcast(c1, draw_id + location + scenario ~ metric, value.var = "value")
  c1 <- meta$loc_region[c1, on = "location"]
  geo <- function(by, dead_col) {
    x <- c1[, .(deaths = sum(get(dead_col))), by = c("draw_id", "scenario", by)]
    b <- x[scenario == "baseline", c("draw_id", by, "deaths"), with = FALSE]
    setnames(b, "deaths", "baseline_deaths")
    x <- b[x[scenario != "baseline"], on = c("draw_id", by)]
    x[, `:=`(deaths_projected = deaths, deaths_delayed = baseline_deaths - deaths,
             intervention = lab(scenario))]
    x[, `:=`(deaths_delayed_ave = deaths_delayed / 25,
             deaths_delayed_pct = deaths_delayed / baseline_deaths)]
    x[]
  }
  out$country <- geo(c("location", "region"), "dead_2025_2050")
  out$region <- geo("region", "dead_2025_2050")
  c2 <- psa_array_dt(bound$c2, list(scenario = PSA_SCENARIOS, year = PSA_REPORT_YEAR0:max(PSA_YEARS),
                                    draw_id = ids, location = meta$locs), "dead")
  c2 <- meta$loc_region[, .(location, region)][c2, on = "location"]
  ra <- c2[, .(deaths = sum(dead)), by = .(draw_id, scenario, region, year)]
  rb <- ra[scenario == "baseline", .(draw_id, region, year, baseline_deaths = deaths)]
  ra <- rb[ra[scenario != "baseline"], on = .(draw_id, region, year)]
  ra[, `:=`(deaths_delayed = baseline_deaths - deaths, intervention = lab(scenario))]
  setorder(ra, draw_id, scenario, region, year)
  ra[, cum_deaths_delayed := cumsum(deaths_delayed), by = .(draw_id, scenario, region)]
  out$region_annual <- ra
  # ASMR under each scenario (report section "ASMR Under Intervention Scenarios").
  out$asmr <- psa_array_dt(bound$asmr, list(scenario = PSA_SCENARIOS,
                                            year = PSA_ASMR_YEAR0:max(PSA_YEARS), draw_id = ids), "ASMR")
  out$asmr[, intervention := lab(scenario)]
  # Burden of disease (07 analogue; 2026-2050).
  g2 <- psa_array_dt(bound$g2, list(scenario = PSA_SCENARIOS, cause = PSA_CAUSES,
                                    year = PSA_INT_YEAR:max(PSA_YEARS), draw_id = ids,
                                    metric = c("yll", "yld", "daly")), "value")
  g2 <- dcast(g2, draw_id + scenario + cause + year ~ metric, value.var = "value")
  d26 <- g1[year >= PSA_INT_YEAR, .(dead = sum(dead)), by = .(draw_id, scenario, cause, year)]
  g2 <- d26[g2, on = .(draw_id, scenario, cause, year)]
  bod_by <- function(by) {
    x <- g2[, .(deaths = sum(dead), yll = sum(yll), yld = sum(yld), daly = sum(daly)),
            by = c("draw_id", "scenario", by)]
    b <- x[scenario == "baseline", c("draw_id", by, "deaths", "yll", "yld", "daly"), with = FALSE]
    setnames(b, c("deaths", "yll", "yld", "daly"), c("base_deaths", "base_yll", "base_yld", "base_daly"))
    x <- b[x[scenario != "baseline"], on = c("draw_id", by)]
    x[, `:=`(Deaths = base_deaths - deaths, YLLs = base_yll - yll, YLDs = base_yld - yld,
             DALYs = base_daly - daly, intervention = lab(scenario))]
    x[]
  }
  out$bod <- bod_by(character())
  out$bod_cause <- bod_by("cause")
  out$bod_cause[, `:=`(pct_deaths = Deaths / sum(Deaths) * 100, pct_dalys = DALYs / sum(DALYs) * 100),
                by = .(draw_id, scenario)]
  out$bod_annual <- bod_by("year")
  setorder(out$bod_annual, draw_id, scenario, year)
  out$bod_annual[, cum_daly_averted := cumsum(DALYs), by = .(draw_id, scenario)]
  bod_geo <- function(by) {
    x <- c1[, .(deaths = sum(dead_2026_2050), yll = sum(yll), yld = sum(yld), daly = sum(daly)),
            by = c("draw_id", "scenario", by)]
    b <- x[scenario == "baseline", c("draw_id", by, "deaths", "yll", "yld", "daly"), with = FALSE]
    setnames(b, c("deaths", "yll", "yld", "daly"), c("base_deaths", "base_yll", "base_yld", "base_daly"))
    x <- b[x[scenario != "baseline"], on = c("draw_id", by)]
    x[, `:=`(Deaths = base_deaths - deaths, YLLs = base_yll - yll, YLDs = base_yld - yld,
             DALYs = base_daly - daly, intervention = lab(scenario))]
    x[]
  }
  out$bod_region <- bod_geo("region")
  out$bod_income <- bod_geo("location_income")
  out$econ <- psa_econ_summary(bound, meta)
  out
}

# 08 sections 13-14 (region x scenario summary tables), per draw.
psa_econ_summary <- function(bound, meta) {
  ids <- bound$draw_ids
  regions <- c(meta$who_levels, "World")
  e1 <- psa_array_dt(bound$e1, list(who_region = regions, scenario = PSA_SCENARIOS,
                                    year = PSA_YEARS, draw_id = ids,
                                    metric = c("economic_value_e1_2_disc_r3", "vsly_value_e1_2_disc_r3",
                                               "economic_value_e1_5_disc_r3", "vsly_value_e1_5_disc_r3")),
                     "value")
  e1 <- e1[scenario != "baseline"]
  inc <- meta$income_08                                  # who_region x year (+ World)
  e1 <- inc[e1, on = .(who_region, year)]
  e1[, `:=`(valuation_type = fifelse(grepl("^vsly", metric), "VSLY", "VSL"),
            elasticity_case = fifelse(grepl("e1_2", metric), "e1_2_primary", "e1_5_sensitivity"))]
  yrs <- meta$k08$SUMMARY_YEARS
  snap <- e1[year %in% yrs, .(metric = value, share = value / total_income),
             by = .(draw_id, valuation_type, elasticity_case, who_region, scenario, year)]
  snap <- dcast(snap, draw_id + valuation_type + elasticity_case + who_region + scenario ~ year,
                value.var = c("metric", "share"))
  tot <- e1[year >= meta$k08$BASE_YEAR & year <= 2050,
            .(metric_total = sum(value), share_total = sum(value) / sum(total_income)),
            by = .(draw_id, valuation_type, elasticity_case, who_region, scenario)]
  out <- snap[tot, on = .(draw_id, valuation_type, elasticity_case, who_region, scenario)]
  out[who_region == PSA_NA_REGION, who_region := NA_character_]
  out[, intervention := unname(PSA_SCENARIO_LABEL[scenario])]
  out[]
}

# Deterministic BAU series (identical in every draw; taken from draw 0).
psa_bau_series <- function(block1, meta) {
  if (block1$draw_ids[1] != 0L) stop("The first block must start with draw 0.")
  g5 <- as.data.table(block1$g5)[, year := PSA_YEARS]
  trend <- g5[year >= PSA_ASMR_YEAR0, .(year, new_cases = newcases, sick, pop)]
  key <- data.table(cause = rep(PSA_CAUSES, each = length(PSA_AGES)),
                    age = rep(PSA_AGES, times = length(PSA_CAUSES)))
  yrs <- PSA_ASMR_YEAR0:max(PSA_YEARS)
  g4 <- rbindlist(lapply(seq_along(yrs), function(j) {
    key[, .(cause, age, year = yrs[j], deaths_dx = block1$g4$dead[, j],
            well = block1$g4$well[, j], sick = block1$g4$sick[, j])]
  }))
  g4[, Nx := well + sick]
  w <- psa_who_std()
  g4 <- w[g4, on = "age"]
  asmr_cause <- g4[, .(ASMR = sum(deaths_dx / Nx * weight, na.rm = TRUE) * 1e5), by = .(year, cause)]
  base <- asmr_cause[year == 2019, .(cause, ASMR_base = ASMR)]
  asmr_cause <- base[asmr_cause, on = "cause"]
  asmr_cause[, index := ASMR / ASMR_base * 100]
  list(trend = trend, asmr_cause = asmr_cause)
}

#...........................................................
# Interval summaries and Monte Carlo diagnostics ----
#...........................................................

PSA_PROBS <- c(0.025, 0.975)

# Point = draw 0 (the deterministic 06 result); interval = 2.5th/97.5th
# percentiles of the matched Monte Carlo draws (draw_id >= 1), taken AFTER the
# derived estimand has been formed within each draw.
psa_summarise <- function(dt, by, vars, probs = PSA_PROBS) {
  rbindlist(lapply(vars, function(v) {
    x <- dt[, c("draw_id", by, v), with = FALSE]
    setnames(x, v, ".psa_value")                     # fixed name: no column collisions
    s <- x[draw_id > 0L, .(
      lower = quantile(.psa_value, probs[1], type = 7, names = FALSE),
      upper = quantile(.psa_value, probs[2], type = 7, names = FALSE),
      mc_mean = mean(.psa_value), mc_median = median(.psa_value),
      mc_sd = if (.N > 1) sd(.psa_value) else NA_real_,
      mc_min = min(.psa_value), mc_max = max(.psa_value), n_draws = .N), by = by]
    p <- x[draw_id == 0L, c(by, ".psa_value"), with = FALSE]
    setnames(p, ".psa_value", "point")
    m <- if (length(by)) s[p, on = by] else cbind(p, s)
    m[, variable := v]
    m[, degenerate := n_draws > 0 & (mc_max - mc_min) <= 1e-12 * pmax(1, abs(point))]
    m[, point_in_interval := point >= lower - 1e-9 * pmax(1, abs(point)) &
                             point <= upper + 1e-9 * pmax(1, abs(point))]
    m[]
  }), fill = TRUE)
}

psa_summarise_endpoints <- function(ep) {
  spec <- list(
    annual = list(by = c("scenario", "intervention", "year"),
                  vars = c("baseline_deaths", "deaths", "deaths_delayed", "cum_deaths_delayed")),
    cumul = list(by = c("scenario", "intervention"),
                 vars = c("baseline_deaths", "deaths", "deaths_delayed", "deaths_delayed_ave",
                          "pct_of_bau", "bau_total_deaths")),
    cause = list(by = c("scenario", "intervention", "cause"),
                 vars = c("baseline_deaths", "deaths", "deaths_delayed")),
    age_sex = list(by = c("scenario", "intervention", "age_group", "sex"), vars = "deaths_delayed"),
    age = list(by = c("scenario", "intervention", "age_group"),
               vars = c("deaths_projected", "baseline_deaths", "deaths_delayed",
                        "deaths_delayed_ave", "deaths_delayed_pct")),
    sex = list(by = c("scenario", "intervention", "sex"), vars = "deaths_delayed"),
    country = list(by = c("scenario", "intervention", "location", "region"),
                   vars = c("deaths_projected", "baseline_deaths", "deaths_delayed",
                            "deaths_delayed_ave", "deaths_delayed_pct")),
    region = list(by = c("scenario", "intervention", "region"),
                  vars = c("deaths_projected", "baseline_deaths", "deaths_delayed",
                           "deaths_delayed_ave", "deaths_delayed_pct")),
    region_annual = list(by = c("scenario", "intervention", "region", "year"),
                         vars = c("deaths_delayed", "cum_deaths_delayed")),
    asmr = list(by = c("scenario", "intervention", "year"), vars = "ASMR"),
    bod = list(by = c("scenario", "intervention"), vars = c("Deaths", "YLLs", "YLDs", "DALYs")),
    bod_cause = list(by = c("scenario", "intervention", "cause"),
                     vars = c("Deaths", "YLLs", "YLDs", "DALYs", "pct_deaths", "pct_dalys")),
    bod_annual = list(by = c("scenario", "intervention", "year"), vars = c("DALYs", "cum_daly_averted")),
    bod_region = list(by = c("scenario", "intervention", "region"), vars = c("Deaths", "YLLs", "YLDs", "DALYs")),
    bod_income = list(by = c("scenario", "intervention", "location_income"),
                      vars = c("Deaths", "YLLs", "YLDs", "DALYs"))
  )
  out <- lapply(names(spec), function(nm) psa_summarise(ep[[nm]], spec[[nm]]$by, spec[[nm]]$vars))
  names(out) <- names(spec)
  ecv <- setdiff(grep("^(metric|share)_", names(ep$econ), value = TRUE), character())
  out$econ <- psa_summarise(ep$econ, c("valuation_type", "elasticity_case", "who_region",
                                       "scenario", "intervention"), ecv)
  out
}

# Headline outcomes for convergence and Monte Carlo error.
psa_headlines <- function(ep) {
  rbindlist(list(
    ep$cumul[, .(draw_id, outcome = paste0("Deaths averted 2025-2050: ", intervention), value = deaths_delayed)],
    ep$bod[, .(draw_id, outcome = paste0("DALYs averted 2026-2050: ", intervention), value = DALYs)],
    ep$econ[valuation_type == "VSLY" & elasticity_case == "e1_2_primary" & who_region == "World",
            .(draw_id, outcome = paste0("VSLY share of GNI 2026-2050 (World): ", intervention),
              value = share_total)]
  ))
}

psa_order_stat_ci <- function(x, p, conf = 0.95) {
  n <- length(x); xs <- sort(x); z <- qnorm(1 - (1 - conf) / 2)
  j <- max(1L, floor(n * p - z * sqrt(n * p * (1 - p))))
  k <- min(n, ceiling(n * p + z * sqrt(n * p * (1 - p))))
  c(xs[j], xs[k])
}

psa_convergence <- function(ep, checkpoints = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000),
                            rel_tol = 0.05) {
  h <- psa_headlines(ep)
  n_mc <- h[draw_id > 0L, uniqueN(draw_id)]
  cps <- sort(unique(c(checkpoints[checkpoints <= n_mc], n_mc)))
  path <- rbindlist(lapply(cps, function(n) {
    h[draw_id > 0L & draw_id <= n, .(n = n,
      lower = quantile(value, PSA_PROBS[1], type = 7, names = FALSE),
      upper = quantile(value, PSA_PROBS[2], type = 7, names = FALSE),
      mean = mean(value), mcse_mean = if (.N > 1) sd(value) / sqrt(.N) else NA_real_), by = outcome]
  }))
  point <- h[draw_id == 0L, .(outcome, point = value)]
  final <- h[draw_id > 0L, {
    lo_ci <- psa_order_stat_ci(value, PSA_PROBS[1]); hi_ci <- psa_order_stat_ci(value, PSA_PROBS[2])
    lo <- quantile(value, PSA_PROBS[1], type = 7, names = FALSE)
    hi <- quantile(value, PSA_PROBS[2], type = 7, names = FALSE)
    width <- hi - lo
    .(n = .N, lower = lo, upper = hi,
      lower_os_ci_lo = lo_ci[1], lower_os_ci_hi = lo_ci[2],
      upper_os_ci_lo = hi_ci[1], upper_os_ci_hi = hi_ci[2],
      rel_mc_error_lower = if (width > 0) (lo_ci[2] - lo_ci[1]) / 2 / width else NA_real_,
      rel_mc_error_upper = if (width > 0) (hi_ci[2] - hi_ci[1]) / 2 / width else NA_real_,
      degenerate = width <= 1e-12 * max(1, abs(mean(value))))
  }, by = outcome]
  final <- point[final, on = "outcome"]
  final[, converged := degenerate | (pmax(rel_mc_error_lower, rel_mc_error_upper) <= rel_tol)]
  list(path = path, final = final, rel_tol = rel_tol, n_mc = n_mc)
}
