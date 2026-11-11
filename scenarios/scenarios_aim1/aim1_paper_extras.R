# =============================================================================
# aim1_paper_extras.R
# -----------------------------------------------------------------------------
# Reporting-only helper that builds the additional artefacts the health-outcomes
# manuscript (docs/who_cvd_targets_paper1.Rmd) loads and displays:
#
#   * paper_table1.rds  -- target populations and coverage (HTN + statins), by
#                          WHO region x diabetes status (+ Global), on the fixed
#                          2025 denominator used by 04_define_interventions.R.
#   * paper_params.rds  -- the policy/effect settings actually used by the
#                          executed model run and the ACHIEVED control rates, so
#                          the manuscript never hard-codes a parameter value.
#   * paper_strata.rds  -- WHO region and World Bank income group ranked by
#                          deaths and DALYs averted and by % of BAU (for the
#                          "rank strata" reviewer request).
#
# It contains NO model logic: it only reads existing inputs (the target CSVs and
# country groupings that 06 consumes) and existing model output (out_model/ for
# the demographic denominator and BAU deaths, and the already-written
# paper_bod_* artefacts for DALYs). It is deterministic and side-effect free
# apart from the three saveRDS() writes.
#
# Sourced by aim1_report.Rmd (canonical) and runnable stand-alone; both call
# build_paper_extras() with the same wd_* paths and the same run settings, so
# they produce identical artefacts.
# =============================================================================

build_paper_extras <- function(wd_data, wd_raw, wd_outp,
                               # settings mirrored from the executed 06 run call
                               htn_target_total       = 150e6,
                               htn_diabetes_cap       = 0.80,
                               control_start_year     = 2026,
                               control_target_year    = 2030,
                               statin_target_coverage = 0.50,
                               statin_start_year      = 2026,
                               statin_target_year     = 2030,
                               adherence_ir           = 0.575,
                               adherence_cf           = 0.644,
                               prop_athero_stroke     = 0.60,
                               statin_age_min         = 40,
                               reporting_start_year   = 2026,
                               reporting_end_year     = 2050,
                               verbose                = TRUE) {
  suppressWarnings(suppressMessages({
    library(data.table)
  }))
  say <- function(...) if (verbose) message(...)
  paper_dir <- paste0(wd_outp, "paper/")
  if (!dir.exists(paper_dir)) dir.create(paper_dir, recursive = TRUE)

  ## ---------------------------------------------------------------------------
  ## (A) Country metadata: WHO region + World Bank income group.
  ##     Reproduces aim1_report.Rmd's country-metadata chunk exactly.
  ## ---------------------------------------------------------------------------
  countries_dt <- fread(paste0(wd_data, "Country_groupings_extended.csv"))
  who_regions  <- fread(paste0(wd_raw, "who-regions.csv"))
  setnames(who_regions,
           old = c("Entity", "Code", "World regions according to WHO"),
           new = c("location", "iso3", "region_who"))
  who_regions[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
  who_regions[, c("Year", "location") := NULL]
  countries_dt <- merge(countries_dt, who_regions, by = "iso3", all.x = TRUE)
  who_fix <- data.table(
    iso3 = c("BMU", "GRL", "PSE", "SDS", "TWN", "VCT", "VIR"),
    region_who_fix = c("Americas", "Europe", "Eastern Mediterranean", "Africa",
                       "Western Pacific", "Americas", "Americas")
  )
  countries_dt <- merge(countries_dt, who_fix, by = "iso3", all.x = TRUE)
  countries_dt[, region_who := fcoalesce(region_who, region_who_fix)]
  countries_dt[, region_who_fix := NULL]
  countries_dt[, region := fifelse(!is.na(region_who), region_who, region)]
  countries_dt[, region_who := NULL]
  income_groups <- fread(paste0(wd_raw, "World Bank Income Group 2026.csv"))
  income_groups[location_income == "", location_income := "L"]
  income_groups[, location_income := fcase(
    location_income == "L",  "Low income",
    location_income == "LM", "Lower-middle income",
    location_income == "UM", "Upper-middle income",
    location_income == "H",  "High income",
    default = location_income
  )]
  countries_dt <- merge(countries_dt, income_groups[, .(iso3, location_income)],
                        by = "iso3", all.x = TRUE)
  countries_dt <- unique(countries_dt, by = "location")

  region_order <- c("Africa", "Americas", "Eastern Mediterranean", "Europe",
                    "South-East Asia", "Western Pacific")
  income_order <- c("Low income", "Lower-middle income",
                    "Upper-middle income", "High income")

  ## ---------------------------------------------------------------------------
  ## (B) Model denominators from out_model: 40+ population (statin eligibility)
  ##     for 2025 and 2030, and cumulative BAU deaths by location. Read only the
  ##     slices needed, so the 709 MB DALY file is never touched.
  ## ---------------------------------------------------------------------------
  out_dir <- paste0(wd_outp, "out_model/")
  files   <- list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) stop("No out_model/*.rds found; run 06 first.")

  pull_country <- function(f) {
    x <- readRDS(f); setDT(x)
    if ("scenario" %in% names(x) && !"intervention" %in% names(x))
      setnames(x, "scenario", "intervention")
    base_lbl <- c("baseline", "BAU", "b.a.u")
    # pop is replicated across causes/interventions -> take one cause, baseline.
    pop_slice <- x[intervention %in% base_lbl & cause == "ihd" &
                     year %in% c(2025, 2030) & age >= statin_age_min,
                   .(pop40 = sum(pop, na.rm = TRUE)), by = .(location, year)]
    bau_slice <- x[intervention %in% base_lbl &
                     year >= reporting_start_year & year <= reporting_end_year,
                   .(bau_deaths = sum(dead, na.rm = TRUE)), by = .(location)]
    list(pop = pop_slice, bau = bau_slice)
  }
  parts <- lapply(files, pull_country)
  pop40 <- rbindlist(lapply(parts, `[[`, "pop"))
  bau   <- rbindlist(lapply(parts, `[[`, "bau"))
  pop40 <- pop40[!is.na(location)]
  bau   <- bau[!is.na(location)]
  pop40 <- merge(pop40, countries_dt[, .(location, region, location_income)],
                 by = "location", all.x = TRUE)
  bau   <- merge(bau,   countries_dt[, .(location, region, location_income)],
                 by = "location", all.x = TRUE)

  ## ---------------------------------------------------------------------------
  ## (C) HTN control targets (150M, mutually-exclusive subgroups), by region.
  ##     Same aggregation as aim1_report.Rmd's control-summary chunk.
  ## ---------------------------------------------------------------------------
  htn <- fread(paste0(wd_data, "htn_control_targets_by_loc.csv"))
  htn <- htn[scenario_id == "bp_combined"]
  htn <- merge(htn, countries_dt[, .(location, region)], by = "location", all.x = TRUE)

  agg_htn <- function(dt) {
    base <- dt[, .(
      htn_pop_2025    = sum(htn_population_2025, na.rm = TRUE),
      controlled_2025 = sum(controlled_2025, na.rm = TRUE),
      controlled_2030 = sum(controlled_2030, na.rm = TRUE),
      additional_2030 = sum(additional_controlled_2030, na.rm = TRUE)
    ), by = .(region, subgroup)]
    tot <- dt[, .(
      subgroup        = "total",
      htn_pop_2025    = sum(htn_population_2025, na.rm = TRUE),
      controlled_2025 = sum(controlled_2025, na.rm = TRUE),
      controlled_2030 = sum(controlled_2030, na.rm = TRUE),
      additional_2030 = sum(additional_controlled_2030, na.rm = TRUE)
    ), by = .(region)]
    rbind(base, tot)
  }
  htn_reg <- rbind(agg_htn(htn[!is.na(region)]),
                   agg_htn(copy(htn)[, region := "Global"]))
  htn_reg[, `:=`(
    coverage_2025 = fifelse(htn_pop_2025 > 0, controlled_2025 / htn_pop_2025, NA_real_),
    coverage_2030 = fifelse(htn_pop_2025 > 0, controlled_2030 / htn_pop_2025, NA_real_)
  )]
  htn_reg[, change_pp := (coverage_2030 - coverage_2025) * 100]

  ## ---------------------------------------------------------------------------
  ## (D) Statin eligibility (adults >= 40, the model's operational eligibility).
  ##     Baseline coverage and the 2025/2030 eligible denominators by region.
  ## ---------------------------------------------------------------------------
  st <- fread(paste0(wd_data, "statins_control_targets_by_loc.csv"))
  st <- merge(st, countries_dt[, .(iso3, region)], by = "iso3", all.x = TRUE)
  st_base <- st[, .(
    statin_eligible_2025   = sum(pop_40plus_2025, na.rm = TRUE),
    statin_covered_2025    = sum(pop_40plus_2025 * statins_baseline_2025, na.rm = TRUE)
  ), by = .(region)]
  st_glob <- st[, .(region = "Global",
                    statin_eligible_2025 = sum(pop_40plus_2025, na.rm = TRUE),
                    statin_covered_2025  = sum(pop_40plus_2025 * statins_baseline_2025, na.rm = TRUE))]
  st_reg <- rbind(st_base, st_glob)
  # eligible in 2030 from the model's own 40+ population projection
  pop40_2030_reg <- pop40[year == 2030, .(statin_eligible_2030 = sum(pop40, na.rm = TRUE)), by = region]
  pop40_2030_glob <- pop40[year == 2030, .(region = "Global",
                                           statin_eligible_2030 = sum(pop40, na.rm = TRUE))]
  st_reg <- merge(st_reg, rbind(pop40_2030_reg, pop40_2030_glob), by = "region", all.x = TRUE)
  st_reg[, statin_coverage_2025 := fifelse(statin_eligible_2025 > 0,
                                           statin_covered_2025 / statin_eligible_2025, NA_real_)]
  st_reg[, statin_coverage_2030 := statin_target_coverage]

  ## ---------------------------------------------------------------------------
  ## paper_table1: region x diabetes-status HTN block + statin fields on totals.
  ## ---------------------------------------------------------------------------
  subgroup_lbl <- c(htn_no_diabetes = "No diabetes",
                    htn_diabetes    = "Diabetes",
                    total           = "Total")
  paper_table1 <- copy(htn_reg)
  paper_table1[, diabetes_status := subgroup_lbl[subgroup]]
  # attach statin fields to the Total row of each region only
  paper_table1 <- merge(paper_table1, st_reg, by = "region", all.x = TRUE)
  paper_table1[subgroup != "total",
               c("statin_eligible_2025", "statin_eligible_2030",
                 "statin_coverage_2025", "statin_coverage_2030",
                 "statin_covered_2025") := NA]
  reg_lvls <- c(region_order, "Global")
  paper_table1[, region := factor(region, levels = reg_lvls)]
  paper_table1[, diabetes_status := factor(diabetes_status,
                                           levels = c("No diabetes", "Diabetes", "Total"))]
  setorder(paper_table1, region, diabetes_status)
  paper_table1 <- paper_table1[, .(region, diabetes_status, subgroup,
                                   htn_pop_2025, controlled_2025, coverage_2025,
                                   controlled_2030, coverage_2030, additional_2030, change_pp,
                                   statin_eligible_2025, statin_eligible_2030,
                                   statin_coverage_2025, statin_coverage_2030)]
  saveRDS(paper_table1, paste0(paper_dir, "paper_table1.rds"))
  say("Saved paper_table1.rds (", nrow(paper_table1), " rows)")

  ## ---------------------------------------------------------------------------
  ## paper_params: settings actually run + achieved control rates (global).
  ## ---------------------------------------------------------------------------
  g_total <- htn_reg[region == "Global" & subgroup == "total"]
  g_nod   <- htn_reg[region == "Global" & subgroup == "htn_no_diabetes"]
  g_dia   <- htn_reg[region == "Global" & subgroup == "htn_diabetes"]
  st_g    <- st_reg[region == "Global"]

  paper_params <- list(
    # business-as-usual CVD deaths over the reporting window (2026-2050), matching
    # int_year in 07_output_dalys.R and the DSA; the ordinary paper_scalars
    # bau_total_deaths spans 2025-2050 and is ~19 M larger.
    bau_total_deaths_2026   = sum(bau$bau_deaths, na.rm = TRUE),
    # policy targets / horizon
    htn_target_total        = htn_target_total,
    htn_diabetes_cap        = htn_diabetes_cap,
    control_start_year      = control_start_year,
    control_target_year     = control_target_year,
    statin_target_coverage  = statin_target_coverage,
    statin_start_year       = statin_start_year,
    statin_target_year      = statin_target_year,
    statin_age_min          = statin_age_min,
    reporting_start_year    = reporting_start_year,
    reporting_end_year      = reporting_end_year,
    # effect / adherence parameters actually passed to the run call
    adherence_ir            = adherence_ir,
    adherence_cf            = adherence_cf,
    prop_athero_stroke      = prop_athero_stroke,
    statin_rr_ir_ihd        = 0.74,
    statin_rr_ir_istroke    = 0.80,
    statin_rr_cf_ihd        = 0.80,
    statin_rr_cf_istroke    = 0.96,
    af_ihd_default          = 0.1497,
    af_istroke_default      = 0.1161,
    cf_reduction_ihd        = 0.24,
    cf_reduction_istroke    = 0.36,
    cf_reduction_hstroke    = 0.76,
    cf_reduction_hhd        = 0.20,
    # achieved 150M split and control rates (2030, fixed 2025 denominator)
    added_total             = g_total$additional_2030,
    added_no_diabetes       = g_nod$additional_2030,
    added_diabetes          = g_dia$additional_2030,
    share_no_diabetes       = g_nod$additional_2030 / g_total$additional_2030,
    share_diabetes          = g_dia$additional_2030 / g_total$additional_2030,
    htn_pop_total_2025      = g_total$htn_pop_2025,
    coverage_total_2025     = g_total$coverage_2025,
    coverage_total_2030     = g_total$coverage_2030,
    achieved_control_no_diabetes_2030 = g_nod$coverage_2030,
    achieved_control_diabetes_2030    = g_dia$coverage_2030,
    baseline_control_no_diabetes_2025 = g_nod$coverage_2025,
    baseline_control_diabetes_2025    = g_dia$coverage_2025,
    # statin coverage (global, adults 40+)
    statin_eligible_2025    = st_g$statin_eligible_2025,
    statin_eligible_2030    = st_g$statin_eligible_2030,
    statin_baseline_coverage_2025 = st_g$statin_coverage_2025
  )
  saveRDS(paper_params, paste0(paper_dir, "paper_params.rds"))
  say("Saved paper_params.rds (achieved diabetes control 2030 = ",
      round(100 * paper_params$achieved_control_diabetes_2030, 1), "%)")

  ## ---------------------------------------------------------------------------
  ## paper_strata: region & income ranked by deaths & DALYs averted and % of BAU.
  ## Deaths averted / DALYs averted come from the already-written paper artefacts;
  ## BAU deaths come from out_model (above). All Interventions scenario.
  ## ---------------------------------------------------------------------------
  read_if <- function(f) { p <- paste0(paper_dir, f); if (file.exists(p)) { x <- readRDS(p); setDT(x); x } else NULL }
  reg_bod    <- read_if("paper_bod_region.rds")     # region x intervention: Deaths, DALYs averted
  inc_bod    <- read_if("paper_bod_income.rds")     # income x intervention: Deaths, DALYs averted
  ALLINT <- "All Interventions"

  # BAU deaths (2026-2050) by region and income from out_model, matched window.
  bau_reg <- bau[!is.na(region),         .(bau_deaths = sum(bau_deaths, na.rm = TRUE)), by = region]
  bau_inc <- bau[!is.na(location_income),.(bau_deaths = sum(bau_deaths, na.rm = TRUE)), by = location_income]

  # ---- region strata (All Interventions only) ----
  strata_region <- NULL
  if (!is.null(reg_bod)) {
    rb <- reg_bod[as.character(intervention) == ALLINT,
                  .(stratum = as.character(region), deaths_averted = Deaths, dalys_averted = DALYs)]
    rb <- merge(rb, bau_reg[, .(stratum = region, bau_deaths)], by = "stratum", all.x = TRUE)
    rb[, deaths_pct_bau := fifelse(bau_deaths > 0, deaths_averted / bau_deaths, NA_real_)]
    rb <- rb[stratum %in% region_order]
    rb[, stratum := factor(stratum, levels = region_order)]; setorder(rb, stratum)
    rb[, `:=`(stratum = as.character(stratum), stratum_type = "WHO region")]
    strata_region <- rb
  }

  # ---- income strata (All Interventions only) ----
  strata_income <- NULL
  if (!is.null(inc_bod)) {
    ib <- inc_bod[as.character(intervention) == ALLINT,
                  .(stratum = as.character(location_income), deaths_averted = Deaths, dalys_averted = DALYs)]
    ib <- merge(ib, bau_inc[, .(stratum = as.character(location_income), bau_deaths)],
                by = "stratum", all.x = TRUE)
    ib[, deaths_pct_bau := fifelse(bau_deaths > 0, deaths_averted / bau_deaths, NA_real_)]
    ib <- ib[stratum %in% income_order]
    ib[, stratum := factor(stratum, levels = income_order)]; setorder(ib, stratum)
    ib[, `:=`(stratum = as.character(stratum), stratum_type = "World Bank income group")]
    strata_income <- ib
  }

  paper_strata <- rbindlist(list(strata_region, strata_income),
                            use.names = TRUE, fill = TRUE)
  saveRDS(paper_strata, paste0(paper_dir, "paper_strata.rds"))
  say("Saved paper_strata.rds (", nrow(paper_strata), " rows)")

  invisible(list(paper_table1 = paper_table1,
                 paper_params = paper_params,
                 paper_strata = paper_strata))
}

# ---- stand-alone execution (Rscript aim1_paper_extras.R) --------------------
if (sys.nframe() == 0L) {
  wd      <- "C:/Users/wrgar/OneDrive - UW/02Work/WHO-CVD/github/uw-who-cvdtargets/"
  wd_data <- paste0(wd, "data/processed/")
  wd_raw  <- paste0(wd, "data/raw/")
  wd_outp <- paste0(wd, "output/")
  build_paper_extras(wd_data, wd_raw, wd_outp)
}
