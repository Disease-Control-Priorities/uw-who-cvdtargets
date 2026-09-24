# =============================================================================
# 094_psa_validate.R -- Aim 1 PSA: validation against 06 / 07 / 08 / report
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions only. Every comparison
# returns a data.table row with the observed maximum absolute and relative
# difference and the explicit tolerance used; nothing is "fixed" to match.
# Reference inputs are read from output/ (read-only); results are written by
# the caller under output_psa/.

PSA_TOL <- list(
  exact_states = 0,          # batched engine vs saved 06 rows: bit-identical expected
  states_rel = 1e-12,        # batched vs per-draw reference path
  totals_rel = 1e-9,         # global/aggregate totals vs original artefacts
  totals_abs = 1e-6,         # plus an absolute floor for values that are exactly 0 (e.g. 2025 averted)
  params_abs = 1e-12         # rebuilt Sheet1 / sodium tables vs workbook / 06
)

psa_cmp <- function(check, x, y, tol_rel = NA_real_, tol_abs = NA_real_, scope = "") {
  x <- as.numeric(x); y <- as.numeric(y)
  if (length(x) != length(y)) {
    return(data.table(check = check, scope = scope, n = NA_integer_, max_abs_diff = NA_real_,
                      max_rel_diff = NA_real_, tol_abs = tol_abs, tol_rel = tol_rel,
                      identical = FALSE, pass = FALSE, note = "length mismatch"))
  }
  na_mismatch <- sum(is.na(x) != is.na(y))
  ok <- !is.na(x) & !is.na(y)
  d <- abs(x[ok] - y[ok])
  r <- d / pmax(abs(y[ok]), .Machine$double.xmin)
  r[d == 0] <- 0
  mad <- if (length(d)) max(d) else 0
  mrd <- if (length(r)) max(r) else 0
  # Elementwise |x - y| <= tol_abs + tol_rel * |y| (absolute part handles values
  # that are exactly zero in the reference, e.g. deaths averted in 2025).
  el_ok <- d <= (if (is.na(tol_abs)) 0 else tol_abs) + (if (is.na(tol_rel)) 0 else tol_rel) * abs(y[ok])
  pass <- na_mismatch == 0 && all(el_ok)
  data.table(check = check, scope = scope, n = length(x), max_abs_diff = mad, max_rel_diff = mrd,
             tol_abs = tol_abs, tol_rel = tol_rel, identical = na_mismatch == 0 && mad == 0,
             pass = pass, note = if (na_mismatch) paste(na_mismatch, "NA-pattern mismatches") else "")
}

#...........................................................
# V1 parameter reconstruction ----
#...........................................................

psa_validate_parameters <- function(sheet1, rr10, inp, wb_sheet1) {
  s0 <- psa_sheet1_for_draw(sheet1, 0L)
  rc <- psa_check_sheet1_reconstruction(s0, wb_sheet1, PSA_TOL$params_abs)
  v <- rc[, .(check = "V1 Sheet1 draw-0 rebuild vs workbook Sheet1", scope = column, n = n_rows,
              max_abs_diff, max_rel_diff = NA_real_, tol_abs = PSA_TOL$params_abs, tol_rel = NA_real_,
              identical = max_abs_diff == 0, pass, note = "")]
  m <- merge(s0, inp$det_etihad_rr_bin, by = c("cause", "bp_cat"))
  v <- rbind(v, psa_cmp("V1 Sheet1 draw-0 vs 06 ETIHAD_RR_BIN (effect sizes)",
                        c(m$effect_size_nodiabetes.x, m$effect_size_diabetes.x),
                        c(m$effect_size_nodiabetes.y, m$effect_size_diabetes.y),
                        tol_abs = PSA_TOL$params_abs, scope = "all cause x bin rows"))
  r0 <- rr10[draw_id == 0][, draw_id := NULL][]
  same_shape <- nrow(r0) == nrow(inp$det_etihad_rr10) &&
    identical(r0$cause, inp$det_etihad_rr10$cause) && identical(r0$bp_cat, inp$det_etihad_rr10$bp_cat)
  v <- rbind(v, psa_cmp("V1 sodium RR table draw 0 vs 06 ETIHAD_RR", r0$rr_per_10mmhg,
                        inp$det_etihad_rr10$rr_per_10mmhg, tol_abs = PSA_TOL$params_abs,
                        scope = if (same_shape) "rows, order and NA pattern identical" else "SHAPE DIFFERS"))
  v[check == "V1 sodium RR table draw 0 vs 06 ETIHAD_RR", pass := pass & same_shape]
  v[]
}

#...........................................................
# V2/V4 model draw 0 vs saved and live 06 ----
#...........................................................

PSA_STATE_VARS <- c("well", "sick", "newcases", "dead", "pop", "all.mx", "eff_ir", "eff_cf")

psa_compare_model_tables <- function(check, a, b, tol_abs = 0, tol_rel = NA_real_, scope = "") {
  keys <- c("scenario", "sex", "cause", "age", "year")
  a <- copy(a); b <- copy(b)
  setkeyv(a, keys); setkeyv(b, keys)
  if (nrow(a) != nrow(b) || !isTRUE(all.equal(a[, ..keys], b[, ..keys], check.attributes = FALSE))) {
    return(data.table(check = check, scope = scope, n = NA_integer_, max_abs_diff = NA_real_,
                      max_rel_diff = NA_real_, tol_abs = tol_abs, tol_rel = tol_rel,
                      identical = FALSE, pass = FALSE, note = "row keys differ"))
  }
  rbindlist(lapply(PSA_STATE_VARS, function(v) {
    psa_cmp(check, a[[v]], b[[v]], tol_rel = tol_rel, tol_abs = tol_abs, scope = paste(scope, v))
  }))
}

psa_read_saved_06 <- function(paths, Country) {
  f <- file.path(paths$wd_outp_original, "out_model", paste0("model_output_", Country, ".rds"))
  if (!file.exists(f)) return(NULL)
  x <- readRDS(f); setDT(x); x
}

psa_validate_model_draw0 <- function(long_by_country, paths) {
  rbindlist(lapply(names(long_by_country), function(Country) {
    saved <- psa_read_saved_06(paths, Country)
    if (is.null(saved)) {
      return(data.table(check = "V2 draw 0 vs saved 06 output", scope = Country, pass = NA,
                        note = "saved 06 output not found"))
    }
    d0 <- long_by_country[[Country]][draw_id == 0L]
    psa_compare_model_tables("V2 PSA batched draw 0 vs saved 06 output (all 6 scenarios)",
                             d0, saved, tol_abs = PSA_TOL$exact_states, scope = Country)
  }), fill = TRUE)
}

# Live 06: 06's own run_multiple_scenarios() from the setup environment.
psa_live_06 <- function(env06_setup, Country, run_args) {
  res <- env06_setup$run_multiple_scenarios(
    Country = Country, scenario_list = env06_setup$scenarios,
    htn_scenario_ids = env06_setup$htn_scenario_ids, dt_hbp_targets = env06_setup$dt_hbp_targets,
    statin_target_coverage = run_args$statin_target_coverage,
    statin_start_year = run_args$statin_start_year, statin_target_year = run_args$statin_target_year,
    adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
    baseline_statin_coverage = run_args$baseline_statin_coverage,
    saltmet = run_args$saltmet, salteff = run_args$salteff,
    saltyear1 = run_args$saltyear1, saltyear2 = run_args$saltyear2,
    tfa_target_tfa = run_args$tfa_target_tfa, tfa_policy_start_year = run_args$tfa_policy_start_year)
  res[, htn_target_scenario := "aim1"]
  res[]
}

#...........................................................
# V3 batched engine vs per-draw reference path ----
#...........................................................

psa_reference_draw <- function(task) {
  fns <- get(".psa_fns", envir = globalenv())
  sink(tempfile()); on.exit(sink(), add = TRUE)          # 06 prints progress; discard it
  s1 <- psa_sheet1_for_draw(task$sheet1, task$draw_id)
  r1 <- task$rr10[draw_id == task$draw_id][, draw_id := NULL][]
  res <- psa_run_multiple_scenarios(task$Country, task$scenarios, task$htn_scenario_ids,
                                    task$dt_hbp_targets, task$run_args, task$cin, fns,
                                    task$repYear, task$dt_gbd_rr,
                                    etihad_rr_table = s1, etihad_rr10_table = r1)
  res[, draw_id := task$draw_id]
  res[]
}

psa_validate_batched_vs_reference <- function(ref_list, long) {
  rbindlist(lapply(ref_list, function(ref) {
    id <- ref$draw_id[1]
    psa_compare_model_tables(sprintf("V3 batched vs per-draw reference (draw %d)", id),
                             long[draw_id == id], ref, tol_abs = NA_real_,
                             tol_rel = PSA_TOL$states_rel, scope = ref$location[1])
  }))
}

#...........................................................
# V5 07 analogue vs original dt_output_dalys.rds ----
#...........................................................

psa_validate_07 <- function(paths, countries, ep) {
  f <- file.path(paths$wd_outp_original, "dt_output_dalys.rds")
  if (!file.exists(f)) return(data.table(check = "V5 07 per-country", pass = NA, note = "file missing"))
  o <- readRDS(f); setDT(o)
  o <- o[location %in% countries]
  gc(verbose = FALSE)
  lab07 <- c(bp_no_diabetes_only = "HTN Control (No Diabetes)", bp_diabetes_only = "HTN Control (Diabetes)",
             statins_only = "Improved Statin Uptake", all_interventions = "All Interventions",
             baseline = "b.a.u")
  orig <- o[, .(yll = sum(yll, na.rm = TRUE), yld = sum(yld, na.rm = TRUE),
                daly = sum(daly, na.rm = TRUE), dead = sum(dead, na.rm = TRUE)),
            by = .(location, intervention)]
  c1 <- ep$c1_draw0[location %in% countries]
  c1[, intervention := lab07[scenario]]
  m <- merge(c1[!is.na(intervention)], orig, by = c("location", "intervention"))
  rbind(
    psa_cmp("V5 07 analogue YLL 2026-2050 vs dt_output_dalys.rds", m$yll.x, m$yll.y,
            tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = paste(countries, collapse = ", ")),
    psa_cmp("V5 07 analogue YLD 2026-2050 vs dt_output_dalys.rds", m$yld.x, m$yld.y,
            tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = paste(countries, collapse = ", ")),
    psa_cmp("V5 07 analogue DALY 2026-2050 vs dt_output_dalys.rds", m$daly.x, m$daly.y,
            tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = paste(countries, collapse = ", ")),
    psa_cmp("V5 07 analogue deaths 2026-2050 vs dt_output_dalys.rds", m$dead_2026_2050, m$dead,
            tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = paste(countries, collapse = ", ")))
}

#...........................................................
# V6 08 analogue vs original 08 outputs ----
#...........................................................

psa_validate_08_detail <- function(paths, econ_detail) {
  f <- file.path(paths$wd_outp_original, "08_vsl_results.rds")
  if (!file.exists(f) || !length(econ_detail)) {
    return(data.table(check = "V6 08 per-country", pass = NA, note = "file or detail missing"))
  }
  o <- readRDS(f); setDT(o)
  rows <- rbindlist(lapply(names(econ_detail), function(Country) {
    rbindlist(lapply(names(econ_detail[[Country]]), function(sn) {
      d <- econ_detail[[Country]][[sn]]
      data.table(location = Country, scenario = sn, year = PSA_YEARS,
                 deaths_averted = d$deaths_averted[, 1], life_years_gained_undisc = d$life_years_gained_undisc[, 1],
                 avg_adult_age = d$avg_adult_age[, 1], age_ref_5y = d$age_ref_5y[, 1],
                 le_avg_adult = d$le_avg_adult[, 1], vsly_e1_2 = d$vsly_e1_2[, 1],
                 economic_value_e1_2 = d$economic_value_e1_2[, 1], vsly_value_e1_2 = d$vsly_value_e1_2[, 1])
    }))
  }))
  m <- merge(rows, o, by = c("location", "scenario", "year"), suffixes = c(".psa", ".orig"))
  vars <- c("deaths_averted", "life_years_gained_undisc", "avg_adult_age", "age_ref_5y",
            "le_avg_adult", "vsly_e1_2", "economic_value_e1_2", "vsly_value_e1_2")
  rbindlist(lapply(vars, function(v) {
    psa_cmp(paste("V6 08 analogue", v, "vs 08_vsl_results.rds (draw 0)"),
            m[[paste0(v, ".psa")]], m[[paste0(v, ".orig")]], tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs,
            scope = paste(unique(m$location), collapse = ", "))
  }))
}

# Reference implementation of 08 sections 3-4, 7 and 7b, copied verbatim from
# 08_economic_value_calculation.R, applied to saved 06 outputs. Used only to
# adjudicate cells where the stored 08 artefact and the PSA analogue disagree.
psa_08_verbatim <- function(dt_model, lt_interp, k08) {
  lt_interp <- copy(lt_interp)
  ADULT_MIN_AGE <- k08$ADULT_MIN_AGE; MAX_MODEL_AGE <- k08$MAX_MODEL_AGE
  dt_deaths <- dt_model[, .(deaths = sum(dead, na.rm = TRUE)), by = .(location, year, scenario, htn_target_scenario)]
  dt_pop_unique <- unique(dt_model[, .(location, year, scenario, htn_target_scenario, age, sex, pop)])
  dt_baseline <- dt_deaths[scenario == "baseline", .(location, year, htn_target_scenario, deaths_baseline = deaths)]
  dt_compare <- dt_deaths[scenario != "baseline"]
  dt_compare <- dt_baseline[dt_compare, on = .(location, year, htn_target_scenario)]
  setnames(dt_compare, "deaths", "deaths_intervention")
  dt_compare[, deaths_averted := deaths_baseline - deaths_intervention]
  dt_pop_age <- dt_pop_unique[, .(pop = sum(pop, na.rm = TRUE)), by = .(location, year, scenario, htn_target_scenario, age)]
  dt_avg_adult <- dt_pop_age[age >= ADULT_MIN_AGE, .(adult_population = sum(pop, na.rm = TRUE),
    avg_adult_age = sum(pop * age, na.rm = TRUE) / sum(pop, na.rm = TRUE)),
    by = .(location, year, scenario, htn_target_scenario)]
  dt_avg_adult[, age_ref_5y := pmin(MAX_MODEL_AGE, (as.integer(floor(avg_adult_age)) %/% 5L) * 5L)]
  dt_le_lookup <- copy(dt_avg_adult)[, .(location, year, scenario, htn_target_scenario,
                                         age = age_ref_5y, adult_population, avg_adult_age)]
  setkey(lt_interp, location, age, year)
  setkey(dt_le_lookup, location, age, year)
  dt_le_lookup <- lt_interp[dt_le_lookup, on = .(location, age, year), roll = "nearest"]
  setnames(dt_le_lookup, "age", "age_ref_5y"); setnames(dt_le_lookup, "le", "le_avg_adult")
  dt_compare <- dt_le_lookup[dt_compare, on = .(location, year, scenario, htn_target_scenario)]
  dt_deaths_age <- dt_model[, .(deaths = sum(dead, na.rm = TRUE)), by = .(location, year, scenario, htn_target_scenario, age)]
  dt_baseline_age <- dt_deaths_age[scenario == "baseline", .(location, year, age, htn_target_scenario, deaths_baseline = deaths)]
  dt_deaths_age <- dt_deaths_age[scenario != "baseline"]
  dt_deaths_age <- dt_baseline_age[dt_deaths_age, on = .(location, year, age, htn_target_scenario)]
  setnames(dt_deaths_age, "deaths", "deaths_intervention_age")
  dt_deaths_age[, deaths_averted_age := deaths_baseline - deaths_intervention_age]
  dt_deaths_age[, age_ref_5y := pmin(MAX_MODEL_AGE, (as.integer(floor(age)) %/% 5L) * 5L)]
  lt_age <- lt_interp[, .(location, year, age_ref_5y = age, le_age = le)]
  setkey(lt_age, location, age_ref_5y, year)
  setkey(dt_deaths_age, location, age_ref_5y, year)
  dt_deaths_age <- lt_age[dt_deaths_age, on = .(location, age_ref_5y, year), roll = "nearest"]
  dt_ly_gained <- dt_deaths_age[, .(life_years_gained_undisc = sum(deaths_averted_age * le_age, na.rm = TRUE)),
                                by = .(location, year, scenario, htn_target_scenario)]
  dt_compare <- dt_ly_gained[dt_compare, on = .(location, year, scenario, htn_target_scenario)]
  dt_compare[, .(location, year, scenario, deaths_averted, avg_adult_age, age_ref_5y, le_avg_adult,
                 life_years_gained_undisc)]
}

# Cell-level 08 check for every location-year-scenario (draw 0). A mismatch with
# the stored 08_vsl_results.rds is adjudicated by re-running 08's own code on the
# saved 06 output: if the PSA equals 08's code, the stored cell is classified as
# not reproducible from the saved 06 outputs (reported, not "fixed").
psa_validate_08_cells <- function(paths, econ_detail, k08) {
  f <- file.path(paths$wd_outp_original, "08_vsl_results.rds")
  if (!file.exists(f) || !length(econ_detail)) {
    return(list(checks = data.table(check = "V6 08 cells", pass = NA, note = "file or detail missing"),
                cells = data.table(), adjust = data.table()))
  }
  o <- readRDS(f); setDT(o)
  rows <- rbindlist(lapply(names(econ_detail), function(Country) {
    rbindlist(lapply(names(econ_detail[[Country]]), function(sn) {
      d <- econ_detail[[Country]][[sn]]
      data.table(location = Country, scenario = sn, year = PSA_YEARS,
                 deaths_averted = d$deaths_averted[, 1], life_years_gained_undisc = d$life_years_gained_undisc[, 1],
                 avg_adult_age = d$avg_adult_age[, 1], age_ref_5y = d$age_ref_5y[, 1],
                 le_avg_adult = d$le_avg_adult[, 1], vsly_e1_2 = d$vsly_e1_2[, 1],
                 economic_value_e1_2 = d$economic_value_e1_2[, 1], vsly_value_e1_2 = d$vsly_value_e1_2[, 1])
    }))
  }))
  vars <- c("deaths_averted", "life_years_gained_undisc", "avg_adult_age", "age_ref_5y",
            "le_avg_adult", "vsly_e1_2", "economic_value_e1_2", "vsly_value_e1_2")
  m <- merge(rows, o[, c("location", "scenario", "year", "who_region", "disc_r3", "vsl_e1_2", "vsl_e1_5", vars),
                     with = FALSE],
             by = c("location", "scenario", "year"), suffixes = c(".psa", ".orig"))
  close <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) &
                                                     abs(a - b) <= PSA_TOL$totals_abs + PSA_TOL$totals_rel * abs(b))
  m[, mismatch := !Reduce(`&`, lapply(vars, function(v) close(get(paste0(v, ".psa")), get(paste0(v, ".orig")))))]
  bad <- m[mismatch == TRUE]
  cells <- data.table(); adjust <- data.table()
  if (nrow(bad)) {
    i07 <- psa_load_07_inputs(paths)
    verb <- rbindlist(lapply(unique(bad$location), function(Country) {
      psa_08_verbatim(psa_read_saved_06(paths, Country), i07$lt_interp[location == Country], k08)
    }))
    cells <- merge(bad, verb, by = c("location", "scenario", "year"))
    cells[, explained := close(life_years_gained_undisc.psa, life_years_gained_undisc) &
                         close(avg_adult_age.psa, avg_adult_age) & close(le_avg_adult.psa, le_avg_adult) &
                         close(deaths_averted.psa, deaths_averted)]
    cells[, classification := fifelse(explained,
      "stored 08 cell not reproducible by 08's own code from the saved 06 outputs (PSA = 08 code)",
      "UNEXPLAINED: PSA differs from 08's own code")]
    # Adjusted reference: the stored cell replaced by 08's own recomputation
    # (e1_5 values scale with vsl_e1_5 / vsl_e1_2 at the same LE and life-years).
    adjust <- cells[explained == TRUE, .(who_region, scenario, year,
      d_vsly12 = (vsly_value_e1_2.psa - vsly_value_e1_2.orig) * disc_r3,
      d_vsly15 = (vsly_value_e1_2.psa - vsly_value_e1_2.orig) * vsl_e1_5 / vsl_e1_2 * disc_r3,
      d_econ12 = (economic_value_e1_2.psa - economic_value_e1_2.orig) * disc_r3,
      d_econ15 = (economic_value_e1_2.psa - economic_value_e1_2.orig) * vsl_e1_5 / vsl_e1_2 * disc_r3)]
  }
  n_unexpl <- if (nrow(cells)) cells[explained == FALSE, .N] else 0L
  checks <- rbind(
    data.table(check = "V6 08 analogue vs 08_vsl_results.rds, every location-year-scenario (draw 0)",
               scope = sprintf("%d rows; %d differ; %d of these reproduce 08's own code", nrow(m), nrow(bad),
                               nrow(bad) - n_unexpl),
               n = nrow(m), max_abs_diff = NA_real_, max_rel_diff = NA_real_, tol_abs = PSA_TOL$totals_abs,
               tol_rel = PSA_TOL$totals_rel, identical = nrow(bad) == 0L, pass = n_unexpl == 0L,
               note = if (nrow(bad)) paste(unique(paste(bad$location, bad$scenario, bad$year)), collapse = "; ") else ""),
    fill = TRUE)
  list(checks = checks, cells = cells, adjust = adjust)
}

psa_validate_08_summary <- function(paths, econ_summary_draw0, adjust = data.table(), income = NULL) {
  f <- file.path(paths$wd_outp_original, "08_vsl_vsly_summary_table_appended.rds")
  if (!file.exists(f)) return(data.table(check = "V6 08 summary", pass = NA, note = "file missing"))
  o <- readRDS(f); setDT(o)
  x <- copy(econ_summary_draw0)
  m <- merge(x, o, by = c("valuation_type", "elasticity_case", "who_region", "scenario"),
             suffixes = c(".psa", ".orig"))
  cols <- intersect(grep("^(metric|share)_", names(econ_summary_draw0), value = TRUE),
                    sub("\\.orig$", "", grep("\\.orig$", names(m), value = TRUE)))
  out <- rbindlist(lapply(cols, function(v) {
    psa_cmp(paste("V6 08 summary", v, "(World + regions, all scenarios) vs stored appended table"),
            m[[paste0(v, ".psa")]], m[[paste0(v, ".orig")]], tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs,
            scope = sprintf("%d region x scenario x valuation rows", nrow(m)))
  }))
  if (nrow(adjust) && !is.null(income)) {
    # Same comparison after replacing only the adjudicated cells by 08's own recomputation.
    a <- rbind(adjust, copy(adjust)[, who_region := "World"])[year >= 2026 & year <= 2050,
      .(`VSLY|e1_2_primary` = sum(d_vsly12), `VSLY|e1_5_sensitivity` = sum(d_vsly15),
        `VSL|e1_2_primary` = sum(d_econ12), `VSL|e1_5_sensitivity` = sum(d_econ15)), by = .(who_region, scenario)]
    a <- melt(a, id.vars = c("who_region", "scenario"), variable.name = "vt_ec", value.name = "delta")
    a[, c("valuation_type", "elasticity_case") := tstrsplit(as.character(vt_ec), "|", fixed = TRUE)]
    inc_tot <- income[year >= 2026 & year <= 2050, .(income_total = sum(total_income)), by = who_region]
    ma <- a[, .(who_region, scenario, valuation_type, elasticity_case, delta)][
      m, on = .(who_region, scenario, valuation_type, elasticity_case)]
    ma <- inc_tot[ma, on = "who_region"]
    ma[is.na(delta), delta := 0]
    ma[, `:=`(adj_metric = metric_total.orig + delta, adj_share = share_total.orig + delta / income_total)]
    adj <- rbind(
      psa_cmp("V6 08 summary metric_total vs stored table with adjudicated cells replaced by 08's own code",
              ma$metric_total.psa, ma$adj_metric, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs,
              scope = sprintf("%d rows (both elasticities); %d adjudicated cell(s)", nrow(ma), nrow(adjust))),
      psa_cmp("V6 08 summary share_total vs stored table with adjudicated cells replaced by 08's own code",
              ma$share_total.psa, ma$adj_share, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs,
              scope = sprintf("%d rows (both elasticities); %d adjudicated cell(s)", nrow(ma), nrow(adjust))))
    out <- rbind(out, adj, fill = TRUE)
    # Raw failures are reported as explained ONLY if the adjusted comparison passes.
    if (all(adj$pass)) {
      expl <- grepl("stored appended table", out$check) & out$pass %in% FALSE & grepl("_total", out$check)
      out[expl, `:=`(pass = NA, note = paste0("Differs only through the adjudicated 08 cell(s) ",
                                              "(validation/08_adjudicated_cells.csv); adjusted comparison passes"))]
    }
  }
  out
}

#...........................................................
# V7 draw-0 global endpoints vs original report artefacts ----
#...........................................................

# Reference freshness. A 07/08/report artefact written before the newest saved
# 06 output cannot have been computed from it (e.g. the model was re-run but the
# original report was not re-knitted), so its comparisons are reported as NOT
# COMPARED (pass = NA) instead of as failures or passes.
psa_reference_files <- function(paths) {
  o <- paths$wd_outp_original
  list(V5_07 = file.path(o, "dt_output_dalys.rds"),
       V6_08 = file.path(o, c("08_vsl_results.rds", "08_vsl_vsly_summary_table_appended.rds")),
       V7_report = c(list.files(file.path(o, "paper"), pattern = "^paper_.*[.]rds$", full.names = TRUE),
                     file.path(o, "slides", "sl_dt_asmr.rds")))
}

psa_reference_freshness <- function(paths) {
  m06 <- list.files(file.path(paths$wd_outp_original, "out_model"),
                    pattern = "^model_output_.*[.]rds$", full.names = TRUE)
  newest06 <- if (length(m06)) max(file.mtime(m06)) else as.POSIXct(NA)
  refs <- psa_reference_files(paths)
  rbindlist(lapply(names(refs), function(g) {
    f <- refs[[g]][file.exists(refs[[g]])]
    if (!length(f)) return(NULL)
    data.table(group = g, file = sub(paste0("^", paths$root, "/"), "", normalizePath(f, winslash = "/")),
               mtime = file.mtime(f), newest_06_output = newest06,
               stale = !is.na(newest06) & file.mtime(f) < newest06)
  }))
}

psa_mark_not_compared <- function(res, why) {
  if (is.null(res) || !nrow(res)) return(res)
  res <- copy(res)
  if (!"note" %in% names(res)) res[, note := ""]
  res[, note := paste0(fifelse(is.na(note) | !nzchar(note), "", paste0(note, "; ")), "NOT COMPARED: ", why)]
  res[, pass := NA]
  res[]
}

psa_validate_report_artifacts <- function(paths, sm) {
  pdir <- file.path(paths$wd_outp_original, "paper")
  sdir <- file.path(paths$wd_outp_original, "slides")
  rd <- function(d, f) { p <- file.path(d, f); if (file.exists(p)) readRDS(p) else NULL }
  out <- list()
  ps <- rd(pdir, "paper_scalars.rds")
  if (!is.null(ps)) {
    cu <- sm$cumul[variable == "deaths_delayed"]
    get_c <- function(lbl) cu[intervention == lbl, point]
    out$scalars <- rbind(
      psa_cmp("V7 total_all (All Interventions deaths averted)", get_c("All Interventions"), ps$total_all, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs),
      psa_cmp("V7 total_bp (HTN no diabetes)", get_c("HTN Control (No Diabetes)"), ps$total_bp, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs),
      psa_cmp("V7 total_bpd (HTN diabetes)", get_c("HTN Control (Diabetes)"), ps$total_bpd, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs),
      psa_cmp("V7 total_stat (statins)", get_c("Improved Statin Uptake"), ps$total_stat, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs),
      psa_cmp("V7 bau_total_deaths (2025-2050)", sm$cumul[variable == "bau_total_deaths"][1, point],
              ps$bau_total_deaths, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs),
      psa_cmp("V7 BOD totals All Interventions (DALY/YLL/YLD)",
              sm$bod[intervention == "All Interventions" & variable %in% c("DALYs", "YLLs", "YLDs")][
                order(match(variable, c("DALYs", "YLLs", "YLDs"))), point],
              c(ps$total_dalys_all, ps$total_ylls_all, ps$total_ylds_all), tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs))
  }
  dc <- rd(pdir, "paper_dt_cumul.rds")
  if (!is.null(dc)) {
    x <- sm$cumul[variable == "deaths_delayed", .(intervention, point)]
    m <- merge(x, as.data.table(dc)[, .(intervention = as.character(intervention), deaths_delayed)], by = "intervention")
    out$cumul <- psa_cmp("V7 cumulative deaths averted by intervention (paper_dt_cumul)", m$point, m$deaths_delayed,
                         tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d interventions", nrow(m)))
  }
  da <- rd(pdir, "paper_dt_annual.rds")
  if (!is.null(da)) {
    x <- sm$annual[variable == "deaths_delayed", .(intervention, year, point)]
    m <- merge(x, as.data.table(da)[, .(intervention = as.character(intervention), year, deaths_delayed)],
               by = c("intervention", "year"))
    out$annual <- psa_cmp("V7 annual deaths averted (paper_dt_annual)", m$point, m$deaths_delayed,
                          tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d intervention-years", nrow(m)))
  }
  dca <- rd(pdir, "paper_dt_cause.rds")
  if (!is.null(dca)) {
    x <- sm$cause[variable == "deaths_delayed", .(intervention, cause, point)]
    m <- merge(x, as.data.table(dca)[, .(intervention = as.character(intervention), cause, deaths_delayed)],
               by = c("intervention", "cause"))
    out$cause <- psa_cmp("V7 deaths averted by intervention x cause (paper_dt_cause)", m$point, m$deaths_delayed,
                         tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d rows", nrow(m)))
  }
  dr <- rd(pdir, "paper_dt_region.rds")
  if (!is.null(dr)) {
    x <- sm$region[intervention == "All Interventions" & variable == "deaths_delayed", .(region, point)]
    m <- merge(x, as.data.table(dr)[, .(region, deaths_delayed)], by = "region")
    out$region <- psa_cmp("V7 deaths averted by region, All Interventions (paper_dt_region)", m$point,
                          m$deaths_delayed, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d regions", nrow(m)))
  }
  dco <- rd(pdir, "paper_dt_country.rds")
  if (!is.null(dco)) {
    x <- sm$country[intervention == "All Interventions" & variable == "deaths_delayed", .(location, point)]
    m <- merge(x, as.data.table(dco)[, .(location, deaths_delayed)], by = "location")
    out$country <- psa_cmp("V7 deaths averted by country, All Interventions (paper_dt_country)", m$point,
                           m$deaths_delayed, tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d countries", nrow(m)))
  }
  das <- rd(pdir, "paper_dt_age_sex.rds")
  if (!is.null(das)) {
    x <- sm$age_sex[variable == "deaths_delayed", .(intervention, age_group, sex, point)]
    m <- merge(x, as.data.table(das)[, .(intervention = as.character(intervention),
                                          age_group = as.character(age_group), sex, deaths_delayed)],
               by = c("intervention", "age_group", "sex"))
    out$age_sex <- psa_cmp("V7 deaths averted by age group x sex (paper_dt_age_sex)", m$point, m$deaths_delayed,
                           tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d rows", nrow(m)))
  }
  for (nm in c("bod_summary", "bod_cause", "bod_region", "bod_income")) {
    b <- rd(pdir, paste0("paper_", nm, ".rds"))
    if (is.null(b)) next
    b <- as.data.table(b)
    tab <- c(bod_summary = "bod", bod_cause = "bod_cause", bod_region = "bod_region", bod_income = "bod_income")[nm]
    by <- c(bod_summary = "", bod_cause = "cause", bod_region = "region", bod_income = "location_income")[nm]
    b[, intervention := as.character(intervention)]
    if (nzchar(by)) b[, (by) := as.character(get(by))]
    x <- dcast(sm[[tab]][variable %in% c("Deaths", "YLLs", "YLDs", "DALYs")],
               as.formula(paste("intervention", if (nzchar(by)) paste("+", by) else "", "~ variable")),
               value.var = "point")
    keys <- c("intervention", if (nzchar(by)) by)
    m <- merge(x, b, by = keys, suffixes = c(".psa", ".orig"))
    out[[nm]] <- rbindlist(lapply(c("Deaths", "YLLs", "YLDs", "DALYs"), function(v) {
      psa_cmp(sprintf("V7 %s %s (paper_%s)", nm, v, nm), m[[paste0(v, ".psa")]], m[[paste0(v, ".orig")]],
              tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d rows", nrow(m)))
    }))
  }
  asm <- rd(sdir, "sl_dt_asmr.rds")
  if (!is.null(asm)) {
    asm <- as.data.table(asm)
    x <- sm$asmr[variable == "ASMR"]
    x[, intervention := fifelse(intervention == "b.a.u", "b.a.u", intervention)]
    m <- merge(x[, .(intervention, year, point)],
               asm[, .(intervention = as.character(intervention), year, ASMR)], by = c("intervention", "year"))
    out$asmr <- psa_cmp("V7 ASMR by intervention-year (sl_dt_asmr)", m$point, m$ASMR,
                        tol_rel = PSA_TOL$totals_rel, tol_abs = PSA_TOL$totals_abs, scope = sprintf("%d rows", nrow(m)))
  }
  rbindlist(out, fill = TRUE)
}

#...........................................................
# V8 invariants ----
#...........................................................

psa_validate_invariants <- function(ep, long_by_country, checks) {
  out <- list()
  a <- ep$annual
  bau_spread <- a[, .(s = max(baseline_deaths) - min(baseline_deaths)), by = year][, max(s)]
  out$bau <- data.table(check = "V8 BAU deaths identical in every draw (global, by year)", scope = "2025-2050",
                        n = a[, uniqueN(year)], max_abs_diff = bau_spread, max_rel_diff = NA_real_,
                        tol_abs = 0, tol_rel = NA_real_, identical = bau_spread == 0, pass = bau_spread == 0, note = "")
  for (sn in c("statins_only", "all_interventions")) {
    sp <- a[scenario == sn, .(s = max(deaths) - min(deaths)), by = year][, max(s)]
    out[[sn]] <- data.table(check = sprintf("V8 %s deaths identical in every draw", sn),
                            scope = fifelse(sn == "all_interventions",
                                            "expected: 06 defect D1 removes the BP incidence pathway",
                                            "expected: no sampled parameter enters statins"),
                            n = a[scenario == sn, uniqueN(year)], max_abs_diff = sp, max_rel_diff = NA_real_,
                            tol_abs = 0, tol_rel = NA_real_, identical = sp == 0, pass = sp == 0, note = "")
  }
  for (sn in c("bp_no_diabetes_only", "bp_diabetes_only", "bp_combined")) {
    sp <- ep$cumul[scenario == sn, max(deaths_delayed) - min(deaths_delayed)]
    out[[sn]] <- data.table(check = sprintf("V8 %s varies across draws (sampled BP pathway active)", sn),
                            scope = "cumulative deaths averted", n = ep$cumul[scenario == sn, .N],
                            max_abs_diff = sp, max_rel_diff = NA_real_, tol_abs = NA_real_, tol_rel = NA_real_,
                            identical = FALSE, pass = sp > 0, note = "")
  }
  g <- ep$annual[year <= 2025]
  pre <- if (nrow(g)) g[, max(abs(deaths_delayed))] else 0
  out$pre <- data.table(check = "V8 pre-2026 identity: deaths averted in 2025 = 0 (every scenario, every draw)",
                        scope = "global", n = nrow(g), max_abs_diff = pre, max_rel_diff = NA_real_,
                        tol_abs = 1e-6, tol_rel = NA_real_, identical = pre == 0, pass = pre <= 1e-6, note = "")
  for (Country in names(long_by_country)) {
    l <- long_by_country[[Country]]
    dup <- anyDuplicated(l, by = c("draw_id", "location", "scenario", "sex", "age", "year", "cause"))
    out[[paste0("keys_", Country)]] <- data.table(
      check = "V8 unique (draw_id, location, scenario, sex, age, year, cause) keys", scope = Country,
      n = nrow(l), max_abs_diff = NA_real_, max_rel_diff = NA_real_, tol_abs = NA_real_, tol_rel = NA_real_,
      identical = dup == 0, pass = dup == 0, note = "")
    b0 <- l[scenario == "baseline" & year <= 2025]
    for (sn in setdiff(PSA_SCENARIOS, "baseline")) {
      s0 <- l[scenario == sn & year <= 2025]
      setkey(b0, draw_id, sex, cause, age, year); setkey(s0, draw_id, sex, cause, age, year)
      # BP eff_ir before 2026 is sum_b(IR_b p_b)/IR, i.e. 1 only to rounding in 06
      # itself, so states agree to ~1e-16 relative rather than bit-for-bit.
      out[[paste0("pre_", Country, sn)]] <- psa_cmp(
        "V8 pre-2026 states equal BAU (every draw; dead, sick, well)",
        c(s0$dead, s0$sick, s0$well), c(b0$dead, b0$sick, b0$well),
        tol_rel = 1e-12, scope = paste(Country, sn))
    }
  }
  ck <- checks
  out$prob <- data.table(check = "V8 all IR/CF probabilities finite and within [0, 0.99]",
                         scope = "every country x scenario x draw", n = nrow(ck), max_abs_diff = NA_real_,
                         max_rel_diff = NA_real_, tol_abs = NA_real_, tol_rel = NA_real_,
                         identical = all(ck$prob_in_range), pass = all(ck$prob_in_range),
                         note = sprintf("IR range [%.3g, %.3g]; CF range [%.3g, %.3g]", min(ck$ir_min),
                                        max(ck$ir_max), min(ck$cf_min), max(ck$cf_max)))
  out$finite <- data.table(check = "V8 no non-finite state values", scope = "every country x scenario x draw",
                           n = nrow(ck), max_abs_diff = sum(ck$n_nonfinite_states), max_rel_diff = NA_real_,
                           tol_abs = 0, tol_rel = NA_real_, identical = sum(ck$n_nonfinite_states) == 0,
                           pass = sum(ck$n_nonfinite_states) == 0, note = "")
  out$neg <- data.table(check = "V8 negative state values (06 defect D10: initial well < 0 at ages 93-95)",
                        scope = "count across all cells", n = nrow(ck), max_abs_diff = sum(ck$n_negative_states),
                        max_rel_diff = NA_real_, tol_abs = NA_real_, tol_rel = NA_real_,
                        identical = sum(ck$n_negative_states) == 0, pass = NA,
                        note = "Reported, not fixed: inherited from 06 initial states")
  bp06 <- ck[!is.na(bp_draw0_vs_06), max(bp_draw0_vs_06)]
  out$bp06 <- data.table(check = "V8 in-worker check: batched BP eff_ir (draw 0) vs 06 calculate_antihypertensive_split",
                         scope = "every country x BP scenario", n = ck[!is.na(bp_draw0_vs_06), .N],
                         max_abs_diff = bp06, max_rel_diff = NA_real_, tol_abs = 0, tol_rel = NA_real_,
                         identical = bp06 == 0, pass = bp06 == 0, note = "")
  sna <- ck[!is.na(salt_na_share_2026plus), range(salt_na_share_2026plus)]
  out$d1 <- data.table(check = "V8 06 defect D1 reproduced: sodium eff_ir NA from 2026 -> BP eff_ir reset to 1",
                       scope = "all_interventions, every country", n = ck[!is.na(salt_na_share_2026plus), .N],
                       max_abs_diff = NA_real_, max_rel_diff = NA_real_, tol_abs = NA_real_, tol_rel = NA_real_,
                       identical = all(sna == 1), pass = all(sna == 1),
                       note = sprintf("share of 2026+ sodium cells NA: min %.3f, max %.3f", sna[1], sna[2]))
  rbindlist(out, fill = TRUE)
}

# Zero-effect and zero-coverage invariants on one country (fresh context).
psa_validate_zero_effect <- function(Country, cin, fns, inp, s6, rr10_0, sheet1_0) {
  zero <- psa_generate_draws(s6, 0L, 1L)
  zero[, hr5 := 1]
  sh_z <- psa_build_sheet1_draws(zero, s6)
  ctx <- psa_country_context(Country, cin, fns, inp$repYear, inp$dt_gbd_rr, inp$run_args,
                             inp$scenarios, inp$htn_scenario_ids, sheet1_0, rr10_0)
  ctx$draw0_col <- 0L
  E <- psa_effect_arrays(sh_z, 0L)
  RR <- psa_rr10_array(data.table(draw_id = 0L, rr10_0), 0L)
  bp_iv <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
  # Zero effect: every incremental effect is 0 x delta / 1 = 0, so each bin
  # multiplier is exactly 1 and eff_ir = sum_b(IR_b p_b) / IR, which equals 1
  # only to rounding (the bin identity holds to ~2e-16).
  e_zero <- psa_bp_eff_ir_batch(ctx$bp[["bp_combined|TRUE|TRUE"]], E, fns)
  # Zero coverage: targets equal baselines (scenario_id "baseline").
  cc <- psa_prepare_country_context(Country, cin, fns, inp$repYear, inp$dt_gbd_rr, TRUE)
  bpz <- psa_bp_bin_context(Country, cin$targets[scenario_id == "baseline"], TRUE, TRUE,
                            cc$bp_baseline, cc$diabetes_age, fns)
  e_cov <- psa_bp_eff_ir_batch(bpz, psa_effect_arrays(data.table(draw_id = 0L, sheet1_0), 0L), fns)
  rbind(
    psa_cmp("V8 zero-effect draw (all HR5 = 1): BP eff_ir = 1", e_zero, rep(1, length(e_zero)),
            tol_abs = 1e-15, scope = paste(Country, "(CF pathway with fixed rho stays active by design)")),
    psa_cmp("V8 zero coverage (targets = baseline): BP eff_ir = 1", e_cov, rep(1, length(e_cov)),
            tol_abs = 1e-15, scope = Country),
    psa_cmp("V8 zero-effect and zero-coverage eff_ir bit-identical", e_zero, e_cov,
            tol_abs = 0, scope = Country))
}
