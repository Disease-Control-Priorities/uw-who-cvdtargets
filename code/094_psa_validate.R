# =============================================================================
# 094_psa_validate.R -- Aim 1 PSA: validation against 06 and the report
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions only. Every check returns a
# data.table row with the observed maximum absolute / relative difference and the
# explicit tolerance used; nothing is "fixed" to match. Reference inputs are read
# from output/ (read-only); results are written by the caller under output_psa/.
#
#   V1  parameters: Sheet1 rebuild, statin points parsed from 06, draw 0, draws
#   V2  parameterised statin function == 06's own at the point RRs
#   V3  live 06 run_multiple_scenarios() == saved 06 output == PSA draw 0
#   V4  batched engine == per-draw reference path (06 re-homed) for test draws
#   V5  draw 0 == saved 06 output for EVERY country (all states, all scenarios)
#   V6  draw-0 aggregates == deterministic report artefacts (2026-2050)
#   V7  perturbation propagation (BP-only / statin-only perturbations)
#   V8  completeness, finiteness, duplicates
#   V9  accounting identities, 25-year divisor, fixed baseline, region = global
#   V10 quantiles finite and ordered
#   V11 transition validity handling (caps/clamps) relative to draw 0

PSA_TOL <- list(
  exact = 0,
  states_cell_abs = 1e-6,    # draw 0 vs saved 06 per cell (persons); identical expected
  states_rel = 1e-12,        # batched vs per-draw reference path
  totals_rel = 1e-9,         # aggregates vs report artefacts
  params_abs = 1e-12         # rebuilt Sheet1 vs workbook
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
  el_ok <- d <= (if (is.na(tol_abs)) 0 else tol_abs) + (if (is.na(tol_rel)) 0 else tol_rel) * abs(y[ok])
  data.table(check = check, scope = scope, n = length(x), max_abs_diff = mad, max_rel_diff = mrd,
             tol_abs = tol_abs, tol_rel = tol_rel, identical = na_mismatch == 0 && mad == 0,
             pass = na_mismatch == 0 && all(el_ok),
             note = if (na_mismatch) paste(na_mismatch, "NA-pattern mismatches") else "")
}

psa_flag <- function(check, scope, pass, note = "", n = NA_integer_, value = NA_real_) {
  data.table(check = check, scope = scope, n = n, max_abs_diff = value, max_rel_diff = NA_real_,
             tol_abs = NA_real_, tol_rel = NA_real_, identical = NA, pass = pass, note = note)
}

PSA_STATE_VARS <- c("well", "sick", "newcases", "dead", "pop", "all.mx", "eff_ir", "eff_cf")

psa_compare_model_tables <- function(check, a, b, tol_abs = 0, tol_rel = NA_real_, scope = "") {
  keys <- c("scenario", "sex", "cause", "age", "year")
  a <- copy(a); b <- copy(b)
  setkeyv(a, keys); setkeyv(b, keys)
  if (nrow(a) != nrow(b) || !isTRUE(all.equal(a[, ..keys], b[, ..keys], check.attributes = FALSE))) {
    return(psa_flag(check, scope, FALSE, "row keys differ"))
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

#...........................................................
# V1 parameters ----
#...........................................................

psa_validate_parameters <- function(s6, st, draws, sheet1, wb_sheet1, det_rr_bin, sset, n_draws) {
  s0 <- psa_sheet1_for_draw(sheet1, 0L)
  rc <- psa_check_sheet1_reconstruction(s0, wb_sheet1, PSA_TOL$params_abs)
  cols <- c("effect_size_nodiabetes", "effect_size_diabetes")
  m06 <- merge(s0, det_rr_bin[, c("cause", "bp_cat", cols), with = FALSE], by = c("cause", "bp_cat"))
  out <- list(
    rc[, .(check = "V1 Sheet1 draw-0 rebuild vs workbook", scope = column, n = n_rows,
           max_abs_diff, max_rel_diff = NA_real_, tol_abs = PSA_TOL$params_abs, tol_rel = NA_real_,
           identical = max_abs_diff == 0, pass, note = "")],
    psa_cmp("V1 Sheet1 draw-0 rebuild vs 06 ETIHAD_RR_BIN (effect sizes)",
            c(m06$effect_size_nodiabetes.x, m06$effect_size_diabetes.x),
            c(m06$effect_size_nodiabetes.y, m06$effect_size_diabetes.y), tol_abs = 0, scope = "ETIHAD_RR_BIN"),
    psa_cmp("V1 statin point RRs parsed from 06 vs register", sset$points[st$model_variable],
            st$model_point, tol_abs = 0, scope = paste(st$model_variable, collapse = ", ")),
    psa_cmp("V1 statin draw 0 equals model point", draws$statin[draw_id == 0L, unlist(.SD), .SDcols = PSA_STATIN_IDS],
            sset$points[PSA_STATIN_IDS], tol_abs = 0, scope = "draw 0"),
    psa_cmp("V1 BP draw 0 equals Appendix S6 point", draws$bp[draw_id == 0L, hr5],
            draws$bp[draw_id == 0L, hr5_point], tol_abs = 0, scope = "draw 0"),
    psa_cmp("V1 statin sdlog = (log U - log L) / (2 qnorm((1+c)/2))", st$sdlog,
            (log(st$ci_upper) - log(st$ci_lower)) / (2 * qnorm((1 + st$ci_level) / 2)),
            tol_abs = 0, scope = paste(round(st$sdlog, 5), collapse = ", ")))
  ids_bp <- draws$bp[, .N, by = draw_id]; ids_st <- draws$statin_long[, .N, by = draw_id]
  out$complete <- psa_flag("V1 draws complete, finite and positive", sprintf("draws 0..%d", n_draws),
    setequal(ids_bp$draw_id, 0:n_draws) && all(ids_bp$N == 15L) && setequal(ids_st$draw_id, 0:n_draws) &&
      all(ids_st$N == 4L) && all(is.finite(draws$bp$hr5)) && all(draws$bp$hr5 > 0) &&
      all(is.finite(draws$statin_long$rr)) && all(draws$statin_long$rr > 0),
    n = nrow(draws$bp) + nrow(draws$statin_long))
  if (n_draws >= 2L) {
    emp <- draws$statin_long[draw_id >= 1L, .(emp_mean_log = mean(log(rr)), emp_sd_log = sd(log(rr)),
                                              n_gt1 = sum(rr > 1)), by = .(param_id, meanlog, sdlog)]
    out$emp <- emp[, .(check = "V1 statin draws: empirical log-mean / log-SD (informational)", scope = param_id,
                       n = n_draws, max_abs_diff = abs(emp_mean_log - meanlog), max_rel_diff = emp_sd_log / sdlog - 1,
                       tol_abs = NA_real_, tol_rel = NA_real_, identical = NA, pass = NA,
                       note = sprintf("RR>1 in %d of %d draws", n_gt1, n_draws))]
  }
  rbindlist(out, fill = TRUE)
}

#...........................................................
# V2 parameterised statin function ----
#...........................................................

psa_validate_statin_function <- function(Country, cin, fns, sset, run_args) {
  rates <- copy(cin$b_rates[year >= 2017])
  rates[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]
  args <- list(cin$statin, rates, Country, cin$af, adherence_ir = run_args$adherence_ir,
               adherence_cf = run_args$adherence_cf, prop_athero_stroke = sset$prop_athero,
               statin_target_coverage = run_args$statin_target_coverage,
               statin_start_year = run_args$statin_start_year, statin_target_year = run_args$statin_target_year,
               baseline_statin_coverage = psa_baseline_statin_cov(Country, cin, run_args))
  sink(tempfile()); on.exit(sink(), add = TRUE)
  a <- do.call(fns$calculate_statins_impact, args)
  b <- do.call(sset$fun, c(args, list(statin_rr = sset$points)))
  rbind(psa_flag("V2 parameterised statin function at point RRs identical to 06 (all columns, real rates)",
                 Country, identical(a, b), n = nrow(a)),
        psa_cmp("V2 parameterised statin function: CF", b$CF, a$CF, tol_abs = 0, scope = Country),
        psa_cmp("V2 parameterised statin function: IR", b$IR, a$IR, tol_abs = 0, scope = Country))
}

#...........................................................
# V3 live 06 ----
#...........................................................

psa_live_06 <- function(env06_setup, Country, run_args) {
  sink(tempfile()); on.exit(sink(), add = TRUE)
  res <- env06_setup$run_multiple_scenarios(
    Country = Country, scenario_list = env06_setup$scenarios,
    htn_scenario_ids = env06_setup$htn_scenario_ids, dt_hbp_targets = env06_setup$dt_hbp_targets,
    statin_target_coverage = run_args$statin_target_coverage,
    statin_start_year = run_args$statin_start_year, statin_target_year = run_args$statin_target_year,
    adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
    baseline_statin_coverage = run_args$baseline_statin_coverage)
  res[, htn_target_scenario := "aim1"]
  res[]
}

#...........................................................
# V4 batched vs reference; V7 perturbations (one validation country) ----
#...........................................................

psa_validate_country_paths <- function(Country, inp, fns, sset, shared, s6, draws, sheet1, ref_ids, live = NULL,
                                       saved = NULL) {
  cin <- inp$cin[[Country]]
  out <- list()
  ids <- sort(unique(c(0L, ref_ids)))
  E <- psa_effect_arrays(sheet1[draw_id %in% ids], ids)
  rrm <- psa_statin_matrix(draws$statin, ids)
  bat <- psa_run_country(Country, cin, fns, sset, shared, ids, E, rrm, keep_long = TRUE)
  d0 <- bat$long[draw_id == 0L][, draw_id := NULL][]
  if (!is.null(live)) {
    out$live_psa <- psa_compare_model_tables("V3 live 06 run_multiple_scenarios() vs PSA batched draw 0",
                                             d0, live, tol_abs = 0, scope = Country)
  }
  if (!is.null(saved)) {
    if (!is.null(live)) out$live_saved <- psa_compare_model_tables(
      "V3 live 06 run_multiple_scenarios() vs saved 06 output", live, saved, tol_abs = 0, scope = Country)
  }
  for (id in ids) {
    env <- psa_rehome_06(fns, sset$fun, cin, shared$repYear, shared$dt_gbd_rr,
                         psa_sheet1_for_draw(sheet1, id), psa_statin_for_draw(draws$statin, id))
    ref <- psa_reference_run(Country, env, shared$scenarios, shared$htn_scenario_ids, cin$targets, shared$run_args)
    out[[paste0("ref_", id)]] <- psa_compare_model_tables(
      sprintf("V4 batched engine vs per-draw reference path (06 re-homed), draw %d", id),
      bat$long[draw_id == id][, draw_id := NULL][], ref, tol_abs = NA_real_,
      tol_rel = PSA_TOL$states_rel, scope = Country)
    rm(env, ref); gc(verbose = FALSE)
  }
  # V7: perturbation propagation. Synthetic ids: 9001 = BP at draw p, statins at
  # point; 9002 = BP at point, statins at draw p.
  p <- ref_ids[ref_ids > 0L][1]
  s1 <- rbind(sheet1[draw_id %in% c(0L, p)], copy(sheet1[draw_id == p])[, draw_id := 9001L],
              copy(sheet1[draw_id == 0L])[, draw_id := 9002L])
  stp <- rbind(draws$statin[draw_id == 0L], copy(draws$statin[draw_id == 0L])[, draw_id := 9001L],
               copy(draws$statin[draw_id == p])[, draw_id := 9002L])
  pid <- c(0L, 9001L, 9002L)
  pr <- psa_run_country(Country, cin, fns, sset, shared, pid, psa_effect_arrays(s1, pid),
                        psa_statin_matrix(stp, pid))
  dd <- pr$deaths
  same <- function(sn, col) dd[sn, col] == dd[sn, "0"]
  bp_sc <- c("bp_no_diabetes_only", "bp_diabetes_only", "bp_combined")
  out$perturb <- rbind(
    psa_flag("V7 BP-only perturbation: BP scenarios and all_interventions respond", Country,
             all(!vapply(c(bp_sc, "all_interventions"), same, logical(1), col = "9001")),
             note = sprintf("BP from draw %d, statins at point", p)),
    psa_flag("V7 BP-only perturbation: baseline and statins_only unchanged (exact)", Country,
             all(vapply(c("baseline", "statins_only"), same, logical(1), col = "9001"))),
    psa_flag("V7 statin-only perturbation: statins_only and all_interventions respond", Country,
             all(!vapply(c("statins_only", "all_interventions"), same, logical(1), col = "9002")),
             note = sprintf("statins from draw %d, BP at point", p)),
    psa_flag("V7 statin-only perturbation: baseline and BP scenarios unchanged (exact)", Country,
             all(vapply(c("baseline", bp_sc), same, logical(1), col = "9002"))))
  out$perturb_values <- data.table(check = "V7 perturbation deaths 2026-2050 (informational)",
    scope = paste(Country, rownames(dd)), n = 3L,
    max_abs_diff = dd[, "9001"] - dd[, "0"], max_rel_diff = dd[, "9002"] - dd[, "0"],
    tol_abs = NA_real_, tol_rel = NA_real_, identical = NA, pass = NA,
    note = "max_abs_diff = BP-only change; max_rel_diff column = statin-only change (deaths)")
  rbindlist(out, fill = TRUE)
}

#...........................................................
# V5 draw 0 vs saved 06 (every country, from the blocks) ----
#...........................................................

psa_validate_draw0_all <- function(d0, locs, country_checks) {
  ok <- d0[!is.na(scenario)]
  n_cty <- uniqueN(ok$location)
  rbind(
    psa_flag("V5 draw 0 vs saved 06 output: every country x scenario compared",
             sprintf("%d/%d countries", n_cty, length(locs)),
             setequal(unique(ok$location), locs) && nrow(ok) == length(locs) * length(PSA_SCENARIOS),
             note = paste(d0[is.na(scenario), unique(location)], collapse = ", ")),
    data.table(check = "V5 draw 0 vs saved 06 output: all state cells (well, sick, newcases, dead, pop, all.mx)",
               scope = sprintf("%d countries x 6 scenarios", n_cty), n = nrow(ok),
               max_abs_diff = max(ok$max_abs_states), max_rel_diff = NA_real_,
               tol_abs = PSA_TOL$states_cell_abs, tol_rel = NA_real_,
               identical = all(ok$max_abs_states == 0), pass = all(ok$max_abs_states <= PSA_TOL$states_cell_abs),
               note = sprintf("%d of %d country-scenarios bit-identical", sum(ok$max_abs_states == 0), nrow(ok))),
    data.table(check = "V5 draw 0 vs saved 06 output: eff_ir / eff_cf", scope = "all", n = nrow(ok),
               max_abs_diff = max(ok$max_abs_eff), max_rel_diff = NA_real_, tol_abs = 1e-12,
               tol_rel = NA_real_, identical = all(ok$max_abs_eff == 0), pass = all(ok$max_abs_eff <= 1e-12),
               note = ""),
    data.table(check = "V5 batched statin factors at point RRs vs 06 calculate_statins_impact() (per country)",
               scope = sprintf("%d countries", nrow(country_checks)), n = nrow(country_checks),
               max_abs_diff = max(country_checks$statin_check0), max_rel_diff = NA_real_, tol_abs = 0,
               tol_rel = NA_real_, identical = all(country_checks$statin_check0 == 0),
               pass = all(country_checks$statin_check0 == 0), note = ""),
    data.table(check = "V5 batched statin factors vs parameterised 06 function, perturbed draw (per country x block)",
               scope = sprintf("%d country-blocks", sum(!is.na(country_checks$statin_check_perturbed))),
               n = sum(!is.na(country_checks$statin_check_perturbed)),
               max_abs_diff = max(country_checks$statin_check_perturbed, na.rm = TRUE), max_rel_diff = NA_real_,
               tol_abs = 1e-15, tol_rel = NA_real_,
               identical = all(country_checks$statin_check_perturbed == 0, na.rm = TRUE),
               pass = all(country_checks$statin_check_perturbed <= 1e-15, na.rm = TRUE), note = ""),
    data.table(check = "V5 batched BP incidence effect at draw 0 vs 06 split (per country)",
               scope = "all", n = sum(!is.na(country_checks$bp_draw0_vs_06)),
               max_abs_diff = max(country_checks$bp_draw0_vs_06, na.rm = TRUE), max_rel_diff = NA_real_,
               tol_abs = 1e-12, tol_rel = NA_real_,
               identical = all(country_checks$bp_draw0_vs_06 == 0, na.rm = TRUE),
               pass = all(country_checks$bp_draw0_vs_06 <= 1e-12, na.rm = TRUE), note = ""))
}

#...........................................................
# V6 draw-0 aggregates vs the deterministic report artefacts ----
#...........................................................

psa_validate_report <- function(paths, md, full_geo) {
  if (!full_geo) return(psa_flag("V6 aggregates vs report artefacts", "country subset", NA,
                                 "Skipped: global totals need the full 06 geography."))
  pdir <- file.path(paths$wd_outp_original, "paper")
  files <- file.path(pdir, c("paper_dt_cumul.rds", "paper_dt_region.rds", "paper_scalars.rds"))
  if (!all(file.exists(files))) return(psa_flag("V6 aggregates vs report artefacts", "output/paper", NA,
                                                "report artefacts missing: NOT COMPARED"))
  newest06 <- max(file.mtime(list.files(file.path(paths$wd_outp_original, "out_model"),
                                        "^model_output_.*[.]rds$", full.names = TRUE)))
  if (any(file.mtime(files) < newest06)) {
    return(psa_flag("V6 aggregates vs report artefacts", "output/paper", NA,
                    "report artefacts older than the saved 06 outputs: NOT COMPARED"))
  }
  cum <- readRDS(files[1]); reg <- readRDS(files[2]); sc <- readRDS(files[3])
  d0 <- md[draw_id == 0L]
  g <- d0[geography_level == "global"][match(cum$intervention, intervention)]
  r <- d0[geography_level == "who_region"][match(reg$region, who_region)]
  rbind(
    psa_cmp("V6 draw-0 global deaths averted 2026-2050 vs paper_dt_cumul.rds", g$deaths_averted,
            cum$deaths_delayed, tol_rel = PSA_TOL$totals_rel, scope = paste(cum$intervention, collapse = "; ")),
    psa_cmp("V6 draw-0 global baseline deaths 2026-2050 vs paper_scalars$bau_total_deaths",
            d0[geography_level == "global", unique(baseline_deaths)], sc$bau_total_deaths,
            tol_rel = PSA_TOL$totals_rel, scope = "Global"),
    psa_cmp("V6 draw-0 regional projected deaths (All Interventions) vs paper_dt_region.rds",
            r$projected_deaths, reg$deaths_projected, tol_rel = PSA_TOL$totals_rel, scope = "6 regions"),
    psa_cmp("V6 draw-0 regional baseline deaths vs paper_dt_region.rds", r$baseline_deaths,
            reg$baseline_deaths, tol_rel = PSA_TOL$totals_rel, scope = "6 regions"),
    psa_cmp("V6 draw-0 regional deaths averted vs paper_dt_region.rds", r$deaths_averted,
            reg$deaths_delayed, tol_rel = PSA_TOL$totals_rel, scope = "6 regions"),
    psa_cmp("V6 draw-0 regional deaths averted/year vs paper_dt_region.rds", r$deaths_averted_per_year,
            reg$deaths_delayed_ave, tol_rel = PSA_TOL$totals_rel, scope = "6 regions"),
    psa_cmp("V6 draw-0 regional % averted vs paper_dt_region.rds", r$percent_averted,
            100 * reg$deaths_delayed_pct, tol_rel = PSA_TOL$totals_rel, scope = "6 regions"))
}

#...........................................................
# V12 draw 0 vs the manuscript's central estimates (docs/*.docx, read-only) ----
#...........................................................

PSA_MANUSCRIPT <- "docs/who_cvd_targets_paper1.docx"
PSA_MS_ROWS <- c(bp_no_diabetes_only = "Hypertension control, without diabetes",
                 bp_diabetes_only = "Hypertension control, with diabetes",
                 statins_only = "Statin scale-up", all_interventions = "All interventions combined")
PSA_MS_REGIONS <- c("Western Pacific" = "Western Pacific Region", "South-East Asia" = "South-East Asia Region",
                    "Europe" = "European Region", "Americas" = "Region of the Americas",
                    "Eastern Mediterranean" = "Eastern Mediterranean Region", "Africa" = "African Region")

# Plain text of the manuscript: the .docx is copied to a temporary file (it may
# be open in Word) and converted with pandoc; the original is never modified.
psa_manuscript_text <- function(paths) {
  f <- file.path(paths$root, PSA_MANUSCRIPT)
  if (!file.exists(f)) return(NULL)
  tmp <- tempfile(fileext = ".docx"); out <- tempfile(fileext = ".txt")
  on.exit(unlink(c(tmp, out)), add = TRUE)
  if (!isTRUE(file.copy(f, tmp))) return(NULL)
  ok <- tryCatch({
    suppressWarnings(rmarkdown::pandoc_convert(tmp, to = "plain", output = out, options = c("--wrap=none", "--quiet"),
                                               verbose = FALSE))
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(out)) return(NULL)
  readLines(out, warn = FALSE, encoding = "UTF-8")
}

# Central estimates reported in the manuscript (Findings, Tables 2-3, Supp. Table 6).
psa_manuscript_values <- function(txt) {
  # The manuscript prints negative numbers with U+2212 (minus sign).
  num <- function(x) as.numeric(gsub("\u2212", "-", gsub(",", "", x)))
  rx_row <- function(label, n) {
    pat <- paste0("^\\s*", gsub("([().])", "\\\\\\1", label),
                  paste(rep("\\s+([-\u2212]?[0-9][0-9.,]*%?)", n), collapse = ""), "\\s*$")
    hit <- regmatches(txt, regexec(pat, txt))
    hit <- hit[lengths(hit) > 0L]
    if (length(hit)) hit[[1]][-1] else NULL
  }
  out <- list()
  m <- regmatches(txt, regexec("BAU projected ([0-9.]+) million CVD deaths", txt))
  m <- m[lengths(m) > 0L]
  if (length(m)) out$bau <- data.table(source = "Findings (abstract)", geography = "Global", scenario_id = "baseline",
                                       metric = "baseline_deaths", unit = "million", digits = 1L,
                                       manuscript = num(m[[1]][2]))
  for (sid in names(PSA_MS_ROWS)) {
    t2 <- rx_row(PSA_MS_ROWS[[sid]], 3L)          # Table 2: averted (M), per year (thousands), share
    if (!is.null(t2)) out[[paste0("t2_", sid)]] <- data.table(
      source = "Table 2", geography = "Global", scenario_id = sid,
      metric = c("deaths_averted", "deaths_averted_per_year", "percent_averted"),
      unit = c("million", "thousand", "percent"), digits = c(1L, 0L, 1L),
      manuscript = num(sub("%", "", t2)))
    t3 <- rx_row(PSA_MS_ROWS[[sid]], 5L)          # Table 3: by cause (M, 2 dp); last = total
    if (!is.null(t3)) out[[paste0("t3_", sid)]] <- data.table(
      source = "Table 3", geography = "Global", scenario_id = sid, metric = "deaths_averted",
      unit = "million", digits = 2L, manuscript = num(t3[5]))
  }
  for (reg in names(PSA_MS_REGIONS)) {
    t6 <- rx_row(PSA_MS_REGIONS[[reg]], 5L)       # Supp. Table 6: BAU, averted, per year, share, DALYs
    if (!is.null(t6)) out[[paste0("t6_", reg)]] <- data.table(
      source = "Supplementary Table 6", geography = reg, scenario_id = "all_interventions",
      metric = c("baseline_deaths", "deaths_averted", "deaths_averted_per_year", "percent_averted"),
      unit = c("million", "million", "thousand", "percent"), digits = c(1L, 1L, 0L, 1L),
      manuscript = num(sub("%", "", t6[1:4])))
  }
  rbindlist(out)
}

psa_manuscript_comparison <- function(paths, sm) {
  txt <- psa_manuscript_text(paths)
  if (is.null(txt)) return(NULL)
  ms <- psa_manuscript_values(txt)
  if (!nrow(ms)) return(NULL)
  scale <- c(million = 1e6, thousand = 1e3, percent = 1)
  key <- sm[, .(geography = who_region, scenario_id, metric, point, lower, upper, is_fixed)]
  base <- key[metric == "baseline_deaths" & geography == "Global"][1][, scenario_id := "baseline"]
  key <- rbind(key, base)
  x <- key[ms, on = .(geography, scenario_id, metric)]
  x[, `:=`(psa_point_rounded = round(point / scale[unit], digits),
           psa_lower = lower / scale[unit], psa_upper = upper / scale[unit])]
  x[, match := abs(psa_point_rounded - manuscript) < 1e-9]
  x[, .(source, geography, scenario_id, metric, unit, digits, manuscript, psa_point_rounded,
        psa_point = point / scale[unit], psa_lower, psa_upper, match)]
}

psa_validate_manuscript <- function(cmp, full_geo) {
  if (!full_geo) return(psa_flag("V12 draw 0 vs manuscript central estimates", "country subset", NA,
                                 "Skipped: the manuscript reports the full geography."))
  if (is.null(cmp)) return(psa_flag("V12 draw 0 vs manuscript central estimates", PSA_MANUSCRIPT, NA,
                                    "manuscript not readable or no estimates parsed: NOT COMPARED"))
  rbind(
    psa_flag("V12 manuscript estimates parsed (Findings, Tables 2-3, Supp. Table 6)", PSA_MANUSCRIPT,
             nrow(cmp) == 1L + 4L * 4L + 6L * 4L, n = nrow(cmp),
             note = "expected 41 values: BAU + 4 interventions x (3 + 1) + 6 regions x 4"),
    psa_flag("V12 draw-0 point (rounded as printed) equals the manuscript's central estimate", PSA_MANUSCRIPT,
             all(cmp$match), n = nrow(cmp),
             note = if (all(cmp$match)) "all match" else
               paste(cmp[match == FALSE, sprintf("%s %s %s: ms %s vs psa %s", source, geography, metric,
                                                 manuscript, psa_point_rounded)], collapse = "; ")))
}

#...........................................................
# V8-V11 on the assembled draw tables ----
#...........................................................

psa_validate_outputs <- function(deaths, md, sm, checks, region_map, n_draws, locs) {
  out <- list()
  ids <- 0:n_draws
  key <- deaths[, .N, by = .(draw_id, scenario_id, geography)]
  out$dup <- psa_flag("V8 no duplicate draw x scenario x geography rows (resumed blocks counted once)",
                      "deaths", all(key$N == 1L), n = nrow(deaths))
  req <- md[, .(n = .N, ids_ok = setequal(draw_id, ids) && !anyDuplicated(draw_id)),
            by = .(scenario_id, geography_level, who_region)]
  out$complete <- psa_flag("V8 every required row present for every draw 0..N exactly once",
                           sprintf("%d reported groups x %d draws", nrow(req), length(ids)),
                           nrow(req) == 4L + uniqueN(region_map$who_region) && all(req$ids_ok),
                           n = nrow(md))
  num <- c("projected_deaths", "baseline_deaths", "deaths_averted", "deaths_averted_per_year", "percent_averted")
  out$finite <- psa_flag("V8 all mortality values finite", "mortality_draws",
                         all(vapply(num, function(v) all(is.finite(md[[v]])), logical(1))), n = nrow(md))
  out$averted <- psa_cmp("V9 deaths_averted = baseline_deaths - projected_deaths", md$deaths_averted,
                         md$baseline_deaths - md$projected_deaths, tol_abs = 0, scope = "all rows")
  out$peryear <- psa_cmp("V9 deaths_averted_per_year x 25 = deaths_averted (25-year divisor)",
                         md$deaths_averted_per_year * 25, md$deaths_averted, tol_rel = 1e-14, scope = "all rows")
  out$years <- psa_flag("V9 window 2026-2050 inclusive, n_years = 25", "all rows",
                        all(md$year_start == 2026L & md$year_end == 2050L & md$n_years == 25L &
                              md$n_years == md$year_end - md$year_start + 1L))
  out$pct <- psa_cmp("V9 percent_averted = 100 x averted / baseline", md$percent_averted,
                     100 * md$deaths_averted / md$baseline_deaths, tol_abs = 0, scope = "all rows")
  bvar <- deaths[scenario_id == "baseline", .(dev = max(abs(deaths - deaths[draw_id == 0L]))), by = geography]
  out$base_fixed <- psa_flag("V9 baseline deaths identical in every draw (fixed; no artificial interval)",
                             "7 geographies", all(bvar$dev == 0), value = max(bvar$dev))
  rs <- deaths[geography != "Global", .(region_sum = sum(deaths)), by = .(draw_id, scenario_id)]
  gl <- deaths[geography == "Global", .(draw_id, scenario_id, global = deaths)]
  rg <- gl[rs, on = .(draw_id, scenario_id)]
  out$reg_glob <- psa_cmp("V9 sum of the six WHO regions = Global (same country set), every draw x scenario",
                          rg$region_sum, rg$global, tol_rel = 1e-12, scope = sprintf("%d rows", nrow(rg)))
  out$map <- psa_flag("V9 WHO mapping: every modelled country in exactly one of six regions",
                      paste(region_map[, .N, by = who_region][, paste0(who_region, " ", N)], collapse = "; "),
                      nrow(region_map) == length(locs) && !anyDuplicated(region_map$location) &&
                        setequal(region_map$location, locs) && all(region_map$who_region %in% PSA_REGION_ORDER),
                      n = nrow(region_map))
  iv <- sm[is_fixed == FALSE]
  out$q <- psa_flag("V10 quantiles finite and ordered (lower <= upper); n_draws = N for every group",
                    sprintf("%d metric-groups", nrow(iv)),
                    all(is.finite(iv$lower) & is.finite(iv$upper) & iv$lower <= iv$upper) &&
                      all(sm$n_draws == n_draws), n = nrow(iv))
  pin <- iv[, point >= lower & point <= upper]
  out$q_point <- psa_flag("V10 draw-0 point inside the empirical interval (informational)",
                          sprintf("%d of %d", sum(pin), nrow(iv)), NA,
                          note = paste(iv[!pin, paste(who_region, scenario_id, metric)], collapse = "; "))
  ch <- copy(checks)
  # Negative states: the deterministic model itself (and the saved 06 output)
  # contains a few negative INITIAL `well` cells (Nx x (1 - PREVt0 - BG.mx) < 0
  # at the oldest ages; inherited defect D10). Sampling must not add any.
  neg0 <- ch[draw_id == 0L, .(scenario_id, neg0 = n_negative)]
  chn <- neg0[ch, on = "scenario_id"]
  out$nonfinite <- psa_flag("V11 no non-finite model states in any draw", "all countries",
                            all(ch$n_nonfinite == 0), value = max(ch$n_nonfinite))
  out$negative <- psa_flag("V11 negative model states in every draw = those of draw 0 (none added by sampling)",
                           "all countries", all(chn$n_negative == chn$neg0), value = max(chn$n_negative - chn$neg0),
                           note = sprintf(paste0("%s negative cells per scenario in draw 0, all inherited from 06's ",
                                                 "initial well states (identical in the saved 06 output)"),
                                          paste(unique(neg0$neg0), collapse = "/")))
  cap0 <- ch[draw_id == 0L, .(scenario_id, ir0 = n_ir_capped, cf0 = n_cf_capped, s0 = clamp_sick,
                              w0 = clamp_well, p0 = clamp_pop, a0 = clamp_allmx)]
  cc <- cap0[ch, on = "scenario_id"]
  sm_cap <- cc[draw_id > 0L, .(max_extra_ir = max(n_ir_capped - ir0), max_extra_cf = max(n_cf_capped - cf0),
                               min_extra_cf = min(n_cf_capped - cf0),
                               max_extra_clamp = max(clamp_sick - s0 + clamp_well - w0 + clamp_pop - p0 + clamp_allmx - a0),
                               draw0_cf = cf0[1], draw0_ir = ir0[1]), by = scenario_id]
  out$caps <- sm_cap[, .(check = "V11 IR/CF cells at the 0.99 cap and state clamps vs draw 0 (informational)",
                         scope = scenario_id, n = n_draws, max_abs_diff = pmax(max_extra_ir, max_extra_cf),
                         max_rel_diff = max_extra_clamp, tol_abs = NA_real_, tol_rel = NA_real_,
                         identical = NA, pass = NA,
                         note = sprintf("draw 0: %g IR / %g CF capped cells; draws: extra IR %+g, extra CF %+g..%+g; extra state clamps %+g",
                                        draw0_ir, draw0_cf, max_extra_ir, min_extra_cf, max_extra_cf, max_extra_clamp))]
  rbindlist(out, fill = TRUE)
}
