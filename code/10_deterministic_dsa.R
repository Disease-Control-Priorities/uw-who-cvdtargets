# =============================================================================
# 10_deterministic_dsa.R -- Aim 1 deterministic sensitivity analysis (DSA)
# =============================================================================
#
# One-way deterministic scenario analyses around the ACTIVE Aim 1 base case
# (00_run_model.R -> 04 -> 05 -> 06_run_scenarios_multiple.R -> 07 -> 08),
# following Briggs et al., "Model Parameter Estimation and Uncertainty",
# ISPOR-SMDM Task Force-6, Value Health 2012;15:835-842. Every DSA setting is a
# deliberately chosen scenario value, NOT a probability distribution: results
# are deterministic spreads, never confidence or credible intervals.
#
# Scenario families (manifest: dsa_manifest(); one source of truth for inputs,
# report captions and slide labels):
#   reference            the active 06 country call, reproduced exactly
#   target_achievement   50% / 75% of the increment above baseline, by 2030
#   implementation_timing full endpoint reached in 2035 / 2040 (BP and statins)
#   statin_adherence     low (0.50/0.65) and moderate (0.70/0.80) IR/CF adherence
#   statin_adherence_income  illustrative World Bank income-group adherence map
#   mortality_trend      CVD case-fatality trend regimes (background trend on)
#   extra_reference      perfect statin adherence (1.00/1.00); NOT the base case
#
# File boundary. This script writes ONLY below output_dsa/ and scenarios_dsa/
# (every write goes through dsa_assert_write()). It reads, never edits, the
# pipeline scripts and inputs:
#   * 00_run_model.R  flag assignments (run_bgmx_trend, ...) and cause_map are
#                     evaluated one by one; nothing else in 00 runs.
#   * 05_build_baseline.R  evaluated in a sandbox after a static write scan.
#                     The statements BEFORE its two trend blocks give one common
#                     pre-trend b_rates; for every trend regime 05's own trend
#                     blocks are re-evaluated on a fresh copy of that state
#                     (never on an already-trended table).
#   * 06_run_scenarios_multiple.R (the 06 script 00 sources) is never run with
#                     its execution guard on. Its helpers and the function
#                     definitions inside the guard are evaluated verbatim; its
#                     read-only setup statements (everything before
#                     `ncores <- ...`) are evaluated per trend regime in a
#                     throw-away environment. The cluster/sink/saveRDS code that
#                     writes to output/ is never evaluated. The executed
#                     run_multiple_scenarios() arguments are parsed from the
#                     %dopar% call (not the unused parameter block above it).
#   * 07_output_dalys.R  its input statements (disability weights, WPP life
#                     expectancy) are evaluated in a sandbox; YLL/YLD/DALY use
#                     07's formulas per country.
#   * 08_economic_value_calculation.R  its constants section and its
#                     make_summary_table()/rename_to_metric() definitions are
#                     evaluated verbatim; sections 3-11 are ported per country
#                     (dsa_08_country(), provenance "08 section N"), because
#                     08's own GNI step needs the United States in the same
#                     table and so cannot run on a country subset.
#   * 04 is never sourced (it rewrites data/processed/); its validated target
#     table data/processed/htn_control_targets_by_loc.csv is read as 06 reads it.
# Ordinary output/ files are read ONLY by the verification checks (V20-V23),
# never as inputs to any DSA result.
#
# Usage (from the repository root; R: "C:/Program Files/R/R-4.5.1/bin/Rscript.exe"):
#   Rscript code/10_deterministic_dsa.R mode=pilot countries=Colombia workers=2
#   Rscript code/10_deterministic_dsa.R mode=full workers=6
#   Rscript code/10_deterministic_dsa.R render=only run_id=full
# Other key=value options: dsa=<id,id,...> (default all), resume=TRUE,
#   validate=TRUE, validate_05_regimes=TRUE, compare_ordinary=TRUE,
#   render=TRUE|FALSE|only, run_id=<id>, ihme_raw_dir=<dir>.
# Interactive use: options(who_cvd.execute_10 = FALSE); source(this file); then
#   dsa_main(dsa_config(mode = "pilot", countries = "Colombia")).
# R options: who_cvd.execute_10 (FALSE = define functions only),
#   who_cvd.dsa_pilot_country (default pilot country), who_cvd.dsa_ids,
#   who_cvd.repo_root (or env var WHO_CVD_ROOT) to override the repository root.
# =============================================================================

suppressPackageStartupMessages({
  # Attach order follows 00_run_model.R (dplyr before data.table), because 05
  # is evaluated in that session layout.
  library(dplyr)
  library(data.table)
  library(tidyr)
  library(readxl)
  library(countrycode)
  library(stringr)
  library(parallel)
})

DSA_VERSION <- "1.0.0"
DSA_RENDER_TOKEN <- "rendered-by-10_deterministic_dsa"
DSA_MODEL_YEARS <- 2017:2050          # rows present in the 06 model output
DSA_REPORT_YEAR0 <- 2019L             # first year kept in the aggregated tables
DSA_AGE_GROUPS <- c("<70", "70-79", "80-89", "90+")
DSA_SCENARIOS <- c("baseline", "bp_no_diabetes_only", "bp_diabetes_only",
                   "bp_combined", "statins_only", "all_interventions")
DSA_BP_SCENARIOS <- c("bp_no_diabetes_only", "bp_diabetes_only", "bp_combined")
DSA_STATIN_SCENARIOS <- c("statins_only", "all_interventions")

#...........................................................
# Paths and write guards ----
#...........................................................

dsa_script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f[1], winslash = "/", mustWork = FALSE)))
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of, winslash = "/", mustWork = FALSE)))
  }
  NULL
}

dsa_find_repo_root <- function(start = NULL) {
  env_root <- Sys.getenv("WHO_CVD_ROOT", "")
  if (nzchar(env_root)) return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  opt_root <- getOption("who_cvd.repo_root", NULL)
  if (!is.null(opt_root)) return(normalizePath(opt_root, winslash = "/", mustWork = TRUE))
  for (s in unique(c(start, dsa_script_dir(), getwd()))) {
    d <- normalizePath(s, winslash = "/", mustWork = FALSE)
    repeat {
      if (file.exists(file.path(d, "uw-who-cvdtargets.Rproj"))) return(d)
      p <- dirname(d)
      if (identical(p, d)) break
      d <- p
    }
  }
  stop("Could not locate the repository root (uw-who-cvdtargets.Rproj). ",
       "Run from inside the repository or set WHO_CVD_ROOT.")
}

dsa_paths <- function(root = dsa_find_repo_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  list(root      = root,
       wd        = paste0(root, "/"),
       wd_code   = paste0(root, "/code/"),
       wd_raw    = paste0(root, "/data/raw/"),
       wd_data   = paste0(root, "/data/processed/"),
       wd_outp_ordinary = paste0(root, "/output/"),   # verification reads only
       rmd_dir   = paste0(root, "/scenarios/scenarios_aim1"),
       dsa_root  = paste0(root, "/output_dsa"),
       scen_root = paste0(root, "/scenarios_dsa"))
}

dsa_norm <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") tolower(p) else p
}

dsa_is_within <- function(path, dir) {
  p <- dsa_norm(path); d <- sub("/+$", "", dsa_norm(dir))
  startsWith(p, paste0(d, "/")) || identical(p, d)
}

dsa_assert_write <- function(path, paths) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("Invalid output path.")
  if (grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", path)) stop("Output path must not contain '..': ", path)
  if (!(dsa_is_within(path, paths$dsa_root) || dsa_is_within(path, paths$scen_root))) {
    stop("Refusing to write outside output_dsa/ and scenarios_dsa/: ", path)
  }
  invisible(path)
}

# OneDrive can briefly lock files that are being synced; retry like the reports.
dsa_with_retry <- function(fun, tries = 8L, wait = 0.6) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({ fun(); TRUE }, error = function(e) conditionMessage(e))
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i == tries) stop(ok, call. = FALSE)
    Sys.sleep(wait)
  }
}

dsa_mkdir <- function(dir, paths) {
  dsa_assert_write(dir, paths)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  invisible(dir)
}

dsa_save_rds <- function(object, path, paths) {
  dsa_assert_write(path, paths)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dsa_with_retry(function() base::saveRDS(object, path))
  invisible(path)
}

dsa_fwrite <- function(x, path, paths, ...) {
  dsa_assert_write(path, paths)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dsa_with_retry(function() data.table::fwrite(x, path, ...))
  invisible(path)
}

dsa_write_lines <- function(text, path, paths) {
  dsa_assert_write(path, paths)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dsa_with_retry(function() writeLines(text, path, useBytes = TRUE))
  invisible(path)
}

dsa_write_json <- function(x, path, paths) {
  dsa_assert_write(path, paths)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dsa_with_retry(function() jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE,
                                                 digits = NA, null = "null", na = "null"))
  invisible(path)
}

dsa_run_dir <- function(paths, run_id) file.path(paths$dsa_root, "runs", run_id)

#...........................................................
# Logging ----
#...........................................................

.dsa_log_file <- new.env()
dsa_log <- function(fmt, ...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(fmt, ...))
  message(msg)
  f <- .dsa_log_file$path
  if (!is.null(f)) try(cat(msg, "\n", file = f, append = TRUE, sep = ""), silent = TRUE)
  invisible(msg)
}

#...........................................................
# Static write scan (preflight for pipeline code the DSA evaluates) ----
#...........................................................
# Adapted from code/092_psa_model.R (psa_walk_calls / psa_scan_writes).

DSA_WRITE_FUNS <- c(
  "saveRDS", "save", "save.image", "fwrite", "write.csv", "write.csv2",
  "write.table", "writeLines", "writeBin", "writeChar", "write", "sink",
  "ggsave", "png", "pdf", "jpeg", "tiff", "bmp", "svg", "cairo_pdf",
  "file.create", "dir.create", "unlink", "file.remove", "file.rename",
  "file.copy", "file.append", "saveWorkbook", "write.xlsx", "write_xlsx",
  "writeData", "makeCluster", "registerDoParallel", "clusterExport",
  "download.file", "system", "system2", "shell", "Sys.setenv", "setwd"
)

dsa_is_empty_arg <- function(e, i) is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")

dsa_walk_calls <- function(expr, visit) {
  walk <- function(e) {
    if (is.call(e)) {
      visit(e)
      n <- length(e)
      if (n > 1L) for (i in 2:n) if (!dsa_is_empty_arg(e, i)) walk(e[[i]])
      if (!is.name(e[[1]])) walk(e[[1]])
    } else if (is.expression(e) || is.list(e)) {
      for (i in seq_along(e)) if (!dsa_is_empty_arg(e, i)) walk(e[[i]])
    }
  }
  walk(expr)
}

dsa_called_functions <- function(expr) {
  out <- character()
  dsa_walk_calls(expr, function(e) {
    f <- e[[1]]
    if (is.name(f)) {
      fn <- as.character(f)
      if (fn == "cat" && "file" %in% names(e)) fn <- "cat(file=)"
      out <<- c(out, fn)
    } else if (is.call(f) && identical(f[[1]], as.name("::"))) {
      out <<- c(out, as.character(f[[3]]))
    }
  })
  unique(out)
}

dsa_scan_writes <- function(exprs, allow = character()) {
  setdiff(intersect(dsa_called_functions(exprs), c(DSA_WRITE_FUNS, "cat(file=)")), allow)
}

#...........................................................
# 00_run_model.R switches (evaluated statement by statement) ----
#...........................................................

DSA_00_FLAGS <- c("run_adjustment_model", "run_bgmx_trend", "run_CF_trend",
                  "run_CF_trend_80", "run_CF_trend_ihme")

dsa_read_00 <- function(paths) {
  exprs <- parse(file.path(paths$wd_code, "00_run_model.R"), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c(DSA_00_FLAGS, "cause_map")) {
      if (length(dsa_called_functions(e[[3]])) && !identical(dsa_called_functions(e[[3]]), "c")) {
        stop("00_run_model.R: unexpected expression for ", as.character(e[[2]]))
      }
      eval(e, env)
    }
  }
  miss <- setdiff(c(DSA_00_FLAGS, "cause_map"), ls(env))
  if (length(miss)) stop("00_run_model.R no longer assigns: ", paste(miss, collapse = ", "))
  flags <- mget(DSA_00_FLAGS, envir = env)
  if (!all(vapply(flags, function(x) is.logical(x) && length(x) == 1L && !is.na(x), logical(1)))) {
    stop("00_run_model.R trend switches must be single TRUE/FALSE values.")
  }
  list(flags = flags, cause_map = env$cause_map)
}

#...........................................................
# Trend regimes and the scenario manifest (single source of truth) ----
#...........................................................

# CVD trend regimes. Background trend (run_bgmx_trend) is ON in every regime.
# `run_CF_trend_80_used` records whether 05 actually reads the 80% switch in
# that regime; where it does not, the value passed is the 00 value and is
# documented as irrelevant.
dsa_trend_regimes <- function(flags00) {
  if (!isTRUE(flags00$run_bgmx_trend) || !isTRUE(flags00$run_CF_trend) ||
      !isTRUE(flags00$run_CF_trend_80) || !isFALSE(flags00$run_CF_trend_ihme) ||
      !isTRUE(flags00$run_adjustment_model)) {
    stop("00_run_model.R switches differ from the documented DSA reference ",
         "(run_bgmx_trend = TRUE, run_CF_trend = TRUE, run_CF_trend_80 = TRUE, ",
         "run_CF_trend_ihme = FALSE, run_adjustment_model = TRUE). Update the DSA regimes first.")
  }
  data.table(
    trend_regime        = c("reference", "trend_full_cvd", "trend_no_cvd", "trend_ihme_cvd"),
    run_bgmx_trend      = TRUE,
    run_CF_trend        = c(TRUE, TRUE, FALSE, TRUE),
    run_CF_trend_80     = c(TRUE, FALSE, TRUE, TRUE),   # value passed to 05
    run_CF_trend_80_used = c(TRUE, TRUE, FALSE, FALSE),  # does 05 read it?
    run_CF_trend_ihme   = c(FALSE, FALSE, FALSE, TRUE),
    cvd_trend_input     = c("tps_bgmx_cvd_forecasted.rds (x 0.8 percent_diff)",
                            "tps_bgmx_cvd_forecasted.rds (x 1.0 percent_diff)",
                            "none (CF keeps its pre-trend post-2019 values)",
                            "tps_bgmx_cvd_ihme.rds (x 1.0 percent_diff; 80% switch not used)"),
    cvd_trend_formula   = c("CF_t = CF_pre,t * (1 + 0.8 * pd[age,sex,cause,t]), t > 2019",
                            "CF_t = CF_pre,t * (1 + pd[age,sex,cause,t]), t > 2019",
                            "CF_t = CF_pre,t",
                            "CF_t = CF_pre,t * (1 + pd_IHME[cause,t]), t > 2019"),
    background_inputs   = "tps_bgmx_forecasted.rds (BG.mx) + tps_bgmx_all_forecasted.rds (BG.mx.all), identical in every regime"
  )
}

# Illustrative, editable income-group statin adherence map (IR = primary
# prevention / incidence; CF = secondary prevention / case fatality). The
# repository holds no sourced income-specific estimates: Basios et al. 2025
# (library/) could not assess income (3 non-high-income countries; meta-
# regression RR 1.08, 95% CI 0.98-1.19). These values are ASSUMPTIONS.
DSA_INCOME_ADHERENCE <- data.table(
  income_code  = c("L", "LM", "UM", "H"),
  income_group = c("Low income", "Lower-middle income", "Upper-middle income", "High income"),
  adherence_ir = c(0.50, 0.60, 0.70, 0.80),
  adherence_cf = c(0.65, 0.70, 0.80, 0.90),
  status       = "ILLUSTRATIVE assumption, not an empirical income-group estimate"
)

# Country-code aliases between the GBD-style grouping file and World Bank files.
DSA_ISO3_ALIASES <- c(SDS = "SSD")   # South Sudan

dsa_manifest <- function(ref, regimes) {
  a_ir <- ref$adherence_ir; a_cf <- ref$adherence_cf
  ty <- as.integer(ref$statin_target_year)
  sc <- ref$statin_target_coverage
  row <- function(dsa_id, order, family, family_label, analysis, label, short_label,
                  target_fraction = 1, bp_target_year = ty, statin_target_year = ty,
                  adherence_mode = "constant", adherence_ir = a_ir, adherence_cf = a_cf,
                  trend_regime = "reference", in_tornado = analysis == "one_way",
                  assumption, source_note, illustrative = FALSE) {
    list(dsa_id = dsa_id, order = order, family = family, family_label = family_label,
         analysis = analysis, label = label, short_label = short_label,
         target_fraction = target_fraction, bp_target_year = as.integer(bp_target_year),
         statin_target_year = as.integer(statin_target_year),
         statin_target_coverage_full = sc, statin_start_year = as.integer(ref$statin_start_year),
         bp_baseline_year = 2025L, bp_scaleup_start_year = 2026L,
         adherence_mode = adherence_mode, adherence_ir = adherence_ir, adherence_cf = adherence_cf,
         trend_regime = trend_regime, in_tornado = in_tornado,
         assumption = assumption, source_note = source_note, illustrative = illustrative)
  }
  fmt2 <- function(x) formatC(x, format = "f", digits = 3)
  m <- rbindlist(list(
    row("reference", 1, "reference", "Reference", "reference",
        "Reference: active 06 base case", "Reference",
        assumption = paste0("Active 06 country call: BP 150M target reached 2030; statins ",
                            round(sc * 100), "% by ", ty, "; statin adherence IR ", fmt2(a_ir),
                            " / CF ", fmt2(a_cf), "; sodium inert (salteff = 0); TFA eliminated from ",
                            ref$tfa_policy_start_year, "; CVD CF trend at 80% of the input series."),
        source_note = "06_run_scenarios_multiple.R %dopar% call; 00_run_model.R switches; Basios et al. 2025 (adherence)"),
    row("target_50", 2, "target_achievement", "Target achievement", "one_way",
        "Partial achievement: 50% of the increment by 2030", "50% of increment",
        target_fraction = 0.50,
        assumption = paste0("BP: target_control = baseline + 0.50 x (target - baseline) per scenario/location/sex/subgroup; ",
                            "statins: baseline + 0.50 x (", sc, " - baseline). Baseline, subgroup shares and allocation unchanged."),
        source_note = "Scenario value (Briggs et al. 2012); fixed 2025 population basis of 04"),
    row("target_75", 3, "target_achievement", "Target achievement", "one_way",
        "Partial achievement: 75% of the increment by 2030", "75% of increment",
        target_fraction = 0.75,
        assumption = paste0("As target_50 with fraction 0.75."),
        source_note = "Scenario value (Briggs et al. 2012); fixed 2025 population basis of 04"),
    row("delay_2035", 4, "implementation_timing", "Implementation timing", "one_way",
        "Delayed implementation: full endpoint in 2035", "Endpoint 2035",
        bp_target_year = 2035L, statin_target_year = 2035L,
        assumption = "Baseline 2025, scale-up from 2026, the ORIGINAL full BP and statin endpoints reached in 2035 and held to 2050.",
        source_note = "Scenario value (Briggs et al. 2012)"),
    row("delay_2040", 5, "implementation_timing", "Implementation timing", "one_way",
        "Delayed implementation: full endpoint in 2040", "Endpoint 2040",
        bp_target_year = 2040L, statin_target_year = 2040L,
        assumption = "As delay_2035 with endpoint 2040.",
        source_note = "Scenario value (Briggs et al. 2012)"),
    row("adherence_low", 6, "statin_adherence", "Statin adherence", "one_way",
        "Statin adherence low (IR 0.50 / CF 0.65)", "Statin adherence low",
        adherence_ir = 0.50, adherence_cf = 0.65,
        assumption = "Assumed effective statin persistence/adherence over the horizon: IR 0.50 (primary prevention), CF 0.65 (secondary prevention). BP effects unchanged.",
        source_note = "Scenario value; below the base case of Basios et al. 2025"),
    row("adherence_moderate", 7, "statin_adherence", "Statin adherence", "one_way",
        "Statin adherence moderate (IR 0.70 / CF 0.80)", "Statin adherence moderate",
        adherence_ir = 0.70, adherence_cf = 0.80,
        assumption = "Assumed effective statin persistence/adherence: IR 0.70, CF 0.80. BP effects unchanged.",
        source_note = "Scenario value; above the base case of Basios et al. 2025"),
    row("adherence_income", 8, "statin_adherence_income", "Statin adherence by income group", "one_way",
        "Statin adherence by World Bank income group (illustrative)", "Statin adherence by income",
        adherence_mode = "income_group", adherence_ir = NA_real_, adherence_cf = NA_real_,
        assumption = paste0("Country adherence from its World Bank income group: ",
                            paste(sprintf("%s %.2f/%.2f", DSA_INCOME_ADHERENCE$income_code,
                                          DSA_INCOME_ADHERENCE$adherence_ir,
                                          DSA_INCOME_ADHERENCE$adherence_cf), collapse = "; "),
                            " (IR/CF). Income group, not WHO region."),
        source_note = "ILLUSTRATIVE assumption; no sourced income-specific estimate in the repository",
        illustrative = TRUE),
    row("adherence_perfect", 9, "extra_reference", "Extra reference (not base case)", "extra_reference",
        "Perfect statin adherence (IR 1.00 / CF 1.00): extra reference, not the base case",
        "Perfect adherence (extra ref.)",
        adherence_ir = 1, adherence_cf = 1, in_tornado = FALSE,
        assumption = "Statin adherence 1.00/1.00 (the calculate_statins_impact() default). Shown for context only; never the DSA anchor.",
        source_note = "Function default in 06; not the active call"),
    row("trend_full_cvd", 10, "mortality_trend", "CVD mortality trend", "one_way",
        "CVD case-fatality trend at 100% of the input series", "Full CVD CF trend",
        trend_regime = "trend_full_cvd",
        assumption = "run_CF_trend_80 = FALSE: CF multiplied by (1 + percent_diff) instead of (1 + 0.8 percent_diff). Same pre-supplied series, not a new Lee-Carter fit.",
        source_note = "tps_bgmx_cvd_forecasted.rds (023_get_tps_bgmx.R, Lee-Carter with damped-trend ETS)"),
    row("trend_no_cvd", 11, "mortality_trend", "CVD mortality trend", "one_way",
        "No CVD case-fatality trend (background trend retained)", "No CVD CF trend",
        trend_regime = "trend_no_cvd",
        assumption = "run_CF_trend = FALSE: no secular trend applied to CF; BG.mx and BG.mx.all keep the reference background trend.",
        source_note = "05_build_baseline.R switches"),
    row("trend_ihme_cvd", 12, "mortality_trend", "CVD mortality trend", "one_way",
        "IHME GBD Foresight CVD trend", "IHME Foresight CVD trend",
        trend_regime = "trend_ihme_cvd",
        assumption = "run_CF_trend_ihme = TRUE: CF multiplied by (1 + cause-year percent_diff) from the IHME Foresight reference-scenario global age-standardised death rates; the 80% switch is not used in this branch.",
        source_note = "tps_bgmx_cvd_ihme.rds (023_get_tps_bgmx.R from gbd2023_foresight_asr_*20+.csv)")
  ), use.names = TRUE)
  m <- merge(m, regimes[, .(trend_regime, run_bgmx_trend, run_CF_trend, run_CF_trend_80,
                            run_CF_trend_80_used, run_CF_trend_ihme, cvd_trend_input,
                            cvd_trend_formula)], by = "trend_regime", all.x = TRUE, sort = FALSE)
  setorder(m, order)
  setcolorder(m, c("dsa_id", "order", "family", "family_label", "analysis", "label", "short_label"))
  if (anyDuplicated(m$dsa_id)) stop("Duplicate dsa_id in the manifest.")
  m[]
}

# Reader-friendly version of the manifest (scenarios_dsa/ CSV, report table).
dsa_manifest_display <- function(m) {
  m[, .(`DSA id` = dsa_id, Family = family_label, Analysis = analysis, Scenario = label,
        `Target fraction` = target_fraction, `BP endpoint year` = bp_target_year,
        `Statin endpoint year` = statin_target_year,
        `Statin endpoint coverage` = statin_target_coverage_full,
        `Statin adherence IR` = fifelse(adherence_mode == "income_group", "income map", sprintf("%.3f", adherence_ir)),
        `Statin adherence CF` = fifelse(adherence_mode == "income_group", "income map", sprintf("%.3f", adherence_cf)),
        `run_bgmx_trend` = run_bgmx_trend, `run_CF_trend` = run_CF_trend,
        `run_CF_trend_80` = fifelse(run_CF_trend_80_used, as.character(run_CF_trend_80), "irrelevant"),
        `run_CF_trend_ihme` = run_CF_trend_ihme, `CVD trend input` = cvd_trend_input,
        Illustrative = illustrative, `In one-way tornado` = in_tornado,
        Assumption = assumption, Source = source_note)]
}

#...........................................................
# 05: parse, pre-trend state, trend regimes ----
#...........................................................

dsa_parse_05 <- function(paths) {
  f <- file.path(paths$wd_code, "05_build_baseline.R")
  exprs <- parse(f, keep.source = FALSE)
  bad <- dsa_scan_writes(exprs, allow = "setwd")
  if (length(bad)) stop("05_build_baseline.R contains write calls: ", paste(bad, collapse = ", "))
  is_trend <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("if")) &&
      any(all.names(e[[2]]) %in% c("run_bgmx_trend", "run_CF_trend"))
  }, logical(1))
  ti <- which(is_trend)
  if (length(ti) != 2L || diff(ti) != 1L) {
    stop("05: expected exactly two consecutive trend blocks (run_bgmx_trend, then run_CF_trend).")
  }
  if (!"run_bgmx_trend" %in% all.names(exprs[[ti[1]]][[2]]) ||
      !"run_CF_trend" %in% all.names(exprs[[ti[2]]][[2]])) {
    stop("05: trend blocks are not in the expected order.")
  }
  list(file = f, exprs = exprs, pre = seq_len(ti[1] - 1L), trend = ti,
       post = seq.int(ti[2] + 1L, length(exprs)))
}

dsa_eval_in_sandbox <- function(exprs, env) {
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  warns <- character()
  withCallingHandlers(
    suppressMessages(for (e in exprs) eval(e, env)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  unique(warns)
}

dsa_05_env <- function(paths, flags) {
  env <- new.env(parent = globalenv())
  env$wd_raw <- paths$wd_raw
  env$wd_data <- paths$wd_data
  for (nm in names(flags)) assign(nm, flags[[nm]], envir = env)
  env
}

# Everything 05 does before its trend blocks: adjusted rates, UNWPP population,
# COVID rebalancing, 2020-2050 replication, IR/CF adjustments, covid.mx merge.
dsa_run_05_pre <- function(paths, p05, flags) {
  env <- dsa_05_env(paths, flags)
  warns <- dsa_eval_in_sandbox(p05$exprs[p05$pre], env)
  # repYear() is a closure over the 05 sandbox; re-home it (outputs unchanged).
  rep_year <- env$repYear
  environment(rep_year) <- baseenv()
  probe <- seq_len(34L * 224L)
  if (!identical(rep_year(probe), env$repYear(probe))) stop("repYear() depends on its 05 environment.")
  list(b_pre = env$b_rates, data.in = env$data.in, inc = env$inc, repYear = rep_year,
       warnings = warns)
}

# 05's own trend blocks re-evaluated on a FRESH copy of the common pre-trend
# state (never on a table that already carries a trend).
dsa_apply_trends <- function(b_pre, flags, paths, p05) {
  env <- dsa_05_env(paths, flags)
  env$b_rates <- data.table::copy(b_pre)
  dsa_eval_in_sandbox(p05$exprs[p05$trend], env)
  env$b_rates
}

# Full 05 in one sandbox (independent reproduction check, V01/V02).
dsa_run_05_full <- function(paths, p05, flags) {
  env <- dsa_05_env(paths, flags)
  dsa_eval_in_sandbox(p05$exprs, env)
  env$b_rates
}

dsa_regime_flags <- function(regimes, regime, flags00) {
  r <- regimes[trend_regime == regime]
  if (nrow(r) != 1L) stop("Unknown trend regime: ", regime)
  list(run_adjustment_model = flags00$run_adjustment_model,
       run_bgmx_trend = r$run_bgmx_trend, run_CF_trend = r$run_CF_trend,
       run_CF_trend_80 = r$run_CF_trend_80, run_CF_trend_ihme = r$run_CF_trend_ihme)
}

#...........................................................
# 06: parse, function definitions, setup statements, executed call ----
#...........................................................

# The Aim 1 06 script that 00_run_model.R sources (comments ignored).
dsa_06_file <- function(paths) {
  code <- sub("#.*$", "", readLines(file.path(paths$wd_code, "00_run_model.R"), warn = FALSE))
  pat <- "06_run_scenarios_[A-Za-z0-9_]+[.]R"
  src <- code[grepl("source[[:space:]]*\\(", code) & grepl(pat, code)]
  hit <- unique(regmatches(src, regexpr(pat, src)))
  if (length(hit) != 1L) {
    stop("Could not identify the one 06 script sourced by 00_run_model.R (found: ",
         paste(hit, collapse = ", "), ").")
  }
  f <- file.path(paths$wd_code, hit)
  if (!file.exists(f)) stop("06 script not found: ", f)
  normalizePath(f, winslash = "/")
}

dsa_parse_06 <- function(paths) {
  file <- dsa_06_file(paths)
  exprs <- parse(file, keep.source = FALSE)
  is_guard <- function(e) is.call(e) && identical(e[[1]], as.name("if")) &&
    any(grepl("who_cvd.execute_06", deparse(e[[2]]), fixed = TRUE))
  gi <- which(vapply(exprs, is_guard, logical(1)))
  if (length(gi) != 1L) stop("Could not find the unique who_cvd.execute_06 guard in 06.")
  is_fun_def <- function(e) is.call(e) && (identical(e[[1]], as.name("<-")) ||
    identical(e[[1]], as.name("="))) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
  pre <- as.list(exprs[seq_len(gi - 1L)])
  if (!all(vapply(pre, is_fun_def, logical(1)))) stop("06: statements above the guard are not all function definitions.")
  body <- as.list(exprs[[gi]][[3]])[-1]
  fun_idx <- which(vapply(body, is_fun_def, logical(1)))
  stop_at <- which(vapply(body, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("ncores"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("Could not find `ncores <- ...` (cluster start) in 06.")
  foreach_idx <- which(vapply(body, function(e) any(grepl("%dopar%", deparse(e), fixed = TRUE)),
                              logical(1)))
  setup <- body[seq_len(stop_at - 1L)]
  bad <- dsa_scan_writes(c(pre, setup))
  if (length(bad)) stop("06 helpers/setup contain write calls: ", paste(bad, collapse = ", "))
  bad_fun <- dsa_scan_writes(body[fun_idx])
  if (length(bad_fun)) stop("06 function definitions contain write calls: ", paste(bad_fun, collapse = ", "))
  list(file = file, exprs = exprs, guard_index = gi, pre = pre, body = body,
       fun_idx = fun_idx, fun_exprs = c(pre, body[fun_idx]),
       fun_names = vapply(c(pre, body[fun_idx]), function(e) as.character(e[[2]]), ""),
       setup = setup, stop_at = stop_at, foreach_idx = foreach_idx)
}

# The run_multiple_scenarios() call EXECUTED inside the %dopar% body.
dsa_06_run_args <- function(p06) {
  if (length(p06$foreach_idx) != 1L) stop("Could not find the unique %dopar% call in 06.")
  found <- NULL
  dsa_walk_calls(p06$body[[p06$foreach_idx]], function(e) {
    if (identical(e[[1]], as.name("run_multiple_scenarios"))) found <<- e
  })
  if (is.null(found)) stop("run_multiple_scenarios() call not found in the %dopar% body.")
  a <- as.list(found)[-1]
  constant <- vapply(a, function(x) is.numeric(x) || is.character(x) || is.logical(x) || is.null(x),
                     logical(1))
  args <- lapply(a[constant], function(x) x)
  need <- c("statin_target_coverage", "statin_start_year", "statin_target_year",
            "adherence_ir", "adherence_cf", "saltmet", "salteff", "saltyear1",
            "saltyear2", "tfa_target_tfa", "tfa_policy_start_year")
  miss <- setdiff(need, names(args))
  if (length(miss)) stop("06 run call no longer passes constants: ", paste(miss, collapse = ", "))
  if (!"baseline_statin_coverage" %in% names(a)) args["baseline_statin_coverage"] <- list(NULL)
  nonconst <- vapply(a[!constant], function(x) paste(deparse(x), collapse = ""), "")
  expected_nc <- c(Country = "country", scenario_list = "scenarios",
                   htn_scenario_ids = "htn_scenario_ids", dt_hbp_targets = "dt_hbp_targets")
  if (!identical(sort(names(nonconst)), sort(names(expected_nc))) ||
      !all(nonconst[names(expected_nc)] == expected_nc)) {
    stop("06 run call passes unexpected non-constant arguments: ",
         paste(names(nonconst), nonconst, sep = " = ", collapse = "; "))
  }
  args
}

# 06's setup statements evaluated read-only on a given b_rates (per regime).
dsa_06_setup <- function(paths, p06, b_rates, base05) {
  env <- new.env(parent = globalenv())
  env$b_rates <- b_rates
  env$data.in <- data.table::copy(base05$data.in)
  env$inc     <- data.table::copy(base05$inc)
  env$repYear <- base05$repYear
  env$wd_raw  <- paths$wd_raw
  env$wd_data <- paths$wd_data
  old <- options(who_cvd.execute_06 = FALSE)
  on.exit(options(old), add = TRUE)
  suppressMessages({
    for (e in p06$pre) eval(e, env)
    for (e in p06$setup) eval(e, env)
  })
  need <- c("b_rates", "data.in", "inc", "dt_gbd_rr", "ETIHAD_RR", "ETIHAD_RR_BIN",
            "dt_tfa_scenarios", "dt_statin_scenarios", "dt_af_statins", "dt_hbp_targets",
            "scenarios", "htn_scenario_ids", "project.all", "prepare_country_context",
            "run_multiple_scenarios")
  miss <- need[!vapply(need, exists, logical(1), envir = env, inherits = FALSE)]
  if (length(miss)) stop("06 setup no longer defines: ", paste(miss, collapse = ", "))
  if (!identical(names(env$scenarios), DSA_SCENARIOS)) {
    stop("06 scenario list changed: ", paste(names(env$scenarios), collapse = ", "))
  }
  env
}

# Location list exactly as 06 builds it.
dsa_06_locations <- function(data_in, dt_hbp_targets) {
  locs <- intersect(unique(data_in$location), unique(dt_hbp_targets$location))
  locs[!locs %in% c("Greenland", "Bermuda")]
}

# Data objects the 06 functions read as globals (06 runs them in the global
# environment; here each country task gets its own environment).
DSA_06_DATA <- c("b_rates", "data.in", "inc", "repYear", "dt_gbd_rr", "ETIHAD_RR",
                 "ETIHAD_RR_BIN", "dt_tfa_scenarios", "dt_statin_scenarios",
                 "dt_af_statins", "dt_hbp_targets", "scenarios", "htn_scenario_ids")

dsa_country_data <- function(env06, Country) {
  data_in <- env06$data.in[location == Country]
  iso <- unique(data_in$iso3)
  list(b_rates = env06$b_rates[location == Country],
       data.in = data_in,
       inc = env06$inc[iso3 %in% iso],
       repYear = env06$repYear,
       dt_gbd_rr = env06$dt_gbd_rr,
       ETIHAD_RR = env06$ETIHAD_RR,
       ETIHAD_RR_BIN = env06$ETIHAD_RR_BIN,
       dt_tfa_scenarios = env06$dt_tfa_scenarios[location == Country],
       dt_statin_scenarios = env06$dt_statin_scenarios[location == Country],
       dt_af_statins = env06$dt_af_statins[location == Country],
       dt_hbp_targets = env06$dt_hbp_targets[location == Country],
       scenarios = env06$scenarios,
       htn_scenario_ids = env06$htn_scenario_ids)
}

# A task environment that plays the role of 06's global environment: the
# country's data plus 06's own function definitions evaluated verbatim.
dsa_task_env <- function(data, fun_exprs, allowed_target_years) {
  env <- new.env(parent = globalenv())
  for (nm in names(data)) assign(nm, data[[nm]], envir = env)
  for (e in fun_exprs) eval(e, env)
  env$validate_htn_target_table_06 <- env$validate_htn_target_table
  env$dsa_allowed_target_years <- allowed_target_years
  # DSA-safe validator (delay scenarios only): the 06 validator rejects any
  # target year other than 2030, although aim2_control_trajectory() already
  # takes baseline_year/target_year from the table. The wrapper accepts ONE
  # declared DSA target year and runs the unchanged 06 validator on a copy
  # whose target_year is 2030, so every other 06 check still applies.
  env$validate_htn_target_table_dsa <- eval(quote(function(targets) {
    if (all(c("baseline_year", "scaleup_start_year", "target_year") %in% names(targets))) {
      ty <- unique(targets$target_year)
      if (length(ty) != 1L || !ty %in% dsa_allowed_target_years) {
        stop("DSA target table must carry one declared target year (",
             paste(dsa_allowed_target_years, collapse = ", "), "); found ",
             paste(ty, collapse = ", "), ".")
      }
      targets <- data.table::copy(targets)
      targets[, target_year := 2030L]
    }
    validate_htn_target_table_06(targets)
  }), env)
  env
}

#...........................................................
# 07 and 08 inputs (sandboxed) ----
#...........................................................

# 07: the statements BEFORE its model-output section (disability weights, WPP
# life expectancy). Adapted from code/093_psa_downstream.R psa_load_07_inputs().
dsa_load_07_inputs <- function(paths, cause_map) {
  exprs <- parse(file.path(paths$wd_code, "07_output_dalys.R"), keep.source = FALSE)
  stop_at <- which(vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("out_model_path"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("07: could not find the model-output section.")
  pre <- exprs[seq_len(stop_at - 1L)]
  bad <- dsa_scan_writes(pre)
  if (length(bad)) stop("07 input section contains write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$wd_raw <- paths$wd_raw
  env$cause_map <- cause_map
  suppressWarnings(suppressMessages(for (e in pre) eval(e, env)))
  # The formula section of 07 (after loading/relabelling the model files).
  formula_ok <- any(vapply(exprs, function(e) identical(deparse(e), deparse(quote(dt[, yld := sick * dw])))
                           , logical(1))) &&
    any(vapply(exprs, function(e) identical(deparse(e), deparse(quote(dt[, yll := dead * le]))), logical(1))) &&
    any(vapply(exprs, function(e) identical(deparse(e), deparse(quote(dt[, daly := yld + yll]))), logical(1)))
  if (!formula_ok) stop("07: the YLD/YLL/DALY formulas changed; update dsa_aggregate().")
  list(dw = data.table::copy(env$dw), lt_interp = data.table::copy(env$lt_interp),
       int_year = env$int_year)
}

# 08 section 1 (constants) plus its summary-table function definitions,
# evaluated verbatim after a write scan.
dsa_load_08 <- function(paths) {
  exprs <- parse(file.path(paths$wd_code, "08_economic_value_calculation.R"), keep.source = FALSE)
  stop_at <- which(vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("model_files"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("08: could not find the model-output section.")
  pre <- exprs[seq_len(stop_at - 1L)]
  bad <- dsa_scan_writes(pre)
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
  # Summary-table functions (08 sections 13-14): definitions only.
  fdefs <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                    as.character(e[[2]]) %in% c("make_summary_table", "rename_to_metric") &&
                    is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")), as.list(exprs))
  if (length(fdefs) != 2L) stop("08: make_summary_table()/rename_to_metric() not found.")
  bad <- dsa_scan_writes(fdefs)
  if (length(bad)) stop("08 summary functions contain write calls: ", paste(bad, collapse = ", "))
  fenv <- new.env(parent = globalenv())
  for (nm in keep) assign(nm, get(nm, envir = env), envir = fenv)
  for (e in fdefs) eval(e, fenv)
  k <- mget(keep, envir = env)
  k$make_summary_table <- fenv$make_summary_table
  k$rename_to_metric <- fenv$rename_to_metric
  k
}

# 08 sections 5-6: iso3 mapping and GNI per capita with SSP2 forward projection
# (code copied from 08; computed once for ALL eligible locations so the United
# States reference income is always present).
dsa_08_gni <- function(k08, locs, model_years = DSA_MODEL_YEARS) {
  country_grp <- fread(k08$COUNTRY_FILE)
  loc <- country_grp[data.table(location = locs), on = .(location)]
  miss <- is.na(loc$iso3)
  if (any(miss)) {
    loc[miss, iso3 := countrycode(location, origin = "country.name", destination = "iso3c", warn = FALSE)]
  }
  gni_raw   <- fread(k08$GNI_FILE, skip = 4, header = TRUE)
  year_cols <- grep("^[0-9]{4}$", names(gni_raw), value = TRUE)
  gni <- melt(gni_raw, id.vars = "Country Code", measure.vars = year_cols,
              variable.name = "year", value.name = "gni_pc_ppp")
  setnames(gni, "Country Code", "iso3")
  gni[, year := as.integer(as.character(year))]
  gni <- gni[!is.na(gni_pc_ppp) & year >= 2000 & year <= 2050]
  ssp_gdp <- as.data.table(read_excel(k08$SSP_FILE, sheet = "data"))
  ssp_pc <- ssp_gdp[Scenario == "SSP2" & Variable == "GDP|PPP [per capita]"]
  if (nrow(ssp_pc) == 0) stop("SSP data filtered to 0 rows.")
  ssp_yr_cols <- grep("^[0-9]{4}$", names(ssp_pc), value = TRUE)
  ssp_pc_long <- melt(ssp_pc, id.vars = "Region", measure.vars = ssp_yr_cols,
                      variable.name = "year", value.name = "ssp_gdp_pc")
  setnames(ssp_pc_long, "Region", "location")
  ssp_pc_long[, year := as.integer(as.character(year))]
  ssp_pc_long[, iso3 := suppressWarnings(countrycode(location, origin = "country.name", destination = "iso3c"))]
  ssp_pc_long <- ssp_pc_long[!is.na(iso3) & !is.na(ssp_gdp_pc) & ssp_gdp_pc > 0]
  iso3_list <- sort(unique(loc[!is.na(iso3), iso3]))
  if (!"USA" %in% iso3_list) stop("08 GNI: the United States must be among the model locations.")
  ssp_annual <- ssp_pc_long[iso3 %in% iso3_list, {
    ord  <- order(year)
    yrs  <- year[ord]
    vals <- ssp_gdp_pc[ord]
    log_interp <- approx(x = yrs, y = log(vals), xout = model_years, rule = 2)$y
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
      if (length(future_idx) > 0) {
        for (j in future_idx) {
          if (year[j] == last_year[1] + 1) {
            if (!is.na(ssp_growth[j])) out[j] <- gni_last[1] * (1 + ssp_growth[j])
          } else {
            if (!is.na(out[j - 1]) && !is.na(ssp_growth[j])) out[j] <- out[j - 1] * (1 + ssp_growth[j])
          }
        }
      }
    }
    out
  }, by = iso3]
  gni_grid[, gni_pc_ppp_final := fifelse(!is.na(gni_pc_ppp), gni_pc_ppp, gni_pc_proj)]
  gni_grid <- gni_grid[, .(iso3, year, gni_pc_ppp = gni_pc_ppp_final)]
  us_gni <- gni_grid[iso3 == "USA", .(year, gni_pc_usa = gni_pc_ppp)]
  if (nrow(us_gni) == 0) stop("No USA GNI values found.")
  list(country_grp = country_grp, gni_grid = gni_grid, us_gni = us_gni,
       n_missing_gni = sum(is.na(gni_grid$gni_pc_ppp)),
       iso3_missing_gni = gni_grid[is.na(gni_pc_ppp), unique(iso3)])
}

# 08 section 11 WHO-region table (who-regions.csv + 08's manual fixes).
dsa_08_regions <- function(paths) {
  country_grp <- fread(file.path(paths$wd, "data", "raw", "who-regions.csv"))
  setnames(country_grp, old = c("Entity", "Code", "World regions according to WHO"),
           new = c("location", "iso3", "region_who"))
  country_grp[, region_who := gsub("\\s*\\(WHO\\)", "", region_who)]
  country_grp[, Year := NULL]
  country_grp
}

DSA_08_FIX_COUNTRY_GRP <- data.table(
  location = c("Brunei Darussalam", "Cabo Verde", "Democratic People's Republic of Korea",
               "Democratic Republic of the Congo", "Iran (Islamic Republic of)", "Ivory Coast",
               "Lao People's Democratic Republic", "Micronesia (Federated States of)", "Palestine",
               "Republic of Korea", "Republic of Moldova", "Russian Federation",
               "Saint Vincent and the Grenadines", "Syrian Arab Republic",
               "Taiwan (Province of China)", "Timor-Leste", "Venezuela (Bolivarian Republic of)",
               "Viet Nam"),
  iso3 = c("BRN", "CPV", "PRK", "COD", "IRN", "CIV", "LAO", "FSM", "PSE",
           "KOR", "MDA", "RUS", "VCT", "SYR", "TWN", "TLS", "VEN", "VNM"),
  who_region = c("WPR", "AFR", "SEAR", "AFR", "EMR", "AFR", "WPR", "WPR", "EMR",
                 "WPR", "EUR", "EUR", "AMR", "EMR", "WPR", "SEAR", "AMR", "WPR")
)

# 08 section 3 UNWPP population totals by location-year.
dsa_08_pop <- function(paths) {
  dt_pop_unwpp <- as.data.table(readRDS(paste0(paths$wd_data, "PopulationsSingleAge0050.rds")))
  dt_pop_unwpp[age >= 95, age := 95]
  setnames(dt_pop_unwpp, c("year_id"), c("year"))
  dt_pop_unwpp[, .(Nx = sum(Nx)), by = .(location, year)]
}

# 08 sections 3-11 for ONE country's model output (port; statements follow 08).
dsa_08_country <- function(dt_model, g) {
  k <- g$k08
  # 08 section 3
  dt_deaths <- dt_model[, .(deaths = sum(dead, na.rm = TRUE)),
                        by = .(location, year, scenario, htn_target_scenario)]
  dt_pop_unique <- unique(dt_model[, .(location, year, scenario, htn_target_scenario, age, sex, pop)])
  dt_pop_total <- dt_pop_unique[, .(population = sum(pop, na.rm = TRUE)),
                                by = .(location, year, scenario, htn_target_scenario)]
  dt_deaths <- dt_pop_total[dt_deaths, on = .(location, year, scenario, htn_target_scenario)]
  dt_pop_unwpp <- data.table::copy(g$pop_unwpp)
  dt_deaths <- dt_pop_unwpp[dt_deaths, on = .(location, year)]
  dt_deaths[, population := Nx]
  dt_deaths[, Nx := NULL]
  # 08 section 4
  dt_baseline <- dt_deaths[scenario == "baseline",
                           .(location, year, htn_target_scenario, deaths_baseline = deaths)]
  dt_compare <- dt_deaths[scenario != "baseline"]
  dt_compare <- dt_baseline[dt_compare, on = .(location, year, htn_target_scenario)]
  setnames(dt_compare, "deaths", "deaths_intervention")
  dt_compare[, deaths_averted := deaths_baseline - deaths_intervention]
  # 08 section 5
  country_grp <- data.table::copy(g$country_grp08)
  dt_compare <- country_grp[dt_compare, on = .(location)]
  missing_iso3 <- is.na(dt_compare$iso3)
  if (any(missing_iso3)) {
    dt_compare[missing_iso3, iso3 := countrycode(location, origin = "country.name",
                                                 destination = "iso3c", warn = FALSE)]
  }
  setnames(dt_compare, "region", "who_region")
  # 08 section 6 (grid precomputed by dsa_08_gni())
  dt_compare <- data.table::copy(g$gni_grid)[dt_compare, on = .(iso3, year)]
  dt_compare <- data.table::copy(g$us_gni)[dt_compare, on = .(year)]
  # 08 section 7
  lt_interp <- data.table::copy(g$lt_interp)
  dt_pop_age <- dt_pop_unique[, .(pop = sum(pop, na.rm = TRUE)),
                              by = .(location, year, scenario, htn_target_scenario, age)]
  dt_avg_adult <- dt_pop_age[age >= k$ADULT_MIN_AGE,
                             .(adult_population = sum(pop, na.rm = TRUE),
                               avg_adult_age    = sum(pop * age, na.rm = TRUE) / sum(pop, na.rm = TRUE)),
                             by = .(location, year, scenario, htn_target_scenario)]
  dt_avg_adult[, age_ref_5y := pmin(k$MAX_MODEL_AGE, (as.integer(floor(avg_adult_age)) %/% 5L) * 5L)]
  dt_le_lookup <- data.table::copy(dt_avg_adult)[, .(location, year, scenario, htn_target_scenario,
                                                     age = age_ref_5y, adult_population, avg_adult_age)]
  setkey(lt_interp, location, age, year)
  setkey(dt_le_lookup, location, age, year)
  dt_le_lookup <- lt_interp[dt_le_lookup, on = .(location, age, year), roll = "nearest"]
  n_miss_le <- sum(is.na(dt_le_lookup$le))
  setnames(dt_le_lookup, "age", "age_ref_5y")
  setnames(dt_le_lookup, "le", "le_avg_adult")
  dt_compare <- dt_le_lookup[dt_compare, on = .(location, year, scenario, htn_target_scenario)]
  dt_compare[, le_avg_adult_disc := k$disc_life_years(le_avg_adult, k$R_VSLY)]
  # 08 section 7b
  dt_deaths_age <- dt_model[, .(deaths = sum(dead, na.rm = TRUE)),
                            by = .(location, year, scenario, htn_target_scenario, age)]
  dt_baseline_age <- dt_deaths_age[scenario == "baseline",
                                   .(location, year, age, htn_target_scenario, deaths_baseline = deaths)]
  dt_deaths_age <- dt_deaths_age[scenario != "baseline"]
  dt_deaths_age <- dt_baseline_age[dt_deaths_age, on = .(location, year, age, htn_target_scenario)]
  setnames(dt_deaths_age, "deaths", "deaths_intervention_age")
  dt_deaths_age[, deaths_averted_age := deaths_baseline - deaths_intervention_age]
  dt_deaths_age[, age_ref_5y := pmin(k$MAX_MODEL_AGE, (as.integer(floor(age)) %/% 5L) * 5L)]
  lt_age <- lt_interp[, .(location, year, age_ref_5y = age, le_age = le)]
  setkey(lt_age, location, age_ref_5y, year)
  setkey(dt_deaths_age, location, age_ref_5y, year)
  dt_deaths_age <- lt_age[dt_deaths_age, on = .(location, age_ref_5y, year), roll = "nearest"]
  n_miss_le_age <- sum(is.na(dt_deaths_age$le_age))
  dt_deaths_age[, le_age_disc := k$disc_life_years(le_age, k$R_VSLY)]
  dt_ly_gained <- dt_deaths_age[, .(
    life_years_gained_undisc = sum(deaths_averted_age * le_age,      na.rm = TRUE),
    life_years_gained_disc   = sum(deaths_averted_age * le_age_disc, na.rm = TRUE),
    avg_age_of_death_averted = sum(age * deaths_averted_age, na.rm = TRUE) /
      sum(deaths_averted_age,        na.rm = TRUE)
  ), by = .(location, year, scenario, htn_target_scenario)]
  dt_compare <- dt_ly_gained[dt_compare, on = .(location, year, scenario, htn_target_scenario)]
  # 08 section 8
  dt_compare[, vsl_e1_0 := k$US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^k$VSL_ELAST_LOW]
  dt_compare[, vsl_e1_5 := k$US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^k$VSL_ELAST_HIGH]
  dt_compare[, vsl_e1_2 := fifelse(
    gni_pc_ppp >= gni_pc_usa,
    k$US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^k$VSL_ELAST_HIC,
    k$US_VSL_RATIO * gni_pc_usa * (gni_pc_ppp / gni_pc_usa)^k$VSL_ELAST_LMIC
  )]
  dt_compare[, vsl_e1_0 := pmax(vsl_e1_0, k$VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
  dt_compare[, vsl_e1_2 := pmax(vsl_e1_2, k$VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
  dt_compare[, vsl_e1_5 := pmax(vsl_e1_5, k$VSL_RATIO_FLOOR * gni_pc_ppp, na.rm = TRUE)]
  # 08 section 9
  dt_compare[le_avg_adult_disc > 0, `:=`(
    vsly_e1_0 = vsl_e1_0 / le_avg_adult,
    vsly_e1_2 = vsl_e1_2 / le_avg_adult,
    vsly_e1_5 = vsl_e1_5 / le_avg_adult
  )]
  dt_compare[, `:=`(
    economic_value_e1_0 = vsl_e1_0 * deaths_averted,
    economic_value_e1_2 = vsl_e1_2 * deaths_averted,
    economic_value_e1_5 = vsl_e1_5 * deaths_averted
  )]
  dt_compare[, `:=`(
    vsly_value_e1_0 = vsly_e1_0 * life_years_gained_undisc,
    vsly_value_e1_2 = vsly_e1_2 * life_years_gained_undisc,
    vsly_value_e1_5 = vsly_e1_5 * life_years_gained_undisc
  )]
  # 08 section 10
  DISC_RATES <- k$DISC_RATES; BASE_YEAR <- k$BASE_YEAR
  dt_compare[, `:=`(
    disc_r1 = 1 / (1 + DISC_RATES["r1"])^(year - BASE_YEAR),
    disc_r3 = 1 / (1 + DISC_RATES["r3"])^(year - BASE_YEAR),
    disc_r5 = 1 / (1 + DISC_RATES["r5"])^(year - BASE_YEAR)
  )]
  dt_compare[, `:=`(
    gni_pc_disc_r3              = gni_pc_ppp            * disc_r3,
    economic_value_e1_2_disc_r3 = economic_value_e1_2   * disc_r3,
    vsly_value_e1_2_disc_r3     = vsly_value_e1_2       * disc_r3,
    economic_value_e1_5_disc_r3 = economic_value_e1_5   * disc_r3,
    vsly_value_e1_5_disc_r3     = vsly_value_e1_5       * disc_r3
  )]
  # 08 section 11
  dt_final <- dt_compare[, .(
    location, iso3, year, scenario, htn_target_scenario, who_region,
    deaths_baseline, deaths_intervention, deaths_averted, avg_age_of_death_averted,
    population, adult_population, avg_adult_age, age_ref_5y, le_avg_adult, le_avg_adult_disc,
    life_years_gained_undisc, life_years_gained_disc, gni_pc_ppp, gni_pc_usa,
    vsl_e1_0, vsl_e1_2, vsl_e1_5, vsly_e1_0, vsly_e1_2, vsly_e1_5,
    economic_value_e1_0, economic_value_e1_2, economic_value_e1_5,
    vsly_value_e1_0, vsly_value_e1_2, vsly_value_e1_5,
    disc_r1, disc_r3, disc_r5, gni_pc_disc_r3,
    economic_value_e1_2_disc_r3, vsly_value_e1_2_disc_r3,
    economic_value_e1_5_disc_r3, vsly_value_e1_5_disc_r3
  )]
  setorder(dt_final, location, year, scenario)
  country_grp <- data.table::copy(g$who_regions08)
  dt_final <- country_grp[dt_final, on = .(location)]
  missing_iso3 <- is.na(dt_final$iso3)
  if (any(missing_iso3)) {
    dt_final[missing_iso3, iso3 := countrycode(location, origin = "country.name",
                                               destination = "iso3c", warn = FALSE)]
  }
  fix_country_grp <- data.table::copy(DSA_08_FIX_COUNTRY_GRP)
  dt_final[fix_country_grp, on = .(location), `:=`(
    iso3       = fcoalesce(iso3, i.iso3),
    region_who = fcoalesce(region_who, i.who_region)
  )]
  dt_final[, region_who := fcase(
    region_who == "AFR",  "Africa",
    region_who == "EMR",  "Eastern Mediterranean",
    region_who == "EUR",  "Europe",
    region_who == "AMR",  "Americas",
    region_who == "SEAR", "South-East Asia",
    region_who == "WPR",  "Western Pacific",
    default = region_who
  )]
  dt_final[, who_region := region_who]
  dt_final[, region_who := NULL]
  setattr(dt_final, "n_miss_le", n_miss_le)
  setattr(dt_final, "n_miss_le_age", n_miss_le_age)
  dt_final
}

#...........................................................
# Country metadata: WHO region and World Bank income group ----
#...........................................................

# WHO region: the aim1_report.Rmd `country-metadata` logic (who-regions.csv by
# iso3 plus its who_fix table). Income group: resolved EXPLICITLY for every
# model country (World Bank 2026 file; iso3 aliases; otherwise the latest
# classification in the World Bank historical file OGHIST). Unresolved -> stop.
dsa_income_history <- function(paths) {
  f <- file.path(paths$wd_raw, "OGHIST_2026_03_10 World Bank Income Group.xlsx")
  if (!file.exists(f)) return(NULL)
  x <- suppressMessages(as.data.table(read_excel(f, sheet = "Country Analytical History",
                                                  col_names = FALSE)))
  hdr <- which(x[[2]] == "Bank's fiscal year:")
  if (length(hdr) != 1L) stop("OGHIST: fiscal-year header row not found.")
  fy <- unlist(x[hdr, -(1:2)], use.names = FALSE)
  rows <- x[grepl("^[A-Z]{3}$", x[[1]])]
  long <- melt(rows, id.vars = names(rows)[1:2], variable.name = "col", value.name = "code")
  setnames(long, names(rows)[1:2], c("iso3", "wb_name"))
  long[, fy := fy[match(col, names(rows)[-(1:2)])]]
  long <- long[code %in% c("L", "LM", "UM", "H") & !is.na(fy)]
  long[, fy_num := as.integer(sub("^FY", "", fy))]
  long[, fy_num := fifelse(fy_num < 50L, 2000L + fy_num, 1900L + fy_num)]   # FY89..FY99, FY00..FY26
  long[order(fy_num), .(last_code = code[.N], last_fy = fy[.N]), by = iso3]
}

dsa_country_metadata <- function(paths, locs) {
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
  countries_dt <- unique(countries_dt, by = "location")
  meta <- countries_dt[data.table(location = locs), on = .(location)]
  if (anyNA(meta$iso3)) stop("Country metadata: no iso3 for ", paste(meta[is.na(iso3), location], collapse = ", "))
  who_levels <- c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")
  if (any(!meta$region %in% who_levels)) {
    stop("Country metadata: no WHO region for ", paste(meta[!region %in% who_levels, location], collapse = ", "))
  }
  # Income group, resolved explicitly: (1) World Bank 2026 file by iso3 (with
  # the documented alias table); (2) if unclassified there, the latest code in
  # the World Bank historical file; (3) otherwise stop. No default group.
  valid <- DSA_INCOME_ADHERENCE$income_code
  ig <- fread(paste0(paths$wd_raw, "World Bank Income Group 2026.csv"))
  ig <- unique(ig[location_income %in% valid, .(iso3, wb_code = location_income)])
  if (anyDuplicated(ig$iso3)) stop("World Bank 2026 file: conflicting codes for ", paste(ig[duplicated(iso3), iso3], collapse = ", "))
  meta[, iso3_wb := fifelse(iso3 %in% names(DSA_ISO3_ALIASES), unname(DSA_ISO3_ALIASES[iso3]), iso3)]
  meta[, income_code := ig$wb_code[match(iso3_wb, ig$iso3)]]
  meta[, income_source := fifelse(is.na(income_code), NA_character_,
                                  fifelse(iso3_wb != iso3, paste0("World Bank 2026 (iso3 alias ", iso3, " -> ", iso3_wb, ")"),
                                          "World Bank 2026"))]
  hist <- dsa_income_history(paths)
  if (anyNA(meta$income_code) && !is.null(hist)) {
    j <- match(meta$iso3_wb, hist$iso3)
    fill <- is.na(meta$income_code) & !is.na(j)
    meta[fill, `:=`(income_code = hist$last_code[j[fill]],
                    income_source = paste0("World Bank historical classification (blank in the 2026 file; latest ",
                                           hist$last_fy[j[fill]], ")"))]
  }
  if (anyNA(meta$income_code)) {
    stop("Income group unresolved for: ", paste(meta[is.na(income_code), location], collapse = ", "),
         ". Add an explicit entry; the DSA never assigns a default income group.")
  }
  meta[, location_income := DSA_INCOME_ADHERENCE$income_group[match(income_code, DSA_INCOME_ADHERENCE$income_code)]]
  meta <- meta[, .(location, iso3, iso3_wb, region, income_code, location_income, income_source)]
  if (anyDuplicated(meta$location)) stop("Country metadata: duplicated locations.")
  meta[]
}

#...........................................................
# DSA inputs per country: targets, statin endpoint, adherence ----
#...........................................................

# BP target table for a DSA. Reference (fraction 1, endpoint 2030) = the 04
# table untouched. Partial: target = baseline + f (target - baseline). Delay:
# target_year only. Baseline, subgroup shares and allocation never change.
dsa_bp_targets <- function(tg, m) {
  x <- data.table::copy(tg)
  if (m$target_fraction != 1) {
    x[, target_control := baseline_control + m$target_fraction * (target_control - baseline_control)]
  }
  if (m$bp_target_year != 2030L) x[, target_year := as.integer(m$bp_target_year)]
  x
}

# Baseline statin coverage exactly as project.all() determines it (2024
# statins_current mean, clamped) and as calculate_statins_impact() then treats
# a missing value (-> 0).
dsa_statin_baseline <- function(dt_statin_scenarios, Country) {
  b <- dt_statin_scenarios[location == Country & year == 2024, mean(statins_current, na.rm = TRUE)]
  b <- max(min(b, 1), 0)
  if (is.na(b)) 0 else b
}

dsa_statin_target <- function(b, m) {
  full <- m$statin_target_coverage_full
  if (m$target_fraction == 1) return(full)
  if (b >= full) return(full)            # baseline >= endpoint: no increment either way
  b + m$target_fraction * (full - b)
}

dsa_adherence <- function(m, code) {
  if (m$adherence_mode == "constant") return(c(ir = m$adherence_ir, cf = m$adherence_cf))
  i <- match(code, DSA_INCOME_ADHERENCE$income_code)
  if (length(code) != 1L || is.na(i)) stop("No adherence mapping for income code ", code)
  c(ir = DSA_INCOME_ADHERENCE$adherence_ir[i], cf = DSA_INCOME_ADHERENCE$adherence_cf[i])
}

#...........................................................
# Target and trajectory tables (from 06's own trajectory functions) ----
#...........................................................

dsa_target_tables <- function(manifest, tg_all, locs, meta, fns06) {
  tg <- tg_all[location %in% locs]
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    m <- manifest[i]
    x <- dsa_bp_targets(tg, m)
    x[, `:=`(dsa_id = m$dsa_id,
             target_control_full = tg$target_control,
             additional_full_endpoint = htn_population_2025 * (tg$target_control - baseline_control),
             additional_dsa_endpoint = htn_population_2025 * (target_control - baseline_control))]
    for (yy in c(2030L, 2035L, 2040L, 2050L)) {
      ct <- fns06$aim2_control_trajectory(yy, x$baseline_control, x$target_control,
                                          x$baseline_year, x$target_year)
      x[, (paste0("control_", yy)) := ct]
      x[, (paste0("additional_", yy)) := htn_population_2025 * (ct - baseline_control)]
    }
    x
  })
  out <- rbindlist(rows, use.names = TRUE)
  out <- merge(out, meta[, .(location, region, income_code, location_income)], by = "location", all.x = TRUE)
  out[]
}

dsa_trajectories <- function(manifest, targets, statin_cfg, pop40, fns06, years = 2025:2050) {
  bp <- rbindlist(lapply(years, function(yy) {
    x <- targets[, .(dsa_id, scenario_id, location, sex, subgroup, htn_population_2025,
                     baseline_control, target_control, baseline_year, target_year)]
    x[, year := yy]
    x[, control_t := fns06$aim2_control_trajectory(yy, baseline_control, target_control,
                                                   baseline_year, target_year)]
    x
  }))
  bp_glob <- bp[, .(control_t = sum(control_t * htn_population_2025) / sum(htn_population_2025),
                    baseline_control = sum(baseline_control * htn_population_2025) / sum(htn_population_2025),
                    additional_controlled = sum(htn_population_2025 * (control_t - baseline_control))),
                by = .(dsa_id, scenario_id, subgroup, year)]
  bp_tot <- bp[, .(subgroup = "total",
                   control_t = sum(control_t * htn_population_2025) / sum(htn_population_2025),
                   baseline_control = sum(baseline_control * htn_population_2025) / sum(htn_population_2025),
                   additional_controlled = sum(htn_population_2025 * (control_t - baseline_control))),
               by = .(dsa_id, scenario_id, year)]
  bp_glob <- rbind(bp_glob, bp_tot, use.names = TRUE)
  # 06's calculate_coverage_by_year() is called exactly as 06 calls it: a vector
  # of years with SCALAR start/target years and coverage (its post-target
  # assignment `coverage[year > target_year] <- target_coverage` is only aligned
  # for a scalar target_coverage), once per country x DSA setting.
  st <- data.table::copy(statin_cfg)[, .(year = years), by = names(statin_cfg)]
  st[, delta_t := fns06$calculate_coverage_by_year(year, start_year = statin_start_year[1],
                                                   target_year = statin_target_year[1],
                                                   target_coverage = statin_increment[1]),
     by = .(dsa_id, location)]
  st[year < statin_start_year, delta_t := 0]
  st <- merge(st, pop40, by = "location", all.x = TRUE)
  # Weighted by the UNWPP 2025 population aged 40+ (countries without a UNWPP
  # row are left out of the weighted mean and counted).
  st_glob <- st[!is.na(pop40_2025),
                .(coverage_t = sum((statin_baseline + delta_t) * pop40_2025) / sum(pop40_2025),
                  baseline_coverage = sum(statin_baseline * pop40_2025) / sum(pop40_2025),
                  mean_increment_t = sum(delta_t * pop40_2025) / sum(pop40_2025),
                  n_countries = .N),
                by = .(dsa_id, year)]
  st_glob[, n_without_pop := st[is.na(pop40_2025), uniqueN(location)]]
  list(bp_global = bp_glob[], statin_global = st_glob[],
       bp_country = bp[year %in% c(2025L, 2026L, 2030L, 2035L, 2040L, 2050L)],
       statin_country = st[year %in% c(2025L, 2026L, 2030L, 2035L, 2040L, 2050L)],
       statin_all_years = st[, .(dsa_id, location, year, statin_increment, statin_target_year, delta_t)])
}

#...........................................................
# Country task (runs on workers) ----
#...........................................................

# run_multiple_scenarios() with a shared country context: 06 builds the
# context once per country and reuses it for all six scenarios; the DSA reuses
# it for every DSA setting of the same trend regime (it depends only on the
# regime's rates and data.in). Arguments follow 06's %dopar% call exactly.
dsa_project_country <- function(env, ctx, Country, d, ra) {
  results <- vector("list", length(env$scenarios))
  names(results) <- names(env$scenarios)
  for (scenario_name in names(env$scenarios)) {
    results[[scenario_name]] <- env$project.all(
      Country             = Country,
      interventions       = env$scenarios[[scenario_name]],
      htn_scenario_id     = env$htn_scenario_ids[[scenario_name]],
      dt_hbp_targets      = d$targets,
      statin_target_coverage   = d$statin_target_coverage,
      statin_start_year        = d$statin_start_year,
      statin_target_year       = d$statin_target_year,
      adherence_ir             = d$adherence_ir,
      adherence_cf             = d$adherence_cf,
      saltmet   = ra$saltmet,
      salteff   = ra$salteff,
      saltyear1 = ra$saltyear1,
      saltyear2 = ra$saltyear2,
      tfa_target_tfa        = ra$tfa_target_tfa,
      tfa_policy_start_year = ra$tfa_policy_start_year,
      baseline_statin_coverage = ra$baseline_statin_coverage,
      country_context = ctx
    )
  }
  res <- rbindlist(results, idcol = "scenario")
  res[, htn_target_scenario := "aim1"]   # as 06's %dopar% body
  res
}

dsa_age_group <- function(age) {
  as.character(cut(age, breaks = c(0, 70, 80, 90, Inf), labels = DSA_AGE_GROUPS,
                   right = FALSE, include.lowest = TRUE))
}

# Aggregates the report needs (07 formulas for YLL/YLD/DALY; 08 port for econ).
dsa_aggregate <- function(res, dl) {
  res[, age_group := dsa_age_group(age)]
  yr <- res[year >= DSA_REPORT_YEAR0,
            .(dead = sum(dead), newcases = sum(newcases), sick = sum(sick)),
            by = .(dsa_id, scenario, location, year, cause)]
  demo <- res[year >= 2025L,
              .(dead_2025_2050 = sum(dead), dead_2026_2050 = sum(dead[year >= 2026L])),
              by = .(dsa_id, scenario, location, cause, sex, age_group)]
  asmr <- res[year >= DSA_REPORT_YEAR0,
              .(dead = sum(dead), well = sum(well), sick = sum(sick)),
              by = .(dsa_id, scenario, year, cause, age)]
  # 07: dt_lives <- data.out[year >= int_year]; merge dw (location, cause) and
  # WPP life expectancy (location, age, year); yld = sick*dw; yll = dead*le.
  dt <- res[year >= dl$int_year, .(dsa_id, scenario, location, cause, sex, age, year, dead, sick)]
  dt <- merge(dt, dl$dw, by = c("location", "cause"), all.x = TRUE)
  dt <- merge(dt, dl$lt, by = c("location", "age", "year"), all.x = TRUE)
  dt[, yld := sick * dw]
  dt[, yll := dead * le]
  dt[, daly := yld + yll]
  daly <- dt[, .(dead = sum(dead, na.rm = TRUE), sick = sum(sick, na.rm = TRUE),
                 yll = sum(yll, na.rm = TRUE), yld = sum(yld, na.rm = TRUE),
                 daly = sum(daly, na.rm = TRUE), n_na_le = sum(is.na(le)), n_na_dw = sum(is.na(dw))),
             by = .(dsa_id, scenario, location, year, cause)]
  res[, age_group := NULL]
  econ <- dsa_08_country(res[, !"dsa_id"], dl$g08)
  econ[, dsa_id := res$dsa_id[1]]
  setcolorder(econ, "dsa_id")
  list(year = yr, demo = demo, asmr = asmr, daly = daly, econ = econ)
}

# Checks that compare DSA settings within one country (same regime).
dsa_country_checks <- function(keep, dsa_rows, Country, regime) {
  out <- list()
  add <- function(check, dsa_id, pass, detail = "") {
    out[[length(out) + 1L]] <<- data.table(location = Country, regime = regime, check = check,
                                           dsa_id = dsa_id, pass = isTRUE(pass), detail = detail)
  }
  cols <- c("dead", "sick", "well", "newcases", "pop", "all.mx", "eff_ir", "eff_cf")
  kcols <- c("scenario", "year", "age", "sex", "cause")
  same <- function(a, b, sc) {
    x <- a[scenario %in% sc]; y <- b[scenario %in% sc]
    nrow(x) == nrow(y) && all(vapply(c(kcols, cols), function(cc) identical(x[[cc]], y[[cc]]), logical(1)))
  }
  ids <- names(keep)
  first <- if ("reference" %in% ids) "reference" else ids[1]
  for (id in ids) {
    k <- keep[[id]]
    b <- k[scenario == "baseline"]
    add("C1 baseline has no intervention effect (eff_ir = eff_cf = 1 exactly)", id,
        all(b$eff_ir == 1) && all(b$eff_cf == 1))
    # 06 recomputes IR_new/IR even when the increment is zero, so BP scenarios
    # carry 1-ulp noise before 2026 (|eff - 1| ~ 2e-16); the check allows
    # machine-precision noise only.
    pre <- k[year <= 2025L]
    dev <- max(abs(pre$eff_ir - 1), abs(pre$eff_cf - 1))
    add("C2 no effect before 2026 (|eff - 1| <= 1e-12 for every scenario, years <= 2025)", id,
        dev <= 1e-12, sprintf("max |eff - 1| = %.1e", dev))
    bd <- b[year <= 2025L, dead]
    rdev <- max(vapply(setdiff(DSA_SCENARIOS, "baseline"), function(sc) {
      x <- k[scenario == sc & year <= 2025L, dead]
      if (length(x) != length(bd)) return(Inf)
      max(abs(x - bd) / pmax(abs(bd), 1e-300))
    }, numeric(1)))
    add("C3 deaths equal baseline deaths for years <= 2025 (relative difference <= 1e-12)", id,
        rdev <= 1e-12, sprintf("max relative difference %.1e", rdev))
    add("C4 BAU identical to the regime's first DSA (intervention settings never reach BAU)", id,
        same(k, keep[[first]], "baseline"))
  }
  if ("reference" %in% ids) {
    ref <- keep[["reference"]]
    for (id in intersect(ids, dsa_rows[family %in% c("statin_adherence", "statin_adherence_income",
                                                     "extra_reference"), dsa_id])) {
      add("C5 statin adherence leaves BP-only scenarios unchanged", id,
          same(keep[[id]], ref, DSA_BP_SCENARIOS))
      st_diff <- !same(keep[[id]], ref, DSA_STATIN_SCENARIOS)
      incr0 <- isTRUE(attr(keep[[id]], "statin_increment") == 0)
      add("C6 statin adherence changes statin-active scenarios (unless no statin increment)", id,
          st_diff || incr0, if (incr0) "statin increment is 0 for this country" else "")
    }
    for (id in intersect(ids, dsa_rows[family == "implementation_timing", dsa_id])) {
      ty <- dsa_rows[dsa_id == id, bp_target_year]
      # Exact after the endpoint year; at the endpoint year 06's statin
      # interpolation target*n/n can differ from target by one ulp.
      a <- keep[[id]][year > ty]; b <- ref[year > ty]
      a1 <- keep[[id]][year == ty]; b1 <- ref[year == ty]
      add("C7 delayed endpoint: effect ratios equal the reference from the endpoint year on", id,
          identical(a$eff_ir, b$eff_ir) && identical(a$eff_cf, b$eff_cf) &&
            max(abs(a1$eff_ir - b1$eff_ir), abs(a1$eff_cf - b1$eff_cf)) < 1e-12,
          paste0("identical for years > ", ty, "; |diff| < 1e-12 in ", ty))
      a2 <- keep[[id]][year >= 2026L & year < ty & scenario %in% DSA_BP_SCENARIOS]
      b2 <- ref[year >= 2026L & year < ty & scenario %in% DSA_BP_SCENARIOS]
      add("C8 delayed endpoint: smaller BP incidence effect before the endpoint", id,
          all(a2$eff_ir >= b2$eff_ir - 1e-12) && any(a2$eff_ir > b2$eff_ir + 1e-12))
    }
    part <- dsa_rows[family == "target_achievement" & dsa_id %in% ids]
    for (i in seq_len(nrow(part))) {
      id <- part$dsa_id[i]
      a <- keep[[id]][year >= 2026L]; b <- ref[year >= 2026L]
      ok <- all(a$eff_ir >= b$eff_ir - 1e-12) && all(a$eff_cf >= b$eff_cf - 1e-12) &&
        all(a$eff_ir <= 1 + 1e-12) && all(a$eff_cf <= 1 + 1e-12)
      add("C9 partial achievement: effects between none and the reference", id, ok)
    }
    if (all(c("target_50", "target_75") %in% ids)) {
      a <- keep[["target_50"]][year >= 2026L]; b <- keep[["target_75"]][year >= 2026L]
      add("C10 partial achievement: 50% effect <= 75% effect", "target_50",
          all(a$eff_ir >= b$eff_ir - 1e-12) && all(a$eff_cf >= b$eff_cf - 1e-12))
    }
  }
  rbindlist(out)
}

dsa_integrity <- function(res, Country, d) {
  key <- c("scenario", "location", "year", "age", "sex", "cause")
  num <- c("dead", "newcases", "sick", "well", "pop", "all.mx", "eff_ir", "eff_cf")
  ok_scen <- setequal(unique(res$scenario), DSA_SCENARIOS)
  ok_years <- setequal(unique(res$year), DSA_MODEL_YEARS)
  dup <- anyDuplicated(res, by = key) > 0
  bad <- vapply(num, function(cc) sum(!is.finite(res[[cc]])), integer(1))
  # 06 sets the initial states directly (year 2017 and age 20: well = Nx x
  # (1 - (PREVt0 + BG.mx))), which is negative where PREVt0 + BG.mx > 1 at the
  # oldest ages; the 2018 newcases then inherit it. Those pre-2019 rows are
  # identical in every scenario and in the ordinary 06 output, and are reported.
  # Any negative value in the projection itself fails.
  init <- res$year <= 2018L | res$age == 20
  neg_proj <- vapply(c("dead", "newcases", "sick", "well", "pop"), function(cc) sum(res[[cc]][!init] < 0), integer(1))
  neg_init <- vapply(c("dead", "newcases", "sick", "well", "pop"), function(cc) sum(res[[cc]][init] < 0), integer(1))
  lab <- unique(res[, .(scenario, intervention)])
  data.table(location = Country, dsa_id = d$dsa_id, n_rows = nrow(res), scenarios_ok = ok_scen,
             years_ok = ok_years, duplicate_keys = dup, n_nonfinite = sum(bad),
             n_negative = sum(neg_proj), n_negative_initial = sum(neg_init),
             negative_initial = if (sum(neg_init)) paste(names(neg_init)[neg_init > 0], neg_init[neg_init > 0], sep = ":", collapse = ",") else "",
             labels = paste(lab$scenario, lab$intervention, sep = "=", collapse = "; "),
             pass = ok_scen && ok_years && !dup && sum(bad) == 0L && sum(neg_proj) == 0L)
}

dsa_run_country_task <- function(task) {
  data.table::setDTthreads(1L)
  Country <- task$Country
  env <- dsa_task_env(task$data, task$fun_exprs, task$allowed_target_years)
  make_bp <- any(vapply(env$scenarios, function(s) {
    any(c("antihypertensive_no_diabetes", "antihypertensive_diabetes", "sodium") %in% s)
  }, logical(1)))
  ctx <- env$prepare_country_context(Country, make_bp_baseline = make_bp)
  ctx_digest <- digest::digest(ctx, algo = "xxhash64")
  aggs <- list(); integ <- list(); files <- list(); keep <- list()
  for (d in task$dsa) {
    cat("\n########## DSA", d$dsa_id, "|", Country, "##########\n")
    env$validate_htn_target_table <- if (isTRUE(d$use_dsa_validator)) {
      env$validate_htn_target_table_dsa
    } else {
      env$validate_htn_target_table_06
    }
    res <- dsa_project_country(env, ctx, Country, d, task$run_args)
    integ[[d$dsa_id]] <- dsa_integrity(res, Country, d)
    res[, dsa_id := d$dsa_id]
    setcolorder(res, "dsa_id")
    f <- file.path(task$paths$dsa_root, "out_model", d$dsa_id, paste0("model_output_", Country, ".rds"))
    dsa_save_rds(res, f, task$paths)
    files[[d$dsa_id]] <- data.table(dsa_id = d$dsa_id, location = Country, file = f,
                                    md5 = unname(tools::md5sum(f)))
    k <- res[, .(scenario, year, age, sex, cause, dead, sick, well, newcases, pop, all.mx, eff_ir, eff_cf)]
    setkeyv(k, c("scenario", "year", "age", "sex", "cause"))
    attr(k, "statin_increment") <- d$statin_increment
    keep[[d$dsa_id]] <- k
    aggs[[d$dsa_id]] <- dsa_aggregate(res, task$downstream)
    rm(res); gc(verbose = FALSE)   # keep worker memory flat across the DSA settings
  }
  if (!identical(digest::digest(ctx, algo = "xxhash64"), ctx_digest)) {
    stop("The shared country context was modified during the projections.")
  }
  checks <- dsa_country_checks(keep, task$dsa_rows, Country, task$regime)
  list(aggs = aggs, integrity = rbindlist(integ), files = rbindlist(files), checks = checks)
}

dsa_worker_task <- function(task) {
  t0 <- Sys.time()
  # The master passes a small stub; the country inputs are read from R's temp
  # directory (outside the repository), which keeps the master's memory low.
  if (!is.null(task$input_file)) {
    task <- c(task[setdiff(names(task), "input_file")], readRDS(task$input_file))
  }
  dsa_assert_write(task$log_file, task$paths)
  dir.create(dirname(task$log_file), recursive = TRUE, showWarnings = FALSE)
  zz <- file(task$log_file, open = "wt")
  sink(zz); sink(zz, type = "message")
  err <- NULL
  out <- tryCatch({
    cat("DSA task:", task$regime, "|", task$Country, "|", as.character(Sys.time()), "\n")
    dsa_run_country_task(task)
  }, error = function(e) {
    err <<- conditionMessage(e)
    cat("ERROR:", err, "\n")
    NULL
  })
  sink(type = "message"); sink(); close(zz)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  mem_mb <- sum(gc(verbose = FALSE)[, 6])   # "max used" (Mb) in this worker so far
  if (!is.null(out)) {
    out$key <- task$key; out$Country <- task$Country; out$regime <- task$regime
    out$dsa_ids <- vapply(task$dsa, `[[`, "", "dsa_id"); out$seconds <- secs; out$mem_mb <- mem_mb
    dsa_save_rds(out, task$checkpoint, task$paths)
  }
  status <- list(Country = task$Country, regime = task$regime, ok = !is.null(out), error = err,
                 seconds = secs, mem_mb = mem_mb)
  rm(out, task); gc(verbose = FALSE)
  status
}

dsa_worker_init <- function(code_file) {
  options(who_cvd.execute_10 = FALSE)
  suppressPackageStartupMessages({ library(data.table); library(dplyr) })  # 06 worker order
  data.table::setDTthreads(1L)
  sys.source(code_file, envir = globalenv())
  TRUE
}

#...........................................................
# Orchestration ----
#...........................................................

dsa_config <- function(mode = c("pilot", "full"), countries = NULL, dsa_ids = NULL,
                       workers = NULL, resume = TRUE, validate = TRUE,
                       validate_05_regimes = TRUE, compare_ordinary = TRUE,
                       render = TRUE, run_id = NULL,
                       # Optional: raw IHME Foresight downloads, used only by the
                       # provenance check V12 (outside the repository).
                       ihme_raw_dir = getOption("who_cvd.ihme_raw_dir",
                                                Sys.getenv("WHO_CVD_IHME_RAW_DIR",
                                                           "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/data/raw/GBD"))) {
  mode <- match.arg(mode)
  if (mode == "pilot" && is.null(countries)) countries <- getOption("who_cvd.dsa_pilot_country", "Colombia")
  if (is.null(dsa_ids)) dsa_ids <- getOption("who_cvd.dsa_ids", NULL)
  if (is.null(workers)) workers <- if (mode == "pilot") 2L else 6L   # full: ~1 GB per worker + ~3 GB master
  if (is.null(run_id)) {
    run_id <- if (mode == "pilot") paste0("pilot_", paste(countries, collapse = "-")) else "full"
    if (!is.null(dsa_ids)) run_id <- paste0(run_id, "_", paste(dsa_ids, collapse = "-"))
    run_id <- gsub("[^A-Za-z0-9_.-]+", "_", run_id)
  }
  list(mode = mode, countries = countries, dsa_ids = dsa_ids, workers = as.integer(workers),
       resume = isTRUE(resume), validate = isTRUE(validate),
       validate_05_regimes = isTRUE(validate_05_regimes), compare_ordinary = isTRUE(compare_ordinary),
       render = render, run_id = run_id, ihme_raw_dir = ihme_raw_dir)
}

dsa_parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(args, "=", fixed = TRUE)
  opt <- setNames(lapply(kv, function(x) paste(x[-1], collapse = "=")), vapply(kv, `[`, "", 1))
  lg <- function(x, default) if (is.null(x)) default else toupper(x) %in% c("TRUE", "T", "1", "YES")
  split <- function(x) if (is.null(x) || x %in% c("", "all")) NULL else trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  known <- c("mode", "countries", "dsa", "workers", "resume", "validate", "validate_05_regimes",
             "compare_ordinary", "render", "run_id", "ihme_raw_dir")
  bad <- setdiff(names(opt), known)
  if (length(bad)) stop("Unknown option(s): ", paste(bad, collapse = ", "), ". Known: ", paste(known, collapse = ", "))
  o <- function(nm) opt[[nm, exact = TRUE]]   # exact: `$` would let "validate" match "validate_05_regimes"
  render <- if (is.null(o("render"))) TRUE else if (tolower(o("render")) == "only") "only" else lg(o("render"), TRUE)
  args <- list(mode = if (is.null(o("mode"))) "pilot" else o("mode"),
               countries = split(o("countries")), dsa_ids = split(o("dsa")),
               workers = if (is.null(o("workers"))) NULL else as.integer(o("workers")),
               resume = lg(o("resume"), TRUE), validate = lg(o("validate"), TRUE),
               validate_05_regimes = lg(o("validate_05_regimes"), TRUE),
               compare_ordinary = lg(o("compare_ordinary"), TRUE),
               render = render, run_id = o("run_id"))
  if (!is.null(o("ihme_raw_dir"))) args$ihme_raw_dir <- o("ihme_raw_dir")
  do.call(dsa_config, args)
}

dsa_md5 <- function(files) {
  files <- files[file.exists(files)]
  setNames(unname(tools::md5sum(files)), basename(files))
}

dsa_input_files <- function(paths) {
  c(list.files(paths$wd_data, pattern = "adjusted", full.names = TRUE),
    file.path(paths$wd_data, c("wpp.adj.Rda", "PopulationsSingleAge0050.rds", "PopulationsAge20_2050.csv",
                               "bp_data6.csv", "covfxn2.csv", "adjustments2023_age.csv",
                               "tps_bgmx_forecasted.rds", "tps_bgmx_all_forecasted.rds",
                               "tps_bgmx_cvd_forecasted.rds", "tps_bgmx_cvd_ihme.rds",
                               "htn_control_targets_by_loc.csv", "htn_control_targets_summary.csv",
                               "ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv",
                               "ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx",
                               "sodium_policy_scenarios.rds", "tfa_policy_scenarios.rds",
                               "statin_data.rds", "af_statins.rds", "Country_groupings_extended.csv")),
    file.path(paths$wd_raw, c("IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx",
                              "World Bank Income Group 2026.csv", "who-regions.csv",
                              "OGHIST_2026_03_10 World Bank Income Group.xlsx",
                              "API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_7203.csv",
                              "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx",
                              "1721734326790-ssp_basic_drivers_release_3.1_full.xlsx",
                              "GBD/IHME-GBD_2023_DATA-52d32656-1.csv")))
}

dsa_code_files <- function(paths, p06_file) {
  c(file.path(paths$wd_code, c("00_run_model.R", "05_build_baseline.R", "07_output_dalys.R",
                               "08_economic_value_calculation.R", "10_deterministic_dsa.R")), p06_file)
}

# Snapshot of every repository file outside output_dsa/, scenarios_dsa/, .git/
# (size + mtime; no reads, so OneDrive placeholders are never downloaded).
dsa_snapshot <- function(paths) {
  f <- list.files(paths$root, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  rel <- substring(f, nchar(paths$root) + 2L)
  keep <- !grepl("^(output_dsa|scenarios_dsa|\\.git)(/|$)", rel) & !grepl("(^|/)\\.Rhistory$", rel)
  info <- file.info(f[keep], extra_cols = FALSE)
  data.table(file = rel[keep], size = info$size, mtime = as.numeric(info$mtime))
}

dsa_snapshot_diff <- function(a, b) {
  m <- merge(a, b, by = "file", all = TRUE, suffixes = c("_before", "_after"))
  m[, status := fcase(is.na(size_before), "ADDED", is.na(size_after), "REMOVED",
                      size_before != size_after | mtime_before != mtime_after, "MODIFIED",
                      default = "unchanged")]
  m[status != "unchanged"]
}

dsa_git <- function(paths, args) {
  out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(paths$root), args),
                                           stdout = TRUE, stderr = TRUE)),
                  error = function(e) paste("git unavailable:", conditionMessage(e)))
  out
}

# Everything the regime-independent part of the run needs.
dsa_prepare_shared <- function(paths, cfg) {
  t0 <- Sys.time()
  r00 <- dsa_read_00(paths)
  regimes <- dsa_trend_regimes(r00$flags)
  p05 <- dsa_parse_05(paths)
  p06 <- dsa_parse_06(paths)
  ra <- dsa_06_run_args(p06)
  dsa_log("06 script: %s | executed call: statins %.2f by %d (start %d), adherence IR %.3f / CF %.3f, salteff %s, TFA %s from %d.",
          basename(p06$file), ra$statin_target_coverage, as.integer(ra$statin_target_year),
          as.integer(ra$statin_start_year), ra$adherence_ir, ra$adherence_cf, ra$salteff,
          ra$tfa_target_tfa, as.integer(ra$tfa_policy_start_year))
  manifest <- dsa_manifest(ra, regimes)
  if (!is.null(cfg$dsa_ids)) {
    bad <- setdiff(cfg$dsa_ids, manifest$dsa_id)
    if (length(bad)) stop("Unknown dsa ids: ", paste(bad, collapse = ", "))
  }
  dsa_log("Evaluating 05 (pre-trend state) ...")
  flags_ref <- dsa_regime_flags(regimes, "reference", r00$flags)
  base05 <- dsa_run_05_pre(paths, p05, flags_ref)
  dsa_log("05 pre-trend state: %d rate rows (%.0f s).", nrow(base05$b_pre),
          as.numeric(difftime(Sys.time(), t0, units = "secs")))
  tg_all <- fread(paste0(paths$wd_data, "htn_control_targets_by_loc.csv"))
  # 06 builds its list from its own data.in (after prepare_sodium_data, which
  # only merges salt); dsa_main() confirms it against each regime's 06 setup.
  all_locs <- dsa_06_locations(base05$data.in, tg_all)
  locs <- all_locs
  if (!is.null(cfg$countries)) {
    bad <- setdiff(cfg$countries, all_locs)
    if (length(bad)) stop("Requested countries are not eligible 06 locations: ", paste(bad, collapse = ", "))
    locs <- cfg$countries
  }
  # Only the modelled countries' rates are kept (05's trend merges are by
  # age/sex/cause/year, so no country depends on another).
  base05$b_pre <- base05$b_pre[location %in% locs]
  gc(verbose = FALSE)
  meta_all <- dsa_country_metadata(paths, all_locs)
  i07 <- dsa_load_07_inputs(paths, r00$cause_map)
  k08 <- dsa_load_08(paths)
  g08 <- dsa_08_gni(k08, all_locs)
  pop_unwpp <- dsa_08_pop(paths)
  who_reg08 <- dsa_08_regions(paths)
  wpp <- as.data.table(readRDS(paste0(paths$wd_data, "PopulationsSingleAge0050.rds")))
  pop20_2019 <- wpp[year_id == 2019 & age >= 20, .(pop = sum(Nx, na.rm = TRUE)), by = location]
  pop40 <- wpp[year_id == 2025 & age >= 40, .(pop40_2025 = sum(Nx, na.rm = TRUE)), by = location]
  rm(wpp)
  dsa_log("Shared inputs ready (%.0f s).", as.numeric(difftime(Sys.time(), t0, units = "secs")))
  list(r00 = r00, regimes = regimes, p05 = p05, p06 = p06, run_args = ra, manifest = manifest,
       base05 = base05, tg_all = tg_all, all_locs = all_locs, locs = locs, meta_all = meta_all,
       meta = meta_all[location %in% locs], i07 = i07, k08 = k08, g08 = g08,
       pop_unwpp = pop_unwpp, who_reg08 = who_reg08, pop20_2019 = pop20_2019, pop40 = pop40)
}

# Downstream inputs for one country (07 + 08 port).
dsa_downstream_inputs <- function(sh, Country) {
  cg <- sh$g08$country_grp[location == Country]
  iso <- unique(cg$iso3)
  list(int_year = sh$i07$int_year,
       dw = sh$i07$dw[location == Country],
       lt = sh$i07$lt_interp[location == Country],
       g08 = list(k08 = sh$k08, pop_unwpp = sh$pop_unwpp[location == Country],
                  country_grp08 = sh$g08$country_grp, gni_grid = sh$g08$gni_grid[iso3 %in% iso],
                  us_gni = sh$g08$us_gni, lt_interp = sh$i07$lt_interp[location == Country],
                  who_regions08 = sh$who_reg08))
}

# Checkpoint key for the code a country task executes: the worker-side
# functions and constants of this script and 06's function definitions (the
# task's data, settings and downstream inputs are hashed separately in
# dsa_build_tasks()). Editing validation, finalisation or rendering code does
# not invalidate checkpoints; editing anything a task runs does.
DSA_WORKER_FUNS <- c("dsa_worker_task", "dsa_run_country_task", "dsa_task_env", "dsa_project_country",
                     "dsa_integrity", "dsa_aggregate", "dsa_age_group", "dsa_08_country",
                     "dsa_country_checks", "dsa_save_rds", "dsa_assert_write", "dsa_is_within",
                     "dsa_norm", "dsa_with_retry", "dsa_worker_init")
DSA_WORKER_CONSTS <- c("DSA_SCENARIOS", "DSA_BP_SCENARIOS", "DSA_STATIN_SCENARIOS", "DSA_MODEL_YEARS",
                       "DSA_REPORT_YEAR0", "DSA_AGE_GROUPS", "DSA_08_FIX_COUNTRY_GRP")
dsa_code_key <- function(paths, sh) {
  fx <- lapply(DSA_WORKER_FUNS, function(f) paste(deparse(get(f, mode = "function")), collapse = "\n"))
  digest::digest(list(DSA_VERSION, fx, mget(DSA_WORKER_CONSTS, inherits = TRUE),
                      lapply(sh$p06$fun_exprs, deparse)), algo = "xxhash64")
}

dsa_build_tasks <- function(regime, dsa_rows, env06, sh, paths, cfg, code_key) {
  run_dir <- dsa_run_dir(paths, cfg$run_id)
  allowed_years <- sort(unique(c(2030L, sh$manifest$bp_target_year)))
  # Country inputs are written to R's session temp directory (outside the
  # repository; removed when R exits); the master keeps only small stubs.
  in_dir <- file.path(tempdir(), "dsa_task_inputs", cfg$run_id, regime)
  dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
  lapply(sh$locs, function(Country) {
    data <- dsa_country_data(env06, Country)
    inc_code <- sh$meta[location == Country, income_code]
    b_stat <- dsa_statin_baseline(data$dt_statin_scenarios, Country)
    dsa <- lapply(seq_len(nrow(dsa_rows)), function(i) {
      m <- dsa_rows[i]
      adh <- dsa_adherence(m, inc_code)
      st <- dsa_statin_target(b_stat, m)
      list(dsa_id = m$dsa_id,
           targets = dsa_bp_targets(data$dt_hbp_targets, m),
           use_dsa_validator = m$bp_target_year != 2030L,
           statin_target_coverage = st,
           statin_start_year = as.integer(m$statin_start_year),
           statin_target_year = as.integer(m$statin_target_year),
           adherence_ir = unname(adh["ir"]), adherence_cf = unname(adh["cf"]),
           statin_baseline = b_stat, statin_increment = max(st - b_stat, 0))
    })
    body <- list(data = data, dsa = dsa, dsa_rows = dsa_rows, run_args = sh$run_args,
                 fun_exprs = sh$p06$fun_exprs, allowed_target_years = allowed_years,
                 downstream = dsa_downstream_inputs(sh, Country))
    key <- digest::digest(list(code_key, regime, data, lapply(dsa, function(d) d[setdiff(names(d), "targets")]),
                               lapply(dsa, `[[`, "targets"), sh$run_args, body$downstream),
                          algo = "xxhash64")
    input_file <- file.path(in_dir, paste0(gsub("[^A-Za-z0-9]+", "_", Country), ".rds"))
    saveRDS(body, input_file, compress = FALSE)
    list(Country = Country, regime = regime, key = key, input_file = input_file, paths = paths,
         log_file = file.path(paths$dsa_root, "logs", cfg$run_id, paste0("log_", regime, "_", Country, ".txt")),
         checkpoint = file.path(run_dir, "checkpoints", regime, paste0("task_", Country, ".rds")),
         statin = rbindlist(lapply(dsa, function(d) data.table(
           dsa_id = d$dsa_id, location = Country, statin_baseline = d$statin_baseline,
           statin_target_coverage = d$statin_target_coverage, statin_increment = d$statin_increment,
           statin_start_year = d$statin_start_year, statin_target_year = d$statin_target_year,
           adherence_ir = d$adherence_ir, adherence_cf = d$adherence_cf))))
  })
}

dsa_task_done <- function(task) {
  if (!file.exists(task$checkpoint)) return(FALSE)
  ck <- tryCatch(readRDS(task$checkpoint), error = function(e) NULL)
  !is.null(ck) && identical(ck$key, task$key) && all(file.exists(ck$files$file)) &&
    identical(unname(tools::md5sum(ck$files$file)), ck$files$md5)
}

dsa_start_cluster <- function(n, paths) {
  cl <- parallel::makeCluster(n)
  tryCatch(parallel::clusterCall(cl, dsa_worker_init, file.path(paths$wd_code, "10_deterministic_dsa.R")),
           error = function(e) { try(parallel::stopCluster(cl), silent = TRUE); stop(e) })
  cl
}

# Runs the tasks on the cluster held in `clh` (an environment with $cl and $n).
# If a worker process dies (e.g. the machine runs out of commit memory), the
# cluster is restarted with fewer workers and only unfinished tasks are rerun;
# finished tasks are recognised by their checkpoints.
dsa_run_tasks <- function(tasks, cfg, clh, paths, max_restarts = 3L) {
  if (is.null(clh$cl)) return(lapply(tasks, dsa_worker_task))
  out <- list(); todo <- tasks; restarts <- 0L
  repeat {
    res <- tryCatch(parallel::clusterApplyLB(clh$cl, todo, dsa_worker_task),
                    error = function(e) e)
    if (!inherits(res, "error")) { out <- c(out, res); break }
    restarts <- restarts + 1L
    dsa_log("A worker process failed (%s). Restart %d of %d.", conditionMessage(res), restarts, max_restarts)
    try(parallel::stopCluster(clh$cl), silent = TRUE); clh$cl <- NULL
    gc(verbose = FALSE)
    done <- vapply(todo, dsa_task_done, logical(1))
    out <- c(out, lapply(todo[done], function(t) list(Country = t$Country, regime = t$regime, ok = TRUE,
                                                      error = NA_character_, seconds = NA_real_, mem_mb = NA_real_)))
    todo <- todo[!done]
    if (!length(todo)) break
    if (restarts > max_restarts) {
      out <- c(out, lapply(todo, function(t) list(Country = t$Country, regime = t$regime, ok = FALSE,
                                                  error = paste("worker failure:", conditionMessage(res)),
                                                  seconds = NA_real_, mem_mb = NA_real_)))
      break
    }
    clh$n <- max(1L, floor(clh$n * 0.75))
    dsa_log("Restarting the cluster with %d workers for %d unfinished task(s).", clh$n, length(todo))
    clh$cl <- dsa_start_cluster(min(clh$n, length(todo)), paths)
  }
  out
}
#...........................................................
# Input validation (trend regimes, targets, trajectories, mappings) ----
#...........................................................

dsa_check <- function(id, group, pass, detail = "", n = NA_integer_) {
  data.table(check_id = id, group = group, pass = if (is.na(pass)) NA else isTRUE(pass),
             n = as.integer(n), detail = as.character(detail))
}

DSA_RATE_KEYS <- c("location", "year", "age", "sex", "cause")
DSA_RATE_COLS <- c("IR", "CF", "BG.mx", "BG.mx.all", "covid.mx", "Nx", "PREVt0", "DIS.mx.t0")

dsa_same_table <- function(a, b) {
  isTRUE(all.equal(a, b, check.attributes = FALSE, tolerance = 0)) && identical(names(a), names(b))
}

dsa_slim <- function(x) {
  x <- x[, c(DSA_RATE_KEYS, DSA_RATE_COLS), with = FALSE]
  setkeyv(x, DSA_RATE_KEYS)
  x
}

# Checks on the trend input files that do not depend on a regime.
dsa_trend_input_checks <- function(sh, paths, cfg) {
  v <- list()
  pre <- dsa_slim(sh$base05$b_pre)
  pd <- as.data.table(readRDS(file.path(paths$wd_data, "tps_bgmx_cvd_forecasted.rds")))[year > 2019]
  pd <- unique(pd, by = c("age", "sex", "cause", "year"))
  pd[, cause := fcase(cause == "Ischemic heart disease", "ihd", cause == "Ischemic stroke", "istroke",
                      cause == "Intracerebral hemorrhage", "hstroke", cause == "Hypertensive heart disease", "hhd",
                      default = cause)]
  e <- merge(pre[, DSA_RATE_KEYS, with = FALSE], pd[, .(age, sex, cause, year, pd = percent_diff)],
             by = c("age", "sex", "cause", "year"), all.x = TRUE)
  v[[length(v) + 1]] <- dsa_check("V06b", "CVD trend series covers every model age/sex/cause after 2019",
                                   e[year > 2019L, !anyNA(pd)],
                                   sprintf("%d model rows without percent_diff after 2019", e[year > 2019L, sum(is.na(pd))]))
  ih <- as.data.table(readRDS(file.path(paths$wd_data, "tps_bgmx_cvd_ihme.rds")))
  cov_ok <- all(vapply(c("ihd", "istroke", "hstroke", "hhd"), function(cc) {
    y <- ih[cause == cc & year > 2019, year]; setequal(y, 2020:2050) && !anyDuplicated(y)
  }, logical(1))) && all(is.finite(ih$percent_diff))
  v[[length(v) + 1]] <- dsa_check("V08", "IHME series: 4 causes x 2020-2050, unique and finite", cov_ok,
                                   paste0(nrow(ih), " rows; causes ", paste(sort(unique(ih$cause)), collapse = ","),
                                          "; years ", min(ih$year), "-", max(ih$year)))
  # BG.mx trend input: 05 multiplies BG.mx only where percent_diff is non-NA.
  bg <- as.data.table(readRDS(file.path(paths$wd_data, "tps_bgmx_forecasted.rds")))
  n_bg <- bg[year > 2019, sum(!is.na(percent_diff))]
  bga <- as.data.table(readRDS(file.path(paths$wd_data, "tps_bgmx_all_forecasted.rds")))
  n_bga <- bga[year > 2019, sum(!is.na(percent_diff))]
  v[[length(v) + 1]] <- dsa_check("V11", "background trend inputs (informational)", NA,
                                   paste0("tps_bgmx_forecasted.rds: ", n_bg, " of ", bg[year > 2019, .N],
                                          " post-2019 percent_diff non-missing (",
                                          if (n_bg == 0) "the BG.mx trend is INERT in 05 in every regime, including the reference" else "active",
                                          "); tps_bgmx_all_forecasted.rds: ", n_bga, " non-missing (BG.mx.all trend active)"), n_bg)
  # IHME provenance: recompute percent_diff from the raw Foresight downloads.
  fs <- if (nzchar(cfg$ihme_raw_dir) && dir.exists(cfg$ihme_raw_dir)) {
    list.files(cfg$ihme_raw_dir, pattern = "gbd2023_foresight_asr_.*20[+][.]csv$", full.names = TRUE)
  } else character()
  if (length(fs) == 4L) {
    g <- suppressWarnings(rbindlist(lapply(fs, fread), fill = TRUE))
    setnames(g, tolower(gsub(" ", "_", names(g))))
    meta_txt <- paste(unique(g$location), unique(g$scenario), unique(g$age), unique(g$sex),
                      unique(g$measure), sep = " | ")
    g <- g[year >= 2000 & scenario %in% c("Past", "Reference") & !is.na(value),
           .(year, cause = cause_of_death_or_injury, ASMR = value)]
    g[, cause := fcase(cause == "Ischemic heart disease", "ihd", cause == "Ischemic stroke", "istroke",
                       cause == "Intracerebral hemorrhage", "hstroke", cause == "Hypertensive heart disease", "hhd",
                       default = cause)]
    g <- merge(g, g[year == 2019, .(cause, ASMRB = ASMR)], by = "cause")
    g[, pdr := (ASMR - ASMRB) / ASMRB]
    mm <- merge(ih, g[, .(cause, year, pdr)], by = c("cause", "year"), all = TRUE)
    ok <- !anyNA(mm$percent_diff) && !anyNA(mm$pdr) && max(abs(mm$percent_diff - mm$pdr)) < 1e-12
    v[[length(v) + 1]] <- dsa_check("V12", "IHME series reproduces from the raw Foresight downloads", ok,
                                     paste0(length(fs), " files (", meta_txt, "). The download footer cites ",
                                            "'GBD Foresight Data Visualization ... GBD Study 2019' although the files ",
                                            "are named gbd2023; the GBD round cannot be confirmed from the files."))
  } else {
    v[[length(v) + 1]] <- dsa_check("V12", "IHME series reproduces from the raw Foresight downloads", NA,
                                     paste0("raw Foresight files not found (ihme_raw_dir = '", cfg$ihme_raw_dir,
                                            "'); provenance documented in 023_get_tps_bgmx.R only"))
  }
  rbindlist(v)
}

# Checks for one trend regime against the common pre-trend state (and, for
# alternative regimes, against the reference regime).
dsa_trend_regime_checks <- function(r, x, pre, ref, sh, paths) {
  v <- list()
  rel <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-300))
  # 05's trend merges turn `year` from double into integer; keys are compared by value.
  same_keys <- nrow(x) == nrow(pre) && all(vapply(DSA_RATE_KEYS, function(k) all(x[[k]] == pre[[k]]), logical(1)))
  v[[length(v) + 1]] <- dsa_check("V09", "trend join cardinality (no rows added, dropped or duplicated)",
                                   same_keys && !anyDuplicated(x, by = DSA_RATE_KEYS),
                                   paste0(r, ": ", nrow(x), " rows vs pre-trend ", nrow(pre)), nrow(x))
  h <- x[year <= 2019L, DSA_RATE_COLS, with = FALSE]; h0 <- pre[year <= 2019L, DSA_RATE_COLS, with = FALSE]
  v[[length(v) + 1]] <- dsa_check("V03", "historical rates unchanged", dsa_same_table(h, h0),
                                   paste0(r, ": years 2017-2019, ", paste(DSA_RATE_COLS, collapse = "/")))
  other <- setdiff(DSA_RATE_COLS, c("CF", "BG.mx", "BG.mx.all"))
  v[[length(v) + 1]] <- dsa_check("V03b", "trend blocks change only CF, BG.mx and BG.mx.all",
                                   dsa_same_table(x[, other, with = FALSE], pre[, other, with = FALSE]),
                                   paste0(r, ": ", paste(other, collapse = "/"), " identical to the pre-trend state, all years"))
  nf <- x[, sum(!is.finite(IR) | !is.finite(CF) | !is.finite(BG.mx) | !is.finite(BG.mx.all))]
  ng <- x[, sum(IR < 0 | CF < 0 | BG.mx < 0 | BG.mx.all < 0, na.rm = TRUE)]
  v[[length(v) + 1]] <- dsa_check("V10", "rates finite and non-negative after trend", nf == 0 && ng == 0,
                                   paste0(r, ": non-finite ", nf, ", negative ", ng, "; CF >= 1 (06 caps at 0.99): ",
                                          x[, sum(CF >= 1)]), nf + ng)
  if (r == "reference") {
    v[[length(v) + 1]] <- dsa_check("V04b", "background trend active in the reference (BG.mx.all changes after 2019)",
                                     x[year > 2019L, any(BG.mx.all != pre[year > 2019L]$BG.mx.all)],
                                     sprintf("BG.mx changed in %d rows, BG.mx.all in %d rows",
                                             x[, sum(BG.mx != pre$BG.mx)], x[, sum(BG.mx.all != pre$BG.mx.all)]))
  } else {
    v[[length(v) + 1]] <- dsa_check("V04", "background mortality identical to the reference (run_bgmx_trend = TRUE)",
                                     identical(x$BG.mx, ref$BG.mx) && identical(x$BG.mx.all, ref$BG.mx.all),
                                     paste0(r, ": BG.mx and BG.mx.all, all years"))
  }
  pd_expected <- function(file, by, mult) {
    p <- as.data.table(readRDS(file.path(paths$wd_data, file)))[year > 2019]
    p <- unique(p, by = c(by, "year"))
    p[, cause := fcase(cause == "Ischemic heart disease", "ihd", cause == "Ischemic stroke", "istroke",
                       cause == "Intracerebral hemorrhage", "hstroke", cause == "Hypertensive heart disease", "hhd",
                       default = cause)]
    e <- merge(pre[, c(DSA_RATE_KEYS, "CF"), with = FALSE], p[, c(by, "year", "percent_diff"), with = FALSE],
               by = c(by, "year"), all.x = TRUE)
    setkeyv(e, DSA_RATE_KEYS)
    e[, fifelse(year > 2019L & !is.na(percent_diff), CF * (1 + percent_diff * mult), CF)]
  }
  if (r == "reference") {
    exp_cf <- pd_expected("tps_bgmx_cvd_forecasted.rds", c("age", "sex", "cause"), 0.8)
    v[[length(v) + 1]] <- dsa_check("V06", "reference CF = CF_pre x (1 + 0.8 pd), recomputed from the input file",
                                     rel(x$CF, exp_cf) < 1e-12, sprintf("max relative difference %.2e", rel(x$CF, exp_cf)))
  }
  if (r == "trend_full_cvd") {
    exp_cf <- pd_expected("tps_bgmx_cvd_forecasted.rds", c("age", "sex", "cause"), 1)
    v[[length(v) + 1]] <- dsa_check("V06", "trend_full_cvd CF = CF_pre x (1 + pd), recomputed from the input file",
                                     rel(x$CF, exp_cf) < 1e-12, sprintf("max relative difference %.2e", rel(x$CF, exp_cf)))
    d80 <- ref$CF - pre$CF; d100 <- x$CF - pre$CF
    dev <- max(abs(d80 - 0.8 * d100))
    v[[length(v) + 1]] <- dsa_check("V06", "80% vs 100%: (CF_ref - CF_pre) = 0.8 x (CF_full - CF_pre)",
                                     dev <= 1e-12 * max(abs(pre$CF)), sprintf("max abs deviation %.2e", dev))
  }
  if (r == "trend_no_cvd") {
    v[[length(v) + 1]] <- dsa_check("V05", "trend_no_cvd: CF equals the pre-trend CF in every year",
                                     identical(x$CF, pre$CF), "no CVD CF trend; background trend as the reference")
  }
  if (r == "trend_ihme_cvd") {
    exp_cf <- pd_expected("tps_bgmx_cvd_ihme.rds", "cause", 1)
    v[[length(v) + 1]] <- dsa_check("V07", "trend_ihme_cvd CF = CF_pre x (1 + pd_IHME[cause, year]), recomputed",
                                     rel(x$CF, exp_cf) < 1e-12, sprintf("max relative difference %.2e", rel(x$CF, exp_cf)))
    f_alt <- dsa_regime_flags(sh$regimes, "trend_ihme_cvd", sh$r00$flags)
    f_alt$run_CF_trend_80 <- !f_alt$run_CF_trend_80
    alt <- dsa_slim(dsa_apply_trends(sh$base05$b_pre, f_alt, paths, sh$p05))
    v[[length(v) + 1]] <- dsa_check("V07", "IHME branch takes precedence over the 80% switch",
                                     identical(alt$CF, x$CF),
                                     "trend_ihme_cvd rebuilt with run_CF_trend_80 flipped: identical CF")
  }
  rbindlist(v)
}

dsa_target_checks <- function(targets, sh, trajectories, statin_cfg) {
  v <- list()
  ref <- targets[dsa_id == "reference"]
  tgo <- sh$tg_all[location %in% sh$locs]
  setkey(ref, scenario_id, location, sex, subgroup); setkey(tgo, scenario_id, location, sex, subgroup)
  v[[1]] <- dsa_check("V15", "reference targets are the 04 table unchanged",
                      identical(ref$target_control, tgo$target_control) && identical(ref$target_year, tgo$target_year))
  v[[2]] <- dsa_check("V15", "04 additional_controlled_2030 = htn_pop_2025 x (target - baseline)",
                      max(abs(tgo$additional_controlled_2030 - tgo$htn_population_2025 *
                                (tgo$target_control - tgo$baseline_control))) < 1e-3)
  for (id in unique(targets$dsa_id)) {
    x <- targets[dsa_id == id]; m <- sh$manifest[dsa_id == id]
    setkey(x, scenario_id, location, sex, subgroup)
    f <- m$target_fraction
    ok <- max(abs((x$target_control - x$baseline_control) - f * (tgo$target_control - tgo$baseline_control))) < 1e-12 &&
      all(x$target_control >= 0 & x$target_control <= 1) &&
      identical(x$baseline_control, tgo$baseline_control) &&
      identical(x$diabetes_share_among_htn_assumed, tgo$diabetes_share_among_htn_assumed) &&
      all(x$target_year == m$bp_target_year) && all(x$baseline_year == 2025L) && all(x$scaleup_start_year == 2026L)
    v[[length(v) + 1]] <- dsa_check("V15", "DSA BP targets: increment = f x original, baseline/shares unchanged, declared year",
                                     ok, paste0(id, ": f = ", f, ", endpoint ", m$bp_target_year))
    tot <- x[scenario_id == "bp_combined", sum(additional_dsa_endpoint)]
    v[[length(v) + 1]] <- dsa_check("V15b", "achieved additional controlled at the DSA endpoint (bp_combined, informational)", NA,
                                     sprintf("%s: %.3f million (= %.2f x 150M)", id, tot / 1e6, tot / 150e6))
  }
  bpg <- trajectories$bp_global[scenario_id == "bp_combined" & subgroup == "total"]
  for (id in unique(bpg$dsa_id)) {
    m <- sh$manifest[dsa_id == id]; T <- m$bp_target_year
    x <- bpg[dsa_id == id]
    c25 <- x[year == 2025, control_t]; c0 <- x[year == 2025, baseline_control]
    cT <- x[year == T, control_t]; c50 <- x[year == 2050, control_t]
    c26 <- x[year == 2026, control_t]
    full <- targets[dsa_id == id & scenario_id == "bp_combined",
                    sum(target_control * htn_population_2025) / sum(htn_population_2025)]
    ok <- abs(c25 - c0) < 1e-12 && abs(cT - full) < 1e-12 && abs(c50 - full) < 1e-12 &&
      abs((c26 - c0) - (full - c0) / (T - 2025)) < 1e-12
    v[[length(v) + 1]] <- dsa_check("V16", "BP trajectory: 2025 = baseline, 2026 = 1/(T-2025) step, T and 2050 = endpoint",
                                     ok, sprintf("%s: control 2025 %.4f, 2026 %.4f, %d %.4f, 2050 %.4f", id, c25, c26, T, cT, c50))
  }
  # Independent re-implementation of the timing rule at every year (statins) and
  # at the key years (BP): value = increment x min(max(t - 2025, 0) / (T - 2025), 1).
  sa <- data.table::copy(trajectories$statin_all_years)
  sa[, expected := statin_increment * pmin(pmax(year - 2025, 0) / (statin_target_year - 2025), 1)]
  dev_s <- sa[, max(abs(delta_t - expected))]
  v[[length(v) + 1]] <- dsa_check("V16b", "statin increment = independent timing formula in every year 2025-2050 (all countries, settings)",
                                   dev_s < 1e-12, sprintf("%d country-setting-years; max |difference| %.1e", nrow(sa), dev_s))
  bc <- data.table::copy(trajectories$bp_country)
  bc[, expected := baseline_control + (target_control - baseline_control) *
       pmin(pmax((year - baseline_year) / (target_year - baseline_year), 0), 1)]
  dev_b <- bc[, max(abs(control_t - expected))]
  v[[length(v) + 1]] <- dsa_check("V16b", "BP control = independent timing formula in 2025/2026/2030/2035/2040/2050 (all rows)",
                                   dev_b < 1e-12, sprintf("%d rows; max |difference| %.1e", nrow(bc), dev_b))
  sc <- trajectories$statin_country
  for (id in unique(sc$dsa_id)) {
    x <- sc[dsa_id == id]; T <- x$statin_target_year[1]
    ok <- all(x[year == 2025, delta_t] == 0) &&
      max(abs(x[year == 2026, delta_t] - x[year == 2026, statin_increment] / (T - 2025))) < 1e-12 &&
      max(abs(x[year == T, delta_t] - x[year == T, statin_increment])) < 1e-12 &&
      max(abs(x[year == 2050, delta_t] - x[year == 2050, statin_increment])) < 1e-12
    v[[length(v) + 1]] <- dsa_check("V16", "statin trajectory: 0 in 2025, 1/(T-2025) step in 2026, full increment at T and 2050",
                                     ok, paste0(id, ": endpoint ", T))
  }
  for (id in intersect(c("target_50", "target_75"), unique(statin_cfg$dsa_id))) {
    f <- sh$manifest[dsa_id == id, target_fraction]
    a <- statin_cfg[dsa_id == id][order(location)]; b <- statin_cfg[dsa_id == "reference"][order(location)]
    ok <- max(abs(a$statin_increment - f * b$statin_increment)) < 1e-12
    v[[length(v) + 1]] <- dsa_check("V15", "statin partial target: increment = f x reference increment", ok,
                                     paste0(id, ": ", sum(b$statin_increment == 0), " countries with baseline >= endpoint (no increment)"))
  }
  rbindlist(v)
}

dsa_mapping_checks <- function(sh) {
  m <- sh$meta
  v <- list(dsa_check("V14", "income mapping: one valid code for every model country",
                      nrow(m) == length(sh$locs) && !anyNA(m$income_code) && all(m$income_code %in% DSA_INCOME_ADHERENCE$income_code) &&
                        !anyDuplicated(m$location),
                      paste(names(table(m$income_code)), table(m$income_code), sep = ":", collapse = ", "), nrow(m)))
  fb <- m[income_source != "World Bank 2026"]
  v[[2]] <- dsa_check("V14b", "income mapping fallbacks (informational)", NA,
                      if (nrow(fb)) paste(paste0(fb$location, " -> ", fb$income_code, " [", fb$income_source, "]"), collapse = "; ") else "none")
  ra <- sh$run_args
  v[[3]] <- dsa_check("V13", "reference = executed 06 call (statins 0.50 by 2030 from 2026, adherence 0.575/0.664, salteff 0, TFA 0 from 2028)",
                      isTRUE(all.equal(c(ra$statin_target_coverage, ra$statin_start_year, ra$statin_target_year,
                                         ra$adherence_ir, ra$adherence_cf, ra$salteff, ra$tfa_target_tfa,
                                         ra$tfa_policy_start_year), c(0.50, 2026, 2030, 0.575, 0.664, 0, 0, 2028))),
                      paste(names(ra), vapply(ra, function(x) if (is.null(x)) "NULL" else as.character(x), ""), sep = "=", collapse = ", "))
  rbindlist(v)
}

# No 06 function reads a global that the task environment does not provide.
dsa_globals_check <- function(env) {
  if (!requireNamespace("codetools", quietly = TRUE)) return(dsa_check("V17", "06 function globals resolved", NA, "codetools unavailable"))
  fns <- ls(env)[vapply(ls(env), function(n) is.function(get(n, envir = env)), logical(1))]
  dt_special <- c(".", ".N", ".SD", ".I", ".GRP", ".BY", ".EACHI", "..", "J")
  miss_fun <- unique(unlist(lapply(fns, function(n) {
    g <- codetools::findGlobals(get(n, envir = env), merge = FALSE)$functions
    g <- setdiff(g, dt_special)
    g[!vapply(g, exists, logical(1), envir = env, inherits = TRUE)]
  })))
  # Variables reported by findGlobals are mostly data.table column names, so
  # only functions are required to resolve.
  dsa_check("V17", "every function called by the 06 code resolves in the task environment",
            length(miss_fun) == 0L,
            if (length(miss_fun)) paste("unresolved:", paste(miss_fun, collapse = ", ")) else
              paste(length(fns), "functions checked"), length(miss_fun))
}

#...........................................................
# Finalisation: combine checkpoints, 08 summaries, run-level checks ----
#...........................................................

dsa_add_into <- function(acc, x, keys, vals) {
  if (is.null(acc)) return(data.table::copy(x))
  z <- rbindlist(list(acc, x), use.names = TRUE)
  z[, lapply(.SD, sum), by = keys, .SDcols = vals]
}

dsa_finalize <- function(sh, cfg, paths, statuses, trend_diag, targets, trajectories, statin_cfg,
                         input_checks) {
  run_dir <- dsa_run_dir(paths, cfg$run_id)
  res_dir <- file.path(run_dir, "results")
  man <- sh$manifest[dsa_id %in% (if (is.null(cfg$dsa_ids)) sh$manifest$dsa_id else cfg$dsa_ids)]
  expected <- CJ(regime = unique(man$trend_regime), location = sh$locs)
  ck_files <- file.path(run_dir, "checkpoints", expected$regime, paste0("task_", expected$location, ".rds"))
  have <- file.exists(ck_files)
  if (!all(have)) stop("Missing task checkpoints: ", paste(head(basename(ck_files[!have]), 20), collapse = ", "))
  year_l <- demo_l <- daly_l <- econ_l <- integ_l <- files_l <- checks_l <- list()
  asmr_acc <- list()
  for (i in seq_along(ck_files)) {
    ck <- readRDS(ck_files[i])
    for (id in names(ck$aggs)) {
      if (!id %in% man$dsa_id) next
      a <- ck$aggs[[id]]
      year_l[[length(year_l) + 1]] <- a$year
      demo_l[[length(demo_l) + 1]] <- a$demo
      daly_l[[length(daly_l) + 1]] <- a$daly
      econ_l[[length(econ_l) + 1]] <- a$econ
      asmr_acc[[id]] <- dsa_add_into(asmr_acc[[id]], a$asmr, c("dsa_id", "scenario", "year", "cause", "age"),
                                     c("dead", "well", "sick"))
    }
    integ_l[[i]] <- ck$integrity[dsa_id %in% man$dsa_id]
    files_l[[i]] <- ck$files[dsa_id %in% man$dsa_id]
    checks_l[[i]] <- ck$checks[dsa_id %in% man$dsa_id]
  }
  res_year <- rbindlist(year_l); res_demo <- rbindlist(demo_l); res_daly <- rbindlist(daly_l)
  econ <- rbindlist(econ_l, fill = TRUE); res_asmr <- rbindlist(asmr_acc)
  integrity <- rbindlist(integ_l); model_files <- rbindlist(files_l); country_checks <- rbindlist(checks_l)
  rm(year_l, demo_l, daly_l, econ_l, asmr_acc); gc(verbose = FALSE)
  # 08 summary tables per DSA with 08's own make_summary_table()/rename_to_metric().
  k <- sh$k08
  econ_summary <- rbindlist(lapply(unique(econ$dsa_id), function(id) {
    dt_final <- econ[dsa_id == id][, dsa_id := NULL]
    s <- list(
      data.table::copy(k$rename_to_metric(k$make_summary_table(dt_final, "economic_value_e1_2_disc_r3", "vsl"), "vsl"))[
        , `:=`(valuation_type = "VSL", elasticity_case = "e1_2_primary")],
      data.table::copy(k$rename_to_metric(k$make_summary_table(dt_final, "vsly_value_e1_2_disc_r3", "vsly"), "vsly"))[
        , `:=`(valuation_type = "VSLY", elasticity_case = "e1_2_primary")],
      data.table::copy(k$rename_to_metric(k$make_summary_table(dt_final, "economic_value_e1_5_disc_r3", "vsl"), "vsl"))[
        , `:=`(valuation_type = "VSL", elasticity_case = "e1_5_sensitivity")],
      data.table::copy(k$rename_to_metric(k$make_summary_table(dt_final, "vsly_value_e1_5_disc_r3", "vsly"), "vsly"))[
        , `:=`(valuation_type = "VSLY", elasticity_case = "e1_5_sensitivity")])
    out <- rbindlist(s, fill = TRUE, use.names = TRUE)
    setcolorder(out, c("valuation_type", "elasticity_case", "who_region", "scenario"))
    setorder(out, valuation_type, elasticity_case, scenario, who_region)
    out[, dsa_id := id]
    setcolorder(out, "dsa_id")
    out
  }), use.names = TRUE, fill = TRUE)
  # Run-level checks.
  v <- list()
  v[[1]] <- dsa_check("V25", "every country x DSA has the six scenarios, all years, unique keys, finite non-negative values",
                      all(integrity$pass) && nrow(integrity) == length(sh$locs) * nrow(man),
                      paste0(nrow(integrity), " country-DSA results; failures: ", sum(!integrity$pass)), nrow(integrity))
  parity <- res_year[, .(n = uniqueN(location)), by = dsa_id]
  v[[2]] <- dsa_check("V25", "country-count parity across DSA settings",
                      nrow(parity) == nrow(man) && all(parity$n == length(sh$locs)),
                      paste(parity$dsa_id, parity$n, sep = ":", collapse = ", "))
  v[[3]] <- dsa_check("V25", "no duplicate (dsa_id, scenario, location, year, cause) in the aggregated table",
                      !anyDuplicated(res_year, by = c("dsa_id", "scenario", "location", "year", "cause")))
  v[[4]] <- dsa_check("V24", "country-level DSA checks C1-C10 pass", all(country_checks$pass),
                      paste0(nrow(country_checks), " checks; failed: ",
                             paste(unique(country_checks[pass == FALSE, paste(check, dsa_id, location)]), collapse = "; ")),
                      nrow(country_checks))
  t1 <- res_year[year >= 2025, .(a = sum(dead)), by = .(dsa_id, scenario)]
  t2 <- res_demo[, .(b = sum(dead_2025_2050)), by = .(dsa_id, scenario)]
  t3 <- res_asmr[year >= 2025, .(c = sum(dead)), by = .(dsa_id, scenario)]
  tt <- Reduce(function(x, y) merge(x, y, by = c("dsa_id", "scenario")), list(t1, t2, t3))
  v[[5]] <- dsa_check("V26", "stratified sums equal global totals (location/cause, age/sex, age/cause tables)",
                      tt[, max(abs(a - b) / a, abs(a - c) / a)] < 1e-10,
                      sprintf("max relative difference %.2e", tt[, max(abs(a - b) / a, abs(a - c) / a)]))
  t4 <- res_daly[, .(d = sum(dead)), by = .(dsa_id, scenario)]
  t5 <- res_year[year >= 2026, .(e = sum(dead)), by = .(dsa_id, scenario)]
  t45 <- merge(t4, t5, by = c("dsa_id", "scenario"))
  v[[6]] <- dsa_check("V26", "DALY table deaths equal model deaths 2026-2050", t45[, max(abs(d - e) / e)] < 1e-10,
                      sprintf("max relative difference %.2e; rows without LE: %d; rows without dw: %d",
                              t45[, max(abs(d - e) / e)], res_daly[, sum(n_na_le)], res_daly[, sum(n_na_dw)]))
  meta <- sh$meta
  rg <- merge(res_year[year >= 2025], meta[, .(location, region, location_income)], by = "location")
  s_reg <- rg[, .(x = sum(dead)), by = .(dsa_id, scenario, region)][, .(x = sum(x)), by = .(dsa_id, scenario)]
  s_inc <- rg[, .(y = sum(dead)), by = .(dsa_id, scenario, location_income)][, .(y = sum(y)), by = .(dsa_id, scenario)]
  sr <- Reduce(function(x, y) merge(x, y, by = c("dsa_id", "scenario")), list(t1, s_reg, s_inc))
  v[[7]] <- dsa_check("V26", "WHO-region and income-group sums equal global totals (no unmapped country)",
                      sr[, max(abs(a - x) / a, abs(a - y) / a)] < 1e-10 && !anyNA(rg$region) && !anyNA(rg$location_income))
  ec <- econ[, .(ea = sum(deaths_averted)), by = .(dsa_id, scenario)]
  yy <- res_year[, .(dead = sum(dead)), by = .(dsa_id, scenario, location, year)]
  yb <- yy[scenario == "baseline", .(dsa_id, location, year, bau = dead)]
  ya <- merge(yy[scenario != "baseline"], yb, by = c("dsa_id", "location", "year"))
  ya <- ya[, .(ya = sum(bau - dead)), by = .(dsa_id, scenario)]
  # econ covers 2017-2050, res_year 2019-2050; no effect before 2026 (C2/C3).
  ecm <- merge(ec, ya, by = c("dsa_id", "scenario"))
  v[[8]] <- dsa_check("V29", "econ deaths averted equal model deaths averted (own-regime BAU)",
                      ecm[, max(abs(ea - ya) / pmax(abs(ya), 1))] < 1e-9,
                      sprintf("max relative difference %.2e", ecm[, max(abs(ea - ya) / pmax(abs(ya), 1))]))
  bau <- res_year[scenario == "baseline", .(dsa_id, location, year, cause, dead, newcases, sick)]
  setkey(bau, dsa_id, location, year, cause)
  if ("reference" %in% man$dsa_id) {
    r0 <- bau[dsa_id == "reference"][, dsa_id := NULL]
    for (id in setdiff(man$dsa_id, "reference")) {
      x <- bau[dsa_id == id][, dsa_id := NULL]
      reg <- man[dsa_id == id, trend_regime]
      same <- identical(x$dead, r0$dead) && identical(x$sick, r0$sick)
      if (reg == "reference") {
        v[[length(v) + 1]] <- dsa_check("V27", "BAU identical to the reference BAU (same trend regime)", same, id)
      } else {
        v[[length(v) + 1]] <- dsa_check("V27", "trend regime has its own BAU (differs from the reference BAU)", !same,
                                         sprintf("%s: BAU deaths 2026-2050 %.4f M vs reference %.4f M", id,
                                                 x[year >= 2026, sum(dead)] / 1e6, r0[year >= 2026, sum(dead)] / 1e6))
      }
    }
  }
  ni <- unique(integrity[n_negative_initial > 0, .(location, negative_initial)])
  v[[length(v) + 1]] <- dsa_check("V25b", "pre-2019 initial-state negatives inherited from 06 (informational)", NA,
                                   if (nrow(ni)) paste0(paste(ni$location, ni$negative_initial, sep = " ", collapse = "; "),
                                                        " (years 2017-2018, identical in every scenario and DSA setting; ",
                                                        "06 sets well = Nx x (1 - (PREVt0 + BG.mx)); none after 2018)")
                                   else "none", nrow(ni))
  run_checks <- rbindlist(v)
  results <- list(res_year = res_year, res_demo = res_demo, res_daly = res_daly, res_asmr = res_asmr,
                  econ = econ, econ_summary = econ_summary, integrity = integrity,
                  country_checks = country_checks, model_files = model_files, run_checks = run_checks)
  for (nm in names(results)) dsa_save_rds(results[[nm]], file.path(res_dir, paste0(nm, ".rds")), paths)
  dsa_save_rds(man, file.path(res_dir, "manifest.rds"), paths)
  dsa_save_rds(sh$meta, file.path(res_dir, "country_metadata.rds"), paths)
  dsa_save_rds(sh$pop20_2019[location %in% sh$locs], file.path(res_dir, "pop20_2019.rds"), paths)
  dsa_save_rds(targets, file.path(res_dir, "htn_targets_dsa.rds"), paths)
  dsa_save_rds(trajectories, file.path(res_dir, "trajectories.rds"), paths)
  dsa_save_rds(statin_cfg, file.path(res_dir, "statin_config.rds"), paths)
  dsa_save_rds(trend_diag, file.path(res_dir, "trend_diagnostics.rds"), paths)
  dsa_save_rds(sh$regimes, file.path(res_dir, "trend_regimes.rds"), paths)
  dsa_save_rds(DSA_INCOME_ADHERENCE, file.path(res_dir, "income_adherence_map.rds"), paths)
  dsa_save_rds(sh$run_args, file.path(res_dir, "reference_run_args.rds"), paths)
  results
}

# Trend diagnostics from one regime's rate table (inputs to the projections):
# Nx-weighted mean CF and background rates, and the Nx-weighted CF multiplier
# relative to the common pre-trend state, by year and cause.
dsa_trend_diag <- function(pre, x, r) {
  z <- pre[, .(location, year, age, sex, cause, Nx, CF_pre = CF, BGall_pre = BG.mx.all, BG_pre = BG.mx)][
    x[, .(location, year, age, sex, cause, CF, BG.mx, BG.mx.all)], on = DSA_RATE_KEYS]
  z[, .(CF_mean = sum(CF * Nx, na.rm = TRUE) / sum(Nx, na.rm = TRUE),
        CF_ratio_to_pre = sum(CF * Nx, na.rm = TRUE) / sum(CF_pre * Nx, na.rm = TRUE),
        BGall_ratio_to_pre = sum(BG.mx.all * Nx, na.rm = TRUE) / sum(BGall_pre * Nx, na.rm = TRUE),
        BG_ratio_to_pre = sum(BG.mx * Nx, na.rm = TRUE) / sum(BG_pre * Nx, na.rm = TRUE)),
    by = .(year, cause)][, trend_regime := r][]
}

#...........................................................
# Verification against the ordinary pipeline (read-only; never an input) ----
#...........................................................

dsa_cmp_tables <- function(a, b, keys, cols, tol_rel = 0) {
  a <- data.table::copy(a); b <- data.table::copy(b)
  setkeyv(a, keys); setkeyv(b, keys)
  if (nrow(a) != nrow(b) || !all(vapply(keys, function(k) all(a[[k]] == b[[k]]), logical(1)))) {
    return(list(pass = FALSE, identical = FALSE, max_rel = Inf,
                detail = sprintf("row keys differ (%d vs %d rows)", nrow(a), nrow(b))))
  }
  d <- vapply(cols, function(cc) {
    x <- a[[cc]]; y <- b[[cc]]
    if (is.character(x) || is.character(y)) return(as.numeric(!identical(x, y)))
    if (!all(is.na(x) == is.na(y))) return(Inf)
    x <- x[!is.na(x)]; y <- y[!is.na(y)]
    if (!length(x)) return(0)
    max(abs(x - y) / pmax(abs(y), 1e-300))
  }, numeric(1))
  worst <- names(d)[which.max(d)]
  list(pass = all(d <= tol_rel), identical = all(d == 0), max_rel = max(d),
       detail = sprintf("%d rows, %d columns; max relative difference %.2e (%s)", nrow(a), length(cols),
                        max(d), worst))
}

dsa_verify_countries <- function(sh, cfg) {
  if (cfg$mode == "pilot") return(sh$locs)
  x <- intersect(c("Colombia", "China", "India", "United States", "Nigeria"), sh$locs)
  if (!length(x)) x <- head(sh$locs, 3L)
  x
}

dsa_verify_ordinary <- function(sh, cfg, paths, rates_ref) {
  v <- list()
  vlocs <- dsa_verify_countries(sh, cfg)
  ref_env <- dsa_06_setup(paths, sh$p06, data.table::copy(rates_ref[location %in% vlocs]), sh$base05)
  old <- setDTthreads(1L); on.exit(setDTthreads(old), add = TRUE)
  ra <- sh$run_args
  keys <- c("scenario", "location", "year", "age", "sex", "cause")
  cols <- c("well", "sick", "newcases", "dead", "pop", "all.mx", "intervention", "eff_ir", "eff_cf",
            "htn_target_scenario")
  ck_dir <- file.path(dsa_run_dir(paths, cfg$run_id), "checkpoints", "reference")
  for (Country in vlocs) {
    ck <- readRDS(file.path(ck_dir, paste0("task_", Country, ".rds")))
    dsa <- readRDS(ck$files[dsa_id == "reference", file])[, !"dsa_id"]
    # V20: the literal 06 run_multiple_scenarios() call in 06's own environment
    # (06's setup statements on the reference-regime rates), same arguments as
    # the %dopar% call.
    utils::capture.output(res <- ref_env$run_multiple_scenarios(
      Country = Country, scenario_list = ref_env$scenarios, htn_scenario_ids = ref_env$htn_scenario_ids,
      dt_hbp_targets = ref_env$dt_hbp_targets, statin_target_coverage = ra$statin_target_coverage,
      statin_start_year = ra$statin_start_year, statin_target_year = ra$statin_target_year,
      adherence_ir = ra$adherence_ir, adherence_cf = ra$adherence_cf,
      baseline_statin_coverage = ra$baseline_statin_coverage, saltmet = ra$saltmet, salteff = ra$salteff,
      saltyear1 = ra$saltyear1, saltyear2 = ra$saltyear2, tfa_target_tfa = ra$tfa_target_tfa,
      tfa_policy_start_year = ra$tfa_policy_start_year))
    res[, htn_target_scenario := "aim1"]
    c1 <- dsa_cmp_tables(dsa, res, keys, cols)
    v[[length(v) + 1]] <- dsa_check("V20", "DSA reference = live 06 run_multiple_scenarios() on the same inputs",
                                     c1$identical && identical(names(dsa), names(res)),
                                     paste0(Country, ": ", if (c1$identical) "bit-identical; " else "", c1$detail))
    # V21: the saved ordinary 06 output.
    f_ord <- file.path(paths$wd_outp_ordinary, "out_model", paste0("model_output_", Country, ".rds"))
    if (cfg$compare_ordinary && file.exists(f_ord)) {
      ord <- readRDS(f_ord); setDT(ord)
      c2 <- dsa_cmp_tables(dsa, ord, keys, cols, tol_rel = 1e-10)
      fresh <- file.mtime(f_ord) > file.mtime(sh$p06$file) &&
        file.mtime(f_ord) > file.mtime(file.path(paths$wd_data, "htn_control_targets_by_loc.csv"))
      v[[length(v) + 1]] <- dsa_check("V21", "DSA reference = saved ordinary output/out_model file (population, deaths, labels, effects)",
                                       c2$pass && identical(names(dsa), names(ord)),
                                       paste0(Country, ": ", if (c2$identical) "bit-identical; " else "", c2$detail,
                                              "; ordinary file ", if (fresh) "newer than the 06 script and the target table"
                                              else "OLDER than the 06 script or the target table"))
    } else if (cfg$compare_ordinary) {
      v[[length(v) + 1]] <- dsa_check("V21", "DSA reference = saved ordinary output/out_model file", NA,
                                       paste0(Country, ": no ordinary file"))
    }
    # V22: ordinary 08 results for the country.
    f08 <- file.path(paths$wd_outp_ordinary, "08_vsl_results.rds")
    if (cfg$compare_ordinary && file.exists(f08)) {
      o8 <- as.data.table(readRDS(f08))[location == Country]
      e8 <- ck$aggs$reference$econ[, !"dsa_id"]
      cc <- intersect(setdiff(names(o8), c("location", "year", "scenario")), names(e8))
      c3 <- dsa_cmp_tables(e8[, c("location", "year", "scenario", cc), with = FALSE],
                           o8[, c("location", "year", "scenario", cc), with = FALSE],
                           c("location", "year", "scenario"), cc, tol_rel = 1e-9)
      same_cols <- setequal(names(o8), names(e8))
      v[[length(v) + 1]] <- dsa_check("V22", "DSA reference economic rows = ordinary output/08_vsl_results.rds",
                                       c3$pass && same_cols,
                                       paste0(Country, ": ", if (c3$identical) "identical; " else "", c3$detail,
                                              if (!same_cols) paste0("; column sets differ: ",
                                                                     paste(setdiff(union(names(o8), names(e8)), cc), collapse = ",")) else ""))
    }
    # V23: ordinary 07 DALY rows for the country (optional; the file is large).
    f07 <- file.path(paths$wd_outp_ordinary, "dt_output_dalys.rds")
    if (cfg$compare_ordinary && cfg$mode == "pilot" && file.exists(f07) && Country == vlocs[1]) {
      o7 <- tryCatch(as.data.table(readRDS(f07))[location %in% vlocs], error = function(e) NULL)
      if (!is.null(o7)) {
        lab <- c("b.a.u" = "baseline", "HTN Control (No Diabetes)" = "bp_no_diabetes_only",
                 "HTN Control (Diabetes)" = "bp_diabetes_only", "Improved Statin Uptake" = "statins_only",
                 "All Interventions" = "all_interventions")
        for (C2 in vlocs) {
          ckc <- readRDS(file.path(ck_dir, paste0("task_", C2, ".rds")))
          d7 <- ckc$aggs$reference$daly[scenario %in% lab]
          a7 <- o7[location == C2, .(dead = sum(dead, na.rm = TRUE), yll = sum(yll, na.rm = TRUE),
                                     yld = sum(yld, na.rm = TRUE), daly = sum(daly, na.rm = TRUE)),
                   by = .(scenario = unname(lab[intervention]), location, year, cause)]
          c4 <- dsa_cmp_tables(d7[, .(scenario, location, year, cause, dead, yll, yld, daly)], a7,
                               c("scenario", "location", "year", "cause"), c("dead", "yll", "yld", "daly"),
                               tol_rel = 1e-9)
          v[[length(v) + 1]] <- dsa_check("V23", "DSA reference DALYs = ordinary output/dt_output_dalys.rds",
                                           c4$pass, paste0(C2, ": ", c4$detail))
        }
        rm(o7); gc(verbose = FALSE)
      }
    }
  }
  rbindlist(v)
}

# V01/V02: a regime table built from the common pre-trend state equals 05
# evaluated in full, in one sandbox, with that regime's switches.
dsa_verify_05_regime <- function(r, x, sh, paths) {
  fl <- dsa_regime_flags(sh$regimes, r, sh$r00$flags)
  full <- dsa_run_05_full(paths, sh$p05, fl)[location %in% sh$locs]
  ok <- identical(dim(full), dim(x)) && identical(names(full), names(x)) &&
    isTRUE(all.equal(full, x, check.attributes = FALSE, tolerance = 0))
  out <- dsa_check(if (r == "reference") "V01" else "V02",
                   "regime rates (pre-trend state + 05's trend blocks) = 05 evaluated in full with the regime switches",
                   ok, paste0(r, ": ", nrow(x), " rows x ", ncol(x), " columns, row order and values compared (tolerance 0)"))
  rm(full); gc(verbose = FALSE)
  out
}

# After a full run and render: the DSA reference scalars against the ordinary
# report's paper scalars (verification only).
dsa_verify_report <- function(paths) {
  f_ord <- file.path(paths$wd_outp_ordinary, "paper", "paper_scalars.rds")
  f_dsa <- file.path(paths$dsa_root, "paper", "paper_scalars_reference.rds")
  if (!file.exists(f_ord) || !file.exists(f_dsa)) return(dsa_check("V30", "reference report scalars = ordinary report", NA, "files missing"))
  a <- readRDS(f_dsa); b <- readRDS(f_ord)
  nm <- intersect(names(a), names(b))
  nm <- nm[vapply(nm, function(n) is.numeric(a[[n]]) && is.numeric(b[[n]]) && length(a[[n]]) == length(b[[n]]), logical(1))]
  d <- vapply(nm, function(n) max(abs(a[[n]] - b[[n]]) / pmax(abs(b[[n]]), 1)), numeric(1))
  dsa_check("V30", "DSA reference paper scalars = ordinary output/paper/paper_scalars.rds",
            all(d < 1e-8), paste0(length(nm), " numeric scalars; max relative difference ", sprintf("%.2e", max(d)),
                                  if (any(d >= 1e-8)) paste0("; differing: ", paste(nm[d >= 1e-8], collapse = ", ")) else ""))
}

#...........................................................
# Rendering (report, then deck from the report's artefacts) ----
#...........................................................

dsa_find_pandoc <- function() {
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))
  cand <- c(Sys.getenv("RSTUDIO_PANDOC"),
            "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
            "C:/Program Files/RStudio/bin/quarto/bin/tools",
            "C:/Program Files/RStudio/resources/app/bin/pandoc",
            "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
            "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools")
  for (d in cand[nzchar(cand)]) {
    if (file.exists(file.path(d, "pandoc.exe")) || file.exists(file.path(d, "pandoc"))) {
      Sys.setenv(RSTUDIO_PANDOC = d)
      if (rmarkdown::pandoc_available()) return(invisible(TRUE))
    }
  }
  stop("pandoc not found; set RSTUDIO_PANDOC to the directory containing pandoc.")
}

dsa_latest_run <- function(paths) {
  f <- file.path(paths$dsa_root, "runs", "LATEST.txt")
  if (!file.exists(f)) stop("No DSA run found (", f, "). Run code/10_deterministic_dsa.R first.")
  trimws(readLines(f, warn = FALSE)[1])
}

# The repository path contains spaces ("OneDrive - UW"), which LaTeX cannot
# resolve, so both documents are compiled in a private copy inside R's temp
# directory (as 09's psa_render_documents()) and only the final HTML/PDF are
# copied to scenarios_dsa/. Nothing is written next to the source Rmds.
dsa_render_documents <- function(run_id = NULL, paths = dsa_paths(), which = c("report", "slides")) {
  if (is.null(run_id)) run_id <- dsa_latest_run(paths)
  run_dir <- dsa_run_dir(paths, run_id)
  mf <- file.path(run_dir, "run_manifest.rds")
  if (!file.exists(mf)) stop("Run manifest not found for ", run_id, ". Run the DSA first.")
  man <- readRDS(mf)
  if (!identical(man$status, "COMPLETE")) {
    stop("DSA run ", run_id, " has status ", man$status, "; refusing to render. See ", run_dir, "/validation/.")
  }
  dsa_find_pandoc()
  src <- paths$rmd_dir
  src_before <- list.files(src, all.files = TRUE, no.. = TRUE)
  work <- file.path(tempdir(), paste0("dsa_render_", run_id))
  unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  if (grepl(" ", work, fixed = TRUE)) stop("R's temp directory contains spaces; set TMPDIR to a path without spaces.")
  need <- c(if ("report" %in% which) "aim1_report_dsa.Rmd",
            if ("slides" %in% which) c("aim1_executive_slides_dsa.Rmd", "beamer_preamble.tex"))
  stopifnot(file.copy(file.path(src, need), work))
  dsa_mkdir(paths$scen_root, paths)
  out <- character()
  if ("report" %in% which) {
    rmarkdown::render(file.path(work, "aim1_report_dsa.Rmd"), output_dir = work, intermediates_dir = work,
                      knit_root_dir = work,
                      params = list(run_dir = run_dir, repo_root = paths$root, render_token = DSA_RENDER_TOKEN),
                      envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
    f <- file.path(paths$scen_root, paste0("aim1_report_dsa_", run_id, ".html"))
    dsa_assert_write(f, paths)
    stopifnot(file.copy(file.path(work, "aim1_report_dsa.html"), f, overwrite = TRUE))
    out <- c(out, f)
    dsa_log("Rendered %s", f)
  }
  if ("slides" %in% which) {
    # PILOT banner on every slide (thin strip above the frame title), written
    # into the private render folder only.
    banner <- if (identical(man$mode, "pilot")) {
      c("\\usepackage{eso-pic}",
        sprintf(paste0("\\AddToShipoutPictureFG{\\AtPageUpperLeft{\\makebox[\\paperwidth][r]{",
                       "\\raisebox{-\\height}{\\setlength{\\fboxsep}{1.2pt}\\colorbox{red!85!black}{",
                       "\\textcolor{white}{\\bfseries\\scriptsize\\ PILOT --- %d countr%s; not global results\\ }}}}}}"),
                man$n_countries, if (man$n_countries == 1) "y" else "ies"))
    } else character()
    writeLines(c("% Generated by 10_deterministic_dsa.R", banner), file.path(work, "dsa_deck_header.tex"))
    rmarkdown::render(file.path(work, "aim1_executive_slides_dsa.Rmd"), output_dir = work,
                      intermediates_dir = work, knit_root_dir = work,
                      params = list(slides_dir = file.path(paths$dsa_root, "slides"), run_id = run_id,
                                    render_token = DSA_RENDER_TOKEN),
                      output_options = list(includes = list(in_header = c("beamer_preamble.tex", "dsa_deck_header.tex"))),
                      envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
    f <- file.path(paths$scen_root, paste0("aim1_executive_slides_dsa_", run_id, ".pdf"))
    dsa_assert_write(f, paths)
    stopifnot(file.copy(file.path(work, "aim1_executive_slides_dsa.pdf"), f, overwrite = TRUE))
    out <- c(out, f)
    dsa_log("Rendered %s", f)
  }
  src_after <- list.files(src, all.files = TRUE, no.. = TRUE)
  if (!setequal(src_before, src_after)) {
    stop("Rendering left files in scenarios/scenarios_aim1/: ", paste(setdiff(src_after, src_before), collapse = ", "))
  }
  invisible(out)
}

#...........................................................
# Main ----
#...........................................................

dsa_main <- function(cfg = dsa_config()) {
  paths <- dsa_paths()
  if (identical(cfg$render, "only")) return(invisible(dsa_render_documents(cfg$run_id, paths)))
  t_start <- Sys.time()
  run_dir <- dsa_run_dir(paths, cfg$run_id)
  dsa_mkdir(run_dir, paths)
  .dsa_log_file$path <- file.path(paths$dsa_root, "logs", cfg$run_id, "run.log")
  dsa_mkdir(dirname(.dsa_log_file$path), paths)
  on.exit(.dsa_log_file$path <- NULL, add = TRUE)
  dsa_log("DSA %s | run %s | mode %s | workers %d", DSA_VERSION, cfg$run_id, cfg$mode, cfg$workers)
  snap0 <- dsa_snapshot(paths)
  git0 <- dsa_git(paths, c("status", "--porcelain"))
  status_file <- file.path(run_dir, "run_manifest.rds")
  if (file.exists(status_file)) {
    old <- readRDS(status_file); old$status <- "SUPERSEDED_BY_RERUN"
    dsa_save_rds(old, status_file, paths)
  }
  sh <- dsa_prepare_shared(paths, cfg)
  man <- sh$manifest
  sel <- if (is.null(cfg$dsa_ids)) man else man[dsa_id %in% cfg$dsa_ids]
  dsa_save_rds(man, file.path(paths$dsa_root, "manifest", "dsa_scenario_manifest.rds"), paths)
  dsa_save_rds(DSA_INCOME_ADHERENCE, file.path(paths$dsa_root, "manifest", "dsa_income_adherence_map.rds"), paths)
  dsa_save_rds(sh$regimes, file.path(paths$dsa_root, "manifest", "dsa_trend_regimes.rds"), paths)
  dsa_fwrite(dsa_manifest_display(man), file.path(paths$scen_root, "dsa_scenario_manifest.csv"), paths)
  dsa_log("Countries: %d | DSA settings: %s", length(sh$locs), paste(sel$dsa_id, collapse = ", "))
  checks <- list()
  if (cfg$validate) checks$trend_inputs <- dsa_trend_input_checks(sh, paths, cfg)
  # Targets, statin configuration and trajectories use 06's own functions.
  fenv <- new.env(parent = globalenv())
  for (e in sh$p06$fun_exprs) eval(e, fenv)
  targets <- dsa_target_tables(sel, sh$tg_all, sh$locs, sh$meta, fenv)
  code_key <- dsa_code_key(paths, sh)
  clh <- new.env()   # cluster handle: dsa_run_tasks() may restart it with fewer workers
  clh$cl <- NULL; clh$n <- min(cfg$workers, length(sh$locs))
  on.exit(if (!is.null(clh$cl)) try(parallel::stopCluster(clh$cl), silent = TRUE), add = TRUE)
  on.exit(unlink(file.path(tempdir(), "dsa_task_inputs", cfg$run_id), recursive = TRUE), add = TRUE)
  pre_slim <- dsa_slim(sh$base05$b_pre)
  ref_slim <- NULL; rates_ref <- NULL
  statuses <- list(); statin_cfg <- list(); timing <- list(); trend_diag <- list()
  vlocs <- dsa_verify_countries(sh, cfg)
  for (r in unique(c("reference", sel$trend_regime))) {
    t0 <- Sys.time()
    # Each regime: 05's trend blocks on a fresh copy of the common pre-trend state.
    rates <- dsa_apply_trends(sh$base05$b_pre, dsa_regime_flags(sh$regimes, r, sh$r00$flags), paths, sh$p05)
    x_slim <- dsa_slim(rates)
    if (r == "reference") {
      ref_slim <- x_slim
      rates_ref <- rates[location %in% vlocs]   # kept only for the V20 live-06 check
    }
    if (cfg$validate) {
      checks[[paste0("trend_", r)]] <- dsa_trend_regime_checks(r, x_slim, pre_slim, ref_slim, sh, paths)
      if (r == "reference" || cfg$validate_05_regimes) {
        dsa_log("Regime %s: reproducing it with 05 evaluated in full ...", r)
        checks[[paste0("v05_", r)]] <- dsa_verify_05_regime(r, rates, sh, paths)
      }
    }
    trend_diag[[r]] <- dsa_trend_diag(pre_slim, x_slim, r)
    rm(x_slim)
    if (!r %in% sel$trend_regime) { rm(rates); gc(verbose = FALSE); next }
    rows <- sel[trend_regime == r]
    env06 <- dsa_06_setup(paths, sh$p06, rates, sh$base05)   # 06 clamps env06$b_rates in place
    rm(rates)
    locs06 <- dsa_06_locations(env06$data.in, env06$dt_hbp_targets)
    if (!setequal(locs06, sh$all_locs)) stop("06's location list differs from the one derived from 05's data.in.")
    if (r == "reference" && cfg$validate) {
      checks$globals <- dsa_globals_check(dsa_task_env(dsa_country_data(env06, sh$locs[1]), sh$p06$fun_exprs, 2030L))
    }
    tasks <- dsa_build_tasks(r, rows, env06, sh, paths, cfg, code_key)
    rm(env06); gc(verbose = FALSE)
    statin_cfg[[r]] <- rbindlist(lapply(tasks, `[[`, "statin"))
    tasks <- lapply(tasks, function(t) t[setdiff(names(t), "statin")])
    todo <- if (cfg$resume) Filter(Negate(dsa_task_done), tasks) else tasks
    dsa_log("Regime %s: %d DSA setting(s) x %d countr%s; %d task(s) to run, %d reused from checkpoints.",
            r, nrow(rows), length(tasks), if (length(tasks) == 1L) "y" else "ies", length(todo),
            length(tasks) - length(todo))
    if (length(todo) && cfg$workers > 1L && is.null(clh$cl)) {
      clh$cl <- dsa_start_cluster(min(clh$n, length(todo)), paths)
    }
    st <- if (length(todo)) dsa_run_tasks(todo, cfg, clh, paths) else list()
    # Idle workers keep their memory; stop them before the next regime's checks.
    if (!is.null(clh$cl)) { try(parallel::stopCluster(clh$cl), silent = TRUE); clh$cl <- NULL }
    rm(tasks, todo); gc(verbose = FALSE)
    statuses[[r]] <- if (length(st)) rbindlist(lapply(st, function(s) data.table(
      Country = s$Country, regime = s$regime, ok = isTRUE(s$ok), seconds = s$seconds,
      mem_mb = if (is.null(s$mem_mb)) NA_real_ else s$mem_mb,
      error = if (is.null(s$error)) NA_character_ else as.character(s$error)))) else data.table()
    unlink(file.path(tempdir(), "dsa_task_inputs", cfg$run_id, r), recursive = TRUE)
    timing[[r]] <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    nfail <- if (nrow(statuses[[r]])) sum(!statuses[[r]]$ok) else 0L
    dsa_log("Regime %s finished in %.1f min; failed tasks: %d; max worker memory %.0f MB.", r, timing[[r]], nfail,
            if (nrow(statuses[[r]])) max(statuses[[r]]$mem_mb, na.rm = TRUE) else NA_real_)
  }
  if (!is.null(clh$cl)) { parallel::stopCluster(clh$cl); clh$cl <- NULL }
  status_dt <- rbindlist(statuses, fill = TRUE)
  fail <- if (nrow(status_dt)) status_dt[ok == FALSE] else status_dt
  if (nrow(fail)) dsa_fwrite(fail, file.path(paths$dsa_root, "logs", cfg$run_id, "failures.csv"), paths)
  statin_cfg <- rbindlist(statin_cfg)
  trend_diag <- rbindlist(trend_diag)
  trajectories <- dsa_trajectories(sel, targets, statin_cfg, sh$pop40, fenv)
  if (cfg$validate) {
    checks$targets <- dsa_target_checks(targets, sh, trajectories, statin_cfg)
    checks$mapping <- dsa_mapping_checks(sh)
  }
  manifest_out <- list(run_id = cfg$run_id, dsa_version = DSA_VERSION, mode = cfg$mode,
                       started = format(t_start), countries = sh$locs, n_countries = length(sh$locs),
                       n_eligible_countries = length(sh$all_locs),
                       dsa_ids = sel$dsa_id, trend_regimes = unique(sel$trend_regime),
                       config = cfg[setdiff(names(cfg), "render")], timing_min = timing,
                       failures = if (nrow(fail)) fail else NULL,
                       script06 = basename(sh$p06$file), run_args = sh$run_args,
                       flags00 = sh$r00$flags, code_md5 = dsa_md5(dsa_code_files(paths, sh$p06$file)),
                       input_md5 = dsa_md5(dsa_input_files(paths)),
                       git_head = dsa_git(paths, c("rev-parse", "HEAD")), git_status_before = git0,
                       R = R.version.string,
                       packages = vapply(c("data.table", "dplyr", "readxl", "countrycode", "rmarkdown", "knitr"),
                                         function(p) as.character(utils::packageVersion(p)), ""))
  if (nrow(fail)) {
    manifest_out$status <- "FAILED"
    dsa_save_rds(manifest_out, status_file, paths)
    dsa_write_json(manifest_out, file.path(run_dir, "run_manifest.json"), paths)
    stop(nrow(fail), " country task(s) failed; see ", file.path(paths$dsa_root, "logs", cfg$run_id),
         ". No totals were produced.")
  }
  dsa_log("Finalising (combining checkpoints) ...")
  results <- dsa_finalize(sh, cfg, paths, statuses, trend_diag, targets, trajectories, statin_cfg, checks)
  if (cfg$validate && "reference" %in% sel$dsa_id) {
    dsa_log("Verifying against the live 06 functions and the ordinary outputs ...")
    checks$ordinary <- dsa_verify_ordinary(sh, cfg, paths, rates_ref)
  }
  rm(rates_ref); gc(verbose = FALSE)
  validation <- rbindlist(c(checks, list(results$run_checks)), fill = TRUE)
  dsa_save_rds(validation, file.path(run_dir, "validation", "validation.rds"), paths)
  dsa_fwrite(validation, file.path(run_dir, "validation", "validation.csv"), paths)
  n_fail <- validation[pass %in% FALSE, .N]
  snap1 <- dsa_snapshot(paths)
  iso <- dsa_snapshot_diff(snap0, snap1)
  manifest_out$validation <- validation[, .(n = .N, passed = sum(pass %in% TRUE), failed = sum(pass %in% FALSE),
                                            informational = sum(is.na(pass)))]
  manifest_out$isolation_changes_outside_dsa <- if (nrow(iso)) iso else "none"
  manifest_out$git_status_after <- dsa_git(paths, c("status", "--porcelain"))
  manifest_out$finished <- format(Sys.time())
  manifest_out$status <- if (n_fail == 0L) "COMPLETE" else "VALIDATION_FAILED"
  rf <- list.files(file.path(run_dir, "results"), full.names = TRUE)
  manifest_out$results <- data.table(file = basename(rf), md5 = unname(tools::md5sum(rf)))
  dsa_save_rds(manifest_out, status_file, paths)
  dsa_write_json(manifest_out, file.path(run_dir, "run_manifest.json"), paths)
  dsa_write_lines(cfg$run_id, file.path(paths$dsa_root, "runs", "LATEST.txt"), paths)
  dsa_log("Validation: %d checks, %d failed, %d informational. Status %s.", nrow(validation), n_fail,
          validation[is.na(pass), .N], manifest_out$status)
  if (n_fail) print(validation[pass %in% FALSE])
  if (nrow(iso)) dsa_log("WARNING: %d file(s) outside output_dsa/ and scenarios_dsa/ changed during the run.", nrow(iso))
  rm(results); gc(verbose = FALSE)
  if (isTRUE(cfg$render) && manifest_out$status == "COMPLETE") {
    dsa_render_documents(cfg$run_id, paths)
    if (cfg$mode == "full" && is.null(cfg$dsa_ids)) {
      v30 <- dsa_verify_report(paths)
      validation <- rbind(validation, v30, fill = TRUE)
      dsa_save_rds(validation, file.path(run_dir, "validation", "validation.rds"), paths)
      dsa_fwrite(validation, file.path(run_dir, "validation", "validation.csv"), paths)
      dsa_log("V30 %s: %s", if (isTRUE(v30$pass)) "PASS" else "FAIL/NA", v30$detail)
    }
  }
  dsa_log("Done in %.1f min.", as.numeric(difftime(Sys.time(), t_start, units = "mins")))
  invisible(manifest_out)
}

#...........................................................
# Execute ----
#...........................................................

.dsa_exec <- getOption("who_cvd.execute_10", NA)
if (isTRUE(.dsa_exec) || (is.na(.dsa_exec) && !interactive())) {
  dsa_main(dsa_parse_cli())
}
