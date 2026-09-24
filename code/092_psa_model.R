# =============================================================================
# 092_psa_model.R -- Aim 1 PSA: inputs, PSA-local 06 model, batched engine
# =============================================================================
# Sourced by 09_uncertainty_psa.R (and tests/test_aim1_psa.R). Defines functions
# only. Nothing here writes outside output_psa/: every write goes through
# psa_save_rds()/psa_fwrite(), which refuse paths outside the PSA root.
#
# How the PSA uses the existing pipeline (all read-only):
#   * 05_build_baseline.R is evaluated in a sandbox environment after a static
#     scan confirms it contains no write calls; its setwd() is undone on exit.
#   * The Aim 1 06 script (psa_06_file(): the one 00_run_model.R sources, or
#     script06=; "06o:<line>" references below are to
#     06_run_scenarios_multiple_optimized.R at commit fee6d22) is NEVER sourced with its execution
#     guard on. Its pure helpers are sourced with options(who_cvd.execute_06 =
#     FALSE) (the documented test mode) and the function definitions inside the
#     guard are parsed and evaluated WITHOUT running the guard. The guard's
#     read-only setup statements (everything before `ncores <- 10`) are evaluated
#     in a throw-away environment, after the same write scan, to obtain exactly
#     the deterministic inputs 06 builds. The cluster, sink() and saveRDS() code
#     after that point is never evaluated.
#   * The model functions that PSA calls live in `fns` (an environment holding
#     06's function definitions and NO data objects), so a sampled pathway can
#     never fall through to a deterministic ETIHAD_RR/ETIHAD_RR_BIN global: such
#     a lookup errors with "object not found".
#
# Two implementations of the Aim 1 model are provided:
#   1. psa_project_all()/psa_run_multiple_scenarios(): the per-draw REFERENCE
#      path, a faithful copy of 06's project.all()/run_multiple_scenarios()
#      whose only change is that every sampled table is an explicit argument.
#   2. psa_country_context() + psa_run_scenario_batch(): the PRODUCTION path.
#      Draw-invariant pieces are built once per country (with 06's own
#      functions); the draw-dependent BP incidence effect is computed for a
#      batch of draws at once; the state transitions keep 06's sequential
#      year loop, vectorised over strata x draws only.
#   Validation (094) requires the production path to reproduce the reference
#   path draw by draw, and draw 0 to reproduce the saved 06 outputs.

#...........................................................
# Paths and write guards ----
#...........................................................

psa_find_repo_root <- function(start = getwd()) {
  env_root <- Sys.getenv("WHO_CVD_ROOT", "")
  if (nzchar(env_root)) return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  opt_root <- getOption("who_cvd.repo_root", NULL)
  if (!is.null(opt_root)) return(normalizePath(opt_root, winslash = "/", mustWork = TRUE))
  d <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "uw-who-cvdtargets.Rproj"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate the repository root (uw-who-cvdtargets.Rproj). ",
       "Run from inside the repository or set WHO_CVD_ROOT.")
}

psa_paths <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  list(root     = root,
       wd       = paste0(root, "/"),
       wd_code  = paste0(root, "/code/"),
       wd_raw   = paste0(root, "/data/raw/"),
       wd_data  = paste0(root, "/data/processed/"),
       wd_outp_original = paste0(root, "/output/"),        # read-only reference
       psa_root = paste0(root, "/output_psa"))
}

psa_norm <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") tolower(p) else p
}

psa_is_within <- function(path, dir) {
  p <- psa_norm(path); d <- sub("/+$", "", psa_norm(dir))
  startsWith(p, paste0(d, "/")) || identical(p, d)
}

psa_assert_output_path <- function(path, psa_root) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("Invalid output path.")
  if (grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", path)) {
    stop("Output path must not contain '..': ", path)
  }
  if (!psa_is_within(path, psa_root)) {
    stop("Refusing to write outside output_psa/: ", path)
  }
  invisible(path)
}

psa_out <- function(psa_root, ...) {
  p <- file.path(psa_root, ...)
  psa_assert_output_path(p, psa_root)
  p
}

# OneDrive can briefly lock files that are being synced; retry like the report.
psa_with_retry <- function(expr_fun, tries = 8L, wait = 0.6) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({ expr_fun(); TRUE }, error = function(e) conditionMessage(e))
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i == tries) stop(ok, call. = FALSE)
    Sys.sleep(wait)
  }
}

psa_save_rds <- function(object, path, psa_root) {
  psa_assert_output_path(path, psa_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  psa_with_retry(function() base::saveRDS(object, path))
  invisible(path)
}

psa_fwrite <- function(x, path, psa_root, ...) {
  psa_assert_output_path(path, psa_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  psa_with_retry(function() data.table::fwrite(x, path, ...))
  invisible(path)
}

psa_write_lines <- function(text, path, psa_root) {
  psa_assert_output_path(path, psa_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  psa_with_retry(function() writeLines(text, path, useBytes = TRUE))
  invisible(path)
}

#...........................................................
# Static write scan (preflight for any code the PSA evaluates) ----
#...........................................................

PSA_WRITE_FUNS <- c(
  "saveRDS", "save", "save.image", "fwrite", "write.csv", "write.csv2",
  "write.table", "writeLines", "writeBin", "writeChar", "write", "sink",
  "ggsave", "png", "pdf", "jpeg", "tiff", "bmp", "svg", "cairo_pdf",
  "file.create", "dir.create", "unlink", "file.remove", "file.rename",
  "file.copy", "file.append", "saveWorkbook", "write.xlsx", "write_xlsx",
  "writeData", "makeCluster", "registerDoParallel", "clusterExport",
  "download.file", "system", "system2", "shell", "Sys.setenv", "setwd"
)

# Empty call arguments (as in x[, 1]) are the empty symbol, which cannot be bound
# to a variable and evaluated; skip them by index.
psa_is_empty_arg <- function(e, i) is.symbol(e[[i]]) && identical(as.character(e[[i]]), "")

psa_walk_calls <- function(expr, visit) {
  walk <- function(e) {
    if (is.call(e)) {
      visit(e)
      n <- length(e)
      if (n > 1L) for (i in 2:n) if (!psa_is_empty_arg(e, i)) walk(e[[i]])
      if (!is.name(e[[1]])) walk(e[[1]])
    } else if (is.expression(e) || is.list(e)) {
      for (i in seq_along(e)) if (!psa_is_empty_arg(e, i)) walk(e[[i]])
    }
  }
  walk(expr)
}

psa_called_functions <- function(expr) {
  out <- character()
  psa_walk_calls(expr, function(e) {
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

psa_scan_writes <- function(exprs, allow = character()) {
  fns <- psa_called_functions(exprs)
  bad <- setdiff(intersect(fns, c(PSA_WRITE_FUNS, "cat(file=)")), allow)
  bad
}

#...........................................................
# Inputs: 05 (sandbox) and 06 (parsed, guard never executed) ----
#...........................................................

# 00_run_model.R flags consumed by 05.
PSA_05_FLAGS <- list(run_adjustment_model = TRUE, run_bgmx_trend = TRUE,
                     run_CF_trend = TRUE, run_CF_trend_80 = TRUE,
                     run_CF_trend_ihme = FALSE)

psa_load_baseline_05 <- function(paths, flags = PSA_05_FLAGS, log = NULL) {
  f <- file.path(paths$wd_code, "05_build_baseline.R")
  exprs <- parse(f, keep.source = FALSE)
  bad <- psa_scan_writes(exprs, allow = "setwd")
  if (length(bad)) stop("05_build_baseline.R contains write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$wd_raw <- paths$wd_raw
  env$wd_data <- paths$wd_data
  for (nm in names(flags)) assign(nm, flags[[nm]], envir = env)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  warns <- character()
  withCallingHandlers(
    suppressMessages(for (e in exprs) eval(e, env)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  setwd(owd)
  # repYear()'s body uses only its argument and base arithmetic, but as a closure
  # it would carry the whole 05 sandbox (every country's b_rates, ~1 GB when
  # serialized) to each worker. Re-home it in baseenv(); outputs are unchanged.
  rep_year <- env$repYear
  environment(rep_year) <- baseenv()
  probe <- seq_len(34L * 224L)
  if (!identical(rep_year(probe), env$repYear(probe))) stop("repYear() depends on its 05 environment.")
  list(b_rates = env$b_rates, data.in = env$data.in, inc = env$inc,
       repYear = rep_year, warnings = unique(warns))
}

# The Aim 1 06 script the PSA reproduces: an explicit override
# (options(who_cvd.psa_script06), set from script06= by 09), otherwise the
# 06_run_scenarios_*.R script that 00_run_model.R sources (comments ignored).
psa_06_file <- function(paths) {
  f <- getOption("who_cvd.psa_script06", "")
  if (nzchar(f)) {
    if (!grepl("^([A-Za-z]:)?[/\\\\]", f)) f <- file.path(paths$root, f)
  } else {
    code <- sub("#.*$", "", readLines(file.path(paths$wd_code, "00_run_model.R"), warn = FALSE))
    pat <- "06_run_scenarios_[A-Za-z0-9_]+[.]R"
    src <- code[grepl("source[[:space:]]*\\(", code) & grepl(pat, code)]
    hit <- unique(regmatches(src, regexpr(pat, src)))
    if (length(hit) != 1L) {
      stop("Could not identify the one 06 script sourced by 00_run_model.R (found: ",
           paste(hit, collapse = ", "), "); pass script06=.")
    }
    f <- file.path(paths$wd_code, hit)
  }
  if (!file.exists(f)) stop("06 script not found: ", f)
  normalizePath(f, winslash = "/")
}

psa_parse_06 <- function(paths) {
  exprs <- parse(psa_06_file(paths), keep.source = FALSE)
  is_guard <- function(e) is.call(e) && identical(e[[1]], as.name("if")) &&
    any(grepl("who_cvd.execute_06", deparse(e[[2]]), fixed = TRUE))
  gi <- which(vapply(exprs, is_guard, logical(1)))
  if (length(gi) != 1L) stop("Could not find the unique who_cvd.execute_06 guard in 06.")
  body <- as.list(exprs[[gi]][[3]])[-1]
  is_fun_def <- function(e) is.call(e) && (identical(e[[1]], as.name("<-")) ||
    identical(e[[1]], as.name("="))) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
  fun_idx <- which(vapply(body, is_fun_def, logical(1)))
  stop_at <- which(vapply(body, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("ncores"))
  }, logical(1)))
  if (length(stop_at) != 1L) stop("Could not find `ncores <- ...` (cluster start) in 06.")
  foreach_idx <- which(vapply(body, function(e) any(grepl("%dopar%", deparse(e), fixed = TRUE)),
                              logical(1)))
  list(exprs = exprs, guard_index = gi, body = body, fun_idx = fun_idx,
       fun_names = vapply(body[fun_idx], function(e) as.character(e[[2]]), ""),
       setup_idx = seq_len(stop_at - 1L), stop_at = stop_at, foreach_idx = foreach_idx)
}

# Environment holding ONLY 06's function definitions (no data objects).
psa_load_06_functions <- function(paths, parsed = psa_parse_06(paths)) {
  env <- new.env(parent = globalenv())
  old <- options(who_cvd.execute_06 = FALSE)
  on.exit(options(old), add = TRUE)
  pre <- parsed$exprs[seq_len(parsed$guard_index - 1L)]
  bad <- psa_scan_writes(pre)
  if (length(bad)) stop("06 helpers above the guard contain write calls: ", paste(bad, collapse = ", "))
  for (e in pre) eval(e, env)
  for (i in parsed$fun_idx) eval(parsed$body[[i]], env)
  need <- c("calculate_antihypertensive_split", "aim2_incremental_effect",
            "aim2_subgroup_incidence_multiplier", "aim2_control_trajectory",
            "validate_htn_target_table", "get.bp.prob", "calculate_baseline_incidence_gbd",
            "expand_to_single_year_ages", "calculate_aggregate_coverage",
            "calculate_etihad_cumulative_rr", "calculate_tfa_impact",
            "calculate_statins_impact", "prepare_country_context", "project.all",
            "run_multiple_scenarios", "calculate_coverage_by_year", "get_gbd_relative_risks")
  miss <- need[!vapply(need, exists, logical(1), envir = env, inherits = FALSE)]
  if (length(miss)) stop("06 no longer defines: ", paste(miss, collapse = ", "))
  data_objs <- c("ETIHAD_RR", "ETIHAD_RR_BIN", "dt_gbd_rr", "b_rates", "data.in",
                 "inc", "dt_hbp_targets", "dt_statin_scenarios", "dt_af_statins",
                 "dt_tfa_scenarios")
  if (any(vapply(data_objs, exists, logical(1), envir = env, inherits = FALSE))) {
    stop("The 06 function environment unexpectedly contains data objects.")
  }
  env
}

# Deterministic 06 environment: 06's guard setup statements evaluated read-only
# (after a write scan) on top of the 05 inputs. Used (i) to extract 06's own
# deterministic inputs for the PSA and (ii) as the live 06 reference in 094.
psa_06_setup_env <- function(paths, base05, parsed = psa_parse_06(paths)) {
  setup <- parsed$body[parsed$setup_idx]
  bad <- psa_scan_writes(setup)
  if (length(bad)) stop("06 setup statements contain write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$b_rates <- base05$b_rates
  env$data.in <- copy(base05$data.in)
  env$inc     <- copy(base05$inc)
  env$repYear <- base05$repYear
  env$wd_raw  <- paths$wd_raw
  env$wd_data <- paths$wd_data
  old <- options(who_cvd.execute_06 = FALSE)
  on.exit(options(old), add = TRUE)
  for (e in parsed$exprs[seq_len(parsed$guard_index - 1L)]) eval(e, env)
  for (e in setup) eval(e, env)
  env
}

# The executed run call (inside the %dopar% body) -- not the unused parameter
# block above it (review defect D12).
psa_06_run_args <- function(parsed) {
  if (length(parsed$foreach_idx) != 1L) stop("Could not find the unique %dopar% call in 06.")
  found <- NULL
  psa_walk_calls(parsed$body[[parsed$foreach_idx]], function(e) {
    if (identical(e[[1]], as.name("run_multiple_scenarios"))) found <<- e
  })
  if (is.null(found)) stop("run_multiple_scenarios() call not found in the %dopar% body.")
  a <- as.list(found)[-1]
  constant <- vapply(a, function(x) is.numeric(x) || is.character(x) || is.logical(x) ||
                       is.null(x), logical(1))
  args <- lapply(a[constant], function(x) x)
  args$non_constant <- vapply(a[!constant], function(x) paste(deparse(x), collapse = ""), "")
  need <- c("statin_target_coverage", "statin_start_year", "statin_target_year",
            "adherence_ir", "adherence_cf", "saltmet", "salteff", "saltyear1",
            "saltyear2", "tfa_target_tfa", "tfa_policy_start_year")
  miss <- setdiff(need, names(args))
  if (length(miss)) stop("06 run call no longer passes: ", paste(miss, collapse = ", "))
  if (!"baseline_statin_coverage" %in% names(a)) args["baseline_statin_coverage"] <- list(NULL)
  args
}

# Location list exactly as 06 builds it (06o:2078-2081).
psa_06_locations <- function(data_in, dt_hbp_targets) {
  model_locs  <- unique(data_in$location)
  target_locs <- unique(dt_hbp_targets$location)
  locs <- intersect(model_locs, target_locs)
  locs[!locs %in% c("Greenland", "Bermuda")]
}

# All deterministic inputs the PSA needs, keyed by country where possible.
psa_prepare_inputs <- function(paths, countries = NULL, log = message, keep_setup_env_for = NULL) {
  t0 <- Sys.time()
  base05 <- psa_load_baseline_05(paths)
  log(sprintf("05 baseline loaded (%.0f s; %d rate rows).",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), nrow(base05$b_rates)))
  parsed <- psa_parse_06(paths)
  env <- psa_06_setup_env(paths, base05, parsed)
  run_args <- psa_06_run_args(parsed)
  locs <- all_locs <- psa_06_locations(env$data.in, env$dt_hbp_targets)
  if (!is.null(countries)) {
    bad <- setdiff(countries, locs)
    if (length(bad)) stop("Requested countries are not eligible 06 locations: ", paste(bad, collapse = ", "))
    locs <- countries
  }
  b_rates <- env$b_rates[location %in% locs & year >= 2017]
  data_in <- env$data.in[location %in% locs]
  iso_by_loc <- unique(data_in[, .(location, iso3)])
  cin <- lapply(setNames(locs, locs), function(Country) {
    iso <- iso_by_loc[location == Country, iso3]
    list(Country = Country,
         b_rates = b_rates[location == Country],
         data_in = data_in[location == Country],
         inc     = env$inc[iso3 %in% iso],
         targets = env$dt_hbp_targets[location == Country],
         statin  = env$dt_statin_scenarios[location == Country],
         af      = env$dt_af_statins[location == Country],
         tfa     = env$dt_tfa_scenarios[location == Country])
  })
  out <- list(
    locs = locs, all_locs = all_locs, cin = cin, repYear = env$repYear, dt_gbd_rr = copy(env$dt_gbd_rr),
    dt_hbp_targets = copy(env$dt_hbp_targets),
    scenarios = env$scenarios, htn_scenario_ids = env$htn_scenario_ids,
    run_args = run_args,
    det_etihad_rr10 = copy(env$ETIHAD_RR),        # verification only
    det_etihad_rr_bin = copy(env$ETIHAD_RR_BIN),  # verification only
    warnings_05 = base05$warnings,
    fun_names_06 = parsed$fun_names
  )
  if (length(keep_setup_env_for)) {
    # Live 06 reference for validation: 06's own environment, slimmed to the
    # validation countries (its functions subset b_rates by location).
    env$b_rates <- env$b_rates[location %in% keep_setup_env_for]
    out$env06_setup <- env
  }
  rm(env, base05); gc(verbose = FALSE)
  out
}

# Guard: sensitive deterministic objects must not be visible to 06 functions.
PSA_SENSITIVE_GLOBALS <- c("ETIHAD_RR", "ETIHAD_RR_BIN", "dt_gbd_rr", "b_rates",
                           "data.in", "inc", "dt_hbp_targets", "dt_statin_scenarios",
                           "dt_af_statins", "dt_tfa_scenarios", "repYear")

psa_assert_no_sensitive_globals <- function(fns = NULL) {
  hit <- PSA_SENSITIVE_GLOBALS[vapply(PSA_SENSITIVE_GLOBALS, exists, logical(1),
                                      envir = globalenv(), inherits = FALSE)]
  if (!is.null(fns)) {
    hit <- c(hit, PSA_SENSITIVE_GLOBALS[vapply(PSA_SENSITIVE_GLOBALS, exists, logical(1),
                                              envir = fns, inherits = FALSE)])
  }
  if (length(hit)) stop("Deterministic objects visible to 06 functions: ", paste(unique(hit), collapse = ", "))
  invisible(TRUE)
}

#...........................................................
# PSA-local reference path (per draw; faithful copy of 06) ----
#...........................................................

# 06o:1312-1341 with explicit inputs instead of the b_rates/data.in/inc globals.
psa_prepare_country_context <- function(Country, cin, fns, repYear, dt_gbd_rr,
                                        make_bp_baseline = TRUE) {
  base_rates <- cin$b_rates[location == Country & year >= 2017]
  DT <- unique(cin$data_in[location == Country][, Year := 2017][,
    -c("Lower95", "Upper95")])
  DT.in <- as.data.table(left_join(
    DT[rep(seq_len(nrow(DT)), 34L)][, Year := repYear(.I)],
    cin$inc %>% select(-location),
    by = c("iso3", "Year")
  ))
  DT.in[, c("aroc", "aroc2", "p_change", "p_change2", "a_change",
            "a_change2", "ideal", "drugaroc") := 0]
  bp_baseline <- diabetes_age <- NULL
  if (make_bp_baseline) {
    baseline_rates <- copy(base_rates)
    baseline_rates[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]
    bp_prob <- fns$get.bp.prob(copy(DT.in), rx = 0, drugaroc = "baseline")
    bp_baseline <- fns$calculate_baseline_incidence_gbd(
      copy(bp_prob), baseline_rates, Country, dt_gbd_rr
    )
    diabetes_age <- fns$expand_to_single_year_ages(copy(DT.in))
    diabetes_age <- unique(diabetes_age[, .(
      location, Year, age, sex, bp_cat,
      diabetes_share_among_htn_assumed_age_specific = diabetes
    )])
    setnames(diabetes_age, "Year", "year")
  }
  list(base_rates = base_rates, DT.in = DT.in,
       bp_baseline = bp_baseline, diabetes_age = diabetes_age)
}

# 06o:786-948 with the two ETIHAD tables passed explicitly (06 reads the global
# ETIHAD_RR and, through calculate_etihad_cumulative_rr's default, ETIHAD_RR_BIN).
psa_calculate_sodium_impact_etihad <- function(intervention_rates, Country, DT.in,
                                               salteff, saltmet, saltyear1 = 2026,
                                               saltyear2 = 2050, dt_gbd_rr,
                                               bp_baseline = NULL, fns,
                                               etihad_rr10_table, etihad_rr_table) {
  if (is.null(bp_baseline)) {
    bp_prob_base <- fns$get.bp.prob(DT.in, rx = 0, drugaroc = "baseline")
    dt_baseline <- fns$calculate_baseline_incidence_gbd(
      copy(bp_prob_base), intervention_rates, Country, dt_gbd_rr)
  } else {
    dt_baseline <- copy(bp_baseline)
  }
  salt_info <- unique(DT.in[, .(age, sex, salt, raisedBP, Year, aroc)])
  setnames(salt_info, "Year", "year")
  expand_age <- function(x) {
    if (x == "85plus") return(85:95)
    bounds <- as.numeric(unlist(strsplit(x, "-")))
    seq(bounds[1], bounds[2])
  }
  dt_expanded <- salt_info[, .(age_single = expand_age(age)),
                           by = .(age, sex, salt, raisedBP, aroc, year)]
  dt_expanded <- dt_expanded[, .(age = age_single, sex, salt, raisedBP, aroc, year)]
  dt_baseline <- merge(dt_baseline, dt_expanded, by = c("age", "sex", "year"), all.x = TRUE)
  if (saltmet == "percent") {
    dt_baseline[, salt_target := salt * salteff]
  } else if (saltmet == "target") {
    dt_baseline[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    dt_baseline[, salt_target := pmax(0, salt - salteff)]
  }
  dt_baseline[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]
  dt_baseline[year >= saltyear1 & year <= saltyear2,
              salt_reduction := salt_target * (year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)]
  dt_baseline[year > saltyear2, salt_reduction := salt_target]
  dt_baseline[year < saltyear1, salt_reduction := 0]
  dt_baseline[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]
  dt_baseline[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]
  dt_baseline <- merge(dt_baseline, etihad_rr10_table, by = c("bp_cat", "cause"), all.x = TRUE)
  etihad_effects <- dt_baseline[, list(N = mean(pop)), by = list(location, year, age, sex, bp_cat, cause)]
  diabetes_prop <- fns$expand_to_single_year_ages(DT.in)
  diabetes_prop <- diabetes_prop[, c("location", "Year", "age", "sex", "bp_cat", "diabetes"), with = F]
  setnames(diabetes_prop, "Year", "year")
  etihad_effects <- merge(etihad_effects, diabetes_prop, all.x = T)
  etihad_effects[, etihad_effect := fns$calculate_etihad_cumulative_rr(
    bp_cat, cause, diabetes_weight = diabetes, etihad_rr_table = etihad_rr_table)]
  etihad_effects[, c("diabetes", "N") := NULL]
  dt_baseline <- merge(dt_baseline, etihad_effects,
                       by = c("location", "year", "age", "sex", "bp_cat", "cause"), all.x = TRUE)
  dt_baseline[, etihad_effect := (1 - rr_per_10mmhg)]
  dt_baseline[, etihad_effect_sodium := etihad_effect * 0.1 * sbp_reduction]
  dt_baseline[, IR_bin_new := IR_bin * (1 - etihad_effect_sodium)]
  dt_baseline[, IR_new := sum(IR_bin_new * prob), by = .(age, sex, location, cause, year)]
  dt_baseline[year < saltyear1, IR_new := IR]
  dt_baseline[, eff_ir := IR_new / IR]
  cf_etihad <- data.table(
    cause = c("ihd", "istroke", "hstroke", "hhd", "aod"),
    cf_reduction_per_control = c(0.24, 0.36, 0.76, 0.20, 0.047)
  )
  dt_baseline <- merge(dt_baseline, cf_etihad, by = "cause", all.x = TRUE)
  dt_baseline[, CF_new := CF]
  dt_baseline[, eff_cf := CF_new / CF]
  dt_final <- unique(dt_baseline[, .(
    age, sex, location, cause, year, IR = IR_new, CF = CF_new,
    BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, ALL.mx, eff_ir, eff_cf
  )])
  setorder(dt_final, year, sex, location, cause, age)
  dt_final
}

# 06o:1343-1677 with every sampled table an explicit argument.
psa_project_all <- function(Country, interventions, htn_scenario_id, dt_hbp_targets,
                            statin_target_coverage, statin_start_year, statin_target_year,
                            adherence_ir, adherence_cf, saltmet = "percent", salteff = 0.3,
                            saltyear1 = 2026, saltyear2 = 2030, tfa_target_tfa = 0,
                            tfa_policy_start_year = 2027, baseline_statin_coverage = NULL,
                            country_context, cin, fns, dt_gbd_rr,
                            etihad_rr_table, etihad_rr10_table) {
  valid_interventions <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes",
                           "sodium", "tfa", "statins")
  if (!all(interventions %in% valid_interventions)) stop("Invalid intervention(s).")
  base_rates <- country_context$base_rates
  DT.in <- country_context$DT.in
  bp_interventions <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
  has_bp <- any(bp_interventions %in% interventions)
  htn_target_rows <- NULL
  if (has_bp) {
    htn_target_rows <- dt_hbp_targets[location == Country & scenario_id == htn_scenario_id]
    if (!nrow(htn_target_rows)) stop("No Aim 2 target rows for ", Country, " / ", htn_scenario_id, ".")
    fns$validate_htn_target_table(htn_target_rows)
  }
  if (!is.null(baseline_statin_coverage)) {
    baseline_statin_cov <- baseline_statin_coverage
  } else {
    baseline_statin_cov <- cin$statin[location == Country & year == 2024,
                                      mean(statins_current, na.rm = TRUE)]
  }
  baseline_statin_cov <- max(min(baseline_statin_cov, 1), 0)
  intervention_rates <- copy(base_rates)
  intervention_rates[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]
  intervention_rates_bau <- copy(intervention_rates)
  applied_interventions <- character()
  intervention_label <- "baseline"
  intervention_effects <- list()
  if (has_bp) {
    intervention_rates_drug <- fns$calculate_antihypertensive_split(
      intervention_rates = intervention_rates_bau, Country = Country, DT.in = DT.in,
      dt_gbd_rr = dt_gbd_rr, target_rows = htn_target_rows,
      bp_baseline = country_context$bp_baseline,
      diabetes_age_prepared = country_context$diabetes_age,
      active_no_diabetes = "antihypertensive_no_diabetes" %in% interventions,
      active_diabetes = "antihypertensive_diabetes" %in% interventions,
      etihad_rr_table = etihad_rr_table)
    intervention_effects[["antihypertensive_split"]] <-
      intervention_rates_drug[, .(age, sex, location, cause, year,
                                  eff_ir_bp = eff_ir, eff_cf_bp = eff_cf)]
    if ("antihypertensive_no_diabetes" %in% interventions) applied_interventions <- c(applied_interventions, "BP")
    if ("antihypertensive_diabetes" %in% interventions) applied_interventions <- c(applied_interventions, "BP_diabetes")
  }
  if ("sodium" %in% interventions) {
    intervention_rates_sodium <- psa_calculate_sodium_impact_etihad(
      intervention_rates_bau, Country, copy(DT.in), salteff, saltmet,
      saltyear1, saltyear2, dt_gbd_rr, bp_baseline = country_context$bp_baseline,
      fns = fns, etihad_rr10_table = etihad_rr10_table, etihad_rr_table = etihad_rr_table)
    intervention_effects[["sodium"]] <-
      intervention_rates_sodium[, .(age, sex, location, cause, year,
                                    eff_ir_salt = eff_ir, eff_cf_salt = eff_cf)]
    applied_interventions <- c(applied_interventions, "Salt")
  }
  if (length(intervention_effects) > 0) {
    intervention_rates <- copy(intervention_rates_bau)
    for (int_name in names(intervention_effects)) {
      intervention_rates <- merge(intervention_rates, intervention_effects[[int_name]],
                                  by = c("age", "sex", "location", "cause", "year"), all.x = TRUE)
    }
    if (has_bp && "sodium" %in% interventions) {
      intervention_rates[, `:=`(eff_ir = eff_ir_bp * eff_ir_salt, eff_cf = eff_cf_bp * eff_cf_salt)]
    } else if (has_bp) {
      intervention_rates[, `:=`(eff_ir = eff_ir_bp, eff_cf = eff_cf_bp)]
    } else if ("sodium" %in% interventions) {
      intervention_rates[, `:=`(eff_ir = eff_ir_salt, eff_cf = eff_cf_salt)]
    }
    intervention_rates[is.na(eff_cf), eff_cf := 1]
    intervention_rates[is.na(eff_ir), eff_ir := 1]
    intervention_rates[, `:=`(CF = CF * eff_cf, IR = IR * eff_ir)]
    effect_cols <- grep("^eff_(ir|cf)_(bp|salt)$", names(intervention_rates), value = TRUE)
    intervention_rates[, (effect_cols) := NULL]
  }
  if ("tfa" %in% interventions) {
    intervention_rates <- fns$calculate_tfa_impact(
      dt_tfa_scenarios = cin$tfa, intervention_rates = intervention_rates, Country = Country,
      target_tfa = tfa_target_tfa, policy_start_year = tfa_policy_start_year)
    applied_interventions <- c(applied_interventions, "TFA")
  }
  if ("statins" %in% interventions) {
    intervention_rates <- fns$calculate_statins_impact(
      cin$statin, intervention_rates, Country, cin$af,
      adherence_ir = adherence_ir, adherence_cf = adherence_cf,
      prop_athero_stroke = 0.60, statin_target_coverage = statin_target_coverage,
      statin_start_year = statin_start_year, statin_target_year = statin_target_year,
      baseline_statin_coverage = baseline_statin_cov)
    applied_interventions <- c(applied_interventions, "Statins")
  }
  if (length(applied_interventions) > 0) intervention_label <- paste(applied_interventions, collapse = " + ")
  intervention_rates[, intervention := intervention_label]
  intervention_rates[year == 2017 | age == 20, `:=`(
    sick = Nx * PREVt0, dead = Nx * DIS.mx.t0, well = Nx * (1 - (PREVt0 + BG.mx)),
    pop = Nx, all.mx = Nx * DIS.mx.t0 + Nx * BG.mx)]
  intervention_rates[CF > 0.99, CF := 0.99]
  intervention_rates[IR > 0.99, IR := 0.99]
  setorder(intervention_rates, sex, location, cause, age)
  for (i in 1:41) {
    b2 <- intervention_rates[year <= 2017 + i & year >= 2017 + i - 1]
    b2[, age2 := age + 1]
    b2[, newcases2 := shift(well) * IR, by = .(sex, location, cause, age, intervention)]
    b2[, sick2 := shift(sick) * (1 - (CF + BG.mx + covid.mx)) + shift(well) * IR,
       by = .(sex, location, cause, age, intervention)]
    b2[sick2 < 0, sick2 := 0]
    b2[, dead2 := shift(sick) * CF, by = .(sex, location, cause, age, intervention)]
    b2[dead2 < 0, dead2 := 0]
    b2[, pop2 := shift(pop) - shift(all.mx), by = .(sex, location, cause, age, intervention)]
    b2[pop2 < 0, pop2 := 0]
    b2[, all.mx2 := sum(dead2), by = .(sex, location, year, age, intervention)]
    b2[, all.mx2 := all.mx2 + (pop2 * BG.mx.all) + (pop2 * covid.mx)]
    b2[all.mx2 < 0, all.mx2 := 0]
    b2[, well2 := pop2 - all.mx2 - sick2]
    b2[well2 < 0, well2 := 0]
    b2 <- b2[year == 2017 + i & age2 < 96,
             .(age2, newcases2, sick2, dead2, well2, pop2, all.mx2, sex, location, cause, intervention)]
    setnames(b2, "age2", "age")
    intervention_rates[year == 2017 + i & age > 20, `:=`(
      newcases = b2$newcases2, sick = b2$sick2, dead = b2$dead2,
      well = b2$well2, pop = b2$pop2, all.mx = b2$all.mx2)]
  }
  intervention_rates[, .(age, cause, sex, year, well, sick, newcases,
                         dead, pop, all.mx, intervention, location, eff_ir, eff_cf)]
}

# 06o:1739-1793 with the draw tables passed to every scenario.
psa_run_multiple_scenarios <- function(Country, scenario_list, htn_scenario_ids, dt_hbp_targets,
                                       run_args, cin, fns, repYear, dt_gbd_rr,
                                       etihad_rr_table, etihad_rr10_table,
                                       country_context = NULL) {
  if (is.null(country_context)) {
    country_context <- psa_prepare_country_context(
      Country, cin, fns, repYear, dt_gbd_rr,
      make_bp_baseline = any(vapply(scenario_list, function(s) any(c(
        "antihypertensive_no_diabetes", "antihypertensive_diabetes", "sodium") %in% s), logical(1))))
  }
  res <- lapply(setNames(names(scenario_list), names(scenario_list)), function(sn) {
    psa_project_all(
      Country = Country, interventions = scenario_list[[sn]],
      htn_scenario_id = htn_scenario_ids[[sn]], dt_hbp_targets = dt_hbp_targets,
      statin_target_coverage = run_args$statin_target_coverage,
      statin_start_year = run_args$statin_start_year,
      statin_target_year = run_args$statin_target_year,
      adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
      saltmet = run_args$saltmet, salteff = run_args$salteff,
      saltyear1 = run_args$saltyear1, saltyear2 = run_args$saltyear2,
      tfa_target_tfa = run_args$tfa_target_tfa,
      tfa_policy_start_year = run_args$tfa_policy_start_year,
      baseline_statin_coverage = run_args$baseline_statin_coverage,
      country_context = country_context, cin = cin, fns = fns, dt_gbd_rr = dt_gbd_rr,
      etihad_rr_table = etihad_rr_table, etihad_rr10_table = etihad_rr10_table)
  })
  out <- rbindlist(res, idcol = "scenario")
  out[, htn_target_scenario := "aim1"]
  out[]
}

#...........................................................
# Production path: draw-invariant country context ----
#...........................................................

PSA_SEXES  <- c("Female", "Male")
PSA_CAUSES <- c("hhd", "hstroke", "ihd", "istroke")     # 06 row order (alphabetical)
PSA_AGES   <- 20:95
PSA_YEARS  <- 2017:2050
# Within each (age, sex, location, cause, year) group, 06's split and sodium
# tables hold the 8 bins in this row order (data.table merge sorts bp_cat in C
# locale), which fixes the order of the long-double group sums reproduced below.
PSA_BIN_SUM_ORDER <- c("120-129", "130-139", "140-149", "150-159",
                       "160-169", "170-179", "180+", "<120")

psa_strata <- function() {
  st <- CJ(sex = PSA_SEXES, cause = PSA_CAUSES, age = PSA_AGES, sorted = FALSE)
  st[, s := .I]
  st[]
}

# Map a keyed table (age, sex, cause, year, value) to an [S x Y] matrix.
psa_to_matrix <- function(dt, value, strata = psa_strata(), fill = NA_real_) {
  m <- matrix(fill, nrow(strata), length(PSA_YEARS))
  si <- (match(dt$sex, PSA_SEXES) - 1L) * length(PSA_CAUSES) * length(PSA_AGES) +
        (match(dt$cause, PSA_CAUSES) - 1L) * length(PSA_AGES) + (dt$age - 19L)
  yi <- dt$year - 2016L
  ok <- !is.na(si) & yi >= 1L & yi <= length(PSA_YEARS)
  if (any(duplicated(cbind(si, yi)[ok, , drop = FALSE]))) stop("psa_to_matrix: duplicate keys for ", value)
  m[cbind(si[ok], yi[ok])] <- dt[[value]][ok]
  m
}

# Group x bin arrays from a bin-level table with exactly 8 bins per group.
psa_bin_arrays <- function(dt, cols) {
  S <- length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES)
  G <- S * length(PSA_YEARS)
  si <- (match(dt$sex, PSA_SEXES) - 1L) * length(PSA_CAUSES) * length(PSA_AGES) +
        (match(dt$cause, PSA_CAUSES) - 1L) * length(PSA_AGES) + (dt$age - 19L)
  g <- si + (dt$year - 2017L) * S
  j <- match(dt$bp_cat, PSA_BIN_SUM_ORDER)
  if (anyNA(g) || anyNA(j) || any(duplicated(cbind(g, j))) || length(g) != G * 8L) {
    stop("Bin table is not a complete, unique (group x 8 bins) grid.")
  }
  lapply(setNames(cols, cols), function(v) {
    a <- matrix(NA_real_, G, 8L)
    a[cbind(g, j)] <- dt[[v]]
    a
  })
}

# Draw-invariant BP bin context for one target scenario (copied from 06's
# calculate_antihypertensive_split up to the effect-size lookup, 06o:99-199).
psa_bp_bin_context <- function(Country, target_rows, active_no_diabetes, active_diabetes,
                               bp_baseline, diabetes_age, fns) {
  fns$validate_htn_target_table(target_rows)
  targets_wide <- dcast(target_rows,
    location + sex + baseline_year + scaleup_start_year + target_year ~ subgroup,
    value.var = c("baseline_control", "target_control", "diabetes_share_among_htn_assumed"))
  setnames(targets_wide,
           c("baseline_control_htn_no_diabetes", "baseline_control_htn_diabetes",
             "target_control_htn_no_diabetes", "target_control_htn_diabetes"),
           c("baseline_control_no_diabetes", "baseline_control_diabetes",
             "target_control_no_diabetes", "target_control_diabetes"))
  targets_wide[, diabetes_share_among_htn_assumed := diabetes_share_among_htn_assumed_htn_no_diabetes]
  targets_wide[, c("diabetes_share_among_htn_assumed_htn_no_diabetes",
                   "diabetes_share_among_htn_assumed_htn_diabetes") := NULL]
  if (!active_no_diabetes) targets_wide[, target_control_no_diabetes := baseline_control_no_diabetes]
  if (!active_diabetes) targets_wide[, target_control_diabetes := baseline_control_diabetes]
  dt <- copy(bp_baseline)
  dt <- merge(dt, diabetes_age, by = c("location", "year", "age", "sex", "bp_cat"), all.x = TRUE)
  dt <- merge(dt, targets_wide, by = c("location", "sex"), all.x = TRUE)
  if (anyNA(dt[, .(baseline_control_no_diabetes, baseline_control_diabetes,
                   target_control_no_diabetes, target_control_diabetes,
                   diabetes_share_among_htn_assumed_age_specific)])) {
    stop("Missing BP target or diabetes-share join in model strata for ", Country, ".")
  }
  dt[, `:=`(
    control_no_diabetes_t = fns$aim2_control_trajectory(
      year, baseline_control_no_diabetes, target_control_no_diabetes, baseline_year, target_year),
    control_diabetes_t = fns$aim2_control_trajectory(
      year, baseline_control_diabetes, target_control_diabetes, baseline_year, target_year))]
  arr <- psa_bin_arrays(dt, c("IR_bin", "prob", "IR",
                              "diabetes_share_among_htn_assumed_age_specific",
                              "baseline_control_no_diabetes", "baseline_control_diabetes",
                              "control_no_diabetes_t", "control_diabetes_t"))
  # Group-level quantities are identical across the 8 bin rows; keep one column.
  one <- function(a) {
    if (max(abs(a - a[, 1L]), na.rm = TRUE) > 0) stop("Group-level value varies across bins.")
    a[, 1L]
  }
  list(IRbin = arr$IR_bin, prob = arr$prob,
       w = arr$diabetes_share_among_htn_assumed_age_specific,
       IR = one(arr$IR),
       cN0 = one(arr$baseline_control_no_diabetes), cD0 = one(arr$baseline_control_diabetes),
       cNt = one(arr$control_no_diabetes_t), cDt = one(arr$control_diabetes_t),
       hyp_cols = which(PSA_BIN_SUM_ORDER %in% PSA_HYP_BINS))
}

# Draw-invariant sodium bin context (06o:797-864): sbp_reduction per bin.
psa_sodium_bin_context <- function(DT.in, bp_baseline, run_args, fns, etihad_rr_table0) {
  DT.in <- copy(DT.in)
  dt <- copy(bp_baseline)
  salt_info <- unique(DT.in[, .(age, sex, salt, raisedBP, Year, aroc)])
  setnames(salt_info, "Year", "year")
  expand_age <- function(x) {
    if (x == "85plus") return(85:95)
    bounds <- as.numeric(unlist(strsplit(x, "-")))
    seq(bounds[1], bounds[2])
  }
  dt_expanded <- salt_info[, .(age_single = expand_age(age)),
                           by = .(age, sex, salt, raisedBP, aroc, year)]
  dt_expanded <- dt_expanded[, .(age = age_single, sex, salt, raisedBP, aroc, year)]
  dt <- merge(dt, dt_expanded, by = c("age", "sex", "year"), all.x = TRUE)
  salteff <- run_args$salteff; saltmet <- run_args$saltmet
  saltyear1 <- run_args$saltyear1; saltyear2 <- run_args$saltyear2
  if (saltmet == "percent") {
    dt[, salt_target := salt * salteff]
  } else if (saltmet == "target") {
    dt[, salt_target := pmin(salt, salteff)]
  } else if (saltmet == "app") {
    dt[, salt_target := pmax(0, salt - salteff)]
  }
  dt[, salt_target := ifelse(salt - salt_target < 2, salt - 2, salt_target)]
  dt[year >= saltyear1 & year <= saltyear2,
     salt_reduction := salt_target * (year - saltyear1 + 1) / (saltyear2 - saltyear1 + 1)]
  dt[year > saltyear2, salt_reduction := salt_target]
  dt[year < saltyear1, salt_reduction := 0]
  dt[is.na(salt_reduction) | salt_reduction < 0, salt_reduction := 0]
  dt[, sbp_reduction := ((2.8 * raisedBP) + ((1 - raisedBP) * 1.0)) * salt_reduction]
  # 06 also runs calculate_etihad_cumulative_rr() on the draw's Sheet1 table and
  # then overwrites the result (06o:883, 892). Its only lasting effect is the key
  # check, which is draw-invariant; run it once so a missing key still fails.
  invisible(fns$calculate_etihad_cumulative_rr(dt$bp_cat, dt$cause, diabetes_weight = 0,
                                               etihad_rr_table = etihad_rr_table0))
  arr <- psa_bin_arrays(dt, c("IR_bin", "prob", "IR", "sbp_reduction", "CF"))
  year_g <- rep(PSA_YEARS, each = length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES))
  list(IRbin = arr$IR_bin, prob = arr$prob, sbp = arr$sbp_reduction,
       IR = arr$IR[, 1L], CF = arr$CF[, 1L], pre = year_g < saltyear1)
}

# Multiplicative factors from 06's own TFA/statin functions, obtained by running
# them on a unit-rate table (IR = CF = eff_ir = eff_cf = 1). Because 06 applies
# these as `rate * (1 - effect_size)`, rate x factor reproduces 06 exactly.
psa_unit_factors <- function(Country, base_rates, cin, run_args, fns) {
  unit <- copy(base_rates)
  unit[, `:=`(IR = 1, CF = 1, eff_ir = 1, eff_cf = 1, intervention = "unit")]
  tfa <- fns$calculate_tfa_impact(dt_tfa_scenarios = cin$tfa, intervention_rates = copy(unit),
                                  Country = Country, target_tfa = run_args$tfa_target_tfa,
                                  policy_start_year = run_args$tfa_policy_start_year)
  if (!is.null(run_args$baseline_statin_coverage)) {
    bsc <- run_args$baseline_statin_coverage
  } else {
    bsc <- cin$statin[location == Country & year == 2024, mean(statins_current, na.rm = TRUE)]
  }
  bsc <- max(min(bsc, 1), 0)
  st <- fns$calculate_statins_impact(
    cin$statin, copy(unit), Country, cin$af,
    adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
    prop_athero_stroke = 0.60, statin_target_coverage = run_args$statin_target_coverage,
    statin_start_year = run_args$statin_start_year,
    statin_target_year = run_args$statin_target_year, baseline_statin_coverage = bsc)
  list(tfa_cf = psa_to_matrix(tfa, "CF"), tfa_eff_cf = psa_to_matrix(tfa, "eff_cf"),
       st_ir = psa_to_matrix(st, "IR"), st_cf = psa_to_matrix(st, "CF"),
       st_eff_ir = psa_to_matrix(st, "eff_ir"), st_eff_cf = psa_to_matrix(st, "eff_cf"),
       baseline_statin_cov = bsc)
}

psa_bp_targets <- function(scenarios, htn_scenario_ids) {
  bp <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
  keys <- unique(rbindlist(lapply(names(scenarios), function(sn) {
    iv <- scenarios[[sn]]
    if (!any(bp %in% iv)) return(NULL)
    data.table(htn_id = htn_scenario_ids[[sn]],
               active_no_diabetes = "antihypertensive_no_diabetes" %in% iv,
               active_diabetes = "antihypertensive_diabetes" %in% iv)
  })))
  keys[, bp_key := paste(htn_id, active_no_diabetes, active_diabetes, sep = "|")]
  keys[]
}

# Diagnostic text for a table with duplicated keys: how many, which non-key
# columns differ within a duplicated key, and the first offending key.
psa_duplicate_report <- function(x, by) {
  d <- x[x[, .I[.N > 1L], by = by]$V1]
  if (!nrow(d)) return("no duplicated keys")
  other <- setdiff(names(d), by)
  vary <- other[vapply(other, function(v) {
    any(d[, .(n = uniqueN(.SD[[1L]])), by = by, .SDcols = v]$n > 1L)
  }, logical(1))]
  first <- d[1L, ..by]
  sprintf("%d rows in %d duplicated keys (of %d rows); columns differing within a key: %s; first key: %s",
          nrow(d), uniqueN(d, by = by), nrow(x),
          if (length(vary)) paste(vary, collapse = ", ") else "none (exact duplicate rows)",
          paste(names(first), unlist(lapply(first, as.character)), sep = "=", collapse = ", "))
}

psa_country_context <- function(Country, cin, fns, repYear, dt_gbd_rr, run_args,
                                scenarios, htn_scenario_ids, sheet1_0, rr10_0,
                                check_against_06 = TRUE) {
  cc <- psa_prepare_country_context(Country, cin, fns, repYear, dt_gbd_rr, make_bp_baseline = TRUE)
  br <- copy(cc$base_rates)
  if (nrow(br) != length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES) * length(PSA_YEARS) ||
      !setequal(unique(br$cause), PSA_CAUSES)) {
    stop("Base rates for ", Country, " are not the complete 2 x 4 x 76 x 34 grid.")
  }
  m <- lapply(setNames(c("IR", "CF", "BG.mx", "BG.mx.all", "covid.mx", "Nx", "PREVt0",
                         "DIS.mx.t0", "newcases"),
                       c("IR", "CF", "BG", "BGall", "covid", "Nx", "PREVt0", "DIS", "newcases")),
              function(v) psa_to_matrix(br, v))
  if (anyNA(unlist(m))) stop("Missing base-rate values for ", Country, ".")
  st <- psa_strata()
  init_mask <- matrix(FALSE, nrow(st), length(PSA_YEARS))
  init_mask[, 1L] <- TRUE
  init_mask[st$age == 20L, ] <- TRUE
  init <- list(
    sick  = m$Nx * m$PREVt0, dead = m$Nx * m$DIS,
    well  = m$Nx * (1 - (m$PREVt0 + m$BG)), pop = m$Nx,
    allmx = m$Nx * m$DIS + m$Nx * m$BG, newcases = m$newcases)
  # BP contexts (one per target scenario / active-subgroup combination).
  keys <- psa_bp_targets(scenarios, htn_scenario_ids)
  intervention_rates_bau <- copy(br)
  intervention_rates_bau[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]
  bp <- list()
  for (k in seq_len(nrow(keys))) {
    kk <- keys[k]
    tr <- cin$targets[location == Country & scenario_id == kk$htn_id]
    if (!nrow(tr)) stop("No BP target rows for ", Country, " / ", kk$htn_id)
    ctx <- psa_bp_bin_context(Country, tr, kk$active_no_diabetes, kk$active_diabetes,
                              cc$bp_baseline, cc$diabetes_age, fns)
    # 06's own split at the deterministic table: eff_cf is draw-invariant; eff_ir
    # is kept so every worker checks the batched computation against 06.
    ref <- fns$calculate_antihypertensive_split(
      intervention_rates = intervention_rates_bau, Country = Country, DT.in = cc$DT.in,
      dt_gbd_rr = dt_gbd_rr, target_rows = tr, active_no_diabetes = kk$active_no_diabetes,
      active_diabetes = kk$active_diabetes, etihad_rr_table = sheet1_0,
      bp_baseline = cc$bp_baseline, diabetes_age_prepared = cc$diabetes_age)
    if (anyDuplicated(ref, by = c("age", "sex", "location", "cause", "year"))) {
      stop("06 BP split returned duplicate keys for ", Country, " (", kk$bp_key, "): ",
           psa_duplicate_report(ref, c("age", "sex", "location", "cause", "year")))
    }
    ctx$eff_cf  <- psa_to_matrix(ref, "eff_cf")
    ctx$eff_ir0 <- psa_to_matrix(ref, "eff_ir")
    if (anyNA(ctx$eff_cf) || anyNA(ctx$eff_ir0)) stop("06 BP split left missing strata for ", Country)
    bp[[kk$bp_key]] <- ctx
  }
  needs_sodium <- any(vapply(scenarios, function(s) "sodium" %in% s, logical(1)))
  sod <- NULL
  if (needs_sodium) {
    sod <- psa_sodium_bin_context(cc$DT.in, cc$bp_baseline, run_args, fns, sheet1_0)
    ref <- psa_calculate_sodium_impact_etihad(
      intervention_rates_bau, Country, copy(cc$DT.in), run_args$salteff, run_args$saltmet,
      run_args$saltyear1, run_args$saltyear2, dt_gbd_rr, bp_baseline = cc$bp_baseline,
      fns = fns, etihad_rr10_table = rr10_0, etihad_rr_table = sheet1_0)
    if (anyDuplicated(ref, by = c("age", "sex", "location", "cause", "year"))) {
      stop("Sodium pathway returned duplicate keys for ", Country, ": ",
           psa_duplicate_report(ref, c("age", "sex", "location", "cause", "year")))
    }
    sod$eff_cf  <- psa_to_matrix(ref, "eff_cf")
    sod$eff_ir0 <- psa_to_matrix(ref, "eff_ir")
  }
  fac <- psa_unit_factors(Country, br, cin, run_args, fns)
  list(Country = Country, strata = st, S = nrow(st), Y = length(PSA_YEARS),
       rates = m[c("IR", "CF", "BG", "BGall", "covid")], init = init, init_mask = init_mask,
       bp = bp, bp_keys = keys, sodium = sod, factors = fac,
       check_against_06 = check_against_06)
}

#...........................................................
# Production path: batched draw-dependent effects ----
#...........................................................

# Effect-size arrays [cause, bin, draw] from the Sheet1 draw table.
psa_effect_arrays <- function(sheet1_block, draw_ids) {
  x <- sheet1_block[draw_id %in% draw_ids & cause %in% PSA_CAUSES]
  ci <- match(x$cause, PSA_CAUSES); bi <- match(x$bp_cat, PSA_BIN_SUM_ORDER)
  di <- match(x$draw_id, draw_ids)
  EN <- ED <- array(NA_real_, c(length(PSA_CAUSES), 8L, length(draw_ids)))
  EN[cbind(ci, bi, di)] <- x$effect_size_nodiabetes
  ED[cbind(ci, bi, di)] <- x$effect_size_diabetes
  if (anyNA(EN) || anyNA(ED)) stop("Incomplete Sheet1 draw table for the batch.")
  list(EN = EN, ED = ED)
}

psa_rr10_array <- function(rr10_block, draw_ids) {
  x <- rr10_block[draw_id %in% draw_ids & !is.na(bp_cat) & cause %in% PSA_CAUSES]
  RR <- array(NA_real_, c(length(PSA_CAUSES), 8L, length(draw_ids)))
  RR[cbind(match(x$cause, PSA_CAUSES), match(x$bp_cat, PSA_BIN_SUM_ORDER),
           match(x$draw_id, draw_ids))] <- x$rr_per_10mmhg
  RR                                   # NA where 06's mapping finds no RR (defect D1)
}

psa_group_cause_idx <- function() {
  S <- length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES)
  ci <- rep(rep(seq_along(PSA_CAUSES), each = length(PSA_AGES)), times = length(PSA_SEXES))
  rep(ci, times = length(PSA_YEARS))
}

# BP incidence effect ratio for a batch of draws: eff_ir [G x B], G = S x Y.
# Same equations and operation order as 06o:186-224; the group sum over the 8 bins
# uses rowSums (long-double accumulation, as base sum()) in 06's bin row order.
psa_bp_eff_ir_batch <- function(bp, E, fns, max_chunk = 50L) {
  G <- length(bp$IR); B <- dim(E$EN)[3]
  cidx <- psa_group_cause_idx()
  out <- matrix(NA_real_, G, B)
  for (chunk in split(seq_len(B), ceiling(seq_len(B) / max_chunk))) {
    nb <- length(chunk)
    terms <- array(0, c(G, nb, 8L))
    for (j in seq_len(8L)) {
      if (j %in% bp$hyp_cols) {
        eN <- matrix(E$EN[cidx, j, chunk], G, nb)
        eD <- matrix(E$ED[cidx, j, chunk], G, nb)
        incN <- fns$aim2_incremental_effect(eN, bp$cNt, bp$cN0)
        incD <- fns$aim2_incremental_effect(eD, bp$cDt, bp$cD0)
        mult <- fns$aim2_subgroup_incidence_multiplier(incN, incD, bp$w[, j])
        terms[, , j] <- (bp$IRbin[, j] * mult) * bp$prob[, j]
      } else {
        terms[, , j] <- bp$IRbin[, j] * bp$prob[, j]      # multiplier is exactly 1
      }
    }
    IR_new <- rowSums(terms, dims = 2L)
    eff <- IR_new / bp$IR
    eff[bp$IR == 0 & IR_new == 0] <- 1
    out[, chunk] <- eff
  }
  out
}

# Sodium incidence effect ratio for a batch (06o:892-908); NA where 06 has NA.
psa_sodium_eff_ir_batch <- function(sod, RR, max_chunk = 50L) {
  G <- length(sod$IR); B <- dim(RR)[3]
  cidx <- psa_group_cause_idx()
  out <- matrix(NA_real_, G, B)
  for (chunk in split(seq_len(B), ceiling(seq_len(B) / max_chunk))) {
    nb <- length(chunk)
    terms <- array(0, c(G, nb, 8L))
    for (j in seq_len(8L)) {
      rr <- matrix(RR[cidx, j, chunk], G, nb)
      eff_sod <- (1 - rr) * 0.1 * sod$sbp[, j]
      terms[, , j] <- (sod$IRbin[, j] * (1 - eff_sod)) * sod$prob[, j]
    }
    IR_new <- rowSums(terms, dims = 2L)
    IR_new[sod$pre, ] <- sod$IR[sod$pre]
    out[, chunk] <- IR_new / sod$IR
  }
  out
}

# Scenario rates for a batch (06o:1432-1614 combine/TFA/statins/clamps).
psa_scenario_rates <- function(ctx, interventions, htn_id, E, RR, fns) {
  S <- ctx$S; Y <- ctx$Y; B <- dim(E$EN)[3]
  bp_iv <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
  has_bp <- any(bp_iv %in% interventions)
  has_salt <- "sodium" %in% interventions
  IR <- array(ctx$rates$IR, c(S, Y, B))
  CF <- ctx$rates$CF
  eff_ir <- array(1, c(S, Y, B)); eff_cf <- matrix(1, S, Y)
  diag <- list()
  if (has_bp || has_salt) {
    if (has_bp) {
      key <- paste(htn_id, "antihypertensive_no_diabetes" %in% interventions,
                   "antihypertensive_diabetes" %in% interventions, sep = "|")
      bp <- ctx$bp[[key]]
      if (is.null(bp)) stop("No BP context for ", key)
      e_bp <- array(psa_bp_eff_ir_batch(bp, E, fns), c(S, Y, B))
      diag$bp_draw0_vs_06 <- if (isTRUE(ctx$draw0_col > 0))
        max(abs(e_bp[, , ctx$draw0_col] - bp$eff_ir0)) else NA_real_
    }
    if (has_salt) {
      e_salt <- array(psa_sodium_eff_ir_batch(ctx$sodium, RR), c(S, Y, B))
      diag$salt_na_share_2026plus <- mean(is.na(e_salt[, PSA_YEARS >= 2026, ]))
      if (isTRUE(ctx$draw0_col > 0)) {
        a <- e_salt[, , ctx$draw0_col]; r <- ctx$sodium$eff_ir0
        diag$salt_draw0_vs_ref <- if (!identical(is.na(a), is.na(r))) Inf
                                  else max(c(0, abs(a - r)), na.rm = TRUE)
      }
    }
    if (has_bp && has_salt) {
      eff_ir <- e_bp * e_salt
      eff_cf <- bp$eff_cf * ctx$sodium$eff_cf
    } else if (has_bp) {
      eff_ir <- e_bp
      eff_cf <- bp$eff_cf
    } else {
      eff_ir <- e_salt
      eff_cf <- ctx$sodium$eff_cf
    }
    diag$eff_ir_reset_to_1 <- mean(is.na(eff_ir))
    eff_cf[is.na(eff_cf)] <- 1
    eff_ir[is.na(eff_ir)] <- 1
    CF <- CF * eff_cf
    IR <- IR * eff_ir
  }
  if ("tfa" %in% interventions) {
    CF <- CF * ctx$factors$tfa_cf
    eff_cf <- eff_cf * ctx$factors$tfa_eff_cf
  }
  if ("statins" %in% interventions) {
    IR <- IR * as.vector(ctx$factors$st_ir)
    CF <- CF * ctx$factors$st_cf
    eff_ir <- eff_ir * as.vector(ctx$factors$st_eff_ir)
    eff_cf <- eff_cf * ctx$factors$st_eff_cf
  }
  CF[CF > 0.99] <- 0.99
  IR[IR > 0.99] <- 0.99
  list(IR = IR, CF = CF, eff_ir = eff_ir, eff_cf = eff_cf, diag = diag)
}

#...........................................................
# Production path: state transitions (06o:1605-1666), batched ----
#...........................................................

psa_transitions <- function(ctx, IR, CF) {
  S <- ctx$S; Y <- ctx$Y; B <- dim(IR)[3]
  st <- ctx$strata
  src <- which(st$age <= 94L); dst <- src + 1L
  nA <- length(PSA_AGES) - 1L
  nS <- length(PSA_SEXES); nC <- length(PSA_CAUSES)
  cf3 <- length(dim(CF)) == 3L
  out <- lapply(setNames(nm = c("well", "sick", "dead", "pop", "allmx", "newcases")),
                function(v) {
                  a <- array(0, c(S, Y, B))
                  a[] <- ctx$init[[v]]                  # recycles the [S x Y] init over draws
                  a
                })
  BG <- ctx$rates$BG; BGall <- ctx$rates$BGall; cov <- ctx$rates$covid
  for (y in 2:Y) {
    pw <- out$well[src, y - 1L, ]; ps <- out$sick[src, y - 1L, ]
    pp <- out$pop[src, y - 1L, ];  pa <- out$allmx[src, y - 1L, ]
    IRy <- IR[src, y, ]
    CFy <- if (cf3) CF[src, y, ] else CF[src, y]
    BGy <- BG[src, y]; covy <- cov[src, y]; BGally <- BGall[src, y]
    newc  <- pw * IRy
    sick2 <- ps * (1 - (CFy + BGy + covy)) + pw * IRy
    sick2[sick2 < 0] <- 0
    dead2 <- ps * CFy
    dead2[dead2 < 0] <- 0
    pop2 <- pp - pa
    pop2[pop2 < 0] <- 0
    # Sum of dead2 over the 4 causes for the same (sex, age), broadcast back to
    # every cause row. 06's `sum(dead2)` by group is GForce gsum, which (data.table
    # 1.17) accumulates sequentially in double in cause row order, so do the same.
    d4 <- array(dead2, c(nA, nC, nS, B))
    ds <- d4[, 1L, , ] + d4[, 2L, , ] + d4[, 3L, , ] + d4[, 4L, , ]
    sumdead <- array(0, c(nA, nC, nS, B))
    for (ci in seq_len(nC)) sumdead[, ci, , ] <- ds
    allmx2 <- matrix(sumdead, length(src), B) + pop2 * BGally + pop2 * covy
    allmx2[allmx2 < 0] <- 0
    well2 <- pop2 - allmx2 - sick2
    well2[well2 < 0] <- 0
    out$newcases[dst, y, ] <- newc
    out$sick[dst, y, ]     <- sick2
    out$dead[dst, y, ]     <- dead2
    out$well[dst, y, ]     <- well2
    out$pop[dst, y, ]      <- pop2
    out$allmx[dst, y, ]    <- allmx2
  }
  out
}

# Full 06-schema model output for a batch (validation and model samples).
psa_batch_to_long <- function(ctx, res, rates, scenario, intervention_label, draw_ids) {
  st <- ctx$strata
  S <- ctx$S; Y <- ctx$Y; B <- length(draw_ids)
  base <- data.table(s = rep(st$s, times = Y * B),
                     year = rep(rep(PSA_YEARS, each = S), times = B),
                     draw_id = rep(draw_ids, each = S * Y))
  base <- st[base, on = "s"]
  effcf <- if (length(dim(rates$eff_cf)) == 3L) as.vector(rates$eff_cf) else rep(as.vector(rates$eff_cf), B)
  base[, `:=`(well = as.vector(res$well), sick = as.vector(res$sick),
              newcases = as.vector(res$newcases), dead = as.vector(res$dead),
              pop = as.vector(res$pop), all.mx = as.vector(res$allmx),
              eff_ir = as.vector(rates$eff_ir), eff_cf = effcf,
              scenario = scenario, intervention = intervention_label,
              location = ctx$Country, htn_target_scenario = "aim1")]
  base[, s := NULL]
  setcolorder(base, c("draw_id", "scenario", "age", "cause", "sex", "year", "well", "sick",
                      "newcases", "dead", "pop", "all.mx", "intervention", "location",
                      "eff_ir", "eff_cf", "htn_target_scenario"))
  base[]
}

psa_intervention_label <- function(interventions) {
  lab <- character()
  if ("antihypertensive_no_diabetes" %in% interventions) lab <- c(lab, "BP")
  if ("antihypertensive_diabetes" %in% interventions) lab <- c(lab, "BP_diabetes")
  if ("sodium" %in% interventions) lab <- c(lab, "Salt")
  if ("tfa" %in% interventions) lab <- c(lab, "TFA")
  if ("statins" %in% interventions) lab <- c(lab, "Statins")
  if (length(lab)) paste(lab, collapse = " + ") else "baseline"
}
