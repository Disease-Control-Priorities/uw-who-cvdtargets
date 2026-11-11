# =============================================================================
# 092_psa_model.R -- Aim 1 PSA: inputs, 06 reference path, batched engine
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions only. Nothing here writes
# outside output_psa/: every write goes through psa_save_*()/psa_fwrite(), which
# refuse paths outside the PSA root.
#
# How the PSA uses the production pipeline (all read-only):
#   * 05_build_baseline.R is evaluated in a sandbox after a static scan confirms
#     it contains no write calls; it only READS the saved stage-031 output
#     (adjusted_searo_part*.rds: calibrated rates with coherent secular trends),
#     UNWPP population, COVID excess mortality and the BP inputs. Stages 02-04,
#     03 and 031 are never run.
#   * The Aim 1 06 script (psa_06_file(): the one 00_run_model.R sources) is
#     NEVER sourced with its execution guard on. Its helpers above the guard are
#     sourced with options(who_cvd.execute_06 = FALSE); the function definitions
#     inside the guard are parsed and evaluated without running the guard; the
#     guard's read-only setup statements (everything before `ncores <- ...`) are
#     evaluated in a throw-away environment after the same write scan. The
#     cluster, sink() and saveRDS() code is never evaluated.
#   * `fns` holds 06's function definitions and NO data objects, so a sampled
#     pathway can never fall through to a deterministic ETIHAD_RR_BIN or statin
#     global (such a lookup fails with "object not found").
#
# Statin RRs are local literals inside 06's calculate_statins_impact(). The PSA
# builds a parameterised copy of that function whose ONLY change is that the four
# `rr_* <- <number>` statements read `statin_rr[["rr_*"]]` instead; at the point
# values it is checked to be identical to 06's own function.
#
# Two implementations of the Aim 1 model:
#   1. psa_rehome_06() + psa_reference_run(): the REFERENCE path. 06's own
#      run_multiple_scenarios()/project.all()/... re-homed into a per-draw
#      environment holding that draw's Sheet1 table (ETIHAD_RR_BIN) and statin RR
#      vector. It executes 06's code verbatim.
#   2. psa_country_context() + psa_scenario_rates() + psa_transitions(): the
#      PRODUCTION path. Draw-invariant pieces are built once per country with
#      06's own functions; the BP incidence effect and every RR-dependent statin
#      quantity (including the baseline-coverage denominator) are computed per
#      draw for a batch; the state transitions keep 06's sequential year loop,
#      vectorised over strata x draws.
#   Validation (094) requires the production path to reproduce the reference
#   path draw by draw, and draw 0 to reproduce the saved 06 outputs exactly.

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
  if (grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", path)) stop("Output path must not contain '..': ", path)
  if (!psa_is_within(path, psa_root)) stop("Refusing to write outside output_psa/: ", path)
  invisible(path)
}

psa_out <- function(psa_root, ...) {
  p <- file.path(psa_root, ...)
  psa_assert_output_path(p, psa_root)
  p
}

# OneDrive can briefly lock files that are being synced; retry.
psa_with_retry <- function(expr_fun, tries = 8L, wait = 0.6) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({ expr_fun(); TRUE }, error = function(e) conditionMessage(e))
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i == tries) stop(ok, call. = FALSE)
    Sys.sleep(wait)
  }
}

# Atomic CSV write: temp file in the same directory, then rename.
psa_fwrite <- function(x, path, psa_root, ...) {
  psa_assert_output_path(path, psa_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  psa_assert_output_path(tmp, psa_root)
  psa_with_retry(function() data.table::fwrite(x, tmp, ...))
  psa_with_retry(function() {
    if (file.exists(path)) unlink(path)
    if (!file.rename(tmp, path)) stop("rename failed: ", tmp)
  })
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

# Empty call arguments (as in x[, 1]) are the empty symbol; skip them by index.
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
  setdiff(intersect(psa_called_functions(exprs), c(PSA_WRITE_FUNS, "cat(file=)")), allow)
}

#...........................................................
# Inputs: 05 (sandbox) and 06 (parsed, guard never executed) ----
#...........................................................

psa_load_baseline_05 <- function(paths) {
  f <- file.path(paths$wd_code, "05_build_baseline.R")
  exprs <- parse(f, keep.source = FALSE)
  bad <- psa_scan_writes(exprs, allow = "setwd")
  if (length(bad)) stop("05_build_baseline.R contains write calls: ", paste(bad, collapse = ", "))
  env <- new.env(parent = globalenv())
  env$wd_raw <- paths$wd_raw
  env$wd_data <- paths$wd_data
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  warns <- character()
  withCallingHandlers(
    suppressMessages(for (e in exprs) eval(e, env)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  setwd(owd)
  # repYear() uses only its argument; re-home it in baseenv() so it does not
  # carry the whole 05 sandbox to each worker when serialized.
  rep_year <- env$repYear
  environment(rep_year) <- baseenv()
  probe <- seq_len(34L * 224L)
  if (!identical(rep_year(probe), env$repYear(probe))) stop("repYear() depends on its 05 environment.")
  list(b_rates = env$b_rates, data.in = env$data.in, inc = env$inc,
       repYear = rep_year, warnings = unique(warns))
}

# The Aim 1 06 script: an explicit override (options(who_cvd.psa_script06)),
# otherwise the 06_run_scenarios_*.R script that 00_run_model.R sources.
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

PSA_06_NEEDED_FUNS <- c(
  "calculate_antihypertensive_split", "aim2_incremental_effect",
  "aim2_subgroup_incidence_multiplier", "aim2_control_trajectory",
  "validate_htn_target_table", "get.bp.prob", "calculate_baseline_incidence_gbd",
  "expand_to_single_year_ages", "calculate_aggregate_coverage",
  "calculate_statins_impact", "prepare_country_context", "project.all",
  "run_multiple_scenarios", "calculate_coverage_by_year", "get_gbd_relative_risks")

# Data objects that must never be visible to 06's functions in the PSA.
PSA_SENSITIVE_GLOBALS <- c("ETIHAD_RR", "ETIHAD_RR_BIN", "dt_gbd_rr", "b_rates",
                           "data.in", "inc", "dt_hbp_targets", "dt_statin_scenarios",
                           "dt_af_statins", "repYear")

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
  miss <- PSA_06_NEEDED_FUNS[!vapply(PSA_06_NEEDED_FUNS, exists, logical(1), envir = env, inherits = FALSE)]
  if (length(miss)) stop("06 no longer defines: ", paste(miss, collapse = ", "))
  if (any(vapply(PSA_SENSITIVE_GLOBALS, exists, logical(1), envir = env, inherits = FALSE))) {
    stop("The 06 function environment unexpectedly contains data objects.")
  }
  env
}

# Deterministic 06 environment: 06's guard setup statements evaluated read-only
# (after a write scan) on top of the 05 inputs.
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

# The executed run call (inside the %dopar% body).
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
            "adherence_ir", "adherence_cf")
  miss <- setdiff(need, names(args))
  if (length(miss)) stop("06 run call no longer passes: ", paste(miss, collapse = ", "))
  if (!"baseline_statin_coverage" %in% names(a)) args["baseline_statin_coverage"] <- list(NULL)
  args
}

# Location list exactly as 06 builds it.
psa_06_locations <- function(data_in, dt_hbp_targets) {
  locs <- intersect(unique(data_in$location), unique(dt_hbp_targets$location))
  locs[!locs %in% c("Greenland", "Bermuda")]
}

#...........................................................
# Statin settings from 06 (point RRs, AF defaults, parameterised copy) ----
#...........................................................

psa_06_statin_settings <- function(fns) {
  f <- fns$calculate_statins_impact
  b <- body(f)
  if (!is.call(b) || !identical(b[[1]], as.name("{"))) stop("calculate_statins_impact body is not a block.")
  points <- c(); af <- c()
  for (i in seq_along(b)[-1]) {
    e <- b[[i]]
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && is.numeric(e[[3]])) {
      nm <- as.character(e[[2]])
      if (nm %in% PSA_STATIN_IDS) {
        if (nm %in% names(points)) stop("06 assigns ", nm, " twice.")
        points[nm] <- e[[3]]
        b[[i]] <- call("<-", as.name(nm), call("[[", as.name("statin_rr"), nm))
      } else if (nm %in% c("af_ihd", "af_istroke")) {
        af[nm] <- e[[3]]
      }
    }
  }
  miss <- setdiff(PSA_STATIN_IDS, names(points))
  if (length(miss)) stop("06 calculate_statins_impact() no longer assigns: ", paste(miss, collapse = ", "))
  if (!setequal(names(af), c("af_ihd", "af_istroke"))) stop("06 statin AF defaults not found.")
  f2 <- f
  formals(f2) <- c(formals(f), list(statin_rr = as.name(".psa_statin_rr")))
  body(f2) <- b
  # prop_athero_stroke as passed by project.all().
  prop <- NULL
  psa_walk_calls(body(fns$project.all), function(e) {
    if (identical(e[[1]], as.name("calculate_statins_impact")) && "prop_athero_stroke" %in% names(e)) {
      prop <<- e[["prop_athero_stroke"]]
    }
  })
  if (!is.numeric(prop) || length(prop) != 1L) stop("project.all() no longer passes a literal prop_athero_stroke.")
  list(points = points[PSA_STATIN_IDS], af_default = c(ihd = unname(af["af_ihd"]),
                                                       istroke = unname(af["af_istroke"])),
       prop_athero = prop, fun = f2)
}

# All deterministic inputs the PSA needs, keyed by country where possible.
psa_prepare_inputs <- function(paths, countries = NULL, log = message, keep_setup_env_for = NULL) {
  t0 <- Sys.time()
  base05 <- psa_load_baseline_05(paths)
  log(sprintf("05 baseline loaded read-only (%.0f s; %d rate rows; years %d-%d).",
              as.numeric(difftime(Sys.time(), t0, units = "secs")), nrow(base05$b_rates),
              min(base05$b_rates$year), max(base05$b_rates$year)))
  parsed <- psa_parse_06(paths)
  env <- psa_06_setup_env(paths, base05, parsed)
  run_args <- psa_06_run_args(parsed)
  if (!identical(names(env$scenarios), PSA_SCENARIOS)) {
    stop("06 scenarios (", paste(names(env$scenarios), collapse = ", "), ") differ from PSA_SCENARIOS.")
  }
  if (!identical(names(env$htn_scenario_ids), PSA_SCENARIOS)) stop("06 htn_scenario_ids do not match its scenarios.")
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
         af      = env$dt_af_statins[location == Country])
  })
  out <- list(
    locs = locs, all_locs = all_locs, cin = cin, repYear = env$repYear, dt_gbd_rr = copy(env$dt_gbd_rr),
    scenarios = env$scenarios, htn_scenario_ids = env$htn_scenario_ids, run_args = run_args,
    det_etihad_rr_bin = copy(env$ETIHAD_RR_BIN),  # verification only
    warnings_05 = base05$warnings, fun_names_06 = parsed$fun_names)
  if (length(keep_setup_env_for)) {
    # Live 06 reference for validation: 06's own environment, slimmed to the
    # validation countries (its functions subset b_rates by location).
    env$b_rates <- env$b_rates[location %in% keep_setup_env_for]
    out$env06_setup <- env
  }
  rm(env, base05); gc(verbose = FALSE)
  out
}

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
# Reference path: 06's own functions re-homed per draw ----
#...........................................................

# Every 06 function is copied into a fresh environment whose data objects are
# this country's inputs and this draw's sampled tables; the parameterised
# calculate_statins_impact() reads `.psa_statin_rr` from it. No code is edited.
psa_rehome_06 <- function(fns, statin_fun, cin, repYear, dt_gbd_rr, sheet1_draw, statin_rr) {
  env <- new.env(parent = globalenv())
  for (nm in ls(fns, all.names = TRUE)) {
    f <- get(nm, envir = fns, inherits = FALSE)
    if (is.function(f)) environment(f) <- env
    assign(nm, f, envir = env)
  }
  sf <- statin_fun; environment(sf) <- env
  env$calculate_statins_impact <- sf
  env$.psa_statin_rr <- statin_rr
  env$b_rates <- cin$b_rates; env$data.in <- cin$data_in; env$inc <- cin$inc
  env$repYear <- repYear; env$dt_gbd_rr <- dt_gbd_rr
  env$ETIHAD_RR_BIN <- sheet1_draw
  env$dt_statin_scenarios <- cin$statin; env$dt_af_statins <- cin$af
  env
}

psa_reference_run <- function(Country, env, scenarios, htn_scenario_ids, targets, run_args) {
  sink(tempfile()); on.exit(sink(), add = TRUE)        # 06 prints progress; discard it
  res <- env$run_multiple_scenarios(
    Country = Country, scenario_list = scenarios, htn_scenario_ids = htn_scenario_ids,
    dt_hbp_targets = targets, statin_target_coverage = run_args$statin_target_coverage,
    statin_start_year = run_args$statin_start_year, statin_target_year = run_args$statin_target_year,
    adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
    baseline_statin_coverage = run_args$baseline_statin_coverage)
  res[, htn_target_scenario := "aim1"]
  res[]
}

#...........................................................
# Production path: draw-invariant country context ----
#...........................................................

PSA_SEXES  <- c("Female", "Male")
PSA_CAUSES <- c("hhd", "hstroke", "ihd", "istroke")     # 06 row order (alphabetical)
PSA_AGES   <- 20:95
PSA_YEARS  <- 2017:2050
# Within each (age, sex, location, cause, year) group, 06's split table holds the
# 8 bins in this row order (data.table merge sorts bp_cat in C locale), which
# fixes the order of the group sums reproduced below.
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

# Draw-invariant BP bin context for one target scenario (06's
# calculate_antihypertensive_split up to the effect-size lookup).
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

# Baseline statin coverage exactly as project.all() derives it.
psa_baseline_statin_cov <- function(Country, cin, run_args) {
  bsc <- if (!is.null(run_args$baseline_statin_coverage)) run_args$baseline_statin_coverage else
    cin$statin[location == Country & year == 2024, mean(statins_current, na.rm = TRUE)]
  max(min(bsc, 1), 0)
}

# Draw-invariant statin quantities (steps 3-5 of 06's calculate_statins_impact).
psa_statin_components <- function(Country, cin, run_args, sset, fns) {
  baseline_cov <- psa_baseline_statin_cov(Country, cin, run_args)
  if (is.na(baseline_cov)) baseline_cov <- 0
  baseline_cov <- max(min(baseline_cov, 1), 0)
  target <- run_args$statin_target_coverage
  incr_target <- max(target - baseline_cov, 0)
  delta <- fns$calculate_coverage_by_year(PSA_YEARS, start_year = run_args$statin_start_year,
                                          target_year = run_args$statin_target_year,
                                          target_coverage = incr_target)
  delta[PSA_YEARS < run_args$statin_start_year] <- 0
  delta[is.na(delta) | delta < 0] <- 0
  delta[delta > 1] <- 1
  af <- cin$af[location == Country]
  if (anyDuplicated(af$cause)) stop("af_statins duplicates a cause for ", Country)
  af_c <- setNames(af$af_statins[match(c("ihd", "istroke"), af$cause)], c("ihd", "istroke"))
  af_c[is.na(af_c)] <- sset$af_default[names(af_c)[is.na(af_c)]]
  list(cov0 = baseline_cov, delta = delta, af = af_c, adh_ir = run_args$adherence_ir,
       adh_cf = run_args$adherence_cf, prop = sset$prop_athero,
       zero = incr_target == 0 || baseline_cov >= target)
}

# Statin multipliers for a batch of RR vectors (rows of `rr`: draws; columns:
# PSA_STATIN_IDS). Same expressions and operation order as 06, so draw 0 is
# bit-identical. fac_*: applied to rates (ages >= 40, ihd/istroke);
# efac_*: 06's eff_ir/eff_cf tracking (all ages of ihd/istroke).
psa_statin_factors <- function(comp, rr, strata = psa_strata()) {
  B <- nrow(rr); S <- nrow(strata); Y <- length(PSA_YEARS)
  out <- lapply(setNames(nm = c("fac_ir", "fac_cf", "efac_ir", "efac_cf")),
                function(v) array(1, c(S, Y, B)))
  cur <- comp$cov0; delta <- comp$delta
  for (b in seq_len(B)) {
    r <- rr[b, ]
    es <- list(
      cf_ihd = (1 - r[["rr_cf_ihd"]]) * delta * comp$adh_cf /
        (1 - (1 - r[["rr_cf_ihd"]]) * (cur * comp$adh_cf)),
      cf_istroke = comp$prop * (1 - r[["rr_cf_istroke"]]) * delta * comp$adh_cf /
        (1 - (1 - r[["rr_cf_istroke"]]) * (cur * comp$adh_cf)),
      ir_ihd = comp$af[["ihd"]] * (1 - r[["rr_ir_ihd"]]) * (delta * comp$adh_ir) /
        (1 - (1 - r[["rr_ir_ihd"]]) * (cur * comp$adh_ir)),
      ir_istroke = comp$af[["istroke"]] * (1 - r[["rr_ir_istroke"]]) * delta * comp$adh_ir /
        (1 - (1 - r[["rr_ir_istroke"]]) * (cur * comp$adh_ir)))
    if (comp$zero) es <- lapply(es, function(x) x * 0)
    for (cz in c("ihd", "istroke")) {
      rows <- which(strata$cause == cz); rows40 <- which(strata$cause == cz & strata$age >= 40L)
      fir <- matrix(1 - es[[paste0("ir_", cz)]], length(rows), Y, byrow = TRUE)
      fcf <- matrix(1 - es[[paste0("cf_", cz)]], length(rows), Y, byrow = TRUE)
      out$efac_ir[rows, , b] <- fir
      out$efac_cf[rows, , b] <- fcf
      k <- match(rows40, rows)
      out$fac_ir[rows40, , b] <- fir[k, , drop = FALSE]
      out$fac_cf[rows40, , b] <- fcf[k, , drop = FALSE]
    }
  }
  out
}

# Statin multipliers from a run of a 06 statin function on a unit-rate table
# (IR = CF = eff = 1): because 06 applies `rate * (1 - effect_size)`, the result
# IS the multiplier.
psa_statin_unit_factors <- function(statin_fun, Country, br, cin, run_args, sset, statin_rr = NULL) {
  unit <- copy(br)
  unit[, `:=`(IR = 1, CF = 1, eff_ir = 1, eff_cf = 1, intervention = "unit")]
  args <- list(cin$statin, unit, Country, cin$af,
               adherence_ir = run_args$adherence_ir, adherence_cf = run_args$adherence_cf,
               prop_athero_stroke = sset$prop_athero,
               statin_target_coverage = run_args$statin_target_coverage,
               statin_start_year = run_args$statin_start_year,
               statin_target_year = run_args$statin_target_year,
               baseline_statin_coverage = psa_baseline_statin_cov(Country, cin, run_args))
  if (!is.null(statin_rr)) args$statin_rr <- statin_rr
  sink(tempfile()); on.exit(sink(), add = TRUE)
  st <- do.call(statin_fun, args)
  list(fac_ir = psa_to_matrix(st, "IR"), fac_cf = psa_to_matrix(st, "CF"),
       efac_ir = psa_to_matrix(st, "eff_ir"), efac_cf = psa_to_matrix(st, "eff_cf"))
}

psa_prepare_country_context <- function(Country, cin, fns, repYear, dt_gbd_rr) {
  env <- psa_rehome_06(fns, fns$calculate_statins_impact, cin, repYear, dt_gbd_rr, NULL, NULL)
  sink(tempfile()); on.exit(sink(), add = TRUE)
  env$prepare_country_context(Country, make_bp_baseline = TRUE)
}

psa_country_context <- function(Country, cin, fns, sset, repYear, dt_gbd_rr, run_args,
                                scenarios, htn_scenario_ids, sheet1_0) {
  cc <- psa_prepare_country_context(Country, cin, fns, repYear, dt_gbd_rr)
  br <- copy(cc$base_rates)
  if (nrow(br) != length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES) * length(PSA_YEARS) ||
      !setequal(unique(br$cause), PSA_CAUSES) || !setequal(unique(br$year), PSA_YEARS) ||
      !setequal(unique(br$age), PSA_AGES)) {
    stop("Base rates for ", Country, " are not the complete 2 x 4 x 76 x 34 grid.")
  }
  m <- lapply(setNames(c("IR", "CF", "BG.mx", "BG.mx.all", "covid.mx", "Nx", "PREVt0",
                         "DIS.mx.t0", "newcases"),
                       c("IR", "CF", "BG", "BGall", "covid", "Nx", "PREVt0", "DIS", "newcases")),
              function(v) psa_to_matrix(br, v))
  if (anyNA(unlist(m))) stop("Missing base-rate values for ", Country, ".")
  st <- psa_strata()
  init <- list(
    sick  = m$Nx * m$PREVt0, dead = m$Nx * m$DIS,
    well  = m$Nx * (1 - (m$PREVt0 + m$BG)), pop = m$Nx,
    allmx = m$Nx * m$DIS + m$Nx * m$BG, newcases = m$newcases)
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
    ref <- local({
      sink(tempfile()); on.exit(sink(), add = TRUE)
      fns$calculate_antihypertensive_split(
        intervention_rates = intervention_rates_bau, Country = Country, DT.in = cc$DT.in,
        dt_gbd_rr = dt_gbd_rr, target_rows = tr, active_no_diabetes = kk$active_no_diabetes,
        active_diabetes = kk$active_diabetes, etihad_rr_table = sheet1_0,
        bp_baseline = cc$bp_baseline, diabetes_age_prepared = cc$diabetes_age)
    })
    if (anyDuplicated(ref, by = c("age", "sex", "location", "cause", "year"))) {
      stop("06 BP split returned duplicate keys for ", Country, " (", kk$bp_key, ").")
    }
    ctx$eff_cf  <- psa_to_matrix(ref, "eff_cf")
    ctx$eff_ir0 <- psa_to_matrix(ref, "eff_ir")
    if (anyNA(ctx$eff_cf) || anyNA(ctx$eff_ir0)) stop("06 BP split left missing strata for ", Country)
    bp[[kk$bp_key]] <- ctx
  }
  # Statins: draw-invariant components, checked at the point values against 06's
  # own (unmodified) calculate_statins_impact().
  comp <- psa_statin_components(Country, cin, run_args, sset, fns)
  f0 <- psa_statin_factors(comp, matrix(sset$points, 1L, dimnames = list(NULL, PSA_STATIN_IDS)), st)
  u0 <- psa_statin_unit_factors(fns$calculate_statins_impact, Country, br, cin, run_args, sset)
  statin_check0 <- max(vapply(names(u0), function(v) max(abs(f0[[v]][, , 1L] - u0[[v]])), 0))
  if (!is.finite(statin_check0) || statin_check0 != 0) {
    stop("Batched statin factors at the point RRs differ from 06's calculate_statins_impact() for ",
         Country, " (max |diff| ", statin_check0, ").")
  }
  list(Country = Country, strata = st, S = nrow(st), Y = length(PSA_YEARS), base_rates = br,
       rates = m[c("IR", "CF", "BG", "BGall", "covid")], init = init,
       bp = bp, bp_keys = keys, statin = comp, statin_check0 = statin_check0)
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

psa_statin_matrix <- function(statin_block, draw_ids) {
  x <- statin_block[match(draw_ids, draw_id)]
  if (anyNA(x$draw_id)) stop("Statin draws missing for the batch.")
  m <- as.matrix(x[, ..PSA_STATIN_IDS])
  rownames(m) <- draw_ids
  m
}

psa_group_cause_idx <- function() {
  S <- length(PSA_SEXES) * length(PSA_CAUSES) * length(PSA_AGES)
  ci <- rep(rep(seq_along(PSA_CAUSES), each = length(PSA_AGES)), times = length(PSA_SEXES))
  rep(ci, times = length(PSA_YEARS))
}

# BP incidence effect ratio for a batch of draws: eff_ir [G x B], G = S x Y.
# Same equations and operation order as 06's split; the group sum over the 8 bins
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

# Scenario rates for a batch (06's BP combine, statins, 0.99 caps).
psa_scenario_rates <- function(ctx, interventions, htn_id, E, SF, fns) {
  S <- ctx$S; Y <- ctx$Y; B <- dim(E$EN)[3]
  bp_iv <- c("antihypertensive_no_diabetes", "antihypertensive_diabetes")
  has_bp <- any(bp_iv %in% interventions)
  has_st <- "statins" %in% interventions
  IR <- array(ctx$rates$IR, c(S, Y, B))
  CF <- ctx$rates$CF
  eff_ir <- array(1, c(S, Y, B)); eff_cf <- matrix(1, S, Y)
  diag <- list()
  if (has_bp) {
    key <- paste(htn_id, "antihypertensive_no_diabetes" %in% interventions,
                 "antihypertensive_diabetes" %in% interventions, sep = "|")
    bp <- ctx$bp[[key]]
    if (is.null(bp)) stop("No BP context for ", key)
    e_bp <- array(psa_bp_eff_ir_batch(bp, E, fns), c(S, Y, B))
    diag$bp_draw0_vs_06 <- if (isTRUE(ctx$draw0_col > 0))
      max(abs(e_bp[, , ctx$draw0_col] - bp$eff_ir0)) else NA_real_
    eff_ir <- e_bp
    eff_cf <- bp$eff_cf
    eff_cf[is.na(eff_cf)] <- 1
    eff_ir[is.na(eff_ir)] <- 1
    CF <- CF * eff_cf
    IR <- IR * eff_ir
  }
  if (has_st) {
    IR <- IR * SF$fac_ir
    CF <- array(CF, c(S, Y, B)) * SF$fac_cf
    eff_ir <- eff_ir * SF$efac_ir
    eff_cf <- array(eff_cf, c(S, Y, B)) * SF$efac_cf
  }
  # Transition validity handling (06): IR and CF capped at 0.99. Cells reaching
  # the cap are counted per draw so consequential clipping can be reported.
  cnt <- function(a) if (length(dim(a)) == 3L) colSums(matrix(a > 0.99, S * Y, B)) else rep(sum(a > 0.99), B)
  diag$n_ir_capped <- cnt(IR); diag$n_cf_capped <- cnt(CF)
  CF[CF > 0.99] <- 0.99
  IR[IR > 0.99] <- 0.99
  list(IR = IR, CF = CF, eff_ir = eff_ir, eff_cf = eff_cf, diag = diag)
}

#...........................................................
# Production path: state transitions (06 year loop), batched ----
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
  clamps <- matrix(0, B, 4L, dimnames = list(NULL, c("sick", "pop", "allmx", "well")))
  BG <- ctx$rates$BG; BGall <- ctx$rates$BGall; cov <- ctx$rates$covid
  for (y in 2:Y) {
    pw <- out$well[src, y - 1L, ]; ps <- out$sick[src, y - 1L, ]
    pp <- out$pop[src, y - 1L, ];  pa <- out$allmx[src, y - 1L, ]
    IRy <- IR[src, y, ]
    CFy <- if (cf3) CF[src, y, ] else CF[src, y]
    BGy <- BG[src, y]; covy <- cov[src, y]; BGally <- BGall[src, y]
    newc  <- pw * IRy
    sick2 <- ps * (1 - (CFy + BGy + covy)) + pw * IRy
    clamps[, "sick"] <- clamps[, "sick"] + colSums(matrix(sick2 < 0, length(src), B))
    sick2[sick2 < 0] <- 0
    dead2 <- ps * CFy
    dead2[dead2 < 0] <- 0
    pop2 <- pp - pa
    clamps[, "pop"] <- clamps[, "pop"] + colSums(matrix(pop2 < 0, length(src), B))
    pop2[pop2 < 0] <- 0
    # Sum of dead2 over the 4 causes for the same (sex, age), broadcast back to
    # every cause row; 06's GForce gsum accumulates sequentially in cause order.
    d4 <- array(dead2, c(nA, nC, nS, B))
    ds <- d4[, 1L, , ] + d4[, 2L, , ] + d4[, 3L, , ] + d4[, 4L, , ]
    sumdead <- array(0, c(nA, nC, nS, B))
    for (ci in seq_len(nC)) sumdead[, ci, , ] <- ds
    allmx2 <- matrix(sumdead, length(src), B) + pop2 * BGally + pop2 * covy
    clamps[, "allmx"] <- clamps[, "allmx"] + colSums(matrix(allmx2 < 0, length(src), B))
    allmx2[allmx2 < 0] <- 0
    well2 <- pop2 - allmx2 - sick2
    clamps[, "well"] <- clamps[, "well"] + colSums(matrix(well2 < 0, length(src), B))
    well2[well2 < 0] <- 0
    out$newcases[dst, y, ] <- newc
    out$sick[dst, y, ]     <- sick2
    out$dead[dst, y, ]     <- dead2
    out$well[dst, y, ]     <- well2
    out$pop[dst, y, ]      <- pop2
    out$allmx[dst, y, ]    <- allmx2
  }
  out$clamps <- clamps
  out
}

# Full 06-schema model output for a batch (validation only; never saved).
psa_batch_to_long <- function(ctx, res, rates, scenario, intervention_label, draw_ids) {
  st <- ctx$strata
  S <- ctx$S; Y <- ctx$Y; B <- length(draw_ids)
  base <- data.table(s = rep(st$s, times = Y * B),
                     year = rep(rep(PSA_YEARS, each = S), times = B),
                     draw_id = rep(draw_ids, each = S * Y))
  base <- st[base, on = "s"]
  as3 <- function(a) if (length(dim(a)) == 3L) as.vector(a) else rep(as.vector(a), B)
  base[, `:=`(well = as.vector(res$well), sick = as.vector(res$sick),
              newcases = as.vector(res$newcases), dead = as.vector(res$dead),
              pop = as.vector(res$pop), all.mx = as.vector(res$allmx),
              eff_ir = as3(rates$eff_ir), eff_cf = as3(rates$eff_cf),
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
  if ("statins" %in% interventions) lab <- c(lab, "Statins")
  if (length(lab)) paste(lab, collapse = " + ") else "baseline"
}
