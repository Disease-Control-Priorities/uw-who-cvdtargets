# =============================================================================
# 09_uncertainty_psa.R -- Aim 1 probabilistic sensitivity analysis (PSA)
# =============================================================================
# Scope: BP INTERVENTION-EFFECT parameter uncertainty. The 15 BPLTTC 2021
# Appendix S6 hazard ratios (per 5 mmHg, IHD/Stroke/Heart failure x baseline SBP
# 130-139 ... >=170) are sampled on the log scale and propagated through the
# actual Aim 1 model (06), then through 07/08-equivalent calculations and the
# report estimands. Every other parameter is fixed (see the parameter register).
# The deterministic 06 result (draw 0) is the reported point estimate; the 95%
# parameter-uncertainty interval is the 2.5th-97.5th percentile of the matched
# Monte Carlo draws (draw_id >= 1) of each derived estimand.
#
# Read-only use of the existing pipeline: 05 is evaluated in a sandbox, 06 is
# never sourced with its execution guard on, 07/08 are never sourced. All
# outputs go to output_psa/ (writes outside it are refused by psa_save_rds()).
#
# Run from the repository root:
#   Rscript code/09_uncertainty_psa.R mode=smoke draws=10
#   Rscript code/09_uncertainty_psa.R mode=production draws=1000 workers=12
# Options (key=value): mode, draws, seed, workers, block, countries (comma
# separated), correlation (independent | comonotone_within_stratum |
# comonotone_all), validate, render, cache, min_final_draws, run_id, script06
# (the Aim 1 06 script; default: the one 00_run_model.R sources).
# From R: options(who_cvd.execute_09 = FALSE); source("code/09_uncertainty_psa.R")
#   psa_main(psa_config(mode = "smoke"))

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(tidyr); library(readxl)
  library(stringr); library(countrycode); library(parallel)
})

PSA_SCHEMA_VERSION <- "aim1-psa-1.0"
PSA_SAMPLE_MAX_DRAW <- 20L    # full 06-schema outputs saved for draws 0..20 of sample countries

psa_code_dir <- function() {
  f <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"), error = function(e) NULL)
  if (is.null(f)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) f <- normalizePath(sub("^--file=", "", a[1]), winslash = "/")
  }
  if (!is.null(f) && file.exists(f)) return(dirname(f))
  file.path(psa_find_repo_root_bootstrap(), "code")
}

psa_find_repo_root_bootstrap <- function() {
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "uw-who-cvdtargets.Rproj"))) return(d)
    if (identical(dirname(d), d)) stop("Run from inside the repository.")
    d <- dirname(d)
  }
}

PSA_HELPER_FILES <- c("091_psa_parameters.R", "092_psa_model.R",
                      "093_psa_downstream.R", "094_psa_validate.R")
local({
  cd <- psa_code_dir()
  for (f in PSA_HELPER_FILES) sys.source(file.path(cd, f), envir = globalenv())
})

#...........................................................
# Configuration ----
#...........................................................

psa_config <- function(mode = c("smoke", "production"), n_draws = NULL, seed = 20260923L,
                       n_workers = NULL, block_size = NULL, countries = NULL,
                       correlation_mode = "independent", validate = NULL, render = TRUE,
                       cache_contexts = TRUE, min_final_draws = 1000L, run_id = NULL,
                       validation_countries = "Colombia", sample_countries = "Colombia",
                       validate_07_detail = NULL, script06 = NULL) {
  mode <- match.arg(mode)
  if (is.null(n_draws)) n_draws <- if (mode == "smoke") 10L else 1000L
  # Physical cores: hyperthreads add memory pressure but little throughput here.
  if (is.null(n_workers)) n_workers <- max(1L, min(12L, parallel::detectCores(logical = FALSE)))
  if (is.null(block_size)) block_size <- if (mode == "smoke") n_draws + 1L else 100L
  if (is.null(validate)) validate <- TRUE
  if (is.null(validate_07_detail)) validate_07_detail <- mode == "smoke"
  list(mode = mode, n_draws = as.integer(n_draws), seed = as.integer(seed),
       n_workers = as.integer(n_workers), block_size = as.integer(block_size),
       countries = countries, correlation_mode = correlation_mode,
       validate = isTRUE(validate), render = isTRUE(render),
       cache_contexts = isTRUE(cache_contexts), min_final_draws = as.integer(min_final_draws),
       run_id = run_id, validation_countries = validation_countries,
       sample_countries = sample_countries, validate_07_detail = isTRUE(validate_07_detail),
       script06 = script06)
}

psa_parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(args[grepl("=", args)], "=", fixed = TRUE)
  a <- setNames(lapply(kv, `[`, 2), vapply(kv, `[`, "", 1))
  lg <- function(x) if (is.null(x)) NULL else tolower(x) %in% c("true", "1", "yes")
  num <- function(x) if (is.null(x)) NULL else as.integer(x)
  psa_config(mode = if (is.null(a$mode)) "smoke" else a$mode, n_draws = num(a$draws),
             seed = if (is.null(a$seed)) 20260923L else as.integer(a$seed),
             n_workers = num(a$workers), block_size = num(a$block),
             countries = if (is.null(a$countries)) NULL else strsplit(a$countries, ",", fixed = TRUE)[[1]],
             correlation_mode = if (is.null(a$correlation)) "independent" else a$correlation,
             validate = lg(a$validate), render = if (is.null(a$render)) TRUE else lg(a$render),
             cache_contexts = if (is.null(a$cache)) TRUE else lg(a$cache),
             min_final_draws = if (is.null(a$min_final_draws)) 1000L else as.integer(a$min_final_draws),
             run_id = a$run_id, script06 = a$script06)
}

psa_run_id <- function(cfg) {
  id <- sprintf("%s_d%d_s%d", cfg$mode, cfg$n_draws, cfg$seed)
  if (cfg$correlation_mode != "independent") id <- paste0(id, "_", cfg$correlation_mode)
  if (!is.null(cfg$countries)) id <- paste0(id, "_n", length(cfg$countries), "countries")
  id
}

#...........................................................
# Repository snapshot (path isolation) ----
#...........................................................

# Every file outside output_psa/, except git internals, other git worktrees
# (.claude/worktrees/: separate checkouts used by other sessions, gitignored) and
# IDE session state (.Rproj.user/, .Rhistory: gitignored, written by RStudio /
# interactive R). The PSA never writes to any of them (psa_assert_output_path()).
# Files locked by another process are recorded as unreadable, not skipped.
PSA_SNAPSHOT_EXCLUDE <- "^([.]git/|output_psa/|[.]Rproj[.]user/|[.]claude/worktrees/)"

psa_snapshot <- function(paths, label) {
  root <- paths$root
  files <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  files <- files[!grepl(PSA_SNAPSHOT_EXCLUDE, files) & !grepl("(^|/)[.]Rhistory$", files)]
  info <- file.info(file.path(root, files))
  md5 <- vapply(file.path(root, files), function(f) {
    tryCatch(unname(tools::md5sum(f)), error = function(e) NA_character_, warning = function(w) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  data.table(path = files, size = info$size, mtime = as.numeric(info$mtime), md5 = md5)[
    , `:=`(unreadable = is.na(md5), label = label)][]
}

psa_git <- function(paths, ...) {
  tryCatch(suppressWarnings(system2("git", c("-C", shQuote(paths$root), ...), stdout = TRUE, stderr = TRUE)),
           error = function(e) paste("git unavailable:", conditionMessage(e)))
}

psa_isolation_report <- function(before, after) {
  m <- merge(before[, .(path, md5_before = md5, size_before = size, in_before = TRUE)],
             after[, .(path, md5_after = md5, size_after = size, in_after = TRUE)], by = "path", all = TRUE)
  m[, status := fcase(is.na(in_before), "ADDED", is.na(in_after), "REMOVED",
                      is.na(md5_before) | is.na(md5_after), "UNREADABLE (locked; not verified)",
                      md5_before != md5_after, "MODIFIED", default = "unchanged")]
  m[status != "unchanged", .(path, status, md5_before, md5_after, size_before, size_after)]
}

psa_isolation_changes <- function(iso) iso[status %in% c("ADDED", "MODIFIED", "REMOVED")]

#...........................................................
# Worker set-up ----
#...........................................................

# Defined at top level on purpose: PSOCK serializes a function together with its
# enclosing environment, so a closure created inside psa_main() would ship that
# whole frame (every input, several GB) to each worker on every call.
psa_worker_init <- function(code_files, root, script06) {
  suppressPackageStartupMessages({
    library(dplyr); library(data.table); library(readxl); library(stringr); library(countrycode)
  })
  data.table::setDTthreads(1L)
  for (f in code_files) sys.source(f, envir = globalenv())
  options(who_cvd.psa_script06 = script06)          # the master's resolved 06 script
  assign(".psa_fns", psa_load_06_functions(psa_paths(root)), envir = globalenv())
  psa_assert_no_sensitive_globals(get(".psa_fns", envir = globalenv()))
  invisible(TRUE)
}

# Guarded delete: every path must lie inside output_psa/ (else nothing is removed).
psa_remove <- function(files, psa_root) {
  files <- files[file.exists(files)]
  for (f in files) psa_assert_output_path(f, psa_root)
  if (length(files)) unlink(files)
  invisible(files)
}

psa_worker_load <- function(wk) {
  wk$ctx_cache <- new.env()
  assign(".psa_worker", wk, envir = globalenv())
  invisible(TRUE)
}

#...........................................................
# Main ----
#...........................................................

psa_main <- function(cfg = psa_config()) {
  t_start <- Sys.time()
  invisible(gc(reset = TRUE, verbose = FALSE))
  paths <- psa_paths(psa_find_repo_root())
  psa_root <- paths$psa_root
  run_id <- if (is.null(cfg$run_id)) psa_run_id(cfg) else cfg$run_id
  run_dir <- psa_out(psa_root, "runs", run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  out <- function(...) psa_out(run_dir, ...)
  log_file <- out("logs", "run.log")
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  log <- function(fmt, ...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", sprintf(fmt, ...))
    cat(msg, "\n")
    cat(msg, "\n", file = log_file, append = TRUE)
  }
  timings <- list()
  tick <- function(name, t0) timings[[name]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (cfg$mode == "production" && cfg$n_draws < cfg$min_final_draws) {
    log("WARNING: production mode with %d < %d draws; documents will refuse to render.",
        cfg$n_draws, cfg$min_final_draws)
  }
  log("Aim 1 PSA %s | mode=%s draws=%d seed=%d workers=%d block=%d correlation=%s",
      run_id, cfg$mode, cfg$n_draws, cfg$seed, cfg$n_workers, cfg$block_size, cfg$correlation_mode)
  # The 06 script is resolved once and pinned for the whole run (master,
  # workers, validation), so an edit to 00_run_model.R mid-run cannot switch it.
  script06 <- local({
    old <- options(who_cvd.psa_script06 = if (is.null(cfg$script06)) "" else cfg$script06)
    on.exit(options(old))
    psa_06_file(paths)
  })
  old_s06 <- options(who_cvd.psa_script06 = script06)
  on.exit(options(old_s06), add = TRUE)
  script06_md5 <- unname(tools::md5sum(script06))
  script06_rel <- if (startsWith(script06, paste0(paths$root, "/"))) substring(script06, nchar(paths$root) + 2L) else script06
  log("06 script: %s (md5 %s).", script06_rel, script06_md5)

  #.. 0. Preflight ............................................................
  t0 <- Sys.time()
  psa_assert_no_sensitive_globals()
  snap_before <- psa_snapshot(paths, "before")
  psa_save_rds(snap_before, psa_out(psa_root, "isolation", paste0("snapshot_", run_id, "_before.rds")), psa_root)
  git_head <- psa_git(paths, "rev-parse", "HEAD")
  git_status_before <- psa_git(paths, "status", "--porcelain=v1", "--untracked-files=all")
  tick("preflight_snapshot", t0)

  #.. 1. Parameters and draws ................................................
  t0 <- Sys.time()
  s6 <- psa_read_appendix_s6(paths$wd_data)
  register <- psa_parameter_register(s6, psa_06_run_args(psa_parse_06(paths)))
  draws <- psa_generate_draws(s6, cfg$n_draws, cfg$seed, cfg$correlation_mode)
  sheet1 <- psa_build_sheet1_draws(draws, s6)
  sodium_csv <- fread(file.path(paths$wd_data, PSA_BP_SODIUM_CSV))
  rr10 <- psa_build_sodium_rr10_draws(draws, s6, sodium_csv)
  wb_sheet1 <- suppressMessages(psa_read_sheet1(paths$wd_data))
  sheet1_0 <- psa_sheet1_for_draw(sheet1, 0L)
  rr10_0 <- rr10[draw_id == 0L][, draw_id := NULL][]
  rc <- psa_check_sheet1_reconstruction(sheet1_0, wb_sheet1)
  if (!all(rc$pass)) stop("Sheet1 draw-0 reconstruction does not match the workbook.")
  psa_save_rds(s6, out("parameters", "appendix_s6_source.rds"), psa_root)
  psa_save_rds(register, out("parameters", "parameter_register.rds"), psa_root)
  psa_fwrite(register, out("parameters", "parameter_register.csv"), psa_root)
  psa_save_rds(draws, out("parameters", "draws_hr5.rds"), psa_root)
  psa_fwrite(draws, out("parameters", "draws_hr5.csv"), psa_root)
  psa_save_rds(sheet1, out("parameters", "sheet1_draws.rds"), psa_root)
  psa_save_rds(rr10, out("parameters", "sodium_rr10_draws.rds"), psa_root)
  psa_save_rds(rc, out("parameters", "sheet1_reconstruction_check.rds"), psa_root)
  log("Parameters: %d sampled / %d register rows; draws 0..%d; Sheet1 draw 0 max diff %.3g.",
      sum(register$status == "SAMPLED"), nrow(register), cfg$n_draws, max(rc$max_abs_diff))
  tick("parameters", t0)

  #.. 2. Deterministic inputs ................................................
  t0 <- Sys.time()
  keep_env <- if (cfg$validate) intersect(cfg$validation_countries,
                                          if (is.null(cfg$countries)) cfg$validation_countries else cfg$countries)
  inp <- psa_prepare_inputs(paths, countries = cfg$countries, log = function(m) log("%s", m),
                            keep_setup_env_for = keep_env)
  fns <- psa_load_06_functions(paths)
  psa_assert_no_sensitive_globals(fns)
  dsin <- psa_downstream_inputs(paths, inp$locs, inp$all_locs)
  saved_locs <- sub("^model_output_(.*)\\.rds$", "\\1",
                    list.files(file.path(paths$wd_outp_original, "out_model"), pattern = "^model_output_.*\\.rds$"))
  log("Inputs: %d eligible 06 locations (%d saved 06 outputs; set equal: %s); 06 run args: %s.",
      length(inp$locs), length(saved_locs), setequal(saved_locs, inp$locs),
      paste(names(inp$run_args)[names(inp$run_args) != "non_constant"],
            vapply(inp$run_args[names(inp$run_args) != "non_constant"],
                   function(x) if (is.null(x)) "NULL" else as.character(x), ""), sep = "=", collapse = ", "))
  shared <- list(repYear = inp$repYear, dt_gbd_rr = inp$dt_gbd_rr, run_args = inp$run_args,
                 scenarios = inp$scenarios, htn_scenario_ids = inp$htn_scenario_ids,
                 sheet1_0 = sheet1_0, rr10_0 = rr10_0, k08 = dsin$k08)
  tick("inputs", t0)

  #.. 3. Cluster (static country groups; inputs sent once per worker) ........
  # Created only when a draw block has to be computed (a resumed run that reuses
  # every checkpoint never starts workers).
  n_workers <- min(cfg$n_workers, length(inp$locs))
  groups <- split(inp$locs, rep_len(seq_len(n_workers), length(inp$locs)))
  cl <- NULL
  ensure_cluster <- function() {
    if (!is.null(cl)) return(invisible(cl))
    t0 <- Sys.time()
    cl <<- makeCluster(n_workers)
    t_mk <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    clusterCall(cl, psa_worker_init, file.path(paths$wd_code, PSA_HELPER_FILES), paths$root, script06)
    t_init <- as.numeric(difftime(Sys.time(), t0, units = "secs")) - t_mk
    for (g in seq_along(groups)) {
      wk <- list(countries = groups[[g]], cin = inp$cin[groups[[g]]], shared = shared,
                 dsc = dsin$per_country[groups[[g]]], who_levels = dsin$who_levels)
      clusterCall(cl[g], psa_worker_load, wk)
    }
    tick("cluster_init", t0)
    log("Cluster init: makeCluster %.1f s, worker setup %.1f s, input transfer %.1f s.",
        t_mk, t_init, timings$cluster_init - t_mk - t_init)
    log("Cluster: %d workers, %d-%d countries each.", n_workers, min(lengths(groups)), max(lengths(groups)))
    invisible(cl)
  }
  on.exit(if (!is.null(cl)) try(stopCluster(cl), silent = TRUE), add = TRUE)

  #.. 4. Draw blocks (checkpointed) ...........................................
  all_ids <- 0:cfg$n_draws
  blocks <- split(all_ids, ceiling(seq_along(all_ids) / cfg$block_size))
  cfg_hash <- digest_config(cfg, run_id, snap_before, extra = c(script06 = script06_md5))
  block_res <- vector("list", length(blocks)); samples <- list(); econ_detail <- NULL
  # Files left by an earlier configuration of this run_id (e.g. another block
  # size) must not be read as part of this run: drop blocks that no longer exist.
  block_of <- function(f) as.integer(sub("^.*block_?([0-9]+)[.]rds$", "\\1", basename(f)))
  old <- c(list.files(out("checkpoints"), pattern = "^block_[0-9]+[.]rds$", full.names = TRUE),
           list.files(out("model_samples"), pattern = "_block[0-9]+[.]rds$", full.names = TRUE))
  old <- old[block_of(old) > length(blocks)]
  if (length(old)) {
    psa_remove(old, psa_root)
    log("Removed %d checkpoint/sample file(s) of blocks beyond this configuration.", length(old))
  }
  t_blocks <- Sys.time()
  for (b in seq_along(blocks)) {
    ids <- blocks[[b]]
    ck <- out("checkpoints", sprintf("block_%03d.rds", b))
    if (file.exists(ck)) {
      prev <- readRDS(ck)
      # The draw-0 block must also carry the 08 cell detail used by validation
      # (checkpoints written without it are recomputed).
      if (identical(prev$cfg_hash, cfg_hash) && identical(prev$draw_ids, ids) &&
          (!(0L %in% ids) || length(prev$econ_detail) > 0L)) {
        block_res[[b]] <- prev
        if (length(prev$econ_detail)) econ_detail <- prev$econ_detail
        log("Block %d/%d (draws %d-%d): reused checkpoint.", b, length(blocks), min(ids), max(ids))
        next
      }
    }
    tb <- Sys.time()
    # A recomputed block rewrites its samples; stale ones (other draw ids) go first.
    psa_remove(list.files(out("model_samples"), pattern = sprintf("_block%03d[.]rds$", b), full.names = TRUE),
               psa_root)
    task <- list(draw_ids = ids, sheet1_block = sheet1[draw_id %in% ids],
                 rr10_block = rr10[draw_id %in% ids],
                 cache_contexts = cfg$cache_contexts && length(blocks) > 1L,
                 keep_long = cfg$sample_countries, sample_max_draw = PSA_SAMPLE_MAX_DRAW,
                 econ_detail = if (0L %in% ids) "ALL" else character())
    ensure_cluster()
    res <- clusterCall(cl, psa_worker_task, task)
    comb <- psa_combine_block(res, dsin$who_levels, ids, inp$locs)
    for (Country in names(comb$long)) {
      psa_save_rds(comb$long[[Country]],
                   out("model_samples", sprintf("model_output_%s_block%03d.rds", Country, b)), psa_root)
    }
    if (length(comb$econ_detail)) econ_detail <- comb$econ_detail   # draw 0 only; kept in checkpoint
    comb$long <- NULL
    comb$cfg_hash <- cfg_hash
    comb$elapsed_sec <- as.numeric(difftime(Sys.time(), tb, units = "secs"))
    psa_save_rds(comb, ck, psa_root)
    block_res[[b]] <- comb
    log("Block %d/%d (draws %d-%d): %.1f s; max worker memory %.0f MB.", b, length(blocks),
        min(ids), max(ids), comb$elapsed_sec, max(comb$worker_mem_mb))
  }
  tick("model_blocks", t_blocks)
  if (!is.null(cl)) { stopCluster(cl); cl <- NULL }

  #.. 5. Endpoints, summaries, convergence ...................................
  t0 <- Sys.time()
  bound <- psa_bind_blocks(block_res)
  meta <- list(locs = inp$locs, loc_region = dsin$loc_region, who_levels = dsin$who_levels,
               income_08 = dsin$income_08, k08 = dsin$k08)
  ep <- psa_build_endpoints(bound, meta)
  sm <- psa_summarise_endpoints(ep)
  bau <- psa_bau_series(block_res[[1]], meta)
  conv <- psa_convergence(ep)
  c1_draw0 <- psa_array_dt(bound$c1[, , match(0L, bound$draw_ids), , drop = FALSE],
                           list(scenario = PSA_SCENARIOS,
                                metric = c("dead_2025_2050", "dead_2026_2050", "yll", "yld", "daly"),
                                draw_id = 0L, location = inp$locs), "value")
  c1_draw0 <- dcast(c1_draw0, location + scenario ~ metric, value.var = "value")
  checks <- rbindlist(lapply(block_res, `[[`, "checks"), idcol = "block")
  timing <- rbindlist(lapply(block_res, `[[`, "timing"), idcol = "block")
  psa_save_rds(ep, out("endpoints", "endpoints_draws.rds"), psa_root)
  psa_save_rds(sm, out("endpoints", "endpoints_summary.rds"), psa_root)
  psa_save_rds(bau, out("endpoints", "bau_reference_series.rds"), psa_root)
  psa_save_rds(conv, out("endpoints", "convergence.rds"), psa_root)
  psa_save_rds(c1_draw0, out("endpoints", "country_totals_draw0.rds"), psa_root)
  psa_save_rds(dsin$country_meta, out("endpoints", "country_metadata_report.rds"), psa_root)
  psa_save_rds(dsin$reg08, out("endpoints", "country_metadata_08.rds"), psa_root)
  psa_save_rds(dsin$income_08, out("endpoints", "income_denominators_08.rds"), psa_root)
  psa_save_rds(checks, out("diagnostics", "model_checks.rds"), psa_root)
  psa_save_rds(timing, out("diagnostics", "country_timing.rds"), psa_root)
  tick("endpoints", t0)
  log("Endpoints: %d tables; convergence: %d/%d headline outcomes meet the MC criterion.",
      length(ep), sum(conv$final$converged), nrow(conv$final))

  #.. 6. Validation ...........................................................
  val <- NULL; val_pass <- NA
  if (cfg$validate) {
    t0 <- Sys.time()
    val <- psa_run_validation(cfg, paths, inp, fns, s6, sheet1, rr10, wb_sheet1, sheet1_0, rr10_0,
                              ep, sm, checks, econ_detail, c1_draw0, run_dir, log, dsin)
    psa_save_rds(val, out("validation", "validation_results.rds"), psa_root)
    psa_fwrite(val, out("validation", "validation_results.csv"), psa_root)
    hard <- val[!is.na(pass)]
    val_pass <- all(hard$pass)
    tick("validation", t0)
    log("Validation: %d/%d checks pass (%d informational).", sum(hard$pass), nrow(hard), sum(is.na(val$pass)))
  }

  #.. 7. Manifest ............................................................
  n_mc <- cfg$n_draws
  stale_refs <- if (!is.null(val) && "freshness" %in% val$group) val[group == "freshness" & is.na(pass), scope] else character()
  manifest <- list(
    schema_version = PSA_SCHEMA_VERSION, run_id = run_id, run_mode = cfg$mode,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), seed = cfg$seed,
    rng = attr(draws, "psa_meta")$rng, n_draws_total = length(all_ids), n_mc_draws = n_mc,
    draw_ids = range(all_ids), point_estimate = "draw_id 0 = deterministic 06 result",
    interval = "2.5th-97.5th percentiles (quantile type 7) of matched draws 1..N",
    parameter_scope = paste0("BP intervention-effect parameter uncertainty only: 15 BPLTTC 2021 ",
                             "Appendix S6 HR per 5 mmHg (IHD, Stroke, Heart failure x baseline SBP ",
                             "130-139, 140-149, 150-159, 160-169, >=170); all other inputs fixed."),
    correlation_mode = cfg$correlation_mode, ci_divisor = PSA_CI_DIVISOR,
    min_final_draws = cfg$min_final_draws,
    is_final = cfg$mode == "production" && n_mc >= cfg$min_final_draws &&
      all(conv$final$converged) && isTRUE(val_pass) && !length(stale_refs),
    convergence_all_met = all(conv$final$converged),
    validation_all_pass = val_pass,
    validation_stale_references = stale_refs,
    countries = inp$locs, n_countries = length(inp$locs), sample_countries = cfg$sample_countries,
    validation_countries = cfg$validation_countries, scenarios = PSA_SCENARIOS,
    script06 = script06_rel, script06_md5 = script06_md5,
    run_args_06 = inp$run_args, n_workers = n_workers, block_size = cfg$block_size,
    n_blocks = length(blocks), cache_contexts = cfg$cache_contexts,
    timings_sec = timings, total_runtime_sec = as.numeric(difftime(Sys.time(), t_start, units = "secs")),
    peak_worker_memory_mb = max(unlist(lapply(block_res, `[[`, "worker_mem_mb"))),
    peak_master_memory_mb = { m <- gc(verbose = FALSE); sum(m[, ncol(m)]) },
    git_head = git_head, git_status_before = git_status_before,
    r_version = R.version.string,
    packages = vapply(c("data.table", "dplyr", "readxl", "rmarkdown", "knitr", "ggplot2"),
                      function(p) as.character(utils::packageVersion(p)), ""),
    input_md5 = rbind(snap_before[path %in% c(PSA_CHECKPOINT_DEPS, "code/094_psa_validate.R",
                                              "code/09_uncertainty_psa.R"), .(path, md5)],
                      data.table(path = script06_rel, md5 = script06_md5)),
    files = list(parameters = "parameters/", endpoints = "endpoints/", checkpoints = "checkpoints/",
                 model_samples = "model_samples/", validation = "validation/", diagnostics = "diagnostics/"),
    warnings_05 = inp$warnings_05)
  psa_save_rds(manifest, out("manifest.rds"), psa_root)
  psa_write_lines(jsonlite::toJSON(manifest[setdiff(names(manifest), c("input_md5", "countries"))],
                                   auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
                  out("manifest.json"), psa_root)

  #.. 8. Path isolation ......................................................
  snap_after <- psa_snapshot(paths, "after")
  psa_save_rds(snap_after, psa_out(psa_root, "isolation", paste0("snapshot_", run_id, "_after.rds")), psa_root)
  iso <- psa_isolation_report(snap_before, snap_after)
  psa_fwrite(if (nrow(iso)) iso else data.table(status = "no changes outside output_psa/"),
             psa_out(psa_root, "isolation", paste0("isolation_report_", run_id, ".csv")), psa_root)
  manifest$isolation_changes_outside_output_psa <- nrow(psa_isolation_changes(iso))
  manifest$isolation_unverified_locked_files <- iso[grepl("UNREADABLE", status), path]
  psa_save_rds(manifest, out("manifest.rds"), psa_root)
  log("Isolation: %d files added/modified/removed outside output_psa/ (%d locked files not verified).",
      nrow(psa_isolation_changes(iso)), sum(grepl("UNREADABLE", iso$status)))
  if (nrow(psa_isolation_changes(iso))) {
    stop("Files outside output_psa/ changed during the PSA run; see the isolation report.")
  }
  if (isFALSE(val_pass)) {
    stop("Validation failed; documents are not rendered. See ", out("validation", "validation_results.csv"))
  }
  psa_write_lines(run_id, psa_out(psa_root, "runs", "LATEST.txt"), psa_root)

  #.. 9. Documents ...........................................................
  if (cfg$render) {
    t0 <- Sys.time()
    psa_render_documents(run_dir, paths, log)
    tick("render", t0)
    manifest$timings_sec <- timings
    psa_save_rds(manifest, out("manifest.rds"), psa_root)
    snap_final <- psa_snapshot(paths, "after_render")
    iso2 <- psa_isolation_report(snap_before, snap_final)
    psa_fwrite(if (nrow(iso2)) iso2 else data.table(status = "no changes outside output_psa/"),
               psa_out(psa_root, "isolation", paste0("isolation_report_", run_id, "_after_render.csv")), psa_root)
    log("Isolation after rendering: %d files changed outside output_psa/.", nrow(psa_isolation_changes(iso2)))
    if (nrow(psa_isolation_changes(iso2))) stop("Rendering changed files outside output_psa/; see the isolation report.")
  }
  log("Done in %.1f min.", as.numeric(difftime(Sys.time(), t_start, units = "mins")))
  invisible(list(run_dir = run_dir, manifest = manifest))
}

# Files whose content determines a block's results: a checkpoint is reused only
# if the configuration AND all of these are unchanged (md5 from the preflight
# snapshot), so an edited model or input can never be mixed with old blocks.
# (The 06 script itself is added by md5 via `extra`, since it may be a pinned
# copy outside the snapshot.)
PSA_CHECKPOINT_DEPS <- c(
  paste0("code/", c("091_psa_parameters.R", "092_psa_model.R", "093_psa_downstream.R",
                    "05_build_baseline.R", "07_output_dalys.R", "08_economic_value_calculation.R")),
  paste0("data/processed/", c("ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx",
                              "ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv",
                              "htn_control_targets_by_loc.csv", "statin_data.rds")))

digest_config <- function(cfg, run_id, snap = NULL, extra = character()) {
  x <- cfg[c("mode", "n_draws", "seed", "countries", "correlation_mode", "block_size")]
  deps <- if (is.null(snap)) character() else
    snap[path %in% PSA_CHECKPOINT_DEPS][order(path), paste0(path, "=", md5)]
  deps <- c(deps, if (length(extra)) paste0(names(extra), "=", extra))
  paste(run_id, paste(deparse(x), collapse = ""), paste(deps, collapse = ";"), sep = "|")
}

#...........................................................
# Validation orchestration ----
#...........................................................

psa_run_validation <- function(cfg, paths, inp, fns, s6, sheet1, rr10, wb_sheet1, sheet1_0, rr10_0,
                               ep, sm, checks, econ_detail, c1_draw0, run_dir, log, dsin) {
  psa_root <- paths$psa_root
  # Optional validation files are written only when needed; drop earlier copies.
  psa_remove(file.path(run_dir, "validation", c("08_adjudicated_cells.csv", "reference_freshness.csv")),
             psa_root)
  v <- list()
  v$params <- psa_validate_parameters(sheet1, rr10, inp, wb_sheet1)
  vc <- intersect(cfg$validation_countries, inp$locs)
  sc <- intersect(cfg$sample_countries, inp$locs)
  long <- lapply(setNames(sc, sc), function(Country) {
    fs <- list.files(file.path(run_dir, "model_samples"),
                     pattern = paste0("^model_output_", Country, "_block[0-9]+[.]rds$"), full.names = TRUE)
    rbindlist(lapply(fs, readRDS))
  })
  v$model_draw0 <- psa_validate_model_draw0(long, paths)
  ep_val <- ep; ep_val$c1_draw0 <- c1_draw0
  if (length(vc) && !is.null(inp$env06_setup)) {
    for (Country in vc) {
      sink(tempfile()); live <- tryCatch(psa_live_06(inp$env06_setup, Country, inp$run_args),
                                         finally = sink())
      saved <- psa_read_saved_06(paths, Country)
      if (!is.null(saved)) {
        v[[paste0("live_saved_", Country)]] <- psa_compare_model_tables(
          "V4 live 06 run_multiple_scenarios() vs saved 06 output", live, saved,
          tol_abs = 0, scope = Country)
      }
      if (Country %in% names(long)) {
        v[[paste0("live_psa_", Country)]] <- psa_compare_model_tables(
          "V4 live 06 run_multiple_scenarios() vs PSA draw 0", live, long[[Country]][draw_id == 0L],
          tol_abs = 0, scope = Country)
      }
    }
  }
  # V3: per-draw reference path for every draw of the sample country (parallel).
  ref_countries <- intersect(vc, names(long))
  if (length(ref_countries)) {
    Country <- ref_countries[1]
    ref_ids <- sort(unique(long[[Country]]$draw_id))
    ref_ids <- ref_ids[seq_len(min(length(ref_ids), 21L))]
    ncl <- max(1L, min(cfg$n_workers, length(ref_ids)))
    cl <- makeCluster(ncl); on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
    clusterCall(cl, psa_worker_init, file.path(paths$wd_code, PSA_HELPER_FILES), paths$root,
                getOption("who_cvd.psa_script06"))
    tasks <- lapply(ref_ids, function(id) list(
      draw_id = id, Country = Country, cin = inp$cin[[Country]], scenarios = inp$scenarios,
      htn_scenario_ids = inp$htn_scenario_ids, dt_hbp_targets = inp$dt_hbp_targets,
      run_args = inp$run_args, repYear = inp$repYear, dt_gbd_rr = inp$dt_gbd_rr,
      sheet1 = sheet1[draw_id == id], rr10 = rr10[draw_id == id]))
    t_ref <- Sys.time()
    refs <- parLapply(cl, tasks, psa_reference_draw)
    stopCluster(cl); on.exit(NULL)
    log("V3 reference path: %d draws for %s in %.1f s.", length(refs), Country,
        as.numeric(difftime(Sys.time(), t_ref, units = "secs")))
    v$batched_vs_ref <- psa_validate_batched_vs_reference(refs, long[[Country]])
  }
  if (length(vc)) {
    v$zero <- psa_validate_zero_effect(vc[1], inp$cin[[vc[1]]], fns, inp, s6, rr10_0, sheet1_0)
    v$econ_detail <- psa_validate_08_detail(paths, econ_detail[intersect(vc, names(econ_detail))])
    if (cfg$validate_07_detail) v$v07 <- psa_validate_07(paths, vc, ep_val)
  }
  cells <- psa_validate_08_cells(paths, econ_detail, dsin$k08)
  v$econ_cells <- cells$checks
  if (nrow(cells$cells)) {
    psa_fwrite(cells$cells, psa_out(run_dir, "validation", "08_adjudicated_cells.csv"), psa_root)
    log("V6: %d stored 08 cell(s) differ from the PSA; %d reproduce 08's own code on the saved 06 outputs.",
        nrow(cells$cells), sum(cells$cells$explained))
  }
  full_geo <- is.null(cfg$countries)
  econ0 <- ep$econ[draw_id == 0L]
  if (full_geo) {
    v$econ_summary <- psa_validate_08_summary(paths, econ0, cells$adjust, dsin$income_08)
    v$report <- psa_validate_report_artifacts(paths, sm)
  } else {
    v$scope_note <- data.table(check = "V6/V7 global comparisons", scope = "country subset", pass = NA,
                               note = "Skipped: global totals need the full 06 geography.")
  }
  v$invariants <- psa_validate_invariants(ep, long, checks)
  # References older than the saved 06 outputs are not comparable (see 094).
  fresh <- psa_reference_freshness(paths)
  if (!is.null(fresh) && nrow(fresh)) {
    psa_fwrite(fresh, psa_out(run_dir, "validation", "reference_freshness.csv"), psa_root)
    stale_files <- function(g) paste(basename(fresh[group == g & stale == TRUE, file]), collapse = ", ")
    why <- function(g) sprintf(paste0("reference artefact(s) older than the newest saved 06 output (%s); ",
                                      "re-run/re-knit the original step, then re-validate"), stale_files(g))
    groups <- list(V5_07 = "v07", V6_08 = c("econ_detail", "econ_cells", "econ_summary"), V7_report = "report")
    for (g in intersect(names(groups), fresh[stale == TRUE, unique(group)])) {
      for (nm in groups[[g]]) v[[nm]] <- psa_mark_not_compared(v[[nm]], why(g))
      log("Validation: %s references are older than the saved 06 outputs (%s): NOT COMPARED.", g, stale_files(g))
    }
    v$freshness <- fresh[, .(check = "Reference artefacts at least as new as the saved 06 outputs",
                             scope = group[1], n = .N, pass = if (any(stale)) NA else TRUE,
                             note = if (any(stale)) paste("stale:", paste(basename(file[stale]), collapse = ", ")) else ""),
                         by = group][, group := NULL][]
  }
  rbindlist(v, fill = TRUE, idcol = "group")
}

#...........................................................
# Rendering (report, then deck from the report's artefacts) ----
#...........................................................

psa_find_pandoc <- function() {
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

psa_render_documents <- function(run_dir, paths = psa_paths(psa_find_repo_root()),
                                 log = function(fmt, ...) message(sprintf(fmt, ...))) {
  psa_root <- paths$psa_root
  man <- readRDS(file.path(run_dir, "manifest.rds"))
  if (man$run_mode == "production" && man$n_mc_draws < man$min_final_draws) {
    stop(sprintf(paste0("Refusing to render production documents from %d Monte Carlo draws: ",
                        "final 95%% intervals need >= %d draws."), man$n_mc_draws, man$min_final_draws))
  }
  psa_find_pandoc()
  src <- file.path(paths$root, "scenarios_psa", "scenarios_aim1")
  src_before <- list.files(src, all.files = TRUE, no.. = TRUE)
  out_dir <- psa_out(psa_root, "rendered")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  report_file <- paste0("aim1_report_psa_", man$run_id, ".html")
  deck_file <- paste0("aim1_executive_slides_psa_", man$run_id, ".pdf")
  # The repository path contains spaces ("OneDrive - UW"), which LaTeX cannot
  # resolve in knitr's figure paths (they become 8.3 names with backslashes), and
  # LaTeX runs in the input file's directory. So both documents are compiled in a
  # private copy inside R's session temp directory (outside the repository; no
  # spaces), and only the final HTML/PDF are copied into output_psa/rendered/.
  work <- file.path(tempdir(), paste0("psa_render_", man$run_id))
  unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  if (grepl(" ", work, fixed = TRUE)) stop("R's temp directory contains spaces; set TMPDIR to a path without spaces.")
  stopifnot(file.copy(file.path(src, c("aim1_report_psa.Rmd", "aim1_executive_slides_psa.Rmd")), work),
            file.copy(file.path(paths$root, "scenarios", "scenarios_aim1", "beamer_preamble.tex"), work))
  rmarkdown::render(file.path(work, "aim1_report_psa.Rmd"), output_dir = work, intermediates_dir = work,
                    knit_root_dir = work,
                    params = list(run_dir = run_dir, repo_root = paths$root, psa_root = psa_root,
                                  render_token = PSA_RENDER_TOKEN),
                    envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
  psa_assert_output_path(file.path(out_dir, report_file), psa_root)
  stopifnot(file.copy(file.path(work, "aim1_report_psa.html"), file.path(out_dir, report_file), overwrite = TRUE))
  log("Rendered %s", file.path(out_dir, report_file))
  sl <- readRDS(psa_out(psa_root, "slides", "sl_psa_meta.rds"))
  # TEST banner on every slide: a thin strip flush with the top edge, right-
  # aligned, above the frame-title line (titles start ~5% down the slide; the
  # strip is ~3.5% tall), so it never covers a title, slide content or the footer.
  banner <- sprintf(paste0("\\AddToShipoutPictureFG{\\AtPageUpperLeft{\\makebox[\\paperwidth][r]{",
                           "\\raisebox{-\\height}{\\setlength{\\fboxsep}{1.2pt}\\colorbox{red!85!black}{",
                           "\\textcolor{white}{\\bfseries\\scriptsize\\ TEST --- %d draws; intervals not for ",
                           "reporting\\ }}}}}}"),
                    sl$n_mc_draws)
  writeLines(c("% Generated by 09_uncertainty_psa.R",
               if (!isTRUE(sl$is_final)) c("\\usepackage{eso-pic}", banner)),
             file.path(work, "psa_deck_header.tex"))
  rmarkdown::render(file.path(work, "aim1_executive_slides_psa.Rmd"), output_dir = work,
                    intermediates_dir = work, knit_root_dir = work,
                    params = list(slides_dir = psa_out(psa_root, "slides"), render_token = PSA_RENDER_TOKEN),
                    output_options = list(includes = list(in_header = c("beamer_preamble.tex",
                                                                        "psa_deck_header.tex"))),
                    envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
  psa_assert_output_path(file.path(out_dir, deck_file), psa_root)
  stopifnot(file.copy(file.path(work, "aim1_executive_slides_psa.pdf"), file.path(out_dir, deck_file), overwrite = TRUE))
  log("Rendered %s", file.path(out_dir, deck_file))
  src_after <- list.files(src, all.files = TRUE, no.. = TRUE)
  if (!setequal(src_before, src_after)) {
    stop("Rendering left files in scenarios_psa/: ", paste(setdiff(src_after, src_before), collapse = ", "))
  }
  invisible(file.path(out_dir, c(report_file, deck_file)))
}

PSA_RENDER_TOKEN <- "rendered-by-09_uncertainty_psa"

#...........................................................
# Execute ----
#...........................................................

if (isTRUE(getOption("who_cvd.execute_09", TRUE)) && !interactive()) {
  psa_main(psa_parse_cli())
}
