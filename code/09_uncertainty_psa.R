# =============================================================================
# 09_uncertainty_psa.R -- Aim 1 probabilistic sensitivity analysis (PSA)
# =============================================================================
# Scope: BP AND STATIN INTERVENTION-EFFECT parameter uncertainty.
#   * 15 BPLTTC 2021 Appendix S6 hazard ratios (per 5 mmHg; IHD / Stroke / Heart
#     failure x baseline SBP 130-139 ... >=170), log-normal;
#   * 4 statin RRs of 06's calculate_statins_impact() (IHD and ischaemic-stroke
#     incidence and case fatality), log-normal around the retained model values
#     with CTT-derived log-scale SDs (see 091 and parameter_register.csv).
# Draws are propagated through the actual Aim 1 model (the 06 script sourced by
# 00_run_model.R, read-only) for all six 06 scenarios. Coverage, targets,
# adherence, epidemiology, calibration and secular trends stay fixed.
#
# Outputs are mortality only (deaths 2026-2050, 25 years): the deterministic 06
# result (draw 0) is the point estimate; the 95% uncertainty interval
# is the 2.5th-97.5th percentile (type 7) of draws 1..N of each draw-level
# estimand. Everything is written as compact CSV under output_psa/<run_id>/; no
# model-output RDS is saved. The production pipeline (00-08, stage 03/031) is
# never run or modified.
#
# Run from the repository root:
#   Rscript code/09_uncertainty_psa.R mode=technical draws=3 countries=Colombia,Chile
#   Rscript code/09_uncertainty_psa.R mode=full draws=1000 workers=6   (default 1000 draws)
#   Rscript code/09_uncertainty_psa.R render=only run_id=full_d1000_s20260923
# Options (key=value): mode (full | technical), draws, seed, workers, block,
# countries (comma separated), correlation (independent |
# comonotone_within_stratum | comonotone_all; BP parameters only), validate,
# render (true | false | only), cache, min_report_draws, run_id, script06.
# From R: options(who_cvd.execute_09 = FALSE); source("code/09_uncertainty_psa.R")

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(readxl); library(parallel); library(jsonlite)
})

PSA_SCHEMA_VERSION <- "aim1-psa-2.0"

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

psa_config <- function(mode = c("full", "technical"), n_draws = NULL, seed = 20260923L,
                       n_workers = NULL, block_size = NULL, countries = NULL,
                       correlation_mode = "independent", validate = TRUE, render = TRUE,
                       render_only = FALSE, cache_contexts = TRUE, min_report_draws = 100L,
                       run_id = NULL, validation_countries = "Colombia", script06 = NULL) {
  mode <- match.arg(mode)
  if (is.null(n_draws)) n_draws <- if (mode == "technical") 3L else 1000L
  if (is.null(n_workers)) n_workers <- max(1L, min(8L, parallel::detectCores(logical = FALSE)))
  if (is.null(block_size)) block_size <- if (mode == "technical") n_draws + 1L else 26L
  list(mode = mode, n_draws = as.integer(n_draws), seed = as.integer(seed),
       n_workers = as.integer(n_workers), block_size = as.integer(block_size),
       countries = countries, correlation_mode = correlation_mode,
       validate = isTRUE(validate), render = isTRUE(render) || isTRUE(render_only),
       render_only = isTRUE(render_only), cache_contexts = isTRUE(cache_contexts),
       min_report_draws = as.integer(min_report_draws), run_id = run_id,
       validation_countries = validation_countries, script06 = script06)
}

psa_parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(args[grepl("=", args)], "=", fixed = TRUE)
  a <- setNames(lapply(kv, `[`, 2), vapply(kv, `[`, "", 1))
  lg <- function(x, default) if (is.null(x)) default else tolower(x) %in% c("true", "1", "yes")
  num <- function(x) if (is.null(x)) NULL else as.integer(x)
  psa_config(mode = if (is.null(a$mode)) "full" else a$mode, n_draws = num(a$draws),
             seed = if (is.null(a$seed)) 20260923L else as.integer(a$seed),
             n_workers = num(a$workers), block_size = num(a$block),
             countries = if (is.null(a$countries)) NULL else strsplit(a$countries, ",", fixed = TRUE)[[1]],
             correlation_mode = if (is.null(a$correlation)) "independent" else a$correlation,
             validate = lg(a$validate, TRUE),
             render = if (identical(a$render, "only")) TRUE else lg(a$render, TRUE),
             render_only = identical(a$render, "only"),
             cache_contexts = lg(a$cache, TRUE),
             min_report_draws = if (is.null(a$min_report_draws)) 100L else as.integer(a$min_report_draws),
             run_id = a$run_id, script06 = a$script06)
}

psa_run_id <- function(cfg) {
  id <- sprintf("%s_d%d_s%d", cfg$mode, cfg$n_draws, cfg$seed)
  if (cfg$correlation_mode != "independent") id <- paste0(id, "_", cfg$correlation_mode)
  if (!is.null(cfg$countries)) id <- paste0(id, "_n", length(cfg$countries), "countries")
  id
}

# Interval tiers: < min_report_draws = technical check (not reportable);
# min_report_draws .. final_draws - 1 = preliminary; >= final_draws = standard
# 95% UI (tail precision still reported in mortality_summary.csv).
psa_interval_label <- function(n_draws, min_report_draws, final_draws = 1000L) {
  if (n_draws >= final_draws) {
    list(tier = "final", short = "95% UI",
         long = sprintf(paste0("95%% uncertainty interval: 2.5th-97.5th percentiles of %s Monte Carlo draws of the ",
                               "BP and statin intervention-effect parameters (intervention-effect parameter ",
                               "uncertainty only)"), format(n_draws, big.mark = ",")),
         reportable = TRUE)
  } else if (n_draws >= min_report_draws) {
    list(tier = "preliminary", short = "95% UI (preliminary)",
         long = sprintf(paste0("Preliminary 95%% uncertainty interval: 2.5th-97.5th percentiles of %d Monte Carlo ",
                               "draws of the BP and statin intervention-effect parameters (limited tail precision; ",
                               "not a converged estimate)"), n_draws),
         reportable = TRUE)
  } else {
    list(tier = "technical", short = "TECHNICAL CHECK interval",
         long = sprintf("TECHNICAL CHECK from %d draws: software test only, NOT a reportable interval", n_draws),
         reportable = FALSE)
  }
}

#...........................................................
# Repository snapshot (path isolation) ----
#...........................................................

PSA_SNAPSHOT_EXCLUDE <- "^([.]git/|output_psa/|[.]Rproj[.]user/|[.]claude/worktrees/)"

psa_snapshot <- function(paths) {
  root <- paths$root
  files <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  files <- files[!grepl(PSA_SNAPSHOT_EXCLUDE, files) & !grepl("(^|/)[.]Rhistory$", files)]
  info <- file.info(file.path(root, files))
  md5 <- vapply(file.path(root, files), function(f) {
    tryCatch(unname(tools::md5sum(f)), error = function(e) NA_character_, warning = function(w) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  data.table(path = files, size = info$size, md5 = md5)
}

psa_isolation_report <- function(before, after) {
  m <- merge(before[, .(path, md5_before = md5, in_before = TRUE)],
             after[, .(path, md5_after = md5, in_after = TRUE)], by = "path", all = TRUE)
  m[, status := fcase(is.na(in_before), "ADDED", is.na(in_after), "REMOVED",
                      is.na(md5_before) | is.na(md5_after), "UNREADABLE (locked; not verified)",
                      md5_before != md5_after, "MODIFIED", default = "unchanged")]
  m[status != "unchanged", .(path, status, md5_before, md5_after)]
}

psa_git <- function(paths, ...) {
  tryCatch(suppressWarnings(system2("git", c("-C", shQuote(paths$root), ...), stdout = TRUE, stderr = TRUE)),
           error = function(e) paste("git unavailable:", conditionMessage(e)))
}

# Inputs whose content determines a block's results: a checkpoint is reused only
# if the configuration and all of these md5s are unchanged.
PSA_CHECKPOINT_DEPS <- c(
  paste0("code/", c("091_psa_parameters.R", "092_psa_model.R", "093_psa_downstream.R",
                    "05_build_baseline.R")),
  paste0("data/processed/", c(PSA_BP_WORKBOOK, "htn_control_targets_by_loc.csv", "statin_data.rds",
                              "af_statins.rds", "bp_data6.csv", "covfxn2.csv", "wpp.adj.Rda",
                              "PopulationsSingleAge0050.rds", "PopulationsAge20_2050.csv",
                              paste0("adjusted_searo_part", 1:10, ".rds"))))

psa_config_hash <- function(cfg, run_id, snap, script06_md5) {
  x <- cfg[c("mode", "n_draws", "seed", "countries", "correlation_mode", "block_size")]
  deps <- snap[path %in% PSA_CHECKPOINT_DEPS][order(path), paste0(path, "=", md5)]
  s <- paste(PSA_SCHEMA_VERSION, run_id, paste(deparse(x), collapse = ""),
             paste(deps, collapse = ";"), paste0("script06=", script06_md5), sep = "|")
  f <- tempfile(); on.exit(unlink(f)); writeLines(s, f)
  unname(tools::md5sum(f))
}

#...........................................................
# Worker set-up ----
#...........................................................

# Top level on purpose: PSOCK serializes a function with its enclosing
# environment, so a closure created in psa_main() would ship that whole frame.
psa_worker_init <- function(code_files, root, script06) {
  suppressPackageStartupMessages({ library(dplyr); library(data.table); library(readxl) })
  data.table::setDTthreads(1L)
  for (f in code_files) sys.source(f, envir = globalenv())
  options(who_cvd.psa_script06 = script06)
  fns <- psa_load_06_functions(psa_paths(root))
  psa_assert_no_sensitive_globals(fns)
  assign(".psa_fns", fns, envir = globalenv())
  assign(".psa_sset", psa_06_statin_settings(fns), envir = globalenv())
  invisible(TRUE)
}

psa_worker_load <- function(wk) {
  wk$ctx_cache <- new.env()
  assign(".psa_worker", wk, envir = globalenv())
  invisible(TRUE)
}

# Guarded delete: every path must lie inside output_psa/.
psa_remove <- function(files, psa_root) {
  files <- files[file.exists(files)]
  for (f in files) psa_assert_output_path(f, psa_root)
  if (length(files)) unlink(files)
  invisible(files)
}

#...........................................................
# Main ----
#...........................................................

psa_main <- function(cfg = psa_config()) {
  t_start <- Sys.time()
  paths <- psa_paths(psa_find_repo_root())
  psa_root <- paths$psa_root
  run_id <- if (is.null(cfg$run_id)) psa_run_id(cfg) else cfg$run_id
  if (!grepl("^[A-Za-z0-9_.-]+$", run_id) || run_id %in% c("runs", "isolation", "rendered")) {
    stop("Invalid run_id: ", run_id)
  }
  run_dir <- psa_out(psa_root, run_id)
  out <- function(...) psa_out(run_dir, ...)
  if (cfg$render_only) {
    if (!file.exists(out("run_manifest.json"))) stop("No completed run in ", run_dir)
    return(invisible(psa_render_documents(run_dir, paths)))
  }
  dir.create(out("logs"), recursive = TRUE, showWarnings = FALSE)
  log_file <- out("logs", "run.log")
  log <- function(fmt, ...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", sprintf(fmt, ...))
    cat(msg, "\n"); cat(msg, "\n", file = log_file, append = TRUE)
  }
  timings <- list()
  tick <- function(name, t0) timings[[name]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  label <- psa_interval_label(cfg$n_draws, cfg$min_report_draws)
  log("Aim 1 PSA %s | mode=%s draws=%d seed=%d workers=%d block=%d correlation=%s",
      run_id, cfg$mode, cfg$n_draws, cfg$seed, cfg$n_workers, cfg$block_size, cfg$correlation_mode)
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
  snap_before <- psa_snapshot(paths)
  git_head <- psa_git(paths, "rev-parse", "HEAD")
  git_status_before <- psa_git(paths, "status", "--porcelain=v1", "--untracked-files=all")
  cfg_hash <- psa_config_hash(cfg, run_id, snap_before, script06_md5)
  tick("preflight_snapshot", t0)
  log("Preflight: %d repository files hashed (%.0f s); config hash %s.", nrow(snap_before),
      timings$preflight_snapshot, cfg_hash)

  #.. 1. Parameters and draws ................................................
  t0 <- Sys.time()
  fns <- psa_load_06_functions(paths)
  psa_assert_no_sensitive_globals(fns)
  sset <- psa_06_statin_settings(fns)
  st <- psa_statin_param_table(sset$points)
  s6 <- psa_read_appendix_s6(paths$wd_data)
  parsed06 <- psa_parse_06(paths)
  run_args <- psa_06_run_args(parsed06)
  register <- psa_parameter_register(s6, run_args, st)
  draws <- psa_generate_draws(s6, st, cfg$n_draws, cfg$seed, cfg$correlation_mode)
  sheet1 <- psa_build_sheet1_draws(draws$bp, s6)
  wb_sheet1 <- suppressMessages(psa_read_sheet1(paths$wd_data))
  sheet1_0 <- psa_sheet1_for_draw(sheet1, 0L)
  rc <- psa_check_sheet1_reconstruction(sheet1_0, wb_sheet1)
  if (!all(rc$pass)) stop("Sheet1 draw-0 reconstruction does not match the workbook.")
  psa_fwrite(register, out("parameter_register.csv"), psa_root, bom = TRUE)
  psa_fwrite(psa_parameter_draws_table(draws), out("parameter_draws.csv"), psa_root)
  log("Parameters: %d sampled (15 BP + 4 statin) / %d register rows; draws 0..%d; Sheet1 draw-0 max diff %.3g; statin points %s.",
      sum(register$status == "SAMPLED"), nrow(register), cfg$n_draws, max(rc$max_abs_diff),
      paste(names(sset$points), sset$points, sep = "=", collapse = ", "))
  tick("parameters", t0)

  #.. 2. Deterministic inputs ................................................
  t0 <- Sys.time()
  keep_env <- if (cfg$validate) intersect(cfg$validation_countries,
                                          if (is.null(cfg$countries)) cfg$validation_countries else cfg$countries)
  inp <- psa_prepare_inputs(paths, countries = cfg$countries, log = function(m) log("%s", m),
                            keep_setup_env_for = keep_env)
  region_map <- psa_region_map(paths, inp$locs)
  saved_locs <- sub("^model_output_(.*)[.]rds$", "\\1",
                    list.files(file.path(paths$wd_outp_original, "out_model"), pattern = "^model_output_.*[.]rds$"))
  log("Inputs: %d eligible 06 locations (%d saved 06 outputs; all present: %s); regions: %s; run args: %s.",
      length(inp$locs), length(saved_locs), all(inp$locs %in% saved_locs),
      paste(region_map[, .N, by = who_region][, paste0(who_region, "=", N)], collapse = ", "),
      paste(c("statin_target_coverage", "statin_start_year", "statin_target_year", "adherence_ir", "adherence_cf"),
            unlist(inp$run_args[c("statin_target_coverage", "statin_start_year", "statin_target_year",
                                  "adherence_ir", "adherence_cf")]), sep = "=", collapse = ", "))
  shared <- list(repYear = inp$repYear, dt_gbd_rr = inp$dt_gbd_rr, run_args = inp$run_args,
                 scenarios = inp$scenarios, htn_scenario_ids = inp$htn_scenario_ids, sheet1_0 = sheet1_0)
  tick("inputs", t0)

  #.. 3. Cluster (static country groups; inputs sent once per worker) ........
  n_workers <- min(cfg$n_workers, length(inp$locs))
  # Balance by round-robin over the location order.
  groups <- split(inp$locs, rep_len(seq_len(n_workers), length(inp$locs)))
  cl <- NULL
  stop_cluster <- function() { if (!is.null(cl)) try(stopCluster(cl), silent = TRUE); cl <<- NULL }
  ensure_cluster <- function() {
    if (!is.null(cl)) return(invisible(cl))
    t0 <- Sys.time()
    cl <<- makeCluster(n_workers)
    clusterCall(cl, psa_worker_init, file.path(paths$wd_code, PSA_HELPER_FILES), paths$root, script06)
    for (g in seq_along(groups)) {
      cs <- groups[[g]]
      wk <- list(countries = cs, cin = inp$cin[cs], shared = shared,
                 region = setNames(as.list(region_map[match(cs, location), who_region]), cs),
                 saved06 = setNames(as.list(file.path(paths$wd_outp_original, "out_model",
                                                      paste0("model_output_", cs, ".rds"))), cs))
      clusterCall(cl[g], psa_worker_load, wk)
    }
    timings$cluster_init <<- c(timings$cluster_init, as.numeric(difftime(Sys.time(), t0, units = "secs")))
    log("Cluster: %d workers, %d-%d countries each (%.0f s to start).", n_workers,
        min(lengths(groups)), max(lengths(groups)), tail(timings$cluster_init, 1))
    invisible(cl)
  }
  on.exit(stop_cluster(), add = TRUE)

  #.. 4. Draw blocks (checkpointed as compact CSV) ............................
  all_ids <- 0:cfg$n_draws
  blocks <- split(all_ids, ceiling(seq_along(all_ids) / cfg$block_size))
  ck_dir <- out("checkpoints")
  dir.create(ck_dir, showWarnings = FALSE)
  ck <- function(b, what) psa_out(ck_dir, sprintf("block_%03d_%s", b, what))
  # Checkpoint files of blocks that do not exist in this configuration are removed.
  old <- list.files(ck_dir, pattern = "^block_[0-9]{3}_", full.names = TRUE)
  old_b <- as.integer(sub("^block_([0-9]{3})_.*$", "\\1", basename(old)))
  if (length(old[old_b > length(blocks)])) {
    psa_remove(old[old_b > length(blocks)], psa_root)
    log("Removed checkpoint files of blocks beyond this configuration.")
  }
  run_block <- function(ids) {
    task <- list(draw_ids = ids, sheet1_block = sheet1[draw_id %in% ids],
                 statin_block = draws$statin[draw_id %in% ids],
                 cache_contexts = cfg$cache_contexts && length(blocks) > 1L)
    ensure_cluster()
    res <- clusterCall(cl, psa_worker_task, task)
    psa_combine_block(res, ids, inp$locs)
  }
  block_status <- list()
  t_blocks <- Sys.time()
  for (b in seq_along(blocks)) {
    ids <- blocks[[b]]
    meta_f <- ck(b, "meta.json")
    files_b <- c(ck(b, "deaths.csv"), ck(b, "checks.csv"), ck(b, "country_checks.csv"))
    if (0L %in% ids) files_b <- c(files_b, ck(b, "draw0_vs_06.csv"))
    if (file.exists(meta_f) && all(file.exists(files_b))) {
      m <- fromJSON(meta_f)
      dd <- fread(ck(b, "deaths.csv"))
      if (identical(m$cfg_hash, cfg_hash) && identical(as.integer(m$draw_ids), ids) &&
          nrow(dd) == length(ids) * length(PSA_SCENARIOS) * (length(PSA_REGION_ORDER) + 1L) &&
          setequal(unique(dd$draw_id), ids) && all(is.finite(dd$deaths))) {
        log("Block %d/%d (draws %d-%d): reused checkpoint.", b, length(blocks), min(ids), max(ids))
        block_status[[b]] <- data.table(block = b, first = min(ids), last = max(ids), status = "reused",
                                        attempts = 0L, seconds = NA_real_)
        next
      }
    }
    psa_remove(c(meta_f, files_b), psa_root)
    tb <- Sys.time(); attempt <- 0L; comb <- NULL; last_err <- NULL
    while (is.null(comb) && attempt < 2L) {
      attempt <- attempt + 1L
      comb <- tryCatch(run_block(ids), error = function(e) { last_err <<- conditionMessage(e); NULL })
      if (is.null(comb)) {
        log("Block %d attempt %d FAILED: %s", b, attempt, last_err)
        stop_cluster()
      }
    }
    if (is.null(comb)) stop("Block ", b, " (draws ", min(ids), "-", max(ids), ") failed twice: ", last_err)
    # Draw-level failure (non-finite totals): recompute those draws once, then stop.
    bad <- comb$deaths[!is.finite(deaths), sort(unique(draw_id))]
    if (length(bad)) {
      log("Block %d: non-finite totals for draws %s; retrying them.", b, paste(bad, collapse = ","))
      redo <- run_block(as.integer(bad))
      if (redo$deaths[, any(!is.finite(deaths))]) {
        stop("Draws failed after retry (not dropped): ", paste(bad, collapse = ", "))
      }
      comb$deaths <- rbind(comb$deaths[!draw_id %in% bad], redo$deaths)
      comb$checks <- rbind(comb$checks[!draw_id %in% bad], redo$checks)
    }
    psa_fwrite(comb$deaths, ck(b, "deaths.csv"), psa_root)
    psa_fwrite(comb$checks, ck(b, "checks.csv"), psa_root)
    psa_fwrite(comb$country_checks[, block := b], ck(b, "country_checks.csv"), psa_root)
    if (0L %in% ids) psa_fwrite(comb$draw0_vs_06, ck(b, "draw0_vs_06.csv"), psa_root)
    sec <- as.numeric(difftime(Sys.time(), tb, units = "secs"))
    psa_write_lines(toJSON(list(cfg_hash = cfg_hash, run_id = run_id, block = b, draw_ids = ids,
                                created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), attempts = attempt,
                                seconds = sec, retried_draws = bad,
                                max_worker_memory_mb = max(comb$worker_mem_mb)),
                           auto_unbox = TRUE, pretty = TRUE), meta_f, psa_root)
    block_status[[b]] <- data.table(block = b, first = min(ids), last = max(ids), status = "computed",
                                    attempts = attempt, seconds = sec)
    log("Block %d/%d (draws %d-%d): %.0f s; max worker memory %.0f MB.", b, length(blocks),
        min(ids), max(ids), sec, max(comb$worker_mem_mb))
  }
  stop_cluster()
  tick("model_blocks", t_blocks)

  #.. 5. Assemble compact outputs ............................................
  t0 <- Sys.time()
  rd <- function(what) rbindlist(lapply(seq_along(blocks), function(b) {
    f <- ck(b, what); if (file.exists(f)) fread(f) else NULL
  }), fill = TRUE)
  deaths <- rd("deaths.csv"); checks <- rd("checks.csv"); country_checks <- rd("country_checks.csv")
  d0cmp <- fread(ck(1L, "draw0_vs_06.csv"))
  if (anyDuplicated(deaths, by = c("draw_id", "scenario_id", "geography"))) {
    stop("Duplicate draw rows across checkpoints (incompatible resume).")
  }
  if (!setequal(unique(deaths$draw_id), all_ids)) stop("Draw ids missing after assembly.")
  md <- psa_mortality_draws(deaths, intersect(PSA_REGION_ORDER, region_map$who_region))
  sm <- psa_mortality_summary(md, cfg$n_draws)
  tab_g <- psa_table_global(sm)
  tab_r <- psa_table_region(sm)
  psa_fwrite(md, out("mortality_draws.csv"), psa_root)
  psa_fwrite(sm, out("mortality_summary.csv"), psa_root)
  psa_fwrite(tab_g, out("table_global_interventions.csv"), psa_root, bom = TRUE)
  psa_fwrite(tab_r, out("table_who_region_all_interventions.csv"), psa_root, bom = TRUE)
  psa_fwrite(checks, out("transition_checks.csv"), psa_root)
  tick("assemble", t0)
  log("Outputs: %d mortality draw rows; %d summary rows.", nrow(md), nrow(sm))

  #.. 6. Validation ...........................................................
  val <- NULL; val_pass <- NA
  if (cfg$validate) {
    t0 <- Sys.time()
    v <- list()
    v$params <- psa_validate_parameters(s6, st, draws, sheet1, wb_sheet1, inp$det_etihad_rr_bin, sset,
                                        cfg$n_draws)
    vc <- intersect(cfg$validation_countries, inp$locs)
    for (Country in vc) {
      v[[paste0("statfun_", Country)]] <- psa_validate_statin_function(Country, inp$cin[[Country]], fns, sset,
                                                                      inp$run_args)
      live <- if (!is.null(inp$env06_setup)) psa_live_06(inp$env06_setup, Country, inp$run_args) else NULL
      saved <- psa_read_saved_06(paths, Country)
      ref_ids <- psa_reference_draw_ids(draws, cfg$n_draws)
      tv <- Sys.time()
      v[[paste0("paths_", Country)]] <- psa_validate_country_paths(Country, inp, fns, sset, shared, s6, draws,
                                                                   sheet1, ref_ids, live, saved)
      log("Validation paths for %s (reference draws %s): %.0f s.", Country, paste(ref_ids, collapse = ","),
          as.numeric(difftime(Sys.time(), tv, units = "secs")))
      rm(live, saved); gc(verbose = FALSE)
    }
    v$draw0 <- psa_validate_draw0_all(d0cmp, inp$locs, country_checks)
    v$report <- psa_validate_report(paths, md, is.null(cfg$countries))
    # Manuscript central estimates (docs/who_cvd_targets_paper1.docx, read from a temporary copy).
    cmp <- NULL
    if (is.null(cfg$countries)) {
      cmp <- tryCatch({ psa_find_pandoc(); psa_manuscript_comparison(paths, sm) },
                      error = function(e) { log("Manuscript comparison not run: %s", conditionMessage(e)); NULL })
      if (!is.null(cmp)) psa_fwrite(cmp, out("manuscript_comparison.csv"), psa_root)
    }
    v$manuscript <- psa_validate_manuscript(cmp, is.null(cfg$countries))
    v$outputs <- psa_validate_outputs(deaths, md, sm, checks, region_map, cfg$n_draws, inp$locs)
    val <- rbindlist(v, fill = TRUE, idcol = "group")
    hard <- val[!is.na(pass)]
    val_pass <- all(hard$pass)
    psa_fwrite(val, out("validation_log.csv"), psa_root)
    tick("validation", t0)
    log("Validation: %d/%d checks pass (%d informational).", sum(hard$pass), nrow(hard), sum(is.na(val$pass)))
    if (!val_pass) log("FAILED checks: %s", paste(hard[pass == FALSE, paste0(check, " [", scope, "]")], collapse = " | "))
  }

  #.. 7. Path isolation ......................................................
  snap_after <- psa_snapshot(paths)
  iso <- psa_isolation_report(snap_before, snap_after)
  n_changed <- nrow(iso[status %in% c("ADDED", "MODIFIED", "REMOVED")])
  psa_fwrite(if (nrow(iso)) iso else data.table(status = "no changes outside output_psa/"),
             out("isolation_report.csv"), psa_root)
  log("Isolation: %d files added/modified/removed outside output_psa/ (%d locked, not verified).",
      n_changed, sum(grepl("UNREADABLE", iso$status)))

  #.. 8. Manifest ............................................................
  n_success <- md[draw_id >= 1L, uniqueN(draw_id)]
  manifest <- list(
    schema_version = PSA_SCHEMA_VERSION, run_id = run_id, run_mode = cfg$mode,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), seed = cfg$seed, rng = draws$meta$rng,
    draw_column_order = draws$meta$column_order,
    n_mc_draws_requested = cfg$n_draws, n_mc_draws_successful = n_success, draw_ids = range(all_ids),
    point_estimate = "draw_id 0 = deterministic 06 result (all sampled parameters at their model values)",
    interval = "2.5th-97.5th percentiles (quantile type 7) of matched draws 1..N; draw 0 excluded",
    interval_label = label$short, interval_label_long = label$long, interval_tier = label$tier,
    reportable = label$reportable,
    min_report_draws = cfg$min_report_draws,
    parameter_scope = paste0("BP and statin intervention-effect parameter uncertainty only: 15 BPLTTC 2021 ",
                             "Appendix S6 HRs and 4 statin RRs (CTT); coverage, targets, adherence, epidemiology, ",
                             "calibration and secular trends fixed. Not total uncertainty."),
    correlation_mode = cfg$correlation_mode,
    statin_points = as.list(sset$points), statin_sdlog = as.list(setNames(st$sdlog, st$model_variable)),
    reporting_window = c(PSA_YEAR_START, PSA_YEAR_END), n_years = PSA_N_YEARS,
    validation_all_pass = val_pass, n_validation_checks = if (is.null(val)) 0L else nrow(val[!is.na(pass)]),
    isolation_changes_outside_output_psa = n_changed,
    n_countries = length(inp$locs), countries_subset = !is.null(cfg$countries),
    regions = as.list(setNames(region_map[, .N, by = who_region]$N, region_map[, .N, by = who_region]$who_region)),
    validation_countries = cfg$validation_countries, scenarios = PSA_SCENARIOS, reported = PSA_REPORTED,
    script06 = script06_rel, script06_md5 = script06_md5, run_args_06 = inp$run_args[setdiff(names(inp$run_args), "non_constant")],
    n_workers = n_workers, block_size = cfg$block_size, n_blocks = length(blocks),
    blocks = rbindlist(block_status), cfg_hash = cfg_hash,
    timings_sec = timings, total_runtime_sec = as.numeric(difftime(Sys.time(), t_start, units = "secs")),
    git_head = git_head, git_status_before = git_status_before, r_version = R.version.string,
    input_md5 = snap_before[path %in% PSA_CHECKPOINT_DEPS, setNames(as.list(md5), path)],
    warnings_05 = inp$warnings_05)
  psa_write_lines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA),
                  out("run_manifest.json"), psa_root)
  if (n_changed) stop("Files outside output_psa/ changed during the PSA run; see ", out("isolation_report.csv"))
  if (isFALSE(val_pass)) stop("Validation failed; documents are not rendered. See ", out("validation_log.csv"))

  #.. 9. Documents ...........................................................
  if (cfg$render) {
    t0 <- Sys.time()
    psa_render_documents(run_dir, paths, log)
    snap_final <- psa_snapshot(paths)
    iso2 <- psa_isolation_report(snap_before, snap_final)
    n2 <- nrow(iso2[status %in% c("ADDED", "MODIFIED", "REMOVED")])
    psa_fwrite(if (nrow(iso2)) iso2 else data.table(status = "no changes outside output_psa/"),
               out("isolation_report_after_render.csv"), psa_root)
    log("Rendering: %.0f s; isolation after rendering: %d files changed outside output_psa/.",
        as.numeric(difftime(Sys.time(), t0, units = "secs")), n2)
    if (n2) stop("Rendering changed files outside output_psa/.")
  }
  log("Done in %.1f min.", as.numeric(difftime(Sys.time(), t_start, units = "mins")))
  invisible(list(run_dir = run_dir, manifest = manifest, summary = sm))
}

# Reference-path test draws: the first two MC draws plus the draws with the most
# extreme statin case-fatality and BP IHD effects (always including draw 0).
psa_reference_draw_ids <- function(draws, n_draws) {
  if (n_draws < 1L) return(0L)
  s <- draws$statin[draw_id >= 1L]
  b <- draws$bp[draw_id >= 1L & param_id == "S6_IHD_140_149"]
  ids <- c(1L, 2L, s[which.max(rr_cf_istroke), draw_id], s[which.min(rr_ir_ihd), draw_id],
           b[which.max(hr5), draw_id])
  sort(unique(as.integer(ids[ids <= n_draws])))
}

#...........................................................
# Rendering (report and deck, from the run's CSV files only) ----
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

PSA_RENDER_TOKEN <- "rendered-by-09_uncertainty_psa"

psa_render_documents <- function(run_dir, paths = psa_paths(psa_find_repo_root()),
                                 log = function(fmt, ...) message(sprintf(fmt, ...))) {
  psa_root <- paths$psa_root
  man <- fromJSON(file.path(run_dir, "run_manifest.json"))
  psa_find_pandoc()
  src <- file.path(paths$root, "scenarios_psa", "scenarios_aim1")
  src_before <- list.files(src, all.files = TRUE, no.. = TRUE)
  out_dir <- psa_out(run_dir, "rendered")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  report_file <- paste0("aim1_report_psa_", man$run_id, ".html")
  deck_file <- paste0("aim1_executive_slides_psa_", man$run_id, ".pdf")
  # The repository path contains spaces ("OneDrive - UW"), which LaTeX cannot
  # resolve; both documents are compiled in a private copy inside R's session
  # temp directory and only the final HTML/PDF are copied into the run folder.
  work <- file.path(tempdir(), paste0("psa_render_", man$run_id))
  unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  if (grepl(" ", work, fixed = TRUE)) stop("R's temp directory contains spaces; set TMPDIR to a path without spaces.")
  stopifnot(file.copy(file.path(src, c("aim1_report_psa.Rmd", "aim1_executive_slides_psa.Rmd")), work),
            file.copy(file.path(paths$root, "scenarios", "scenarios_aim1", "beamer_preamble.tex"), work))
  # The documents read only the run's CSV/JSON files, copied next to them.
  data_files <- c("run_manifest.json", "parameter_register.csv", "parameter_draws.csv", "mortality_draws.csv",
                  "mortality_summary.csv", "table_global_interventions.csv",
                  "table_who_region_all_interventions.csv", "validation_log.csv", "transition_checks.csv",
                  "manuscript_comparison.csv", "isolation_report.csv", "isolation_diagnosis.md")
  data_files <- data_files[file.exists(file.path(run_dir, data_files))]
  dir.create(file.path(work, "data"))
  stopifnot(file.copy(file.path(run_dir, data_files), file.path(work, "data")))
  rmarkdown::render(file.path(work, "aim1_report_psa.Rmd"), output_dir = work, intermediates_dir = work,
                    knit_root_dir = work, params = list(data_dir = "data", render_token = PSA_RENDER_TOKEN),
                    envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
  stopifnot(file.copy(file.path(work, "aim1_report_psa.html"), file.path(out_dir, report_file), overwrite = TRUE))
  log("Rendered %s", file.path(out_dir, report_file))
  banner <- sprintf(paste0("\\AddToShipoutPictureFG{\\AtPageUpperLeft{\\makebox[\\paperwidth][r]{",
                           "\\raisebox{-\\height}{\\setlength{\\fboxsep}{1.2pt}\\colorbox{%s}{",
                           "\\textcolor{white}{\\bfseries\\scriptsize\\ %s\\ }}}}}}"),
                    if (isTRUE(man$reportable)) "black!60" else "red!85!black",
                    switch(if (is.null(man$interval_tier)) "preliminary" else man$interval_tier,
                           final = sprintf("PSA --- 95\\%% UI from %s draws; effect-parameter uncertainty only",
                                           format(man$n_mc_draws_successful, big.mark = ",")),
                           preliminary = sprintf("PRELIMINARY --- 95\\%% UI from %d draws", man$n_mc_draws_successful),
                           sprintf("TECHNICAL CHECK --- %d draws; not for reporting", man$n_mc_draws_successful)))
  writeLines(c("% Generated by 09_uncertainty_psa.R", "\\usepackage{eso-pic}", banner),
             file.path(work, "psa_deck_header.tex"))
  rmarkdown::render(file.path(work, "aim1_executive_slides_psa.Rmd"), output_dir = work,
                    intermediates_dir = work, knit_root_dir = work,
                    params = list(data_dir = "data", render_token = PSA_RENDER_TOKEN),
                    output_options = list(includes = list(in_header = c("beamer_preamble.tex",
                                                                        "psa_deck_header.tex"))),
                    envir = new.env(parent = globalenv()), clean = TRUE, quiet = TRUE)
  stopifnot(file.copy(file.path(work, "aim1_executive_slides_psa.pdf"), file.path(out_dir, deck_file), overwrite = TRUE))
  log("Rendered %s", file.path(out_dir, deck_file))
  src_after <- list.files(src, all.files = TRUE, no.. = TRUE)
  if (!setequal(src_before, src_after)) {
    stop("Rendering left files in scenarios_psa/: ", paste(setdiff(src_after, src_before), collapse = ", "))
  }
  invisible(file.path(out_dir, c(report_file, deck_file)))
}

#...........................................................
# Execute ----
#...........................................................

if (isTRUE(getOption("who_cvd.execute_09", TRUE)) && !interactive()) {
  psa_main(psa_parse_cli())
}
