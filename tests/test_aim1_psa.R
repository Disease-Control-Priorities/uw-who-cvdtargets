# =============================================================================
# tests/test_aim1_psa.R -- fixture tests for the Aim 1 PSA (code/09x, schema 2.0)
# =============================================================================
# Needs only the repository code and the BPLTTC workbook (no model inputs or
# cluster); the WHO-region test runs only if data/raw/who-regions.csv exists.
# Writes nothing except its log under output_psa/tests/.
#   Rscript tests/test_aim1_psa.R

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(readxl)
})
options(who_cvd.execute_09 = FALSE)
root <- local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "uw-who-cvdtargets.Rproj"))) {
    if (identical(dirname(d), d)) stop("Run from inside the repository.")
    d <- dirname(d)
  }
  d
})
for (f in c("091_psa_parameters.R", "092_psa_model.R", "093_psa_downstream.R", "094_psa_validate.R")) {
  sys.source(file.path(root, "code", f), envir = globalenv())
}
paths <- psa_paths(root)

results <- list()
check <- function(name, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  results[[length(results) + 1L]] <<- data.table(test = name, pass = ok)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", name))
}
errors_with <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
  grepl(pattern, msg)
}
quiet <- function(expr) { sink(tempfile()); on.exit(sink()); force(expr) }

# Functions defined in the runner (09) that the tests need, without executing it.
for (e in parse(file.path(root, "code", "09_uncertainty_psa.R"), keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("psa_remove", "psa_interval_label")) {
    eval(e, globalenv())
  }
}

#.. Path guard and guarded delete ............................................
check("path guard accepts output_psa/ paths",
      identical(psa_assert_output_path(file.path(paths$psa_root, "run", "x.csv"), paths$psa_root),
                file.path(paths$psa_root, "run", "x.csv")))
check("path guard rejects output/", errors_with(
  psa_assert_output_path(file.path(root, "output", "x.csv"), paths$psa_root), "outside output_psa"))
check("path guard rejects data/processed", errors_with(
  psa_assert_output_path(file.path(root, "data", "processed", "x.csv"), paths$psa_root), "outside output_psa"))
check("path guard rejects '..' traversal", errors_with(
  psa_assert_output_path(file.path(paths$psa_root, "..", "output", "x.csv"), paths$psa_root), "\\.\\."))
check("path guard rejects a sibling prefix (output_psa_old/)", errors_with(
  psa_assert_output_path(paste0(paths$psa_root, "_old/x.csv"), paths$psa_root), "outside output_psa"))
check("guarded delete refuses a file outside output_psa/ and deletes nothing", {
  f <- tempfile(fileext = ".txt"); writeLines("x", f)
  refused <- errors_with(psa_remove(f, paths$psa_root), "outside output_psa")
  kept <- file.exists(f); unlink(f)
  refused && kept
})

#.. Static write scan and 06 parsing .........................................
check("write scan flags saveRDS / sink / cat(file=)",
      setequal(psa_scan_writes(parse(text = "saveRDS(x, f); sink(f); cat('a', file = f); cat('b')")),
               c("saveRDS", "sink", "cat(file=)")))
check("05 contains no write call other than setwd",
      identical(psa_scan_writes(parse(file.path(paths$wd_code, "05_build_baseline.R"))), "setwd"))
p06 <- psa_parse_06(paths)
check("06 setup statements (before the cluster) contain no write calls",
      length(psa_scan_writes(p06$body[p06$setup_idx])) == 0L)
check("06 %dopar% block is detected as writing (never evaluated by the PSA)",
      all(c("saveRDS", "sink") %in% psa_scan_writes(p06$body[p06$foreach_idx])))
check("06 run-call parser reads the %dopar% call, not an unused parameter block", {
  fake <- parse(text = c(
    "adherence_ir <- 0.575; statin_target_year <- 2050",
    paste0("res <- foreach(i = locs) %dopar% { run_multiple_scenarios(x, statin_target_coverage = 0.5, ",
           "statin_start_year = 2026, statin_target_year = 2030, adherence_ir = 0.9, adherence_cf = 0.8) }")))
  idx <- which(vapply(fake, function(e) any(grepl("%dopar%", deparse(e), fixed = TRUE)), logical(1)))
  a <- psa_06_run_args(list(body = fake, foreach_idx = idx))
  a$adherence_ir == 0.9 && a$adherence_cf == 0.8 && a$statin_target_year == 2030 &&
    is.null(a$baseline_statin_coverage)
})
run_args <- psa_06_run_args(p06)
check("06 executed run call: statin coverage/years/adherence parsed; no sodium/TFA arguments",
      all(c("statin_target_coverage", "statin_start_year", "statin_target_year", "adherence_ir",
            "adherence_cf") %in% names(run_args)) &&
        !any(c("salteff", "saltmet", "tfa_target_tfa") %in% names(run_args)))
check("06 script = the one 00_run_model.R sources, unless script06 is given", {
  auto <- psa_06_file(paths)
  old <- options(who_cvd.psa_script06 = "code/05_build_baseline.R")
  over <- tryCatch(psa_06_file(paths), finally = options(old))
  grepl("^06_run_scenarios_", basename(auto)) && basename(over) == "05_build_baseline.R" &&
    identical(psa_06_file(paths), auto)
})
fns <- psa_load_06_functions(paths)
check("06 function environment holds no data objects", isTRUE(psa_assert_no_sensitive_globals(fns)))
check("06 scenarios are the six PSA scenarios (no sodium/TFA interventions)", {
  sc <- NULL
  for (e in p06$body) if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("scenarios"))) sc <- eval(e[[3]])
  identical(names(sc), PSA_SCENARIOS) && !any(c("sodium", "tfa") %in% unlist(sc)) &&
    setequal(sc$all_interventions, c("antihypertensive_no_diabetes", "antihypertensive_diabetes", "statins"))
})

#.. Statin parameters ........................................................
sset <- psa_06_statin_settings(fns)
st <- psa_statin_param_table(sset$points)
check("statin point RRs parsed from 06: 0.74 / 0.80 / 0.80 / 0.96",
      identical(unname(sset$points), c(0.74, 0.80, 0.80, 0.96)) && identical(names(sset$points), PSA_STATIN_IDS))
check("register refuses statin points that differ from the documented model values",
      errors_with(psa_statin_param_table(c(rr_ir_ihd = 0.76, rr_ir_istroke = 0.8, rr_cf_ihd = 0.8,
                                           rr_cf_istroke = 0.96)), "differ"))
check("statin sdlog = (log U - log L)/(2 qnorm((1+c)/2)); rr_cf_ihd uses its 99% CI",
      abs(st[model_variable == "rr_cf_ihd", sdlog] - (log(0.87) - log(0.74)) / (2 * qnorm(0.995))) < 1e-15 &&
        abs(st[model_variable == "rr_ir_istroke", sdlog] - (log(0.86) - log(0.75)) / (2 * qnorm(0.975))) < 1e-15)
check("statin distributions are centred on the retained model values (not the source points)",
      identical(st$meanlog, log(st$model_point)) && st[model_variable == "rr_ir_ihd", source_point] == 0.76)
check("prop_athero_stroke parsed from project.all() = 0.60", identical(sset$prop_athero, 0.6))

#.. Workbook reconstruction and draws ........................................
s6 <- psa_read_appendix_s6(paths$wd_data)
wb <- suppressMessages(psa_read_sheet1(paths$wd_data))
d10 <- psa_generate_draws(s6, st, 10L, 20260923L)
sh <- psa_build_sheet1_draws(d10$bp, s6)
check("15 sampled Appendix S6 parameters (IHD/Stroke/HF x 130-139..>=170)",
      sum(s6$sampled) == 15L && all(s6[sampled == TRUE, stratum] %in% PSA_SAMPLED_STRATA))
check("draw-0 Sheet1 rebuild equals the workbook bit-for-bit (40 rows x 5 columns)",
      all(psa_check_sheet1_reconstruction(psa_sheet1_for_draw(sh, 0L), wb, tol = 0)$pass))
check("draws are reproducible for a fixed seed (BP and statin)", {
  d2 <- psa_generate_draws(s6, st, 10L, 20260923L)
  identical(d10$bp$hr5, d2$bp$hr5) && identical(d10$statin, d2$statin)
})
check("draw ids are stable: draws 1-10 identical when N = 10 or 1000", {
  big <- psa_generate_draws(s6, st, 1000L, 20260923L)
  identical(d10$bp$hr5, big$bp[draw_id <= 10L, hr5]) && identical(d10$statin, big$statin[draw_id <= 10L])
})
check("draw 0 carries the exact point values (BP HRs and statin RRs)", {
  z <- d10$bp[draw_id == 0L]
  identical(z$hr5, z$hr5_point) &&
    identical(unname(unlist(d10$statin[draw_id == 0L, ..PSA_STATIN_IDS])), unname(sset$points))
})
check("BP sdlog = (log U - log L) / 3.92 (e.g. IHD 130-139: 0.96 (0.88-1.04))",
      abs(s6[param_id == "S6_IHD_130_139", sdlog] - (log(1.04) - log(0.88)) / 3.92) < 1e-15)
big <- psa_generate_draws(s6, st, 20000L, 7L)
check("log-scale moments match meanlog/sdlog (20,000 draws; BP and statin)", {
  b <- big$bp[draw_id > 0L][, .(dm = abs(mean(log(hr5)) - meanlog[1]) / sdlog[1], rs = sd(log(hr5)) / sdlog[1]), by = param_id]
  s <- big$statin_long[draw_id > 0L][, .(dm = abs(mean(log(rr)) - meanlog[1]) / sdlog[1], rs = sd(log(rr)) / sdlog[1]), by = param_id]
  all(c(b$dm, s$dm) < 0.03) && all(abs(c(b$rs, s$rs) - 1) < 0.03)
})
check("statin RR draws above 1 are kept (no truncation): rr_cf_istroke > 1 in ~27% of draws",
      { p <- big$statin[draw_id > 0L, mean(rr_cf_istroke > 1)]; p > 0.24 && p < 0.30 })
check("correlation modes act on BP draws only; statin draws unchanged", {
  cw <- psa_generate_draws(s6, st, 5L, 1L, "comonotone_within_stratum")
  ind <- psa_generate_draws(s6, st, 5L, 1L)
  cw$bp[draw_id > 0L, uniqueN(z), by = .(draw_id, stratum)][, all(V1 == 1L)] && identical(cw$statin, ind$statin)
})
check("parameter_draws table: 19 parameters x (N+1) draws", {
  pt <- psa_parameter_draws_table(d10)
  nrow(pt) == 19L * 11L && pt[, uniqueN(param_id)] == 19L && !anyNA(pt$value)
})

#.. Shared-draw mappings ....................................................
x3 <- sh[draw_id == 3L]
check("Stroke draw shared by istroke and hstroke",
      isTRUE(all.equal(x3[cause == "istroke", -"cause"], x3[cause == "hstroke", -"cause"], tolerance = 0)))
check(">=170 draw shared by 170-179 and 180+",
      identical(x3[bp_cat == "170-179", rr_per_10mmhg], x3[bp_cat == "180+", rr_per_10mmhg]))
check("R_DM = RR10[130-139] x R_noDM in every hypertensive bin", {
  r130 <- x3[bp_cat == "130-139", .(cause, r130 = rr_per_10mmhg)]
  y <- r130[x3[bp_cat %in% PSA_HYP_BINS], on = "cause"]
  identical(y$cummulative_rr_diabetes, y$cummulative_rr_nodiabetes * y$r130)
})

#.. Statin pathway: parameterised 06 function and batched factors ............
grid <- CJ(location = "Testland", year = PSA_YEARS, age = PSA_AGES, sex = PSA_SEXES, cause = PSA_CAUSES)
set.seed(21)
grid[, `:=`(IR = runif(.N, 1e-4, 0.03), CF = runif(.N, 0.01, 0.3))]
mk_cin <- function(cov0) list(
  statin = data.table(location = "Testland", year = 2017:2050, statins_uptake = cov0, statins_current = cov0,
                      statins_target = 0.6, statins_uptake_lag = cov0, statins_uptake_delta_lag = 0,
                      statins_uptake_delta = 0),
  af = data.table(location = "Testland", cause = c("ihd", "istroke"), af_statins = c(0.21, NA)))
cin <- mk_cin(0.12)
rates <- copy(grid)[, `:=`(eff_ir = 1, eff_cf = 1, intervention = "baseline")]
st_args <- list(cin$statin, rates, "Testland", cin$af, adherence_ir = run_args$adherence_ir,
                adherence_cf = run_args$adherence_cf, prop_athero_stroke = 0.6,
                statin_target_coverage = run_args$statin_target_coverage,
                statin_start_year = run_args$statin_start_year, statin_target_year = run_args$statin_target_year,
                baseline_statin_coverage = 0.12)
a06 <- quiet(do.call(fns$calculate_statins_impact, st_args))
check("parameterised statin function at the point RRs is identical to 06's own (all columns)",
      identical(a06, quiet(do.call(sset$fun, c(st_args, list(statin_rr = sset$points))))))
check("parameterised statin function responds to the RR vector",
      !identical(a06$CF, quiet(do.call(sset$fun, c(st_args, list(statin_rr = sset$points * c(1, 1, 1.1, 1)))))$CF))
check("06's statin function cannot be called with a draw vector by mistake (no statin_rr argument)",
      !"statin_rr" %in% names(formals(fns$calculate_statins_impact)))
comp <- psa_statin_components("Testland", cin, run_args, sset, fns)
check("statin AF: country value used; missing value falls back to 06's default (0.1161)",
      comp$af[["ihd"]] == 0.21 && comp$af[["istroke"]] == sset$af_default[["istroke"]])
rrm <- rbind(sset$points, d10$statin[draw_id == 1L, unlist(.SD), .SDcols = PSA_STATIN_IDS],
             c(rr_ir_ihd = 0.70, rr_ir_istroke = 0.85, rr_cf_ihd = 0.75, rr_cf_istroke = 1.08))
colnames(rrm) <- PSA_STATIN_IDS
sf <- psa_statin_factors(comp, rrm)
check("batched statin factors equal the parameterised 06 function (unit table) bit-for-bit, incl. RR > 1", {
  all(vapply(seq_len(nrow(rrm)), function(b) {
    u <- psa_statin_unit_factors(sset$fun, "Testland", grid, cin, run_args, sset, statin_rr = rrm[b, ])
    all(vapply(names(u), function(v) identical(sf[[v]][, , b], u[[v]]), logical(1)))
  }, logical(1)))
})
check("batched factors at the point RRs equal 06's unmodified function", {
  u <- psa_statin_unit_factors(fns$calculate_statins_impact, "Testland", grid, cin, run_args, sset)
  all(vapply(names(u), function(v) identical(sf[[v]][, , 1L], u[[v]]), logical(1)))
})
check("statin RR > 1 increases ischaemic-stroke case fatality (factor > 1 after 2026, ages >= 40)",
      max(sf$fac_cf[psa_strata()$cause == "istroke" & psa_strata()$age >= 40, PSA_YEARS >= 2027, 3L]) > 1)
check("statins act only on ihd/istroke at ages >= 40 (rates); hhd/hstroke untouched",
      all(sf$fac_ir[psa_strata()$cause %in% c("hhd", "hstroke") | psa_strata()$age < 40, , ] == 1))
check("baseline-coverage denominator is RR-dependent: factor(rr) != linear rescaling of factor(point)", {
  es <- function(r) 1 - sf$fac_cf[which(psa_strata()$cause == "ihd" & psa_strata()$age == 60L)[1], 30L, r]
  r1 <- 1 - rrm[1, "rr_cf_ihd"]; r3 <- 1 - rrm[3, "rr_cf_ihd"]
  abs(es(3) / es(1) - r3 / r1) > 1e-6
})
check("no statin effect when baseline coverage >= target (all factors 1)", {
  c2 <- psa_statin_components("Testland", mk_cin(0.6), run_args, sset, fns)
  f2 <- psa_statin_factors(c2, rrm)
  all(vapply(f2, function(a) all(a == 1), logical(1)))
})

#.. BP split: batched vs 06's own function on a synthetic country ...........
set.seed(11)
bgrid <- CJ(location = "Testland", year = PSA_YEARS, age = PSA_AGES, sex = PSA_SEXES,
            cause = PSA_CAUSES, bp_cat = PSA_BP_BINS)
bgrid[, prob := runif(.N)][, prob := prob / sum(prob), by = .(year, age, sex, cause)]
bgrid[, IR := runif(1, 1e-4, 2e-2), by = .(year, age, sex, cause)]
bgrid[, IR_bin := IR * exp(rnorm(.N, 0, 0.3))]
bgrid[, IR_bin := IR_bin * IR / sum(IR_bin * prob), by = .(year, age, sex, cause)]
bgrid[, `:=`(CF = 0.05, BG.mx = 0.01, BG.mx.all = 0.012, PREVt0 = 0.02, DIS.mx.t0 = 0.001,
             Nx = 1e4, ALL.mx = 0.013)]
dage <- unique(bgrid[, .(location, year, age, sex, bp_cat)])
dage[, diabetes_share_among_htn_assumed_age_specific := 0.05 + 0.3 * (age - 20) / 75]
tr <- rbindlist(lapply(PSA_SEXES, function(s) data.table(
  scenario_id = "bp_combined", location = "Testland", sex = s,
  subgroup = c("htn_no_diabetes", "htn_diabetes"), baseline_year = 2025L,
  scaleup_start_year = 2026L, target_year = 2030L, baseline_control = c(0.15, 0.30),
  target_control = c(0.40, 0.70), diabetes_share_among_htn_assumed = 0.2)))
check("06 split without an explicit draw table cannot fall through to a global ETIHAD_RR_BIN",
      errors_with(capture.output(fns$calculate_antihypertensive_split(
        intervention_rates = bgrid, Country = "Testland", DT.in = NULL, dt_gbd_rr = NULL,
        target_rows = tr, bp_baseline = bgrid, diabetes_age_prepared = dage)), "ETIHAD_RR_BIN"))
bpc <- psa_bp_bin_context("Testland", tr, TRUE, TRUE, bgrid, dage, fns)
E <- psa_effect_arrays(sh, 0:3)
eb <- psa_bp_eff_ir_batch(bpc, E, fns)
ref_eff <- sapply(0:3, function(id) {
  r <- suppressMessages(capture.output(z <- fns$calculate_antihypertensive_split(
    intervention_rates = bgrid, Country = "Testland", DT.in = NULL, dt_gbd_rr = NULL,
    target_rows = tr, etihad_rr_table = psa_sheet1_for_draw(sh, id),
    bp_baseline = bgrid, diabetes_age_prepared = dage)))
  as.vector(psa_to_matrix(z, "eff_ir"))
})
check("batched BP eff_ir equals 06 calculate_antihypertensive_split() for draws 0-3 (bit-for-bit)",
      identical(unname(eb), unname(ref_eff)))
check("BP effects differ across draws (sampled pathway is live)", max(abs(eb[, 2] - eb[, 1])) > 0)
tr0 <- copy(tr)[, target_control := baseline_control]
check("zero coverage (targets = baselines): eff_ir = 1 to rounding",
      max(abs(psa_bp_eff_ir_batch(psa_bp_bin_context("Testland", tr0, TRUE, TRUE, bgrid, dage, fns), E, fns) - 1)) <= 1e-15)
check("subgroup mixture is additive: 1 - (1-w) e_N - w e_D",
      identical(fns$aim2_subgroup_incidence_multiplier(0.2, 0.5, 0.25), 1 - 0.75 * 0.2 - 0.25 * 0.5))

#.. Scenario rates: combination order and draw-varying statin CF ............
bkey <- "bp_combined|TRUE|TRUE"
bp_ctx <- c(bpc, list(eff_cf = matrix(0.97, nrow(psa_strata()), length(PSA_YEARS)),
                      eff_ir0 = matrix(as.vector(eb[, 1]), nrow(psa_strata()))))
ctx <- list(S = nrow(psa_strata()), Y = length(PSA_YEARS), draw0_col = 1L,
            rates = list(IR = psa_to_matrix(grid, "IR"), CF = psa_to_matrix(grid, "CF")),
            bp = setNames(list(bp_ctx), bkey))
sf3 <- psa_statin_factors(comp, rrm[c(1, 2, 3), ])
rt_all <- psa_scenario_rates(ctx, c("antihypertensive_no_diabetes", "antihypertensive_diabetes", "statins"),
                             "bp_combined", psa_effect_arrays(sh, 0:2), sf3, fns)
check("all_interventions: CF varies by draw (statin CF factors are per draw)",
      length(dim(rt_all$CF)) == 3L && max(abs(rt_all$CF[, , 3] - rt_all$CF[, , 1])) > 0)
check("all_interventions combination = 06 order: (CF x eff_cf_BP) x statin factor, then 0.99 cap", {
  manual <- pmin(array(ctx$rates$CF * bp_ctx$eff_cf, dim(sf3$fac_cf)) * sf3$fac_cf, 0.99)
  identical(rt_all$CF, manual)
})
rt_bp <- psa_scenario_rates(ctx, c("antihypertensive_no_diabetes", "antihypertensive_diabetes"),
                            "bp_combined", psa_effect_arrays(sh, 0:2), sf3, fns)
check("BP-only scenario ignores statin draws (CF draw-invariant)", length(dim(rt_bp$CF)) == 2L)

#.. Transition engine vs a verbatim copy of 06's loop ........................
ref_loop <- function(dt) {
  intervention_rates <- copy(dt)
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
  intervention_rates
}
stg <- psa_strata()
S <- nrow(stg); Y <- length(PSA_YEARS); B <- 3L
set.seed(5)
mk <- function(lo, hi) matrix(runif(S * Y, lo, hi), S, Y)
tctx <- list(S = S, Y = Y, strata = stg,
             rates = list(BG = mk(0.001, 0.2), BGall = mk(0.002, 0.25), covid = mk(0, 0.01)))
Nx <- mk(1e3, 1e5); PREV <- mk(0, 0.2); DIS <- mk(0, 0.02)
tctx$init <- list(sick = Nx * PREV, dead = Nx * DIS, well = Nx * (1 - (PREV + tctx$rates$BG)),
                  pop = Nx, allmx = Nx * DIS + Nx * tctx$rates$BG, newcases = matrix(0, S, Y))
IR3 <- array(runif(S * Y * B, 0, 0.05), c(S, Y, B)); CF3 <- array(runif(S * Y * B, 0, 0.3), c(S, Y, B))
eng <- psa_transitions(tctx, IR3, CF3)
same <- vapply(seq_len(B), function(b) {
  dt <- data.table(s = rep(stg$s, Y), year = rep(PSA_YEARS, each = S))
  dt <- stg[dt, on = "s"]
  dt[, `:=`(location = "T", intervention = "x", IR = as.vector(IR3[, , b]), CF = as.vector(CF3[, , b]),
            BG.mx = as.vector(tctx$rates$BG), BG.mx.all = as.vector(tctx$rates$BGall),
            covid.mx = as.vector(tctx$rates$covid), well = as.vector(tctx$init$well),
            sick = as.vector(tctx$init$sick), dead = as.vector(tctx$init$dead),
            pop = as.vector(tctx$init$pop), all.mx = as.vector(tctx$init$allmx), newcases = 0)]
  setorder(dt, sex, location, cause, age)
  r <- ref_loop(dt)
  r <- r[order(match(sex, PSA_SEXES), match(cause, PSA_CAUSES), age, year)]
  g <- function(v) as.vector(aperm(v[, , b, drop = FALSE][, , 1], c(2, 1)))
  identical(r$well, g(eng$well)) && identical(r$sick, g(eng$sick)) && identical(r$dead, g(eng$dead)) &&
    identical(r$pop, g(eng$pop)) && identical(r$all.mx, g(eng$allmx)) && identical(r$newcases, g(eng$newcases))
}, logical(1))
check("batched transition engine equals 06's data.table loop bit-for-bit (3 draws, draw-varying CF)", all(same))

#.. Estimands, summaries and tables .........................................
geo <- c(PSA_REGION_ORDER, "Global")
set.seed(9)
dth <- CJ(draw_id = 0:10, scenario_id = PSA_SCENARIOS, geography = PSA_REGION_ORDER)
dth[, deaths := fifelse(scenario_id == "baseline", 1e6 * match(geography, PSA_REGION_ORDER), 0)]
dth[, base := max(deaths), by = .(geography)]
dth[scenario_id != "baseline", deaths := base * (1 - runif(.N, 0.01, 0.08))][, base := NULL]
dth <- rbind(dth, dth[, .(geography = "Global", deaths = sum(deaths)), by = .(draw_id, scenario_id)])
md <- psa_mortality_draws(dth)
check("mortality_draws: 4 global + 6 regional rows per draw, 2026-2050, n_years = 25",
      nrow(md) == 11L * 10L && all(md$n_years == 25L & md$year_start == 2026L & md$year_end == 2050L))
check("estimands formed within draw: averted = base - proj; per year = /25; % = 100 x averted / base",
      identical(md$deaths_averted, md$baseline_deaths - md$projected_deaths) &&
        identical(md$deaths_averted_per_year, md$deaths_averted / 25) &&
        identical(md$percent_averted, 100 * md$deaths_averted / md$baseline_deaths))
check("country subset: only regions with countries get rows", {
  m2 <- psa_mortality_draws(dth, regions = c("Europe", "Americas"))
  setequal(unique(m2[geography_level == "who_region", who_region]), c("Europe", "Americas"))
})
sm <- psa_mortality_summary(md, 10L)
check("summary: point = draw 0; interval = type-7 2.5/97.5% of draws 1..N (draw 0 excluded)", {
  x <- sm[who_region == "Global" & scenario_id == "all_interventions" & metric == "deaths_averted"]
  v <- md[who_region == "Global" & scenario_id == "all_interventions"]
  x$point == v[draw_id == 0L, deaths_averted] && x$n_draws == 10L &&
    abs(x$lower - quantile(v[draw_id > 0L, deaths_averted], 0.025, type = 7, names = FALSE)) == 0 &&
    abs(x$upper - quantile(v[draw_id > 0L, deaths_averted], 0.975, type = 7, names = FALSE)) == 0
})
check("summary: fixed baseline deaths carry no interval",
      all(sm[metric == "baseline_deaths", is_fixed & is.na(lower) & is.na(upper)]))
check("summary: % averted interval from draw-level ratios, not from separately summarised endpoints", {
  x <- sm[who_region == "Europe" & metric == "percent_averted"]
  v <- md[who_region == "Europe" & draw_id > 0L, percent_averted]
  abs(x$lower - quantile(v, 0.025, type = 7, names = FALSE)) == 0
})
check("order-statistic CI brackets the percentile estimate",
      { x <- sort(rnorm(1000)); ci <- psa_order_stat_ci(x, 0.025); q <- quantile(x, 0.025, type = 7)
        ci[1] <= q && q <= ci[2] })
tg <- psa_table_global(sm); trg <- psa_table_region(sm)
check("global table: 4 interventions in order; M with 2 dp, thousands with 1 dp",
      identical(tg$Intervention, unname(PSA_SCENARIO_LABEL[PSA_REPORTED])) &&
        all(grepl("^[0-9.,]+ \\[[0-9.,]+\u2013[0-9.,]+\\]$", tg[[3]])) &&
        all(grepl("^[0-9,]+\\.[0-9]{2} ", tg[[3]])) && all(grepl("^[0-9,]+\\.[0-9] ", tg[[4]])))
check("region table: six regions in WHO-report order; baseline without interval",
      identical(trg$Region, PSA_REGION_ORDER) && !any(grepl("\\[", trg$`Baseline deaths`)))
check("interval label tiers: <100 technical, 100-999 preliminary, >=1000 final",
      psa_interval_label(10L, 100L)$tier == "technical" && psa_interval_label(100L, 100L)$tier == "preliminary" &&
        psa_interval_label(1000L, 100L)$tier == "final" && !psa_interval_label(10L, 100L)$reportable)

#.. Manuscript parser ........................................................
ms_txt <- c("BAU projected 637.1 million CVD deaths. The increment ...",
            "  Hypertension control, without diabetes                           6.4                         257                  1.0%",
            "  Statin scale-up                            11.71               0.78                       \u22120.36                        \u22120.19             11.93",
            "  Statin scale-up                              11.9   151.2   \u22121.4    149.8",
            "  Region of the Americas                            58.9                         4.4                         175                  7.4%                       49.3")
msv <- psa_manuscript_values(ms_txt)
check("manuscript parser: Findings BAU, Table 2 row, Table 3 total (U+2212 minus), Supp. Table 6 row",
      msv[metric == "baseline_deaths" & geography == "Global", manuscript] == 637.1 &&
        msv[source == "Table 2" & metric == "deaths_averted_per_year", manuscript] == 257 &&
        msv[source == "Table 3" & scenario_id == "statins_only", manuscript] == 11.93 &&
        msv[geography == "Americas" & metric == "percent_averted", manuscript] == 7.4 &&
        !nrow(msv[source == "Table 2" & scenario_id == "statins_only"]))   # 4-number Table 4 row not taken

#.. WHO regions (only with local raw data) ..................................
if (file.exists(file.path(paths$wd_raw, "who-regions.csv"))) {
  check("WHO region map: unmapped/non-WHO locations stop the run (never dropped)",
        errors_with(psa_region_map(paths, c("Colombia", "Atlantis")), "six WHO regions"))
  check("WHO region map: Colombia -> Americas, China -> Western Pacific",
        identical(psa_region_map(paths, c("Colombia", "China"))$who_region, c("Americas", "Western Pacific")))
}

#.. Log ........................................................................
res <- rbindlist(results)
cat(sprintf("\n%d/%d tests passed.\n", sum(res$pass), nrow(res)))
log_path <- psa_out(paths$psa_root, "tests", paste0("test_aim1_psa_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
psa_fwrite(res, log_path, paths$psa_root)
if (!all(res$pass)) quit(status = 1)
