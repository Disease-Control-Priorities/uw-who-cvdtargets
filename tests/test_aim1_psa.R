# =============================================================================
# tests/test_aim1_psa.R -- fixture tests for the Aim 1 PSA (code/09x)
# =============================================================================
# Needs only the repository code and the BPLTTC workbook (no model inputs or
# cluster). Writes nothing except its log under output_psa/tests/.
#   Rscript tests/test_aim1_psa.R

suppressPackageStartupMessages({
  library(dplyr); library(data.table); library(readxl); library(stringr)
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

#.. Path guard ................................................................
check("path guard accepts output_psa/ paths",
      identical(psa_assert_output_path(file.path(paths$psa_root, "runs", "x.rds"), paths$psa_root),
                file.path(paths$psa_root, "runs", "x.rds")))
check("path guard rejects output/", errors_with(
  psa_assert_output_path(file.path(root, "output", "x.rds"), paths$psa_root), "outside output_psa"))
check("path guard rejects data/processed", errors_with(
  psa_assert_output_path(file.path(root, "data", "processed", "x.csv"), paths$psa_root), "outside output_psa"))
check("path guard rejects '..' traversal", errors_with(
  psa_assert_output_path(file.path(paths$psa_root, "..", "output", "x.rds"), paths$psa_root), "\\.\\."))
check("path guard rejects a sibling prefix (output_psa_old/)", errors_with(
  psa_assert_output_path(paste0(paths$psa_root, "_old/x.rds"), paths$psa_root), "outside output_psa"))

#.. Guarded delete (09) ......................................................
for (e in parse(file.path(root, "code", "09_uncertainty_psa.R"), keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("psa_remove"))) {
    eval(e, globalenv())
  }
}
check("guarded delete refuses a file outside output_psa/ and deletes nothing", {
  f <- tempfile(fileext = ".txt"); writeLines("x", f)
  refused <- errors_with(psa_remove(f, paths$psa_root), "outside output_psa")
  kept <- file.exists(f); unlink(f)
  refused && kept
})

#.. Static write scan ........................................................
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
           "statin_start_year = 2026, statin_target_year = 2030, adherence_ir = 0.9, adherence_cf = 0.8, ",
           "saltmet = 'percent', salteff = 0, saltyear1 = 2026, saltyear2 = 2030, tfa_target_tfa = 0, ",
           "tfa_policy_start_year = 2028) }")))
  idx <- which(vapply(fake, function(e) any(grepl("%dopar%", deparse(e), fixed = TRUE)), logical(1)))
  a <- psa_06_run_args(list(body = fake, foreach_idx = idx))
  a$adherence_ir == 0.9 && a$adherence_cf == 0.8 && a$statin_target_year == 2030 &&
    is.null(a$baseline_statin_coverage)
})
check("06 executed run call parsed from the resolved 06 script (all required arguments)", {
  a <- psa_06_run_args(p06)
  all(c("statin_target_coverage", "statin_start_year", "statin_target_year", "adherence_ir",
        "adherence_cf", "saltmet", "salteff", "saltyear1", "saltyear2", "tfa_target_tfa",
        "tfa_policy_start_year") %in% names(a)) && a$salteff == 0
})
check("06 script = the one 00_run_model.R sources, unless script06 is given", {
  auto <- psa_06_file(paths)
  old <- options(who_cvd.psa_script06 = "code/05_build_baseline.R")
  over <- tryCatch(psa_06_file(paths), finally = options(old))
  grepl("^06_run_scenarios_", basename(auto)) && basename(over) == "05_build_baseline.R" &&
    identical(psa_06_file(paths), auto)
})

#.. Workbook reconstruction ..................................................
s6 <- psa_read_appendix_s6(paths$wd_data)
wb <- suppressMessages(psa_read_sheet1(paths$wd_data))
d10 <- psa_generate_draws(s6, 10L, 20260923L)
sh <- psa_build_sheet1_draws(d10, s6)
check("15 sampled Appendix S6 parameters (IHD/Stroke/HF x 130-139..>=170)",
      sum(s6$sampled) == 15L && all(s6[sampled == TRUE, stratum] %in% PSA_SAMPLED_STRATA))
check("draw-0 Sheet1 rebuild equals the workbook bit-for-bit (40 rows x 5 columns)",
      all(psa_check_sheet1_reconstruction(psa_sheet1_for_draw(sh, 0L), wb, tol = 0)$pass))
check("Sheet1 column C equals Appendix S6 HR5^2 for mapped rows", {
  x <- psa_sheet1_for_draw(sh, 0L)
  m <- merge(x, wb[, .(cause, bp_cat, rr_wb = rr_per_10mmhg)], by = c("cause", "bp_cat"))
  max(abs(m$rr_per_10mmhg - m$rr_wb)) <= 1e-15
})

#.. Draws ....................................................................
check("draws are reproducible for a fixed seed",
      identical(d10$hr5, psa_generate_draws(s6, 10L, 20260923L)$hr5))
check("draw ids are stable: first 10 draws identical when n = 10 or 500",
      identical(d10$hr5, psa_generate_draws(s6, 500L, 20260923L)[draw_id <= 10L, hr5]))
check("draw 0 carries the exact point HRs", {
  z <- d10[draw_id == 0L][s6[, .(param_id, HR_per_5mmHg)], on = "param_id", nomatch = 0L]
  identical(z$hr5, z$HR_per_5mmHg)
})
check("sdlog = (log U - log L) / 3.92 (e.g. IHD 130-139: 0.96 (0.88-1.04))",
      abs(s6[param_id == "S6_IHD_130_139", sdlog] - (log(1.04) - log(0.88)) / 3.92) < 1e-15)
check("log-scale moments match meanlog/sdlog (20,000 draws)", {
  big <- psa_generate_draws(s6, 20000L, 7L)[draw_id > 0L]
  mm <- big[, .(dm = abs(mean(log_hr5) - meanlog[1]) / sdlog[1], rs = sd(log_hr5) / sdlog[1]), by = param_id]
  all(mm$dm < 0.03) && all(abs(mm$rs - 1) < 0.03)
})
check("comonotone-within-stratum mode shares z across outcomes of a stratum", {
  cw <- psa_generate_draws(s6, 5L, 1L, "comonotone_within_stratum")[draw_id > 0L]
  cw[, uniqueN(z), by = .(draw_id, stratum)][, all(V1 == 1L)]
})

#.. Shared-draw mappings ....................................................
x3 <- sh[draw_id == 3L]
check("Stroke draw shared by istroke and hstroke",
      isTRUE(all.equal(x3[cause == "istroke", -"cause"], x3[cause == "hstroke", -"cause"], tolerance = 0)))
check("Heart-failure draw shared by hhd and the aod placeholder rows",
      isTRUE(all.equal(x3[cause == "hhd", -"cause"], x3[cause == "aod", -"cause"], tolerance = 0)))
check(">=170 draw shared by 170-179 and 180+",
      identical(x3[bp_cat == "170-179", rr_per_10mmhg], x3[bp_cat == "180+", rr_per_10mmhg]))
check("R_DM = RR10[130-139] x R_noDM in every hypertensive bin", {
  r130 <- x3[bp_cat == "130-139", .(cause, r130 = rr_per_10mmhg)]
  y <- r130[x3[bp_cat %in% PSA_HYP_BINS], on = "cause"]
  identical(y$cummulative_rr_diabetes, y$cummulative_rr_nodiabetes * y$r130)
})
check("unsampled strata (<120, 120-129) keep point values in every draw",
      sh[bp_cat %in% c("<120", "120-129"), uniqueN(rr_per_10mmhg), by = .(cause, bp_cat)][, all(V1 == 1L)])
check("sodium RR table: 06 mapping leaves bp_cat NA for 16 of 28 rows (defect D1 pattern)", {
  csv <- fread(file.path(paths$wd_data, PSA_BP_SODIUM_CSV))
  r <- psa_build_sodium_rr10_draws(d10, s6, csv)
  r[draw_id == 0L, sum(is.na(bp_cat))] == 16L && r[, .N, by = draw_id][, all(N == 28L)]
})

#.. BP split: batched vs 06's own function on a synthetic country ...........
fns <- psa_load_06_functions(paths)
check("06 function environment holds no data objects", isTRUE(psa_assert_no_sensitive_globals(fns)))
set.seed(11)
grid <- CJ(location = "Testland", year = PSA_YEARS, age = PSA_AGES, sex = PSA_SEXES,
           cause = PSA_CAUSES, bp_cat = PSA_BP_BINS)
grid[, prob := runif(.N)][, prob := prob / sum(prob), by = .(year, age, sex, cause)]
grid[, IR := runif(1, 1e-4, 2e-2), by = .(year, age, sex, cause)]
grid[, IR_bin := IR * exp(rnorm(.N, 0, 0.3))]
grid[, IR_bin := IR_bin * IR / sum(IR_bin * prob), by = .(year, age, sex, cause)]
grid[, `:=`(CF = 0.05, BG.mx = 0.01, BG.mx.all = 0.012, PREVt0 = 0.02, DIS.mx.t0 = 0.001,
            Nx = 1e4, ALL.mx = 0.013)]
dage <- unique(grid[, .(location, year, age, sex, bp_cat)])
dage[, diabetes_share_among_htn_assumed_age_specific := 0.05 + 0.3 * (age - 20) / 75]
tr <- rbindlist(lapply(PSA_SEXES, function(s) data.table(
  scenario_id = "bp_combined", location = "Testland", sex = s,
  subgroup = c("htn_no_diabetes", "htn_diabetes"), baseline_year = 2025L,
  scaleup_start_year = 2026L, target_year = 2030L, baseline_control = c(0.15, 0.30),
  target_control = c(0.40, 0.70), diabetes_share_among_htn_assumed = 0.2)))
check("06 split without an explicit draw table cannot fall through to a global ETIHAD_RR_BIN",
      errors_with(capture.output(fns$calculate_antihypertensive_split(
        intervention_rates = grid, Country = "Testland", DT.in = NULL, dt_gbd_rr = NULL,
        target_rows = tr, bp_baseline = grid, diabetes_age_prepared = dage)), "ETIHAD_RR_BIN"))
bpc <- psa_bp_bin_context("Testland", tr, TRUE, TRUE, grid, dage, fns)
E <- psa_effect_arrays(sh, 0:3)
eb <- psa_bp_eff_ir_batch(bpc, E, fns)
ref_eff <- sapply(0:3, function(id) {
  r <- suppressMessages(capture.output(z <- fns$calculate_antihypertensive_split(
    intervention_rates = grid, Country = "Testland", DT.in = NULL, dt_gbd_rr = NULL,
    target_rows = tr, etihad_rr_table = psa_sheet1_for_draw(sh, id),
    bp_baseline = grid, diabetes_age_prepared = dage)))
  as.vector(psa_to_matrix(z, "eff_ir"))
})
check("batched BP eff_ir equals 06 calculate_antihypertensive_split() for draws 0-3 (bit-for-bit)",
      identical(unname(eb), unname(ref_eff)))
check("BP effects differ across draws (sampled pathway is live)", max(abs(eb[, 2] - eb[, 1])) > 0)
zero <- copy(d10[draw_id == 0L])[, hr5 := 1]
Ez <- psa_effect_arrays(psa_build_sheet1_draws(zero, s6), 0L)
check("zero-effect draw (all HR5 = 1): eff_ir = 1 to rounding",
      max(abs(psa_bp_eff_ir_batch(bpc, Ez, fns) - 1)) <= 1e-15)
tr0 <- copy(tr)[, target_control := baseline_control]
check("zero coverage (targets = baselines): eff_ir = 1 to rounding",
      max(abs(psa_bp_eff_ir_batch(psa_bp_bin_context("Testland", tr0, TRUE, TRUE, grid, dage, fns), E, fns) - 1)) <= 1e-15)
check("subgroup mixture is additive: 1 - (1-w) e_N - w e_D",
      identical(fns$aim2_subgroup_incidence_multiplier(0.2, 0.5, 0.25), 1 - 0.75 * 0.2 - 0.25 * 0.5))

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
st <- psa_strata()
S <- nrow(st); Y <- length(PSA_YEARS); B <- 3L
set.seed(5)
mk <- function(lo, hi) matrix(runif(S * Y, lo, hi), S, Y)
ctx <- list(S = S, Y = Y, strata = st,
            rates = list(BG = mk(0.001, 0.2), BGall = mk(0.002, 0.25), covid = mk(0, 0.01)))
Nx <- mk(1e3, 1e5); PREV <- mk(0, 0.2); DIS <- mk(0, 0.02)
ctx$init <- list(sick = Nx * PREV, dead = Nx * DIS, well = Nx * (1 - (PREV + ctx$rates$BG)),
                 pop = Nx, allmx = Nx * DIS + Nx * ctx$rates$BG, newcases = matrix(0, S, Y))
IR3 <- array(runif(S * Y * B, 0, 0.05), c(S, Y, B)); CF2 <- mk(0, 0.3)
eng <- psa_transitions(ctx, IR3, CF2)
same <- vapply(seq_len(B), function(b) {
  dt <- data.table(s = rep(st$s, Y), year = rep(PSA_YEARS, each = S))
  dt <- st[dt, on = "s"]
  dt[, `:=`(location = "T", intervention = "x", IR = as.vector(IR3[, , b]), CF = as.vector(CF2),
            BG.mx = as.vector(ctx$rates$BG), BG.mx.all = as.vector(ctx$rates$BGall),
            covid.mx = as.vector(ctx$rates$covid), well = as.vector(ctx$init$well),
            sick = as.vector(ctx$init$sick), dead = as.vector(ctx$init$dead),
            pop = as.vector(ctx$init$pop), all.mx = as.vector(ctx$init$allmx), newcases = 0)]
  setorder(dt, sex, location, cause, age)
  r <- ref_loop(dt)
  r <- r[order(match(sex, PSA_SEXES), match(cause, PSA_CAUSES), age, year)]
  g <- function(v) as.vector(aperm(v[, , b, drop = FALSE][, , 1], c(2, 1)))
  identical(r$well, g(eng$well)) && identical(r$sick, g(eng$sick)) && identical(r$dead, g(eng$dead)) &&
    identical(r$pop, g(eng$pop)) && identical(r$all.mx, g(eng$allmx)) && identical(r$newcases, g(eng$newcases))
}, logical(1))
check("batched transition engine equals 06's data.table loop bit-for-bit (3 draws, random rates)", all(same))

#.. Quantiles and within-draw estimands ......................................
fx <- data.table(draw_id = 0:10, v = c(5.5, 1:10))
q <- psa_summarise(fx, character(), "v")
check("interval = type-7 2.5%/97.5% of draws 1..N; point = draw 0",
      abs(q$lower - 1.225) < 1e-12 && abs(q$upper - 9.775) < 1e-12 && q$point == 5.5 && q$n_draws == 10L)
check("degenerate interval detected when all draws are identical",
      psa_summarise(data.table(draw_id = 0:10, v = 3), character(), "v")$degenerate)
check("differences are formed within draw before quantiles (not by subtracting bounds)", {
  set.seed(3); b <- 100 + rnorm(11); i <- b - 5 - 0.1 * rnorm(11)
  dd <- data.table(draw_id = 0:10, base = b, int = i)[, averted := base - int]
  s <- psa_summarise(dd, character(), c("base", "int", "averted"))
  w_within <- s[variable == "averted", upper - lower]
  w_naive <- s[variable == "base", upper] - s[variable == "int", lower] -
             (s[variable == "base", lower] - s[variable == "int", upper])
  w_within < w_naive / 5
})
check("order-statistic CI brackets the percentile estimate",
      { x <- sort(rnorm(1000)); ci <- psa_order_stat_ci(x, 0.025); q <- quantile(x, 0.025, type = 7)
        ci[1] <= q && q <= ci[2] })

#.. Report ASMR helper ........................................................
check("ASMR weights sum to 1 over ages 20-95", abs(sum(psa_who_std()$weight) - 1) < 1e-15)

#.. Log ........................................................................
res <- rbindlist(results)
cat(sprintf("\n%d/%d tests passed.\n", sum(res$pass), nrow(res)))
log_path <- psa_out(paths$psa_root, "tests", paste0("test_aim1_psa_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
psa_fwrite(res, log_path, paths$psa_root)
if (!all(res$pass)) quit(status = 1)
