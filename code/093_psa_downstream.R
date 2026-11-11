# =============================================================================
# 093_psa_downstream.R -- Aim 1 PSA: mortality reduction, regions, summaries
# =============================================================================
# Sourced by 09_uncertainty_psa.R. Defines functions only.
#
# Only mortality is summarised (no DALYs, economics or ASMR). The death
# definition and aggregation follow the deterministic report
# (scenarios/scenarios_aim1/aim1_report.Rmd): deaths = sum of the model's `dead`
# (cause-specific case-fatality deaths of the four CVD chains; background and
# COVID deaths are not part of `dead`, so shared other-cause mortality is never
# counted once per chain) over location, cause, sex, single-year age 20-95 and
# calendar years 2026-2050 inclusive. WHO regions follow the report's
# country-metadata chunk.
#
# Every estimand is formed WITHIN a draw (sums first, then differences and
# ratios); intervals are quantiles across draws 1..N of those draw-level values.

#...........................................................
# Labels and reporting constants ----
#...........................................................

PSA_SCENARIOS <- c("baseline", "bp_no_diabetes_only", "bp_diabetes_only",
                   "bp_combined", "statins_only", "all_interventions")
PSA_REPORTED <- c("bp_no_diabetes_only", "bp_diabetes_only", "statins_only", "all_interventions")
PSA_SCENARIO_LABEL <- c(baseline = "Business-as-usual",
                        bp_no_diabetes_only = "HTN Control (No Diabetes)",
                        bp_diabetes_only = "HTN Control (Diabetes)",
                        bp_combined = "HTN Control (Combined, 150M)",
                        statins_only = "Improved Statin Uptake",
                        all_interventions = "All Interventions")
PSA_WHO_TARGET <- c(
  bp_no_diabetes_only = "Target 1 (no-diabetes subgroup of the 150M split)",
  bp_diabetes_only = "Target 3: BP control in people with diabetes (0.80 ceiling); diabetes subgroup of the 150M split",
  statins_only = "Target 2: >=50% eligible adults on lipid-lowering therapy by 2030",
  all_interventions = "All WHO targets (1+2+3) combined")
PSA_REGION_ORDER <- c("Western Pacific", "South-East Asia", "Europe", "Americas",
                      "Eastern Mediterranean", "Africa")
PSA_YEAR_START <- 2026L
PSA_YEAR_END   <- 2050L
PSA_N_YEARS    <- length(PSA_YEAR_START:PSA_YEAR_END)       # 25
PSA_METRICS <- c("projected_deaths", "baseline_deaths", "deaths_averted",
                 "deaths_averted_per_year", "percent_averted")

#...........................................................
# WHO regions (aim1_report.Rmd chunk `country-metadata`) ----
#...........................................................

psa_region_map <- function(paths, locs) {
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
  countries_dt[, region := fifelse(!is.na(region_who), region_who, region)]
  hits <- countries_dt[location %in% locs, .(n = .N, n_region = uniqueN(region)), by = location]
  dup <- hits[n_region > 1L, location]
  if (length(dup)) stop("WHO region mapping is ambiguous for: ", paste(dup, collapse = ", "))
  map <- unique(countries_dt[location %in% locs, .(location, iso3, who_region = region)], by = "location")
  map <- map[data.table(location = locs), on = "location"]
  bad <- map[is.na(who_region) | !who_region %in% PSA_REGION_ORDER]
  if (nrow(bad)) {
    stop("Locations without one of the six WHO regions (resolve explicitly; none may be dropped): ",
         paste(sprintf("%s [%s]", bad$location, bad$who_region), collapse = "; "))
  }
  map[]
}

#...........................................................
# Per-country batch (inside workers) ----
#...........................................................

PSA_STATE_RES <- c(well = "well", sick = "sick", newcases = "newcases", dead = "dead",
                   pop = "pop", all.mx = "allmx")

psa_get_context <- function(Country, cin, fns, sset, shared, cache_env = NULL) {
  if (!is.null(cache_env) && exists(Country, envir = cache_env, inherits = FALSE)) {
    return(get(Country, envir = cache_env))
  }
  ctx <- psa_country_context(Country, cin, fns, sset, shared$repYear, shared$dt_gbd_rr,
                             shared$run_args, shared$scenarios, shared$htn_scenario_ids,
                             shared$sheet1_0)
  if (!is.null(cache_env)) assign(Country, ctx, envir = cache_env)
  invisible(gc(verbose = FALSE))
  ctx
}

# Draw-0 comparison of one scenario against the saved 06 output.
psa_compare_saved <- function(ctx, res, rt, saved_sc, col) {
  if (is.null(saved_sc) || nrow(saved_sc) != ctx$S * ctx$Y) {
    return(list(max_abs_states = NA_real_, max_abs_eff = NA_real_, note = "saved rows missing"))
  }
  pick <- function(a) if (length(dim(a)) == 3L) a[, , col] else a
  d_states <- max(vapply(names(PSA_STATE_RES), function(v) {
    max(abs(psa_to_matrix(saved_sc, v) - res[[PSA_STATE_RES[[v]]]][, , col]))
  }, 0))
  d_eff <- max(abs(psa_to_matrix(saved_sc, "eff_ir") - pick(rt$eff_ir)),
               abs(psa_to_matrix(saved_sc, "eff_cf") - pick(rt$eff_cf)))
  list(max_abs_states = d_states, max_abs_eff = d_eff, note = "")
}

psa_run_country <- function(Country, cin, fns, sset, shared, draw_ids, E, rrm,
                            cache_env = NULL, saved06_file = NULL, keep_long = FALSE) {
  t0 <- proc.time()[["elapsed"]]
  ctx <- psa_get_context(Country, cin, fns, sset, shared, cache_env)
  ctx$draw0_col <- match(0L, draw_ids, nomatch = 0L)
  t_ctx <- proc.time()[["elapsed"]] - t0
  B <- length(draw_ids)
  SF <- psa_statin_factors(ctx$statin, rrm, ctx$strata)
  # Per-draw statin check: the first non-zero draw of the block against 06's
  # parameterised function run on a unit table with that draw's RRs.
  nz <- which(draw_ids != 0L)
  statin_check <- NA_real_; statin_check_draw <- NA_integer_
  if (length(nz)) {
    b1 <- nz[1]
    u <- psa_statin_unit_factors(sset$fun, Country, ctx$base_rates, cin, shared$run_args, sset,
                                 statin_rr = rrm[b1, ])
    statin_check <- max(vapply(names(u), function(v) max(abs(SF[[v]][, , b1] - u[[v]])), 0))
    statin_check_draw <- draw_ids[b1]
  }
  saved <- NULL
  if (ctx$draw0_col > 0L && !is.null(saved06_file) && file.exists(saved06_file)) {
    saved <- readRDS(saved06_file); setDT(saved)
  }
  ns <- length(PSA_SCENARIOS)
  yi <- which(PSA_YEARS >= PSA_YEAR_START & PSA_YEARS <= PSA_YEAR_END)
  deaths <- matrix(NA_real_, ns, B, dimnames = list(PSA_SCENARIOS, draw_ids))
  chk <- array(0, c(ns, B, 8L), dimnames = list(PSA_SCENARIOS, draw_ids,
    c("n_ir_capped", "n_cf_capped", "clamp_sick", "clamp_pop", "clamp_allmx", "clamp_well",
      "n_nonfinite", "n_negative")))
  cmp0 <- list(); bp0 <- c(); long <- list()
  states <- c("well", "sick", "dead", "pop", "allmx", "newcases")
  for (k in seq_len(ns)) {
    sn <- PSA_SCENARIOS[k]
    iv <- shared$scenarios[[sn]]
    rt <- psa_scenario_rates(ctx, iv, shared$htn_scenario_ids[[sn]], E, SF, fns)
    res <- psa_transitions(ctx, rt$IR, rt$CF)
    deaths[k, ] <- colSums(matrix(res$dead[, yi, , drop = FALSE], ctx$S * length(yi), B))
    chk[k, , "n_ir_capped"] <- rt$diag$n_ir_capped
    chk[k, , "n_cf_capped"] <- rt$diag$n_cf_capped
    chk[k, , c("clamp_sick", "clamp_pop", "clamp_allmx", "clamp_well")] <- res$clamps
    chk[k, , "n_nonfinite"] <- Reduce(`+`, lapply(states, function(v)
      colSums(matrix(!is.finite(res[[v]]), ctx$S * ctx$Y, B))))
    chk[k, , "n_negative"] <- Reduce(`+`, lapply(states, function(v)
      colSums(matrix(res[[v]] < 0, ctx$S * ctx$Y, B), na.rm = TRUE)))
    if (!is.null(rt$diag$bp_draw0_vs_06)) bp0[sn] <- rt$diag$bp_draw0_vs_06
    if (ctx$draw0_col > 0L && !is.null(saved)) {
      cmp0[[sn]] <- as.data.table(psa_compare_saved(ctx, res, rt, saved[scenario == sn], ctx$draw0_col))
    }
    if (keep_long) {
      long[[sn]] <- psa_batch_to_long(ctx, res[states], rt[c("eff_ir", "eff_cf")], sn,
                                      psa_intervention_label(iv), draw_ids)
    }
    rm(res, rt)
  }
  cc <- data.table(location = Country, statin_check0 = ctx$statin_check0,
                   statin_check_perturbed = statin_check, statin_check_draw = statin_check_draw,
                   bp_draw0_vs_06 = if (length(bp0)) max(bp0) else NA_real_,
                   t_context = t_ctx, t_total = proc.time()[["elapsed"]] - t0)
  d0 <- if (length(cmp0)) rbindlist(cmp0, idcol = "scenario")[, location := Country][] else NULL
  if (ctx$draw0_col > 0L && is.null(saved)) {
    d0 <- data.table(scenario = NA_character_, max_abs_states = NA_real_, max_abs_eff = NA_real_,
                     note = "saved 06 output not found", location = Country)
  }
  list(deaths = deaths, checks = chk, country_checks = cc, draw0_vs_06 = d0,
       long = if (keep_long) rbindlist(long) else NULL)
}

#...........................................................
# Worker task: this worker's countries for one block of draws ----
#...........................................................

psa_worker_task <- function(task) {
  fns <- get(".psa_fns", envir = globalenv())
  sset <- get(".psa_sset", envir = globalenv())
  wk <- get(".psa_worker", envir = globalenv())
  B <- length(task$draw_ids)
  E <- psa_effect_arrays(task$sheet1_block, task$draw_ids)
  rrm <- psa_statin_matrix(task$statin_block, task$draw_ids)
  R <- length(PSA_REGION_ORDER); ns <- length(PSA_SCENARIOS)
  reg <- array(0, c(R + 1L, ns, B))          # 6 regions + Global (summed over countries directly)
  chk <- NULL
  cc <- list(); d0 <- list(); done <- character()
  cache_env <- if (isTRUE(task$cache_contexts)) wk$ctx_cache else NULL
  for (Country in wk$countries) {
    r <- psa_run_country(Country, wk$cin[[Country]], fns, sset, wk$shared, task$draw_ids, E, rrm,
                         cache_env = cache_env, saved06_file = wk$saved06[[Country]])
    ri <- match(wk$region[[Country]], PSA_REGION_ORDER)
    reg[ri, , ] <- reg[ri, , ] + r$deaths
    reg[R + 1L, , ] <- reg[R + 1L, , ] + r$deaths
    chk <- if (is.null(chk)) r$checks else chk + r$checks
    cc[[Country]] <- r$country_checks
    if (!is.null(r$draw0_vs_06)) d0[[Country]] <- r$draw0_vs_06
    done <- c(done, Country)
    rm(r)
    if (B > 20L) invisible(gc(verbose = FALSE))
  }
  mem <- gc(verbose = FALSE)
  list(region = reg, checks = chk, countries = done, country_checks = rbindlist(cc),
       draw0_vs_06 = rbindlist(d0, fill = TRUE), worker_pid = Sys.getpid(),
       max_mem_mb = sum(mem[, ncol(mem)]))
}

# Combine worker results for one block into compact long tables.
psa_combine_block <- function(results, draw_ids, locs) {
  done <- unlist(lapply(results, `[[`, "countries"))
  if (anyDuplicated(done)) stop("A country was processed twice in one block: ",
                                paste(unique(done[duplicated(done)]), collapse = ", "))
  miss <- setdiff(locs, done)
  if (length(miss)) stop("Block is missing countries: ", paste(miss, collapse = ", "))
  reg <- Reduce(`+`, lapply(results, `[[`, "region"))
  chk <- Reduce(`+`, Filter(Negate(is.null), lapply(results, `[[`, "checks")))
  geo <- c(PSA_REGION_ORDER, "Global")
  B <- length(draw_ids); ns <- length(PSA_SCENARIOS)
  deaths <- data.table(
    draw_id = rep(draw_ids, each = length(geo) * ns),
    scenario_id = rep(rep(PSA_SCENARIOS, each = length(geo)), times = B),
    geography = rep(geo, times = ns * B),
    deaths = as.vector(reg))
  checks <- data.table(draw_id = rep(draw_ids, each = ns), scenario_id = rep(PSA_SCENARIOS, times = B))
  for (v in dimnames(chk)[[3]]) checks[, (v) := as.vector(chk[, , v])]
  checks[, n_countries := length(done)]
  list(deaths = deaths, checks = checks,
       country_checks = rbindlist(lapply(results, `[[`, "country_checks")),
       draw0_vs_06 = rbindlist(lapply(results, `[[`, "draw0_vs_06"), fill = TRUE),
       worker_mem_mb = vapply(results, `[[`, numeric(1), "max_mem_mb"))
}

#...........................................................
# Draw-level estimands and summaries ----
#...........................................................

# mortality_draws.csv: one row per draw x reporting geography x reported scenario.
# `regions`: the WHO regions that contain modelled countries (all six for the
# full geography; a country subset may cover fewer).
psa_mortality_draws <- function(deaths, regions = PSA_REGION_ORDER) {
  base <- deaths[scenario_id == "baseline", .(draw_id, geography, baseline_deaths = deaths)]
  x <- deaths[(geography == "Global" & scenario_id %in% PSA_REPORTED) |
                (geography %in% regions & scenario_id == "all_interventions")]
  x <- base[x, on = .(draw_id, geography)]
  x[, `:=`(intervention = unname(PSA_SCENARIO_LABEL[scenario_id]),
           geography_level = fifelse(geography == "Global", "global", "who_region"),
           who_region = geography, year_start = PSA_YEAR_START, year_end = PSA_YEAR_END,
           n_years = PSA_N_YEARS, projected_deaths = deaths)]
  x[, deaths_averted := baseline_deaths - projected_deaths]
  x[, deaths_averted_per_year := deaths_averted / PSA_N_YEARS]
  x[, percent_averted := 100 * deaths_averted / baseline_deaths]
  x[, `:=`(scen_ord = match(scenario_id, PSA_REPORTED),
           geo_ord = match(who_region, c("Global", PSA_REGION_ORDER)))]
  setorder(x, draw_id, geo_ord, scen_ord)
  x[, .(draw_id, scenario_id, intervention, geography_level, who_region, year_start, year_end,
        n_years, projected_deaths, baseline_deaths, deaths_averted, deaths_averted_per_year,
        percent_averted)]
}

# Distribution-free 95% CI for the p-quantile from n draws (order statistics):
# a tail-precision diagnostic, not part of the reported UI.
psa_order_stat_ci <- function(x, p, conf = 0.95) {
  n <- length(x); xs <- sort(x); z <- qnorm(1 - (1 - conf) / 2)
  j <- max(1L, floor(n * p - z * sqrt(n * p * (1 - p))))
  k <- min(n, ceiling(n * p + z * sqrt(n * p * (1 - p))))
  c(xs[j], xs[k])
}

# mortality_summary.csv: deterministic point (draw 0), 2.5/97.5 percentiles
# (type 7) of draws 1..N, and the number of successful draws. Baseline deaths are
# fixed by construction (verified in 094): no interval is attached to them.
psa_mortality_summary <- function(md, n_draws_expected) {
  long <- melt(md, id.vars = c("draw_id", "scenario_id", "intervention", "geography_level", "who_region",
                               "year_start", "year_end", "n_years"),
               measure.vars = PSA_METRICS, variable.name = "metric", value.name = "value",
               variable.factor = FALSE)
  s <- long[, {
    mc <- value[draw_id >= 1L]
    pt <- value[draw_id == 0L]
    fixed <- metric[1] == "baseline_deaths"
    ok <- !fixed && length(mc) > 1L && all(is.finite(mc))
    q <- if (ok) quantile(mc, c(0.025, 0.975), type = 7, names = FALSE) else c(NA_real_, NA_real_)
    lo <- if (ok) psa_order_stat_ci(mc, 0.025) else c(NA_real_, NA_real_)
    hi <- if (ok) psa_order_stat_ci(mc, 0.975) else c(NA_real_, NA_real_)
    .(point = if (length(pt) == 1L) pt else NA_real_, lower = q[1], upper = q[2],
      n_draws = sum(is.finite(mc)), is_fixed = fixed,
      mc_mean = if (length(mc)) mean(mc) else NA_real_, mc_sd = if (length(mc) > 1L) sd(mc) else NA_real_,
      lower_os95_lo = lo[1], lower_os95_hi = lo[2], upper_os95_lo = hi[1], upper_os95_hi = hi[2],
      max_abs_dev_from_point = if (length(mc) && length(pt) == 1L) max(abs(mc - pt)) else NA_real_)
  }, by = .(scenario_id, intervention, geography_level, who_region, year_start, year_end, n_years, metric)]
  s[, n_draws_expected := n_draws_expected]
  s[, `:=`(scen_ord = match(scenario_id, PSA_REPORTED), geo_ord = match(who_region, c("Global", PSA_REGION_ORDER)),
           met_ord = match(metric, PSA_METRICS))]
  setorder(s, geo_ord, scen_ord, met_ord)
  s[, c("scen_ord", "geo_ord", "met_ord") := NULL]
  s[]
}

#...........................................................
# Formatted tables ----
#...........................................................

psa_fmt_num <- function(x, digits = 0) formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
psa_fmt_ui <- function(point, lower, upper, digits = 0, scale = 1) {
  paste0(psa_fmt_num(point / scale, digits), " [", psa_fmt_num(lower / scale, digits), "–",
         psa_fmt_num(upper / scale, digits), "]")
}

psa_table_global <- function(sm) {
  g <- sm[geography_level == "global"]
  rbindlist(lapply(PSA_REPORTED, function(sid) {
    a <- g[scenario_id == sid & metric == "deaths_averted"]
    y <- g[scenario_id == sid & metric == "deaths_averted_per_year"]
    data.table(Intervention = unname(PSA_SCENARIO_LABEL[sid]), `WHO Target` = unname(PSA_WHO_TARGET[sid]),
               `Deaths averted (M, 2026–2050)` = psa_fmt_ui(a$point, a$lower, a$upper, 2, 1e6),
               `Average per year (thousands)` = psa_fmt_ui(y$point, y$lower, y$upper, 1, 1e3))
  }))
}

psa_table_region <- function(sm) {
  r <- sm[geography_level == "who_region" & scenario_id == "all_interventions"]
  rbindlist(lapply(intersect(PSA_REGION_ORDER, r$who_region), function(reg) {
    m <- function(mm) r[who_region == reg & metric == mm]
    p <- m("projected_deaths"); b <- m("baseline_deaths"); a <- m("deaths_averted")
    y <- m("deaths_averted_per_year"); pc <- m("percent_averted")
    data.table(Region = reg,
               `Projected deaths` = psa_fmt_ui(p$point, p$lower, p$upper, 0),
               `Baseline deaths` = psa_fmt_num(b$point, 0),
               `Deaths averted` = psa_fmt_ui(a$point, a$lower, a$upper, 0),
               `Deaths averted/year` = psa_fmt_ui(y$point, y$lower, y$upper, 0),
               `% averted` = psa_fmt_ui(pc$point, pc$lower, pc$upper, 1))
  }))
}
