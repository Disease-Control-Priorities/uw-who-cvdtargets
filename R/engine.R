#===========================================================================
# R/engine.R  --  shared well-sick-dead state-transition engine
#---------------------------------------------------------------------------
# ONE tested transition engine used by BOTH the calibration stage (03) and the
# scenario projection (06), so a no-intervention scenario reproduces the
# calibration baseline by construction. Replaces the two divergent recursions
# that previously (a) subtracted an annual mortality probability from the
# initial living stock and (b) handled the open 95+ interval differently
# (03 silently dropped age 95 -> 96).
#
# STATE (per age, sex, cause):
#   well  = living without the disease
#   sick  = living with the disease (prevalent)
#   pop   = living population (well + sick), shared-pool independent-cause model
#
# TIMING CONVENTION (documented, applied consistently):
#   * Rates (IR, CF, BG.mx, BG.mx.all, covid.mx) apply over the annual interval.
#   * A year's transition flows are computed from the START-OF-YEAR living stock
#     using that interval's rates; deaths are recorded within the interval.
#   * The cohort ages by one year at the interval boundary: origin age a -> a+1,
#     with OPEN_AGE (95) absorbing (age 94 enters 95+, existing 95+ stay; nothing
#     is ever aged into 96 / discarded).
#   * Recorded (well, sick, pop) for year y are the living stocks entering year y
#     (i.e. after aging from y-1); recorded (newcases, dead, all.mx) are that
#     interval's flows. Age 0 is re-seeded from the population each year (births).
#
# INITIALISATION (seed year y0 and every year's age 0):
#   sick = Nx * PREVt0 ; well = Nx - sick ; pop = Nx   (well + sick == pop EXACTLY;
#   NO mortality probability is subtracted from the initial living stock).
#
# FLOWS (per cause, from origin stock well_o, sick_o, pop_o and interval rates):
#   newcases       = well_o * IR                              (0 <= . <= well_o)
#   disease_deaths = sick_o * CF                              (0 <= . <= sick_o)
#   all-cause deaths (count, per confirmed decision) =
#       sum_over_causes(disease_deaths) + pop_o*BG.mx.all + pop_o*covid.mx
#   pop_end  = pop_o - all_cause_deaths
#   sick_end = sick_o*(1 - CF - BG.mx - covid.mx) + newcases
#   well_end = pop_end - sick_end                             (well_end+sick_end==pop_end)
#===========================================================================

suppressWarnings(suppressMessages({ library(data.table) }))

# --- Seed initial living stocks + recorded flows for a set of rate rows -----
# Returns the 10-column recorded row for the seeded (age, sex, cause) cells.
.sh_seed <- function(dt) {
  dt <- copy(dt)
  dt[, sick := Nx * PREVt0]
  dt[, well := Nx - sick]                     # well + sick == Nx == pop (no mortality subtracted)
  dt[, pop  := Nx]
  dt[, newcases := well * IR]
  dt[, dead := sick * CF]                     # disease deaths (== Nx*DIS.mx.t0 since DIS.mx.t0=PREVt0*CF)
  # all-cause death count at seed, computed the SAME way as the loop (consistent).
  dt[, alc := sum(dead), by = .(age, sex)]
  dt[, all.mx := pmax(alc + pop * BG.mx.all + pop * covid.mx, 0)]
  dt[, alc := NULL]
  dt[, .(age, sex, cause, year, well, sick, newcases, dead, pop, all.mx)]
}

#===========================================================================
# sh_project_states()  --  project the cohort over its year range.
#---------------------------------------------------------------------------
# rates : data.table with age, sex, cause, year, IR, CF, BG.mx, BG.mx.all,
#         PREVt0, DIS.mx.t0, Nx and (optionally) covid.mx.
# open_age : integer absorbing open age (default 95).
# residual : optional data.table age, sex, cause, year, res_pop giving an
#         exogenous demographic residual (counts) to ADD to the living stock of
#         each recorded year > y0, split into well/sick by `prev_share`. Used to
#         reconcile the comparator to WPP and to apply the IDENTICAL exogenous
#         flow to every scenario (see sh_demographic_residual).
# prev_share : optional data.table age, sex, cause, year, sick_share in [0,1]
#         (comparator prevalence structure) used to allocate the residual.
# Returns age, sex, cause, year, well, sick, newcases, dead, pop, all.mx.
#===========================================================================
sh_project_states <- function(rates, open_age = 95L, residual = NULL, prev_share = NULL) {
  rates <- as.data.table(copy(rates))
  if (!"covid.mx" %in% names(rates)) rates[, covid.mx := 0]
  req <- c("age", "sex", "cause", "year", "IR", "CF", "BG.mx", "BG.mx.all",
           "PREVt0", "DIS.mx.t0", "Nx", "covid.mx")
  miss <- setdiff(req, names(rates))
  if (length(miss)) stop("sh_project_states: rates missing columns: ", paste(miss, collapse = ", "), call. = FALSE)

  years <- sort(unique(rates$year))
  y0 <- min(years); yE <- max(years)
  key <- c("age", "sex", "cause")
  setkey(rates, year, sex, cause, age)

  out_list <- vector("list", length(years)); names(out_list) <- as.character(years)

  init <- .sh_seed(rates[year == y0])
  out_list[[as.character(y0)]] <- init
  state <- init[, .(age, sex, cause, well, sick, pop)]

  for (y in (y0 + 1L):yE) {
    rr <- rates[year == y, .(age, sex, cause, IR, CF, BG.mx, BG.mx.all, covid.mx, Nx, PREVt0, DIS.mx.t0)]
    m  <- merge(rr, state[, .(age, sex, cause, well_o = well, sick_o = sick, pop_o = pop)],
                by = key, all.x = TRUE)
    # an (age, cause) with no prior-year stock (grid edge) contributes nothing
    m[is.na(well_o), well_o := 0][is.na(sick_o), sick_o := 0][is.na(pop_o), pop_o := 0]
    # mutually-exclusive, non-negative flows from the origin stock
    m[, newcases_t := pmin(pmax(well_o * IR, 0), well_o)]           # 0 <= newcases <= well_o
    m[, dead_t     := pmin(pmax(sick_o * CF, 0), sick_o)]           # 0 <= disease deaths <= sick_o
    # all-cause death COUNT at this (age, sex): disease deaths summed over causes
    # plus the background all-cause envelope and COVID excess on the living pool.
    m[, alc := sum(dead_t), by = .(sex, age)]
    m[, allmx_t := pmax(alc + pop_o * BG.mx.all + pop_o * covid.mx, 0)]
    m[, pop_t   := pmax(pop_o - allmx_t, 0)]                        # end-of-interval living pop
    m[, sick_t  := pmin(pmax(sick_o * (1 - (CF + BG.mx + covid.mx)) + newcases_t, 0), pop_t)]
    m[, well_t  := pmax(pop_t - sick_t, 0)]                         # well + sick == pop
    m[, dest := pmin(age + 1L, open_age)]                          # 94->95, 95 stays 95 (open, never dropped)

    nxt <- m[, .(well = sum(well_t), sick = sum(sick_t), newcases = sum(newcases_t),
                 dead = sum(dead_t), pop = sum(pop_t), all.mx = sum(allmx_t)),
             by = .(age = dest, sex, cause)]

    newborn <- .sh_seed(rates[year == y & age == 0L])[
      , .(age, sex, cause, well, sick, newcases, dead, pop, all.mx)]
    yr <- rbindlist(list(newborn, nxt[age > 0L]), use.names = TRUE)
    yr[, year := y]
    out_list[[as.character(y)]] <- yr
    state <- yr[, .(age, sex, cause, well, sick, pop)]
  }

  res <- rbindlist(out_list, use.names = TRUE)
  if (!is.null(residual)) res <- sh_apply_residual(res, residual, prev_share)
  setorderv(res, c("cause", "sex", "year", "age"))
  res[]
}

#===========================================================================
# sh_apply_residual()  --  add an exogenous demographic residual (counts) to a
# projection's living stocks, splitting each residual into well/sick by the
# supplied comparator prevalence structure (falls back to the state's own
# prevalence). well + sick == pop is preserved. Used inside sh_project_states
# and, in stage 06, to reconcile the comparator itself to WPP.
#===========================================================================
sh_apply_residual <- function(states, residual, prev_share = NULL) {
  res <- as.data.table(copy(states))
  residual <- as.data.table(residual)
  if (is.null(prev_share)) {
    res[, sick_share := fifelse(pop > 0, sick / pop, 0)]
  } else {
    res <- merge(res, as.data.table(prev_share)[, .(age, sex, cause, year, sick_share)],
                 by = c("age", "sex", "cause", "year"), all.x = TRUE)
    res[is.na(sick_share), sick_share := fifelse(pop > 0, sick / pop, 0)]
  }
  res <- merge(res, residual[, .(age, sex, cause, year, res_pop)],
               by = c("age", "sex", "cause", "year"), all.x = TRUE)
  res[is.na(res_pop), res_pop := 0]
  res[, `:=`(pop  = pop  + res_pop,
             sick = sick + res_pop * sick_share,
             well = well + res_pop * (1 - sick_share))]
  res[well < 0, well := 0]; res[sick < 0, sick := 0]; res[pop < 0, pop := 0]
  res[, c("res_pop", "sick_share") := NULL]
  res[]
}

#===========================================================================
# sh_demographic_residual()  --  exogenous residual to reconcile to WPP.
#---------------------------------------------------------------------------
# Given a COMPARATOR projection (sh_project_states output) and the WPP living
# population by age/sex/year, returns the per (age, sex, cause, year) residual
# counts res_pop = WPP_Nx - comparator_pop that, added to the comparator, make
# it reconcile to WPP exactly (for year > init_year; year y0 is already WPP by
# seeding). The SAME residual is applied to every scenario, so scenario minus
# comparator population reflects only intervention-related survival.
#===========================================================================
sh_demographic_residual <- function(comparator_states, wpp_pop, init_year) {
  cs <- as.data.table(comparator_states)[, .(age, sex, cause, year, comp_pop = pop)]
  wp <- as.data.table(wpp_pop)
  if ("year_id" %in% names(wp)) setnames(wp, "year_id", "year")
  wp <- wp[, .(age, sex, year, wpp = Nx)]
  d <- merge(cs, wp, by = c("age", "sex", "year"), all.x = TRUE)
  d[is.na(wpp), wpp := comp_pop]
  d[, res_pop := wpp - comp_pop]
  d[year <= init_year, res_pop := 0]     # seed year already equals WPP
  d[, .(age, sex, cause, year, res_pop)]
}

# Comparator prevalence structure (sick_share) for allocating the residual.
sh_prev_structure <- function(comparator_states) {
  cs <- as.data.table(comparator_states)
  cs[, .(age, sex, cause, year, sick_share = fifelse(pop > 0, sick / pop, 0))]
}
