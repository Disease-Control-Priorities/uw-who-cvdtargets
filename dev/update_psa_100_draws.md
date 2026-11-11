# Claude Code task: update the standalone PSA and execute 100 draws

Implement this update in the current `uw-who-cvdtargets` repository, run it, and render the PSA report and executive slides. This is an implementation and execution task, not only a review or plan.

## Scope and isolation

The production `code/00–06*` pipeline has changed. Treat the current repository version—especially the actual 06 script sourced by `code/00_run_model.R`—as authoritative. Read its settings, saved inputs, intervention functions, scenario definitions, and deterministic results before adapting the PSA. Do not reconstruct the model from the older PSA assumptions.

Modify only:

- `code/09_uncertainty_psa.R` and its existing `code/09*.R` helpers, including `091_psa_parameters.R`, `092_psa_model.R`, `093_psa_downstream.R`, and `094_psa_validate.R`;
- `scenarios_psa/scenarios_aim1/aim1_report_psa.Rmd`;
- `scenarios_psa/scenarios_aim1/aim1_executive_slides_psa.Rmd` (create it if absent, using the existing PSA presentation style).

Keep the PSA separate from production. Do not modify or rerun stages 00–06, particularly expensive calibration. Read their existing saved artifacts. Do not overwrite production inputs/results or source a production script with its execution/writing block enabled. All PSA outputs belong under `output_psa/<run_id>/`; rendered documents can remain in the PSA report directory. Prefer `data.table` for the updated R code.

The attached older orchestrator is named `09_uncertainty_psa(1).R`; update the canonical repository filename. The attached 06 script is a reference, but the current active repository script takes precedence if newer.

## Align the PSA with the current model

Replace stale copied equations, extraction assumptions, input paths, caches, and intervention combinations with the current production behavior. Preserve the actual age range, causes, horizon, adherence settings, baseline coverage, policy ramp, coherent mortality trends, diabetes/no-diabetes split, and haemorrhagic-stroke implementation. Do not add sodium or trans-fat interventions or require their legacy inputs.

The supplied current 06 defines these six scenarios:

`baseline`, `bp_no_diabetes_only`, `bp_diabetes_only`, `bp_combined`, `statins_only`, `all_interventions`.

Use the actual current scenario definitions and target mappings. The requested change is from the existing 10-draw test to **100 stochastic draws**, not the invention of ten policy scenarios. Compute the four global intervention rows below and WHO-region results for `all_interventions`; retain `bp_combined` only as needed for model consistency/validation.

Ensure `all_interventions` receives BOTH the sampled BP effects and sampled statin effects, using the current combination equations. In the older PSA, statin factors were cached as fixed matrices; rebuild every RR-dependent statin quantity for each draw, including baseline-adjustment denominators. Sampling values in a parameter table without passing them into transitions is insufficient. Batched CF arrays must support draw-varying statin effects.

## Parameter uncertainty

Preserve the BP effect uncertainty already implemented, including the BPLTTC 2021 Appendix S6 log-scale draws, correct source CI levels, transformations, subgroup/bin mappings, and shared draws. Preserve supported correlation options; do not introduce additional uncertain parameter families for this task.

Add uncertainty to all four statin RR parameters while retaining these deterministic model values:

| Parameter | Point RR | Model use |
|---|---:|---|
| `rr_ir_ihd` | 0.74 | IHD incidence |
| `rr_ir_istroke` | 0.80 | Ischaemic stroke incidence |
| `rr_cf_ihd` | 0.80 | IHD case fatality |
| `rr_cf_istroke` | 0.96 | Ischaemic stroke case fatality |

Trial reference: Fulcher et al., CTT Collaboration (2015), *Efficacy and safety of LDL-lowering therapy among men and women: meta-analysis of individual data from 174,000 participants in 27 randomised trials*. DOI: `10.1016/S0140-6736(14)61368-4`.

https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(14)61368-4/fulltext

Read the paper, figures, and supplement or a verifiable author/institutional full-text copy. Extract and document the relevant trial confidence limits, their confidence level (95% versus 99%), endpoint, population/sex grouping, and LDL reduction scale. Do not invent limits, substitute arbitrary percentage uncertainty, or treat a lower confidence limit as a published point estimate.

For a verified source interval `[L,U]` with confidence level `c`, use:

```r
se_log <- (log(U) - log(L)) / (2 * qnorm((1 + c) / 2))
rr_draw <- exp(rnorm(1, mean = log(rr_model_point), sd = se_log))
```

Keep draw 0 exactly at the four model values above. If a model value differs from the source point estimate, explicitly record that this distribution is centered on the retained model value with precision borrowed from the stated trial endpoint. Verify any LDL scaling. Do not silently change production point estimates.

The older parameter register flags possible source/estimand mismatches, including mortality RRs used as conditional case-fatality multipliers. Verify those flags rather than copying them unquestioningly. Preserve the current model interpretation for this scoped task, but disclose any mortality-to-case-fatality mapping as a modeling assumption; do not claim the trial directly estimated conditional case fatality. If a parameter lacks a defensible verifiable uncertainty source, identify the specific gap rather than fabricate a distribution or silently leave it fixed.

Draw one four-parameter statin vector per iteration, shared across countries, ages, sexes, years, and scenarios. Use the same BP vector wherever the same underlying effect enters. If covariance is unavailable, document independence as a working assumption. Positive lognormal RR draws may exceed 1: do not truncate all effects to benefits. Apply the current model's transition validity handling and report any consequential clipping.

Leave coverage, targets, adherence, epidemiology, calibration, secular trends, and other parameters fixed unless already sampled by the existing BP PSA. Describe the intervals as **BP and statin intervention-effect parameter uncertainty**, not total uncertainty.

## Run and compact output

Execute **100 successful stochastic draws**, IDs 1–100, with a reproducible seed, plus deterministic draw 0. Replace the old 1,000-draw report gate so the requested run renders. Label the 95% UIs as preliminary estimates from 100 draws, with limited tail precision; do not call them converged. Keep draw count configurable for later larger runs.

Only mortality summaries are required now. Skip economics, DALYs, ASMR, country/cause/age/sex output products, and their unused dependencies. Retain detailed states temporarily in memory only if required to calculate deaths correctly. Read existing input RDS files as needed, but **do not save simulation output RDS files**, sample-country state dumps, full per-draw trajectories, or slide handoff RDS files.

Save compact, machine-readable CSV files:

1. `parameter_register.csv`: source/CI/distribution/mapping documentation.
2. `parameter_draws.csv`: sampled BP and statin values, including draw 0.
3. `mortality_draws.csv`: one numeric row per draw × reporting geography × reported scenario. Store `draw_id`, `scenario_id`, `intervention`, `geography_level`, `who_region`, `year_start`, `year_end`, `n_years`, `projected_deaths`, `baseline_deaths`, `deaths_averted`, `deaths_averted_per_year`, and `percent_averted`. Use `Global` for global rows. Required coverage: four global interventions and combined-intervention results for all six WHO regions, for every draw.
4. `mortality_summary.csv`: numeric deterministic point, lower, upper, and successful draw count for each displayed metric/group.
5. The two formatted table CSVs and a small run manifest/validation log.

Append/checkpoint compact draw summaries safely for resumption. Never mix incompatible configurations or count duplicate/resumed IDs twice. Reports and slides must consume these saved CSVs and render without rerunning the model.

## Estimands and 95% UIs

Restrict all tables to **2026–2050 inclusive: 25 years**. Existing report headings/calculations contain 2025 references; correct both labels and filtering. Use the same death definition and aggregation as the current deterministic reporting pipeline, without double-counting shared other-cause mortality across disease chains.

Within each matched draw and reporting group calculate:

```r
deaths_averted <- baseline_deaths - projected_deaths
deaths_averted_per_year <- deaths_averted / 25
percent_averted <- 100 * deaths_averted / baseline_deaths
```

Sum locations, causes as appropriate, sexes, ages, and years **within each draw first**, then calculate `quantile(x, c(.025, .975), type = 7)` across draws 1–100. Exclude draw 0 from quantiles. Never sum interval endpoints or calculate percentage intervals from separately summarized numerator/denominator endpoints. Report draw 0 as the point estimate; it need not fall inside the empirical interval. Fixed baseline deaths are shown without artificial intervals.

## Required report and slide tables

**Global intervention table**

| Intervention | WHO Target | Deaths averted (M, 2026–2050) | Average per year (thousands) |
|---|---|---|---|
| HTN Control (No Diabetes) | Target 1 (no-diabetes subgroup of the 150M split) | point [lower–upper] | point [lower–upper] |
| HTN Control (Diabetes) | Target 3: BP control in people with diabetes (0.80 ceiling); diabetes subgroup of the 150M split | point [lower–upper] | point [lower–upper] |
| Improved Statin Uptake | Target 2: >=50% eligible adults on lipid-lowering therapy by 2030 | point [lower–upper] | point [lower–upper] |
| All Interventions | All WHO targets (1+2+3) combined | point [lower–upper] | point [lower–upper] |

**WHO-region table: All Interventions**

Columns: `Region`, `Projected deaths`, `Baseline deaths`, `Deaths averted`, `Deaths averted/year`, `% averted`.

Rows in this order: Western Pacific, South-East Asia, Europe, Americas, Eastern Mediterranean, Africa.

Show point [lower–upper] for every intervention-dependent numeric column, including projected deaths and percentage averted. Show fixed baseline deaths as a single value. Use deaths in absolute numbers for the region table; global deaths averted in millions (2 decimals), global annual deaths averted in thousands (1 decimal), region counts rounded to integers, percentages to 1 decimal. Keep unrounded numeric CSVs.

The user's example values (6.97, 13.69, 13.62, and 26.79 million, and the regional totals) illustrate table structure only. Recompute all results; do not hard-code them or reuse stale BP-only intervals. Replace obsolete `‡` fixed-effect markers for statins/combined results, old BP-only notes, legacy defect narratives, and 2025–2050 captions with explanations reflecting the updated run. The device-availability target is not an independently simulated mortality intervention.

## Validation and completion

Before the full run, verify draw 0 matches the active deterministic model, first for a small country subset and then for required aggregate outputs, with documented numeric tolerance. Validate any optimized/batched path against the current reference equations. Check BP and statin parameter perturbations propagate into their respective intervention outputs and into `all_interventions`; unrelated single-intervention outputs should not respond to the other family's perturbations.

Check all 100 draw IDs are complete for every required row; failed draws must be retried/reported, not silently dropped. Check finite results, death accounting identities, the 25-year divisor, fixed baseline consistency, WHO mapping without duplicate locations, and that region totals reconcile with global combined results using the same country set. Resolve missing mappings explicitly; never silently discard countries. Quantiles should be finite and ordered; do not force expected benefit signs.

Run a short technical check, then the full 100-draw run and render BOTH Rmd documents. Confirm no production artifacts changed. Return the files changed, exact run/render commands, output locations, number of successful draws, the two completed tables, and any unresolved evidence or execution issue. If execution is blocked, report the actual blocker and completed work; do not claim the run succeeded.
