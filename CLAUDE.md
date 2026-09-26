# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the Model

Pure R project. No build system or package manager — everything runs by sourcing R scripts in RStudio (`uw-who-cvdtargets.Rproj`) or an R console.

```r
source("code/00_run_model.R")
```

**Hardcoded absolute paths.** `code/00_run_model.R` sets `wd` to a local OneDrive path and derives `wd_code`, `wd_raw`, `wd_data`, `wd_outp`, and `wd_temp` (`data/temp/`, scratch space for stage 02; its contents are gitignored). The reporting `.Rmd` files (`scenarios/scenarios_aim1/aim1_report.Rmd`, `scenarios/scenarios_aim2/aim2_report.Rmd`, `docs/who_cvd_targets_paper1.Rmd`) each re-declare their own `wd`. All of these must be edited when running on a new machine.

**Packages.** Model pipeline: `dplyr`, `data.table`, `tidyr`, `ggplot2`, `RColorBrewer`, `readxl`, `countrycode`, `stringr`, `parallel`, `doParallel`, `foreach`, `gmodels`, `forecast`. `023_get_tps_bgmx.R` additionally loads `StMoMo` and `demography`. Reports additionally need `knitr`, `kableExtra`, `DT`, `scales`, `openxlsx`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `bookdown`. The PSA/DSA (`09*.R`, `10_*.R`) also use `jsonlite`, `htmltools`, `rmarkdown`, `digest`, `codetools`.

**Tests.** Two fixture harnesses, neither needing model inputs or a cluster (run from the repo root):

```bash
Rscript tests/test_aim2_bp_control.R
```

```bash
Rscript tests/test_aim1_psa.R
```

`test_aim2_bp_control.R` sources `04_define_interventions.R` and `06_run_scenarios_multiple.R` with `options(who_cvd.execute_04 = FALSE, who_cvd.execute_06 = FALSE)` and stubs the pipeline-only BP helpers. `test_aim1_psa.R` (41 tests) sources `091`–`094` with `who_cvd.execute_09 = FALSE`; it needs only the BPLTTC workbook and logs to `output_psa/tests/`. It **currently fails** because sodium/TFA were removed from 06 (see below). On this machine R is at `C:\Program Files\R\R-4.5.1\bin\Rscript.exe`; PowerShell mangles inline `Rscript -e`, so write probes to a file.

## Pipeline

`00_run_model.R` sources, in order:

| Script | Role |
|--------|------|
| `01_utils.R`, then `R/utils.R` | Shared helpers (`get.bp.prob`, `calc_mortality_reduction`, `create_age_groups`). `R/utils.R` is a copy of `01_utils.R` plus `gbd_age_lookup` / `gbd_band_label()`, which the alternative calibrators use. Because it is sourced second, its definitions win |
| `02_load_inputs.R` | Thin wrapper that **always** sources `020`–`023` below (there is no skip switch, so every run rebuilds stage 02) |
| `020_get_deaths_who.R` | WHO GHE 2021 CVD/stroke deaths 2000–2023 → `dt_deaths_who_long.rds` (also the default calibration death target) |
| `021_get_base_rates.R` | GBD 2023 incidence/prevalence/mortality → `baseline_rates_part*.rds` |
| `022_get_tps.R` | Transition probabilities (`IR`, `CF`, `BG.mx`) → `tps_inpt_part*.rds` (2000–2019) |
| `023_get_tps_bgmx.R` | Lee–Carter forecasts → `tps_bgmx_*_forecasted.rds`; a closing Li et al. (2019) coherent/MinT section → `tps_mortality_coherent_{forecasted,validation}.rds` (Global; the default trend source for 031) |
| `03_calibration_who.R` | **Only if `run_calibration_par` is `TRUE`** (default `FALSE`). Grid-search `IR`/`CF` calibration → calibrated, **untrended** `calibrated_searo_part*.rds` (`YEARS_HIST`) + `calibration_factors.csv` / `calibration_diagnostics.csv` (+ `*_grid_<WHO\|GBD>.csv` copies) |
| `031_calibration_trends.R` | Reads `calibrated_searo_part*.rds` from disk, repeats 2019 through 2050, applies the secular trends to years > 2019 only, runs the probability guard and validation, and writes the FINAL `adjusted_searo_part*.rds` (2000–2050) |
| `04_define_interventions.R` | Builds the BP-control target table (150M split by diabetes status) and statin targets |
| `05_build_baseline.R` | Loads `adjusted_searo_part*.rds` + UNWPP 2024 population + COVID excess mortality. Calibration and trends are already baked in; 05 re-applies neither |
| `06_run_scenarios_multiple.R` | **Aim 1** — multi-intervention scenarios, parallel by country |
| `06_run_scenarios_targets.R` | **Aim 2** — BP-control scenarios by diabetes subgroup, parallel by country × scenario (commented out in 00) |
| `07_output_dalys.R` | YLL/YLD/DALY calculation → `output/dt_output_dalys.rds` |
| `08_economic_value_calculation.R` | VSL/VSLY monetisation (see below) |

### Calibration (stage 03)

Stage 03 replaces the retired `03_clean_inputs.R → 031_calibration.R → 032_adjustments.R` chain (`adjustments2023_age.csv` in `data/processed/` is that chain's leftover). Calibrators all read `tps_inpt_part*.rds` and write `calibrated_searo_part*.rds`. **Source only one per run**:

- `03_calibration_who.R` (**active**, called by 00): a cohort well–sick–dead projection over `CAL_YEAR_START`–`CAL_YEAR_END` (2009–2019). The grid search minimises `W_DEATHS·RMSE_deaths + W_PREV·RMSE_prev` per `location × sex × cause × 5-year band`, with `(1,1)` always in the grid. Death targets come from `calibration_deaths_source` (`"WHO"` default via `who_deaths_target_file`, or `"GBD"`); prevalence always comes from GBD (`gbd_target_file` = `data/raw/GBD/temp_1baseline_rates_gbd23.rds`). Grids `CAL_IR_GRID` × `CAL_CF_GRID` (180 points) and the other `CAL_*` / `YEARS_*` / `TP_EPS` / stub constants are set in `00_run_model.R`. A full run takes ≈50 min on 8 cores.
- `03_calibration.R`: the same grid calibrator, GBD deaths only (the predecessor of `_who`).
- `03_calibration_random_tp.R`, `03_calibration_stochastic_hill.R`: alternative searches (random coordinate updates / Gaussian hill climb) over a symmetric `[1 − SEARCH_HALFWIDTH, 1 + SEARCH_HALFWIDTH]` range, with `GRANULARITY` `"combo"` or `"age_group"`. Audit files are `calibration_{factors,diagnostics}_random_tp*.csv` (and the hill-climb equivalents).

The header comments of `03_calibration*.R` still say they write "FINAL calibrated + trended" `adjusted_searo`. That is stale: they write untrended `calibrated_searo`, and 031 does the trends. 031's validation reads `calibration_factors.csv` (grid calibrator format: `ir_mult`, `cf_mult`, `base_error`, `cal_error`) and requires its location set to match the calibrated rates.

### Secular trends (stage 031)

- **Source:** `031_calibration_trends.R` picks the trend source with `getOption("who_cvd.trend_method", "coherent")` (`"coherent"` | `"legacy"`). The `trend_method` variable set in `00` is **ignored** (overwritten), so use `options(who_cvd.trend_method = "legacy")`.
- **Coherent:** scales `DIS.mx.t0` by `1 + percent_diff_cvd`. It rebuilds `BG.mx.all` from each country's 2019 residual `ALL.mx − ΣDIS.mx.t0` × `(1 + percent_diff_bgmx_all)`, then sets `ALL.mx = BG.mx.all + ΣDIS` and `BG.mx = ALL.mx − DIS`. A post-guard identity check stops the run if the guard breaks that identity.
- **Legacy:** applies `tps_bgmx_forecasted` / `tps_bgmx_all_forecasted` to `BG.mx` / `BG.mx.all` and `tps_bgmx_cvd_forecasted` to `CF`.
- **Switches:** `run_bgmx_trend` / `run_CF_trend` turn the mortality and CF trends on or off. The CF share is 0.8 when `run_CF_trend_80`, else 1. `run_CF_trend_ihme` swaps the CF source for `tps_bgmx_cvd_ihme.rds` under either method.
- **Accumulation:** `percent_diff` is cumulative from 2019, so it is applied once per projected year, never compounded.
- **Standalone use:** 031 needs 00's settings in the session (it stops listing any that are missing). To change only trends, source 00 up to its settings block, then `source("031_calibration_trends.R")`.

Current state: `adjusted_searo_part*.rds` in the working tree were regenerated by 031 but not committed, and 06 → 07 → 08 and the reports have not been re-run on them.

**Not sourced by `00_run_model.R`**:

- `09_uncertainty_psa.R` (+ `091`–`094`) — Aim 1 probabilistic sensitivity analysis; see **PSA** below.
- `10_deterministic_dsa.R` — Aim 1 deterministic sensitivity analysis; see **DSA** below.

  **Both are currently broken / out of sync:** (1) they re-run `05` expecting its trend blocks and `run_adjustment_model`, which moved to 03/031; (2) they still require the sodium/TFA pieces (`calculate_tfa_impact`, `dt_tfa_scenarios`, `salteff`/`saltmet`/`saltyear*`/`tfa_*` run-call args) that were removed from `06_run_scenarios_multiple.R`. `tests/test_aim1_psa.R` fails for the same reason. Update `091`–`094`, `10` and the test before running them.

### Control flags

Set in `00_run_model.R`, consumed downstream:

| Flag | Used in |
|------|---------|
| `run_calibration_par` (`FALSE`) | `00` — gates whether `03_calibration_who.R` runs at all (and its parallel search) |
| `calibration_deaths_source` (`"WHO"`) | `03_calibration_who.R` and the alternative calibrators |
| `run_bgmx_trend`, `run_CF_trend`, `run_CF_trend_80`, `run_CF_trend_ihme` | `031_calibration_trends.R` |
| `trend_method` | not read (see above); use `options(who_cvd.trend_method = …)` |
| `run_aod_par` | `022_get_tps.R` (dementia arm; off) |
| `run_adjustment_model` | vestigial; kept only because `09_*` / `10_*` still parse it from `00` |

`run_CF_trend_80` implements the baseline assumption that only 80% of the historical secular CF decline is exogenous (the other 20% attributed to past HTN control gains).

R `options()` (default in parentheses):

| Option | Effect |
|--------|--------|
| `who_cvd.execute_04` (`TRUE`) | `FALSE` defines 04's helpers without building/writing targets |
| `who_cvd.execute_06` (`TRUE`) | `FALSE` defines the split-BP helpers in both `06_*` scripts without loading inputs or starting the cluster |
| `who_cvd.aim2_allocation_mode` (`"diabetes_capped_to_target"`) | How 04 splits the 150M target (see below) |
| `who_cvd.run_aim2_example` (`FALSE`) | Runs a single-country example in `06_run_scenarios_targets.R` |
| `who_cvd.pilot_country` (`NULL`) | Restricts the Aim 1 batch in `06_run_scenarios_multiple.R` to one eligible country (e.g. `"Colombia"`) |
| `who_cvd.execute_09` (`TRUE`) | `FALSE` loads the PSA functions without running `psa_main()` |
| `who_cvd.cal_pilot` (`NULL`) | Restricts `03_calibration*.R` to the named location(s). It still overwrites `calibrated_searo_part*.rds` and `calibration_factors.csv`, so restore them afterwards |
| `who_cvd.trend_method` (`"coherent"`) | Trend source in `031_calibration_trends.R` (`"coherent"` \| `"legacy"`) |

## Disease Model

Discrete-time state-transition model, three states per cause (**Well → Sick → Dead**). The projection loop in `project.all()` runs **2017 → 2058**; the reporting window used throughout the papers is **2026–2050** (`int_year <- 2026`).

Four causes modelled jointly:

- `ihd` — Ischemic heart disease
- `istroke` — Ischemic stroke
- `hstroke` — Intracerebral hemorrhage
- `hhd` — Hypertensive heart disease

Rates: `IR` (incidence), `CF` (case fatality), `BG.mx` (background mortality), from GBD 2023 adjusted for COVID excess mortality.

## BP-Control Targets (`04_define_interventions.R`)

Policy: by 2030, **150 million additional controlled hypertensives** vs. baseline (2025 population as fixed denominator), split into two **mutually exclusive** subgroups — hypertensives *without* diabetes and *with* diabetes (80% control target for the latter). `aim2_build_target_tables()` writes a long table to `data/processed/htn_control_targets_by_loc.csv` (`scenario_id × location × sex × subgroup`; `scenario_id ∈ baseline, bp_no_diabetes_only, bp_diabetes_only, bp_combined`) plus `htn_control_targets_summary.csv` (reconciliation audit). This table is loaded as `dt_hbp_targets`; the `06_*` scripts never recompute the allocation.

`allocation_mode`:

- `diabetes_capped_to_target` (**default**) — one shared scale-up solver (`aim2_solve_joint_scale`), diabetes capped at 0.80, total = 150M exactly. Achieved diabetes control ≈ 0.45.
- `diabetes_floor_then_reconcile` — 0.80 as a hard floor; **stops** with real inputs, because the floor alone needs ≈242.7M (this is correct, not a bug).
- `diabetes_floor_uncapped_total` — hard floor, total allowed to exceed 150M.

Small Pacific states lacking a 2025 UNWPP row (e.g. Samoa, Tonga) use a documented latest-available-year (2023) population fallback; locations with missing inputs are excluded and recorded in the audit, not fatal. The old `htncov2_aspirational/ambitious/progress` target columns and the `Scenarios.xlsx` CSV-overwrite block are gone.

The original refactor spec is in `dev/aim2_bp_control_refactor_prompt.md`.

## Interventions

The split-population BP helpers (`aim2_control_trajectory`, `aim2_incremental_effect`, `aim2_subgroup_incidence_multiplier`, `validate_htn_target_table`, `calculate_antihypertensive_split`) sit **above** the `who_cvd.execute_06` guard in both `06_*` scripts and are kept in step. Differences: `06_run_scenarios_multiple.R` has **no sodium or TFA code** (removed 2026-09-24; only `06_run_scenarios_targets.R` still has them), and its `calculate_antihypertensive_split()` also accepts a precomputed `bp_baseline` / `diabetes_age_prepared`, built once per country by `prepare_country_context()` and passed via `project.all(country_context = )`. BP effects combine as an additive population mixture of the two subgroups — no multiplicative cross-term.

`project.all()` intervention names:

| Name | Intervention | Key parameters |
|------|-------------|----------------|
| `antihypertensive_no_diabetes` | BP control, hypertensives without diabetes | `htn_scenario_id`, `dt_hbp_targets` |
| `antihypertensive_diabetes` | BP control, hypertensives with diabetes | `htn_scenario_id`, `dt_hbp_targets` |
| `statins` | Lipid-lowering therapy (`calculate_statins_impact`) | `statin_target_coverage`, `statin_start_year`, `statin_target_year`, `adherence_ir`, `adherence_cf` |
| `sodium` *(Aim 2 script only)* | Dietary sodium reduction (`calculate_sodium_impact_etihad`) | `saltmet`, `salteff` (`0` = off), `saltyear1`, `saltyear2` |
| `tfa` *(Aim 2 script only)* | Trans-fat policy (`calculate_tfa_impact`) | `tfa_target_tfa` (target %E, so `0` = **full elimination**, not off), `tfa_policy_start_year` |

Interventions apply multiplicative relative risk reductions to incidence.

**Aim 1** (`06_run_scenarios_multiple.R`), one job per country running all scenarios:

```r
scenarios <- list(
  baseline            = character(0),
  bp_no_diabetes_only = "antihypertensive_no_diabetes",
  bp_diabetes_only    = "antihypertensive_diabetes",
  bp_combined         = c("antihypertensive_no_diabetes", "antihypertensive_diabetes"),
  statins_only        = "statins",
  all_interventions   = c("antihypertensive_no_diabetes", "antihypertensive_diabetes",
                          "statins")
)
```

`htn_scenario_ids` maps each scenario to the `scenario_id` whose BP targets it uses (`all_interventions` → `bp_combined`). The run call sets statins at 50% coverage by 2030 with `adherence_ir = 0.575`, `adherence_cf = 0.644` (Basios et al. 2025; Aim 2 still uses 1/1). BP effect sizes come from the BPLTTC 2021 files `ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv` (`ETIHAD_RR`) and `ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx` (`ETIHAD_RR_BIN`). Output rows get `htn_target_scenario = "aim1"` (a group key needed by 07/08).

**Aim 2** (`06_run_scenarios_targets.R`): `baseline`, `bp_no_diabetes_only`, `bp_diabetes_only`, `bp_combined`, run over `CJ(location, scenario_id)`; `htn_target_scenario := scenario_id`.

### Parallel execution — output collision

Both scripts use `doParallel`/`foreach` (Aim 1 `ncores <- 10` with `setDTthreads(1)` per worker; Aim 2 `ncores <- 6`), restrict `locs` to locations present in both `data.in` and `dt_hbp_targets` (Aim 1 also drops Greenland and Bermuda), catch per-country errors (Aim 1 returns `list(ok, error)` and prints the first five failures), and write to the **same directory**:

```
Aim 1: output/out_model/model_output_<country>.rds,               log_<country>.txt
Aim 2: output/out_model/model_output_<country>_<scenario_id>.rds, log_<country>_<scenario_id>.txt
```

Filenames no longer overwrite each other, but `07_output_dalys.R`, `08_economic_value_calculation.R`, `aim1_report.Rmd` and `aim2_report.Rmd` all glob *every* `.rds` in `output/out_model/`, so **running both aims pollutes each other's inputs**. Clear or archive `output/out_model/` between aims; the directory is gitignored.

## Reporting Layer

The `.Rmd` files are not sourced by the pipeline — knit them manually after the model run.

- `scenarios/scenarios_aim1/aim1_report.Rmd` — reads `output/out_model/` + `output/dt_output_dalys.rds` + `htn_control_targets_by_loc.csv`; writes `aim1_*.png` figures to `output/`, `aim1_results_tables.xlsx`, plus artefact RDS files to `output/paper/` (`paper_*.rds`) and `output/slides/` (`sl_*.rds`, incl. `sl_control_summary.rds` / `sl_control_scalars.rds`). It sources `aim1_paper_extras.R` (`build_paper_extras()`, reporting only), which writes `paper_table1.rds`, `paper_params.rds` and `paper_strata.rds`. Its run settings (e.g. adherence 0.575/0.644) mirror the 06 run call and must be kept in step by hand.
- `scenarios/scenarios_aim1/aim1_report_dsa.Rmd`, `aim1_executive_slides_dsa.Rmd` — DSA report and deck, rendered by `10_deterministic_dsa.R` (not knitted by hand)
- `scenarios/scenarios_aim2/aim2_report.Rmd` — same pattern for Aim 2; `aim2_*.png`, `aim2_slides_*.rds`. **Stale:** still labels results by the old `htncov2_*` values of `htn_target_scenario`; needs updating to the new `scenario_id`s before re-knitting.
- `scenarios/scenarios_aim1/aim1_executive_slides.Rmd`, `scenarios/scenarios_aim2/executive_slides_htn_targets.Rmd` — Beamer decks consuming the slide artefacts (`beamer_preamble.tex`)
- `docs/who_cvd_targets_paper1.Rmd` — manuscript (`bookdown::word_document2`), consumes `output/paper/paper_*.rds` and `output/slides/`; cites `docs/references.bib`

Scenario labels used downstream: `07_output_dalys.R` maps `bp_no_diabetes_only` → "HTN Control (No Diabetes)", `bp_diabetes_only` → "HTN Control (Diabetes)", `all_interventions` (label "BP + BP_diabetes + Statins") → "All Interventions" (old `bp_only` / "…+ Salt + TFA +…" aliases kept) and drops `bp_combined`.

The artefact hand-off is one-directional: **model → `out_model/` → report `.Rmd` → `paper_*.rds` / `sl_*.rds` → manuscript & slides**. Changing a number in the manuscript means re-knitting the upstream report, not editing the `.Rmd` text.

`docs/` also holds `math-doc.Rmd` (model equations) and `cvd_model_flowchart.html`. Other folders:

- `manuscript/` — co-author revision drafts of Paper 1 (`*_V2_03082026.docx`: main text, figures, tables, supplement; local only, since `*.docx` is gitignored)
- `library/` — reference PDFs (GATHER checklist, Basios 2025 statin adherence, etc.)
- `dev/` — task specs and review notes (`*_prompt.md`, `gate1_*`, `psa_feasibility_review.md`, `prompts.txt` log) — reference only, not instructions
- `config/` — README only, currently unused
- `R/` — `utils.R` is sourced by 00 (see the pipeline table); `engine.R` (`sh_*` state-projection helpers from an abandoned SEAHEARTS port) is not referenced
- root `figure/` — stray knitr chunk output

## Economic Valuation (`08_economic_value_calculation.R`)

Runs after Aim 1 with `wd` already defined (last step of `00_run_model.R`). Monetises deaths averted via VSL and VSLY transferred from a US reference by income adjustment (Robinson & Hammitt 2011; Robinson et al. 2019).

- Primary estimate is `e1_2` — differential elasticity 0.8 at/above US income, 1.2 below. `e1_0` and `e1_5` are sensitivity bounds.
- `BASE_YEAR <- 2026`; calendar discount rates 1%/3%/5%; VSL floor at 20× GNI pc.
- Known limitation documented in-file: SSP2 **GDP** growth rates are applied to a **GNI** base for forward projection.
- Raw inputs (not in git): World Bank GNI pc PPP CSV, IIASA SSP 3.1 xlsx, WPP2024 life-expectancy-by-age xlsx.
- Outputs: `output/08_vsl_results.{rds,csv}`, `08_vsl_summary_table*`, `08_vsly_summary_table*`, `08_vsl_vsly_summary_table_appended.*`. The `*_e1_2_primary.rds` copies are consumed by `aim1_report.Rmd`.

## PSA (`09_uncertainty_psa.R`, Aim 1 only)

Standalone runner, not sourced by `00_run_model.R`. Run from the repo root:

```bash
Rscript code/09_uncertainty_psa.R mode=smoke draws=10
```

```bash
Rscript code/09_uncertainty_psa.R mode=production draws=1000 workers=8
```

Other `key=value` options: `seed` (default 20260923), `block`, `countries`, `correlation` (`independent` | `comonotone_within_stratum` | `comonotone_all`), `validate`, `render`, `cache`, `run_id`, `script06`.

- **Scope:** samples only the 15 BPLTTC 2021 Appendix S6 HRs (IHD/Stroke/Heart failure × 5 baseline-SBP strata, log-normal, read from `ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx`); everything else is fixed. Results are partial parameter-uncertainty intervals.
- **Draw 0** uses the published values and reproduces the deterministic 06 result bit-for-bit. Intervals are 2.5/97.5 percentiles of draws ≥ 1, matched across scenarios.
- **Helpers:** `091` parameters/draws/register, `092` model (reference per-draw path + batched production path), `093` 07/08 analogues and endpoints, `094` validation checks V1–V8.
- **Read-only use of the pipeline:** it resolves the 06 script from what `00_run_model.R` sources, loads it with `who_cvd.execute_06 = FALSE`, and parses the executed `run_multiple_scenarios()` arguments from the `%dopar%` call. It expects the literal `ncores <- …` line in 06, so keep it. It never sources 07/08, and it refuses writes outside `output_psa/`.
- **Isolation:** each run hashes every repo file outside `output_psa/` and refuses to render if any changed during the run, so don't edit, knit or run the model while it runs. Checkpoints are keyed on config, code and input md5s; reruns reuse them.
- **Outputs:** `output_psa/runs/<run_id>/` (parameters, checkpoints, endpoints, validation, `manifest.json`), plus `paper/`, `slides/`, `figures/`, `tables/`, `rendered/`. Reports are in `scenarios_psa/scenarios_aim1/` (`aim1_report_psa.Rmd`, `aim1_executive_slides_psa.Rmd`). The full design is in `output_psa/PSA_IMPLEMENTATION_NOTE.md` (local only, since `output_psa/` is gitignored).
- **Status:** only the 10-draw smoke run exists (labelled TEST; not reportable), and it predates the sodium/TFA removal and the stage-03 refactor. The production run takes ≈2.3 h on 8 workers and ≈13 GB of worker memory.

## DSA (`10_deterministic_dsa.R`, Aim 1 only)

Standalone one-way deterministic scenarios around the active Aim 1 base case (Briggs et al. 2012). These are deterministic ranges, never intervals. It uses the same read-only pattern as the PSA: it parses 00's flags, evaluates 05 in a sandbox, loads 06 with the guard off and parses its `%dopar%` call. It writes only under `output_dsa/` and `scenarios_dsa/`.

```bash
Rscript code/10_deterministic_dsa.R mode=pilot countries=Colombia workers=2
```

```bash
Rscript code/10_deterministic_dsa.R mode=full workers=5
```

`render=only run_id=full` re-renders without recomputing. The 12 settings are `reference`; `target_50`/`target_75`; `delay_2035`/`delay_2040`; `adherence_low`/`moderate`/`income`/`perfect`; and `trend_full`/`no`/`ihme_cvd` (manifest `dsa_manifest()`). The last full run (2026-09-24, 186 countries, ≈3.7 h cold) predates the sodium/TFA removal and stage 03, so its results in `scenarios_dsa/` (xlsx/PDF/manifest tracked; `output_dsa/` ≈13 GB, gitignored) are stale. The trend regimes need re-plumbing now that the trends live in `031_calibration_trends.R` (the regimes map onto its settings).

## Data Conventions

- **Location names** follow GBD 2023. Ad-hoc renames appear in several scripts (e.g. `United States of America` → `United States`, `Bolivia (Plurinational State of)` → `Bolivia`; see `aim2_recode_locations()` in 04). If a join silently drops rows, check location-name alignment first.
- **Population**: UNWPP 2024 single-year age (`PopulationsSingleAge0050.rds`) overrides GBD `Nx` where available. Ages 95+ collapsed to 95.
- **Age groups**: 5-year bins 20–24 through 85+. `create_age_groups()` in `01_utils.R`; `cal_age_group()` in the grid calibrators and `gbd_band_label()` in `R/utils.R` (alternative calibrators) produce GBD-style labels (20–24 … 95+) for calibration merges.
- **Region/income groupings**: `data/processed/Country_groupings_extended.csv` (WHO region + World Bank income).
- **Causes / no dementia**: the project models only the 4 CVD causes + "All causes" (`cause_map` in `00_run_model.R`; `adjusted_searo` keeps full GBD cause names, which 05 renames to the abbreviations). Never add a dementia (`aod`) node. Known inconsistency, not yet fixed: `021_get_base_rates.R` redefines `cause_map` with dementia, so the input `BG.mx.all` = ALL − ΣCVD − dementia, and dementia deaths drop out of model mortality in 2000–2019. Coherent 031 re-anchors the projected years on `ALL.mx − ΣDIS` (dementia included), which leaves a step at 2020. The fix belongs in 021, followed by regenerating 022/03/031. Because 02 runs on every `00` run, 021's `cause_map` (with dementia) and 020's also **overwrite** 00's global `cause_map` before 07 uses it. `MODEL_CAUSES` is computed before 02, so it is unaffected.

### Key in-memory objects

- `b_rates` — baseline transition rates by location/year/age/sex/cause
- `data.in` — BP distribution inputs (mean SBP, SD, by BP category), from `bp_data6.csv`
- `inc` — HTN control coverage scale-up trajectories (`covfxn2.csv`)
- `ETIHAD_RR` / `ETIHAD_RR_BIN` — RR lookups for the BP intervention (BPLTTC 2021; `ettehad_rr_bp_reduction_*_bplttc_2021.*`; the non-suffixed files are the older Ettehad versions)
- `dt_gbd_rr` — GBD 2019 RR per 10 mmHg
- `dt_hbp_control` (`hbp_control_data.rds`), `dt_hbp_targets` (long BP target table from 04)
- `dt_statin_scenarios` (`statin_data.rds`), `dt_af_statins`; `dt_tfa_scenarios` (Aim 2 only)

### Data locations

`data/processed/` is version-controlled and holds the calibrated/derived inputs (`calibrated_searo_part*.rds` + `calibration_factors*.csv` / `calibration_diagnostics*.csv` from stage 03, `adjusted_searo_part*.rds` from stage 031, `dt_deaths_who_long.rds`, `tps_inpt_part*.rds`, `tps_mortality_coherent_*.rds`, `baseline_rates_part*.rds`, `tps_bgmx_*.rds`, `bp_data6.csv`, `covfxn2.csv`, `wpp.adj.Rda`, `Scenarios.xlsx`, `htn_control_targets_*.csv`, `statins_control_targets_by_loc.csv`, statin/sodium/TFA scenario RDS files).

`data/raw/` is gitignored (`data/raw/**/*.*`, README files excepted). Needed there: GBD 2023 extracts, WHO GHE CVD/stroke CSVs, NCD-RisC hypertension estimates, `IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx`, and the three economic-valuation files listed above.

Also gitignored: `data/temp/**/*.*`, `output/out_model/`, `output/dt_output_dalys.rds`, `*.html` (except `docs/*.html`), `*.doc`/`*.docx`, `.claude/worktrees/`, `output_psa/`, `output_dsa/`. `scenarios_psa/` and `scenarios_dsa/` (except `*.html`) are tracked.

The per-directory `README.md` files (`data/`, `docs/`, `output/`, `scenarios/`, `tests/`, `dev/`, `config/`, `manuscript/`, `library/`, …) are all copies of the root `README.md` boilerplate — they do not describe their directories.
