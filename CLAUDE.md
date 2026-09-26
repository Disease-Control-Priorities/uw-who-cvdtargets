# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the Model

Pure R project. No build system or package manager — everything runs by sourcing R scripts in RStudio (`uw-who-cvdtargets.Rproj`) or an R console.

```r
source("code/00_run_model.R")
```

**Hardcoded absolute paths.** `code/00_run_model.R` sets `wd` to a local OneDrive path and derives `wd_code`, `wd_raw`, `wd_data`, `wd_outp`; `wd_temp` points outside the repo. The reporting `.Rmd` files (`scenarios/scenarios_aim1/aim1_report.Rmd`, `scenarios/scenarios_aim2/aim2_report.Rmd`, `docs/who_cvd_targets_paper1.Rmd`) each re-declare their own `wd`. All of these must be edited when running on a new machine.

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
| `01_utils.R` | Shared helpers (`get.bp.prob`, `calc_mortality_reduction`, `create_age_groups`; stage 03/031 artifact helpers `tp_guard`, `write_location_chunks`, manifests, `calibration_artifact_status`) |
| `02_load_inputs.R` | Thin wrapper — sources `020`–`023` below; only when `inputs_mode = "rebuild"` (default `"reuse"` uses the saved stage-02 outputs) |
| `020_get_deaths_who.R` | WHO GHE 2021 CVD/stroke deaths 2000–2023 → `dt_deaths_who_long.rds` |
| `021_get_base_rates.R` | GBD 2023 incidence/prevalence/mortality → `baseline_rates_part*.rds` |
| `022_get_tps.R` | Transition probabilities (`IR`, `CF`, `BG.mx`) → `tps_inpt_part*.rds` |
| `023_get_tps_bgmx.R` | Lee–Carter forecasts → `tps_bgmx_*_forecasted.rds` (sensitivity); a closing Li et al. (2019) coherent/MinT section → `tps_mortality_coherent_{forecasted,validation}.rds` (Global; the DEFAULT trend applied by 031) |
| `03_calibration.R` | In-sample calibration ONLY: `IR`/`CF` multipliers vs GBD 2023 targets, applied to the historical `tps_inpt` → calibrated, UNTRENDED `adjusted_tps_part*.rds` (2000–2019) + `calibration_manifest.csv`; grid-search runs also write `calibration_factors.csv` / `calibration_diagnostics.csv` |
| `031_calibration_trends.R` | Reads `adjusted_tps`, extends 2019 to 2050, applies the selected mortality/CF trends to the projected years only, guard + validation → FINAL `adjusted_searo_part*.rds` + `trends_manifest.csv` |
| `04_define_interventions.R` | Builds the BP-control target table (150M split by diabetes status) and statin targets |
| `05_build_baseline.R` | Loads the final `adjusted_searo_part*.rds` (checked against `trends_manifest.csv`) + UNWPP 2024 population + COVID excess mortality (calibration and trends already baked in by 03/031) |
| `06_run_scenarios_multiple.R` | **Aim 1** — multi-intervention scenarios, parallel by country |
| `06_run_scenarios_targets.R` | **Aim 2** — BP-control scenarios by diabetes subgroup, parallel by country × scenario |
| `07_output_dalys.R` | YLL/YLD/DALY calculation → `output/dt_output_dalys.rds` |
| `08_economic_value_calculation.R` | VSL/VSLY monetisation (see below) |

Stage 03 (`03_calibration.R`) replaces the retired `03_clean_inputs.R → 031_calibration.R → 032_adjustments.R` chain. It runs ONE consolidated `IR`/`CF` calibration — a cohort well–sick–dead projection over `CAL_YEAR_START`–`CAL_YEAR_END`, grid search minimising `W_DEATHS·RMSE_deaths + W_PREV·RMSE_prev` against GBD Number per `location × sex × cause × 5-year band`, with the identity `(1,1)` always in the grid so the fit is never worse than baseline; grids `CAL_IR_GRID` × `CAL_CF_GRID` (180 points) and the other `CAL_*`/`YEARS_*` constants are set in `00_run_model.R`. It writes only the historical years: it never reads a forecast or applies a trend. Consolidating the legacy two-pass (031 fine grid + 032 wide grid, the latter a period model) into one cohort calibration shifts the calibrated numbers.

**Calibration reuse (`calibration_mode`, default `"reuse"`).** `00` skips stage 03 when `calibration_manifest.csv` says `adjusted_tps` matches the current `tps_inpt` md5s, calibration settings and `calibration_factors.csv`. Otherwise stage 03 re-applies `calibration_factors.csv` without the grid search (≈15 s; strict key/grid checks). If neither is possible, 00 stops with instructions and never recalibrates silently. `"factors"` forces that re-apply; `"run"` forces the grid search (≈50 min on 8 cores). The factors from the 2026-09-25 grid search predate the manifest. On their first re-apply, 03 adopts the current `tps_inpt` and settings as their provenance, so a later stage-02 rebuild or grid change stops with instructions. Re-applying them reproduces the 2000–2019 rows of that run's `adjusted_searo` bit for bit (checked 2026-09-25). One-time search: `options(who_cvd.calibration_mode = "run"); source("code/03_calibration.R")`.

**Stage 031 trends.** `trend_method` (`"coherent"` default | `"legacy_lc"`) is the only selector of the mortality forecast. `run_bgmx_trend` / `run_CF_trend` switch the mortality block / CF trend on or off. `run_CF_trend_ihme` swaps only the CF source for IHME Foresight; combined with `run_CF_trend = FALSE` it stops as contradictory. `run_CF_trend_80` sets the CF share (0.8 default, else 1). Each country's own 2019 levels are scaled by the Global cumulative 2019-relative change, never compounded. Only the bottom level (4 × `DIS.mx.t0` + `BG.mx.all`) is scaled; `ALL.mx = BG.mx.all + ΣDIS` and `BG.mx = ALL.mx − DIS` follow. The 2019 `BG.mx.all` anchor is re-derived as `ALL.mx − ΣDIS` (the stage-02 value excludes dementia), which leaves a step of ≈3.9% of all deaths between the 2019 and 2020 rows. The CF rule is `CF_2019 × (1 + 0.8·(R_c − 1))`, while the rate columns carry 100%, so modeled CVD deaths decline by less than the reference `DIS.mx.t0`. Missing, duplicate or non-finite forecast keys and zero Global denominators stop the run; `tps_bgmx_forecasted.rds` is not applied. Standalone: `Rscript code/031_calibration_trends.R` (≈30 s). Changing a trend setting reruns only 031 (plus 04–08), never 03.

Both 03 and 031 run standalone: when the settings are absent they source `00_run_model.R` with `options(who_cvd.settings_only = TRUE)`, which stops 00 before any stage runs. The production `adjusted_searo_part*.rds` still hold the pre-031 legacy trends (no `trends_manifest.csv`, so 05 warns) until 031 is run once. Even then, 06 → 07 → 08 and the reports still hold pre-refactor results.

**Not sourced by `00_run_model.R`**:

- `09_uncertainty_psa.R` (+ `091`–`094`) — Aim 1 probabilistic sensitivity analysis; see **PSA** below.
- `10_deterministic_dsa.R` — Aim 1 deterministic sensitivity analysis; see **DSA** below.

  **Both are currently broken / out of sync:** (1) they re-run `05` expecting its trend blocks and `run_adjustment_model`, which moved to stages 03/031 (trends now in `031_calibration_trends.R`); (2) they still require the sodium/TFA pieces (`calculate_tfa_impact`, `dt_tfa_scenarios`, `salteff`/`saltmet`/`saltyear*`/`tfa_*` run-call args) that were removed from `06_run_scenarios_multiple.R`. `tests/test_aim1_psa.R` fails for the same reason. Update `091`–`094`, `10` and the test before running them.

### Control flags

Set in `00_run_model.R`, consumed downstream:

| Flag | Used in |
|------|---------|
| `run_aod_par` | `022_get_tps.R` (dementia arm) |
| `run_calibration_par` | `03_calibration.R` (parallel calibration search) |
| `inputs_mode` (`"reuse"` \| `"rebuild"`), `calibration_mode` (`"reuse"` \| `"factors"` \| `"run"`) | `00_run_model.R` stage control (also via `options(who_cvd.inputs_mode / who_cvd.calibration_mode)`) |
| `trend_method`, `run_bgmx_trend`, `run_CF_trend`, `run_CF_trend_80`, `run_CF_trend_ihme` | `031_calibration_trends.R` (secular trends; precedence above) |
| `run_adjustment_model` | vestigial — calibration is unconditional in `03_calibration.R`; kept (`TRUE`) only because the standalone `09_*`/`10_*` still parse it from `00` |

`run_CF_trend_80` (consumed by `031_calibration_trends.R`) implements the baseline assumption that only 80% of the historical secular CF decline is exogenous (the other 20% attributed to past HTN control gains).

R `options()` (default in parentheses):

| Option | Effect |
|--------|--------|
| `who_cvd.execute_04` (`TRUE`) | `FALSE` defines 04's helpers without building/writing targets |
| `who_cvd.execute_06` (`TRUE`) | `FALSE` defines the split-BP helpers in both `06_*` scripts without loading inputs or starting the cluster |
| `who_cvd.aim2_allocation_mode` (`"diabetes_capped_to_target"`) | How 04 splits the 150M target (see below) |
| `who_cvd.run_aim2_example` (`FALSE`) | Runs a single-country example in `06_run_scenarios_targets.R` |
| `who_cvd.pilot_country` (`NULL`) | Restricts the Aim 1 batch in `06_run_scenarios_multiple.R` to one eligible country (e.g. `"Colombia"`) |
| `who_cvd.execute_09` (`TRUE`) | `FALSE` loads the PSA functions without running `psa_main()` |
| `who_cvd.cal_pilot` (`NULL`) | Restricts `03_calibration.R` to the named location(s) for a fast test (mirrors `who_cvd.pilot_country`); a pilot grid search overwrites `calibration_factors.csv` / `adjusted_tps` |
| `who_cvd.settings_only` (`FALSE`) | `TRUE` makes `00_run_model.R` define the settings and stop before stage 02 (used by standalone 03/031) |

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
- `R/` — `engine.R` / `utils.R` left over from an abandoned SEAHEARTS port; not referenced by anything
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
- **Age groups**: 5-year bins 20–24 through 85+. `create_age_groups()` in `01_utils.R`; `cal_age_group()` in `03_calibration.R` produces GBD-style labels (20–24 … 95+) for the calibration and trend merges.
- **Region/income groupings**: `data/processed/Country_groupings_extended.csv` (WHO region + World Bank income).
- **Causes / no dementia**: the project models only the 4 CVD causes + "All causes" (`cause_map` in `00_run_model.R`; `adjusted_searo` keeps full GBD cause names, which 05 renames to the abbreviations). Never add a dementia (`aod`) node. Known inconsistency, not yet fixed: `021_get_base_rates.R` redefines `cause_map` with dementia, so the input `BG.mx.all` = ALL − ΣCVD − dementia, and dementia deaths drop out of model mortality in 2000–2019. Stage 031 re-anchors the projected years on `ALL.mx − ΣDIS` (dementia included), which leaves a step at 2020. The fix belongs in 021, followed by regenerating 022/03/031. `00` restores its own `cause_map` after a stage-02 rebuild.

### Key in-memory objects

- `b_rates` — baseline transition rates by location/year/age/sex/cause
- `data.in` — BP distribution inputs (mean SBP, SD, by BP category), from `bp_data6.csv`
- `inc` — HTN control coverage scale-up trajectories (`covfxn2.csv`)
- `ETIHAD_RR` / `ETIHAD_RR_BIN` — RR lookups for the BP intervention (BPLTTC 2021; `ettehad_rr_bp_reduction_*_bplttc_2021.*`; the non-suffixed files are the older Ettehad versions)
- `dt_gbd_rr` — GBD 2019 RR per 10 mmHg
- `dt_hbp_control` (`hbp_control_data.rds`), `dt_hbp_targets` (long BP target table from 04)
- `dt_statin_scenarios` (`statin_data.rds`), `dt_af_statins`; `dt_tfa_scenarios` (Aim 2 only)

### Data locations

`data/processed/` is version-controlled and holds the calibrated/derived inputs (`adjusted_tps_part*.rds` + `calibration_manifest.csv` + `calibration_factors.csv` / `calibration_diagnostics.csv` from stage 03, `adjusted_searo_part*.rds` + `trends_manifest.csv` from stage 031, `tps_inpt_part*.rds`, `tps_mortality_coherent_*.rds`, `baseline_rates_part*.rds`, `tps_bgmx_*.rds`, `bp_data6.csv`, `covfxn2.csv`, `wpp.adj.Rda`, `Scenarios.xlsx`, `htn_control_targets_*.csv`, `statins_control_targets_by_loc.csv`, statin/sodium/TFA scenario RDS files).

`data/raw/` is gitignored (`data/raw/**/*.*`, README files excepted). Needed there: GBD 2023 extracts, WHO GHE CVD/stroke CSVs, NCD-RisC hypertension estimates, `IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx`, and the three economic-valuation files listed above.

Also gitignored: `output/out_model/`, `output/dt_output_dalys.rds`, `*.html` (except `docs/*.html`), `*.doc`/`*.docx`, `.claude/worktrees/`, `output_psa/`, `output_dsa/`. `scenarios_psa/` and `scenarios_dsa/` (except `*.html`) are tracked.

The per-directory `README.md` files (`data/`, `docs/`, `output/`, `scenarios/`, `tests/`, `dev/`, `config/`, `manuscript/`, `library/`, …) are all copies of the root `README.md` boilerplate — they do not describe their directories.
