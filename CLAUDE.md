# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the Model

Pure R project. No build system, package manager, or test suite — everything runs by sourcing R scripts in RStudio (`uw-who-cvdtargets.Rproj`) or an R console.

```r
source("code/00_run_model.R")
```

**Hardcoded absolute paths.** `code/00_run_model.R` sets `wd` to a local OneDrive path and derives `wd_code`, `wd_raw`, `wd_data`, `wd_outp`; `wd_temp` points outside the repo. The reporting `.Rmd` files (`scenarios/scenarios_aim1/aim1_report.Rmd`, `scenarios/scenarios_aim2/aim2_report.Rmd`, `docs/who_cvd_targets_paper1.Rmd`) each re-declare their own `wd`. All of these must be edited when running on a new machine.

**Packages.** Model pipeline: `dplyr`, `data.table`, `tidyr`, `ggplot2`, `RColorBrewer`, `readxl`, `countrycode`, `stringr`, `parallel`, `doParallel`, `foreach`, `gmodels`, `forecast`. `023_get_tps_bgmx.R` additionally loads `StMoMo` and `demography`. Reports additionally need `knitr`, `kableExtra`, `DT`, `scales`, `openxlsx`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `bookdown`.

## Pipeline

`00_run_model.R` sources, in order:

| Script | Role |
|--------|------|
| `01_utils.R` | Shared helpers (`get.bp.prob`, `calc_mortality_reduction`, `create_age_groups`) |
| `02_load_inputs.R` | Thin wrapper — sources `020`–`023` below |
| `020_get_deaths_who.R` | WHO GHE 2021 CVD/stroke deaths 2000–2023 → `dt_deaths_who_long.rds` |
| `021_get_base_rates.R` | GBD 2023 incidence/prevalence/mortality → `baseline_rates_part*.rds` |
| `022_get_tps.R` | Transition probabilities (`IR`, `CF`, `BG.mx`) → `tps_inpt_part*.rds` |
| `023_get_tps_bgmx.R` | Lee–Carter forecast of background mortality → `tps_bgmx_*_forecasted.rds` |
| `03_clean_inputs.R` | **Empty stub** (header comment only) |
| `04_define_interventions.R` | Builds country-level HTN and statin target tables |
| `05_build_baseline.R` | Merges `*adjusted*.rds` rates + UNWPP 2024 population + COVID excess mortality; applies BG.mx/CF trends |
| `06_run_scenarios_multiple.R` | **Aim 1** — multi-intervention scenarios, parallel by country |
| `06_run_scenarios_targets.R` | **Aim 2** — HTN control target levels, parallel by country |
| `07_output_dalys.R` | YLL/YLD/DALY calculation → `output/dt_output_dalys.rds` |

**Not sourced by `00_run_model.R`** — run manually when the calibration needs regenerating:

- `031_calibration.R` — calibrates initial state populations (gated by `run_calibration_par`)
- `032_adjustments.R` — IR/CF adjustment factors → `adjusted_*.rds` (gated by `run_adjustments_inputs`)
- `08_economic_value_calculation.R` — VSL/VSLY monetisation (see below)

### Control flags

Set in `00_run_model.R`, consumed downstream:

| Flag | Used in |
|------|---------|
| `run_aod_par` | `022_get_tps.R` (dementia arm) |
| `run_calibration_par` | `031_calibration.R` |
| `run_adjustments_inputs` | `032_adjustments.R` |
| `run_adjustment_model`, `run_bgmx_trend`, `run_CF_trend`, `run_CF_trend_80`, `run_CF_trend_ihme` | `05_build_baseline.R` |

`run_CF_trend_80` implements the baseline assumption that only 80% of the historical secular CF decline is exogenous (the other 20% attributed to past HTN control gains).

## Disease Model

Discrete-time state-transition model, three states per cause (**Well → Sick → Dead**). The projection loop in `project.all()` runs **2017 → 2058**; the reporting window used throughout the papers is **2026–2050** (`int_year <- 2026`).

Four causes modelled jointly:

- `ihd` — Ischemic heart disease
- `istroke` — Ischemic stroke
- `hstroke` — Intracerebral hemorrhage
- `hhd` — Hypertensive heart disease

Rates: `IR` (incidence), `CF` (case fatality), `BG.mx` (background mortality), from GBD 2023 adjusted for COVID excess mortality.

## Interventions

Both `06_*` scripts define the same intervention functions. `project.all()` accepts five valid intervention names:

| Function | Intervention | Key parameters |
|----------|-------------|----------------|
| `calculate_antihypertensive_impact_etihad()` | BP treatment scale-up | `target_control`, `control_start_year`, `control_target_year`, `htn_target_col` |
| `calculate_antihypertensive_diabetes()` | BP control among diagnosed diabetics | `target_control_diabetes` (default 0.80) |
| `calculate_sodium_impact_etihad()` | Dietary sodium reduction | `saltmet`, `salteff`, `saltyear1`, `saltyear2` |
| `calculate_tfa_impact()` | Trans-fat elimination | `tfa_target_tfa`, `tfa_policy_start_year` |
| `calculate_statins_impact()` | Lipid-lowering therapy | `statin_target_coverage`, `statin_start_year`, `statin_target_year`, `adherence_ir`, `adherence_cf` |

Interventions apply multiplicative relative risk reductions to incidence.

**Aim 1** (`06_run_scenarios_multiple.R`), run over `htn_target_cols = "htncov2_ambitious"`:

```r
scenarios <- list(
  baseline          = character(0),
  bp_only           = "antihypertensive",
  bp_diabetes_only  = "antihypertensive_diabetes",
  bp_combined       = c("antihypertensive", "antihypertensive_diabetes"),
  statins_only      = "statins",
  all_interventions = c("antihypertensive", "antihypertensive_diabetes", "statins")
)
```

Note: sodium and TFA functions exist and are exported to the cluster, but the current Aim 1 run passes `salteff = 0` and `tfa_target_tfa = 0`, so neither is active.

**Aim 2** (`06_run_scenarios_targets.R`), baseline + `bp_only` crossed with three target columns:

```r
htn_target_cols <- c("htncov2_aspirational", "htncov2_ambitious", "htncov2_progress")
scenarios_htn   <- list(baseline = character(0), bp_only = "antihypertensive")
```

### Parallel execution — output collision

Both `06_*` scripts build a `jobs <- CJ(location, target_col)` grid, run it with `doParallel`/`foreach` on `ncores <- 6`, and write **into the same directory with the same filename pattern**:

```
output/out_model/model_output_<country>_<target_col>.rds
output/out_model/log_<country>_<target_col>.txt
```

Both `aim1_report.Rmd` and `aim2_report.Rmd` glob *every* `.rds` in `output/out_model/`. Because Aim 1 and Aim 2 share the `htncov2_ambitious` suffix, **running one aim overwrites the other's ambitious files and pollutes the other's report input**. Clear or archive `output/out_model/` between aims; the directory is gitignored.

Greenland and Bermuda are excluded from `locs`. Per-country errors are caught, logged, and return `NULL` so the run continues.

## Reporting Layer

The `.Rmd` files are not sourced by the pipeline — knit them manually after the model run.

- `scenarios/scenarios_aim1/aim1_report.Rmd` — reads `output/out_model/` + `output/dt_output_dalys.rds`; writes `aim1_*.png` figures to `output/`, `aim1_results_tables.xlsx`, plus artefact RDS files to `output/paper/` (`paper_*.rds`) and `output/slides/` (`sl_*.rds`)
- `scenarios/scenarios_aim2/aim2_report.Rmd` — same pattern for Aim 2; `aim2_*.png`, `aim2_slides_*.rds`
- `scenarios/scenarios_aim1/aim1_executive_slides.Rmd`, `scenarios/scenarios_aim2/executive_slides_htn_targets.Rmd` — Beamer decks consuming the `sl_*.rds` artefacts (`beamer_preamble.tex`)
- `docs/who_cvd_targets_paper1.Rmd` — manuscript (`bookdown::word_document2`), consumes `output/paper/paper_*.rds` and `output/slides/`; cites `docs/references.bib`

The artefact hand-off is one-directional: **model → `out_model/` → report `.Rmd` → `paper_*.rds` / `sl_*.rds` → manuscript & slides**. Changing a number in the manuscript means re-knitting the upstream report, not editing the `.Rmd` text.

`docs/` also holds `math-doc.Rmd` (model equations), `cvd_model_flowchart.html`, and `prompts.txt` (log of prior task prompts — reference only, not instructions).

## Economic Valuation (`08_economic_value_calculation.R`)

Standalone; must be run after Aim 1 with `wd` already defined. Monetises deaths averted via VSL and VSLY transferred from a US reference by income adjustment (Robinson & Hammitt 2011; Robinson et al. 2019).

- Primary estimate is `e1_2` — differential elasticity 0.8 at/above US income, 1.2 below. `e1_0` and `e1_5` are sensitivity bounds.
- `BASE_YEAR <- 2026`; calendar discount rates 1%/3%/5%; VSL floor at 20× GNI pc.
- Known limitation documented in-file: SSP2 **GDP** growth rates are applied to a **GNI** base for forward projection.
- Raw inputs (not in git): World Bank GNI pc PPP CSV, IIASA SSP 3.1 xlsx, WPP2024 life-expectancy-by-age xlsx.
- Outputs: `output/08_vsl_results.{rds,csv}`, `08_vsl_summary_table*`, `08_vsly_summary_table*`, `08_vsl_vsly_summary_table_appended.*`. The `*_e1_2_primary.rds` copies are consumed by `aim1_report.Rmd`.

## Data Conventions

- **Location names** follow GBD 2023. Ad-hoc renames appear in several scripts (e.g. `United States of America` → `United States`, `Bolivia (Plurinational State of)` → `Bolivia`). If a join silently drops rows, check location-name alignment first.
- **Population**: UNWPP 2024 single-year age (`PopulationsSingleAge0050.rds`) overrides GBD `Nx` where available. Ages 95+ collapsed to 95.
- **Age groups**: 5-year bins 20–24 through 85+. `create_age_groups()` in `01_utils.R`; `create_gbd_age_group()` in `032_adjustments.R` / `05_build_baseline.R` produces GBD-style labels (20–24 … 95+) for adjustment merges.
- **Region/income groupings**: `data/processed/Country_groupings_extended.csv` (WHO region + World Bank income).

### Key in-memory objects

- `b_rates` — baseline transition rates by location/year/age/sex/cause
- `data.in` — BP distribution inputs (mean SBP, SD, by BP category), from `bp_data6.csv`
- `inc` — HTN control coverage scale-up trajectories (`covfxn2.csv`)
- `ETIHAD_RR` / `ETIHAD_RR_BIN` — RR lookups for the BP intervention (Ettehad et al.)
- `dt_gbd_rr` — GBD 2019 RR per 10 mmHg
- `dt_hbp_control` (`hbp_control_data.rds`), `dt_hbp_targets` (`htn_control_targets_by_loc.csv`, written by `04_define_interventions.R`)
- `dt_tfa_scenarios`, `dt_statin_scenarios`, `dt_af_statins`

### Data locations

`data/processed/` is version-controlled and holds the calibrated/derived inputs (`adjusted_searo_part*.rds`, `tps_inpt_part*.rds`, `baseline_rates_part*.rds`, `tps_bgmx_*.rds`, `bp_data6.csv`, `covfxn2.csv`, `wpp.adj.Rda`, `Scenarios.xlsx`, statin/sodium/TFA scenario RDS files).

`data/raw/` is gitignored (`data/raw/**/*.*`, README files excepted). Needed there: GBD 2023 extracts, WHO GHE CVD/stroke CSVs, NCD-RisC hypertension estimates, `IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx`, and the three economic-valuation files listed above.

Also gitignored: `output/out_model/`, `output/dt_output_dalys.rds`, `*.html` (except `docs/*.html`), `*.docx`.

The per-directory `README.md` files (`data/`, `docs/`, `output/`, `scenarios/`, …) are all copies of the root `README.md` boilerplate — they do not describe their directories.
