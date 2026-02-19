# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the Model

This is a pure R project. There is no build system, package manager, or test suite — the analysis is run by sourcing R scripts in RStudio or from an R console.

**To run the full pipeline:**
```r
# Open uw-who-cvdtargets.Rproj in RStudio, then:
source("code/00_run_model.R")
```

**Note:** `00_run_model.R` currently sources scripts with legacy filenames (e.g. `functions_review_6_100.R`, `1.get_base_rates_100.R`) that are **not** in the `code/` directory of this repository. The numbered scripts in `code/` (`01_utils.R` through `05_run_scenarios.R`) represent the reorganized/renamed versions of this pipeline.

**Working directories** are hardcoded in `00_run_model.R` to local OneDrive paths. These must be updated when running on a new machine. Key path variables:
- `wd` — repo root (also used as `wd_data` base for processed data)
- `wd_raw` — raw data directory (not version-controlled)
- `wd_data` — `data/processed/` subdirectory
- `wd_outp` — `output/` subdirectory
- `wd_temp` — temporary processing directory

## Architecture

### Pipeline Overview

The model is a multi-state population projection that runs country-by-country in parallel. Each country goes through eight intervention scenarios (baseline, each of four interventions alone, and combinations).

**Script execution order:**

| Script | Role |
|--------|------|
| `01_utils.R` | Shared utility functions sourced first |
| `02_load_inputs.R` | Loads sodium, TFA, statins, BP data; creates 2025–2050 scenarios |
| `03_clean_inputs.R` | Currently identical to `02_load_inputs.R` — likely a work in progress |
| `04_build_baseline.R` | Merges adjusted baseline rates (`*adjusted*.rds`) with UNWPP 2024 populations |
| `05_run_scenarios.R` | Defines intervention functions, runs parallel country-level projections |

### Disease Model

The core is a **discrete-time state-transition model** (2017–2050) with three health states per cause: **Well → Sick → Dead**. Transition rates (`IR` = incidence, `CF` = case fatality, `BG.mx` = background mortality) come from GBD 2023, adjusted for COVID excess mortality.

Four causes are modelled jointly:
- `ihd` — Ischemic heart disease
- `istroke` — Ischemic stroke
- `hstroke` — Intracerebral hemorrhage (hemorrhagic stroke)
- `hhd` — Hypertensive heart disease

### Four Intervention Modules (in `05_run_scenarios.R`)

| Function | Intervention | Key parameters |
|----------|-------------|----------------|
| `calculate_antihypertensive_impact_etihad()` | BP drug treatment scale-up | `target_control`, `control_start_year`, `control_target_year` |
| `calculate_sodium_impact_etihad()` | Dietary sodium reduction | `saltmet`, `salteff`, `saltyear1`, `saltyear2` |
| `calculate_tfa_impact()` | Trans-fat elimination policy | `tfa_target_tfa`, `tfa_policy_start_year` |
| `calculate_statins_impact()` | Lipid-lowering therapy | `statin_target_coverage`, `statin_start_year`, `statin_target_year` |

Interventions apply **multiplicative relative risk reductions** to incidence rates. The eight scenarios run via `run_multiple_scenarios()` are:

```r
scenarios <- list(
  baseline      = character(0),
  bp_only       = "antihypertensive",
  sodium_only   = "sodium",
  tfa_only      = "tfa",
  statins_only  = "statins",
  bp_sodium     = c("antihypertensive", "sodium"),
  bp_sodium_tfa = c("antihypertensive", "sodium", "tfa"),
  all_four      = c("antihypertensive", "sodium", "tfa", "statins")
)
```

### Parallel Execution

`05_run_scenarios.R` uses `doParallel`/`foreach` to run all countries simultaneously. Each country writes its own output file (`output/out_model/model_output_part_<country>.rds`) and log file. Greenland and Bermuda are excluded. On failure, the error is logged and `NULL` returned; the run continues.

### Key Data Objects

- `b_rates` — core data.table of baseline transition rates by location/year/age/sex/cause
- `data.in` — blood pressure distribution data (mean SBP, SD, by BP category)
- `inc` — hypertension control coverage scale-up trajectories (`covfxn2.csv`)
- `ETIHAD_RR` / `ETIHAD_RR_BIN` — relative risk lookup tables for BP intervention
- `dt_tfa_scenarios`, `dt_statin_scenarios` — pre-built scenario trajectories

### Data Conventions

- **Location names** follow GBD 2023 naming. Multiple `name_map` lookup vectors exist throughout the scripts to harmonize names across sources (NCD-RisC, UNWPP, GBD). If joining fails silently, check location name alignment first.
- **Population** uses UNWPP 2024 single-year age (`PopulationsSingleAge0050.rds`), replacing GBD-based `Nx` where available. Ages 95+ are collapsed to 95.
- **Age groups**: 5-year bins from 20–24 to 85+. The `create_age_groups()` function in `01_utils.R` produces these; a separate `create_gbd_age_group()` function in `05_run_scenarios.R` produces GBD-style labels (20–24 through 95+) for adjustment merges.
- Control flags in `00_run_model.R` (e.g. `run_adjustment_model`, `run_bgmx_trend`, `run_CF_trend`) gate optional processing blocks in `05_run_scenarios.R`.

### Raw Data (not in git)

Raw inputs are excluded by `.gitignore` (`data/raw/**/*.*`). Key files needed:
- `data/raw/GBD/totalpop_ihme.rds` — GBD population
- `data/processed/Sodium/sodium_data.rds`
- `data/processed/UN2024/PopulationsSingleAge0050.rds`
- `data/processed/UN2024/PopulationsAge20_2050.csv`
- `data/processed/covfxn2.csv` — HTN control coverage scale-up
- `data/processed/*adjusted*.rds` — calibrated baseline rates (one per location)
- `wpp.adj.Rda` — COVID excess mortality adjustments
- `bp_data6.csv` — blood pressure distribution inputs
- `adjustments2023_age.csv` — IR/CF adjustment factors
