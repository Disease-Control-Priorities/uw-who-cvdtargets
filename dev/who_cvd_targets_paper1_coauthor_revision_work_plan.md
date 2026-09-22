# Work Plan to Complete WHO CVD Targets Paper 1

## Purpose

This plan translates the coauthor comments, email exchange, current model code, figures, tables, supplement, and `who_cvd_targets_paper1.Rmd` into an ordered set of decisions and tasks required to finish Paper 1.

The recommended endpoint is a reproducible, health-outcomes manuscript reporting cardiovascular deaths, YLLs, YLDs, and DALYs for the WHO cardiovascular targets, with uncertainty intervals and documentation aligned with GATHER. The economic valuation should be developed as a separate paper, consistent with the email decision to split health and economic gains.

## Materials Reviewed

- Coauthor email exchange through August 2026.
- `who_cvd_targets_paper1.Rmd`.
- `Main Figures _Paper 1_V2_03082026.docx`.
- `Main Tables_paper 1_V2_03082026.docx`.
- `Supplementary Material_Paper 1_V2_03082026.docx`.
- Model scripts `00_run_model.R` through `08_economic_value_calculation.R` supplied with the paper materials.

The latest commented narrative Word manuscript was not included. It should be added before preparing the final response-to-coauthors matrix so that no narrative comments are missed.

## Executive Priority

Do not begin another full 190-country run until three specifications are frozen:

1. How the target of 150 million additional people with controlled hypertension relates to the target of 80% blood pressure control among people with diabetes.
2. How the updated BPLTTC relative risks affect incidence, case fatality, and cause-specific outcome mappings.
3. Whether Paper 1 is definitively health-only, with all economic valuation moved to Paper 2.

These decisions determine the scenarios, equations, parameter tables, model outputs, and manuscript language. Resolving them first prevents another internally inconsistent results release.

## Major Issues Identified

### 1. Policy targets are not represented consistently

The R Markdown describes three separate targets:

- 150 million additional people with controlled hypertension by 2030.
- 80% blood pressure control among people with diabetes by 2030.
- 50% statin coverage among eligible adults aged 40 years or older by 2030.

However, the current target-building code allocates exactly 150 million additional controlled people between mutually exclusive diabetes and non-diabetes groups. Under its default specification, 80% diabetes control is a ceiling rather than a target that must be achieved. The code comments indicate that imposing 80% as a hard diabetes target could itself exceed 150 million additional controlled people.

The manuscript cannot claim simultaneous achievement of both targets if the implemented scenario does not achieve 80% diabetes control.

### 2. The updated BP evidence is not synchronized across code and manuscript

The model loads an updated RR input derived from BPLTTC 2021, but many objects, functions, equations, parameter tables, and citations still refer to Ettehad 2016. Fixed Ettehad-based case-fatality reductions also remain in the model.

The paper must distinguish:

- GBD exposure-risk RRs used to distribute baseline incidence across BP categories.
- Trial-based treatment RRs used to estimate the effect of improved control.
- Any independent evidence used to modify case fatality among prevalent cases.

Applying a trial event RR to incidence and then applying a second treatment effect to case fatality may double-count benefit unless the case-fatality effect is supported by separate evidence and a clearly defined estimand.

### 3. The diabetes equations in the R Markdown are obsolete

The revised code creates mutually exclusive diabetes and non-diabetes hypertension subgroups and combines their effects as a population mixture. The R Markdown and supplement still describe the earlier method based on diabetes-prevalence weighting, separate downscaling, and multiplicative BP effect ratios.

The source document therefore does not describe the current implementation.

### 4. GATHER requirements are incomplete

The R Markdown explicitly states that probabilistic sensitivity analysis is deferred and reports only central estimates. This leaves key GATHER items incomplete, particularly:

- Model evaluation and relevant sensitivity analyses.
- Methods used to calculate uncertainty.
- Sources of uncertainty included and excluded.
- Quantitative uncertainty intervals.
- Efficiently extractable input data and published estimates.

Reference: WHO GATHER checklist, <https://www.who.int/publications/m/item/gather-checklist>.

### 5. The health and economic papers have not been separated in the source document

The R Markdown still contains:

- “health and economic gains” in the title, introduction, aims, and discussion.
- Economic artifacts loaded during setup.
- Economic methods and results.
- Economic Table 6.
- Supplemental Figure S8.
- A full economic-methods appendix.

This conflicts with the agreement in the email exchange to create separate health and economic papers.

### 6. Figures and tables remain in an intermediate review state

- The requested “All Interventions” line and “General Population” label have been added to Figure 1.
- A regional result has been added.
- Two alternative regional histograms remain in the tables document even though coauthors selected version 1.
- Table 4 and multiple regional visualizations create duplication.
- Tracked deleted and inserted figures remain visible in the review files.
- The R Markdown currently specifies a regional cumulative line chart, whereas the coauthor thread preferred the first histogram.

### 7. The manuscript build is not yet portable or fully reproducible

`who_cvd_targets_paper1.Rmd` uses hard-coded Windows paths and reads precomputed RDS objects and PNG files. It therefore depends on prior execution of another report pipeline and can silently combine stale outputs with revised narrative text.

The source also contains manual values and outdated captions, including a supplemental caption describing the general hypertension scenario as 50% control even though the abstract describes the 150-million target.

## Decision Gate Before Reanalysis

### Decision 1: Relationship between the two hypertension targets

Prepare a short options memo for David and the WHO coauthors.

| Option | Definition | Interpretation |
|---|---|---|
| A. Separate policy scenarios | Model +150M general hypertension control and 80% diabetes control as separate scenarios; the combined scenario implements both and may exceed +150M | Most faithful if the commitments are independent |
| B. Nested exact-150M scenario | Allocate exactly +150M across diabetes and non-diabetes groups; 80% is only a cap | Cannot be described as achieving the 80% diabetes target unless it actually does |
| C. Diabetes-first allocation | Raise diabetes control to 80%, then allocate any remaining portion of +150M to non-diabetes groups; allow the total to exceed +150M if diabetes alone requires it | Prioritizes literal achievement of the diabetes target |

Recommended primary specification: **Option A**, unless WHO confirms that +150M is intended as a binding total that includes and supersedes the diabetes target. Report the other coherent interpretations as structural sensitivity analyses.

### Decision 2: BP treatment-effect estimand

Agree whether each BPLTTC estimate represents:

- First or recurrent cardiovascular events.
- Incident disease.
- Fatal plus non-fatal events.
- Cause-specific mortality.
- Effects among people with or without diabetes.

Recommended primary specification: apply treatment RRs to incident cardiovascular events and allow deaths to decline through fewer incident cases. Apply an additional case-fatality effect only when it comes from independent evidence that directly estimates mortality among prevalent cases.

### Decision 3: Paper scope

Recommended decision: Paper 1 is health-only. Remove all VSL/VSLY methods, economic results, economic figures, economic tables, and economic claims. Retain the economic scripts and artifacts for Paper 2 without deleting them from the repository.

### Decision 4: Regional display

Use the coauthor-selected histogram version 1 as the regional main figure. Retain exact regional values in a table, preferably in the supplement or a machine-readable results file.

## Workstream 1: Freeze the Analysis Specification

### Tasks

- [ ] Create one row per intervention defining the policy wording, denominator, baseline coverage, target, start year, target year, effect parameter, and eligible causes.
- [ ] Specify whether “people with diabetes” means all people with diagnosed diabetes or only people with both diabetes and hypertension.
- [ ] Specify whether statin eligibility means all adults aged 40 years or older, all adults with diabetes aged 40 years or older, or a risk-based eligible population.
- [ ] Define BAU precisely: coverage held at 2024 or 2025 values, risk-factor distributions held constant, and the secular mortality trend retained.
- [ ] Define which scenarios are primary and which are sensitivity analyses.
- [ ] Obtain written coauthor approval of this specification before the full rerun.

### Deliverable

`analysis_specification_paper1.md` and a matching machine-readable scenario table.

## Workstream 2: Update and Validate BP Treatment Effects

### Tasks

- [ ] Create a versioned BP-effect input with outcome, BP category, diabetes subgroup, RR, lower bound, upper bound, effect scale, transformation, and source.
- [ ] Replace misleading `ETIHAD_*` names with evidence-neutral names such as `bp_treatment_rr` where BPLTTC values are used.
- [ ] Document conversion from the original BPLTTC effect scale to the model scale using the log-RR scale.
- [ ] Confirm the mapping of coronary heart disease to IHD.
- [ ] Adjudicate mapping of overall stroke effects to ischemic stroke and intracerebral hemorrhage.
- [ ] Adjudicate mapping of heart-failure effects to hypertensive heart disease.
- [ ] Decide whether the fixed case-fatality reductions remain, are replaced, or are removed from the primary analysis.
- [ ] Run a deterministic comparison of original Ettehad versus updated BPLTTC parameters and document the change in deaths and DALYs.

### Quality checks

- [ ] RR equals 1 when BP reduction equals zero.
- [ ] Conversions reproduce the published effect at the source scale.
- [ ] Lower and upper bounds remain ordered after transformation.
- [ ] Every modeled cause has an explicit, documented mapping.
- [ ] No treatment effect is applied twice to the same causal pathway.

### Deliverable

Versioned BP parameter file, mapping table, validation output, and a short explanation of why the updated evidence produced small or large changes.

## Workstream 3: Finalize the Diabetes and Non-Diabetes Model

### Tasks

- [ ] Preserve mutually exclusive hypertension subgroups.
- [ ] Verify whether the diabetes input is population prevalence or diabetes prevalence among people with hypertension.
- [ ] If direct overlap data are unavailable, state the independence or overlap assumption explicitly.
- [ ] Reconstruct subgroup baseline control so that the weighted average exactly reproduces observed overall control:

\[
C_H=(1-p_D)C_{H,\neg D}+p_D C_{H,D}.
\]

- [ ] Justify the assumed 15-percentage-point higher baseline control among people with diabetes.
- [ ] Evaluate control-gap sensitivities of 0, 10, 15, and 20 percentage points.
- [ ] Replace the obsolete diabetes equations in the R Markdown with the additive population-mixture equations used by the code.
- [ ] Ensure the combined scenario does not multiply effects for mutually exclusive diabetes and non-diabetes groups.

### Automated tests

- [ ] Diabetes and non-diabetes populations sum to the total hypertensive population.
- [ ] Subgroup controlled counts reproduce observed baseline controlled counts.
- [ ] No effect occurs when target control equals baseline control.
- [ ] Diabetes-specific effect disappears when diabetes prevalence is zero.
- [ ] Diabetes-only limiting behavior is correct when diabetes prevalence is one.
- [ ] Individual subgroup contributions sum to the combined BP contribution before interaction with statins.
- [ ] Policy targets reconcile to the chosen scenario definition.

### Deliverable

Tested subgroup module plus a global and regional target-reconciliation table.

## Workstream 4: Add Uncertainty and Sensitivity Analyses

### Probabilistic uncertainty

- [ ] Build distributions for trial log-RRs from reported confidence intervals.
- [ ] Incorporate uncertainty in baseline hypertension control and diabetes prevalence where estimates permit.
- [ ] Propagate GBD uncertainty draws for mortality, incidence, prevalence, and disability weights where computationally feasible.
- [ ] Incorporate uncertainty in statin effects and coverage.
- [ ] Preserve correlation structures when source draws are available; document independence assumptions otherwise.
- [ ] Use reproducible seeds and store draw-level metadata.
- [ ] Pilot approximately 100 draws on a subset of countries to assess runtime and stability.
- [ ] Run 500–1,000 full draws if computationally feasible.
- [ ] Report medians or means with 95% uncertainty intervals for deaths, YLLs, YLDs, and DALYs.

### Structural sensitivity analyses

- [ ] BPLTTC versus Ettehad treatment effects.
- [ ] Incidence-only BP effects versus incidence plus independently supported case-fatality effects.
- [ ] Alternative interpretations of the +150M and 80% diabetes targets.
- [ ] Alternative diabetes-overlap assumptions.
- [ ] Alternative baseline diabetes-control gaps.
- [ ] Linear, delayed, and accelerated scale-up trajectories.
- [ ] Full versus partial adherence.
- [ ] Alternative secular mortality trends, including the current 80% trend assumption.
- [ ] Alternative stroke-subtype and heart-failure/HHD mappings.

### Deliverable

Primary 95% uncertainty intervals, a structural-sensitivity table, and an uncertainty appendix listing included and excluded sources.

## Workstream 5: Validate the Model

### Internal validation

- [ ] Confirm no negative state populations or missing transition effects.
- [ ] Confirm population accounting and state-transition identities.
- [ ] Confirm bounded incidence, case fatality, and intervention multipliers.
- [ ] Confirm that a zero-coverage intervention reproduces BAU.
- [ ] Confirm that combined effects are multiplicative only across separate interventions.
- [ ] Confirm that the combined result is sub-additive when effects overlap multiplicatively.

### Calibration and temporal validation

- [ ] Compare modeled and observed deaths by year, country, sex, age, and cause for 2017–2023.
- [ ] Where feasible, calibrate on earlier years and reserve the latest observed year as a holdout.
- [ ] Report absolute and relative calibration errors globally and by WHO region.
- [ ] Inspect countries with the largest residual errors.

### External plausibility

- [ ] Confirm that short-horizon model effects under trial-like coverage reproduce the approximate source trial effects.
- [ ] Compare regional patterns with baseline CVD burden and coverage gaps.
- [ ] Flag countries with implausibly large per-capita effects or discontinuities.

### Deliverable

Validation report with observed-versus-modeled plots and performance metrics.

## Workstream 6: Rebuild the Reproducible Results Pipeline

### Tasks

- [ ] Replace the hard-coded Windows root in `who_cvd_targets_paper1.Rmd` with a project-root method such as `here::here()` or an R-project-relative configuration.
- [ ] Establish one orchestrator that runs input preparation, baseline construction, scenarios, DALYs, uncertainty, and publication outputs in the required order.
- [ ] Separate Paper 1 health outputs from Paper 2 economic outputs.
- [ ] Add metadata to every results release: git commit, input version, model version, run date, R version, scenario definition, and random seed.
- [ ] Add assertions that required RDS and figure files were generated by the current run rather than inherited from an older run.
- [ ] Eliminate manual manuscript values where they can be generated programmatically.
- [ ] Ensure `aim1_report.Rmd` and the paper source consume the same frozen output objects.
- [ ] Run the complete pipeline from a clean R session.

### Reconciliation checks

- [ ] Abstract totals equal main-text totals.
- [ ] Figures equal tables.
- [ ] Regional totals sum to the global total.
- [ ] Cause totals sum to the intervention total.
- [ ] Age-sex totals sum to the global total.
- [ ] YLL plus YLD equals DALY.
- [ ] Combined effects are not described as the arithmetic sum of individual interventions.

### Deliverable

A versioned, reproducible Paper 1 results release and run manifest.

## Workstream 7: Revise `who_cvd_targets_paper1.Rmd`

### Remove economic-paper content

- [ ] Change the title from “health and economic gains” to health gains only.
- [ ] Remove economic objectives and claims from the abstract, introduction, discussion, and conclusion.
- [ ] Remove loading of economic artifacts from the setup chunk.
- [ ] Remove the economic valuation methods section.
- [ ] Remove economic results and interpretation.
- [ ] Remove economic Table 6 and Supplemental Figure S8.
- [ ] Remove the economic-methods appendix.
- [ ] Retain the economic work in a separate Paper 2 source file.

### Synchronize the intervention methods

- [ ] Replace Ettehad citations and labels with the final BPLTTC-based specification where applicable.
- [ ] Explain the separate roles of GBD exposure RRs and trial treatment RRs.
- [ ] Rewrite the hypertension target section using the approved +150M/diabetes relationship.
- [ ] Replace the old diabetes-prevalence-weighting and downscaling equations.
- [ ] Correct the intervention-combination equations so mutually exclusive BP subgroups are combined as a population mixture before multiplication with statin effects.
- [ ] Revise the case-fatality equations after the scientific decision on double counting.
- [ ] Update the key parameter appendix with point estimates, uncertainty distributions, bounds, and sources.

### Strengthen reporting

- [ ] Add model selection and validation procedures.
- [ ] Add full uncertainty methods and results.
- [ ] Add explicit input inclusion, exclusion, and fallback rules.
- [ ] Add a data-source inventory with population, years, age range, measurement method, and access information.
- [ ] Verify that repository claims match files actually accessible in the public repository.
- [ ] Complete a GATHER checklist with manuscript page or section references.

### Correct identified source inconsistencies

- [ ] Replace the supplemental Figure S2 caption stating “50% control” with the approved general hypertension scenario.
- [ ] Remove references to “economic consequences” from the conceptual framework.
- [ ] Standardize 2025–2050 versus 2026–2050 definitions and explain whether 2025 is a baseline year or an outcome year.
- [ ] Standardize 2024 versus 2025 baseline coverage.
- [ ] Remove duplicated headings and other copy-editing artifacts.
- [ ] Update intervention display labels to precise policy names.

### Deliverable

Revised health-only R Markdown manuscript that compiles from a clean environment.

## Workstream 8: Finalize Figures and Tables

### Main figures

- [ ] Figure 1: retain the four lines, including “All Interventions,” and use “General Population” only if that label remains scientifically accurate after scenario finalization.
- [ ] Figure 2: retain the age-sex distribution after results are rerun.
- [ ] Figure 3: use regional histogram version 1, as selected in the coauthor comments.
- [ ] Add uncertainty intervals where appropriate.

### Main tables

- [ ] Add 95% uncertainty intervals to primary estimates.
- [ ] Update intervention names and target definitions.
- [ ] Correct the malformed Table 2 note.
- [ ] Decide whether the exact regional table remains in the main paper or moves to the supplement.
- [ ] Verify all totals against the frozen output release.

### Supplement

- [ ] Retain detailed regional and country results in machine-readable form.
- [ ] Add validation and sensitivity figures.
- [ ] Add the revised parameter-distribution table.
- [ ] Remove superseded economic material.

### Review cleanup

- [ ] Delete alternative regional figure version 2.
- [ ] Accept final tracked changes.
- [ ] Resolve all completed comment threads.
- [ ] Ensure no deleted figures remain visible.

### Deliverable

Publication-ready figures, tables, supplement, and machine-readable results.

## Workstream 9: Close Coauthor Comments and Prepare Submission

Create a response matrix with the following fields:

| ID | Coauthor comment | Scientific issue | Action taken | Files or sections changed | Rerun required | Status |
|---|---|---|---|---|---|---|
| 1 | Comment text | Target, evidence, methods, result, or presentation | Concise response | Exact locations | Yes or no | Open, awaiting decision, or complete |

Classify responses as:

- Accepted and implemented.
- Addressed through clarification.
- Requiring a coauthor decision.
- Not adopted, with scientific justification.

Before circulation:

- [ ] Obtain the latest commented narrative Word manuscript and add its comments to the matrix.
- [ ] Circulate one clean manuscript and one tracked version.
- [ ] Include a clean supplement, final figures, final tables, GATHER checklist, and response matrix.
- [ ] Confirm authorship, funding, conflicts, data availability, code availability, and acknowledgments.
- [ ] Apply the selected journal’s word limits and formatting requirements only after scientific sign-off.

## Proposed Schedule

| Period | Main work | Decision or output |
|---|---|---|
| Days 1–2 | Resolve target relationship, RR estimand, case-fatality treatment, paper scope, and regional display | Approved analysis specification |
| Days 3–6 | Finalize BPLTTC inputs, diabetes/non-diabetes module, cause mappings, and automated tests | Frozen deterministic model |
| Days 7–9 | Run deterministic scenarios and validation; pilot probabilistic analysis | Validated pilot outputs |
| Days 10–14 | Run full uncertainty and structural sensitivity analyses | Frozen results release |
| Days 15–18 | Revise R Markdown; regenerate figures, tables, and supplement | Complete revised manuscript package |
| Days 19–21 | Complete GATHER checklist, response matrix, reconciliation, and coauthor QA | Submission-ready package |

The schedule assumes that the coauthors resolve the three scientific decisions promptly and that the full-model uncertainty run is computationally feasible. If runtime is prohibitive, preserve the same order but extend the uncertainty phase rather than returning to central estimates only.

## Proposed Responsibilities

| Role | Suggested responsibility |
|---|---|
| William | Analysis specification draft, code changes, tests, reruns, result reconciliation, R Markdown revision, and response matrix |
| David | Scientific adjudication of target interpretation, treatment-effect estimand, case-fatality specification, and main narrative |
| WHO lead coauthors | Confirm policy-target interpretation, denominator definitions, WHO framing, and journal/clearance requirements |
| Clinical or methods coauthors | Review BPLTTC mappings, diabetes assumptions, statin eligibility, and uncertainty distributions |
| Figure/table contributor | Produce the selected regional histogram from the frozen outputs and ensure consistency with Table 4 |

These assignments are proposals and should be confirmed by the team.

## Definition of Done

Paper 1 is ready for submission when all of the following are true:

- [ ] The code and manuscript implement the same target definitions.
- [ ] The 150-million and 80% diabetes targets are described without logical contradiction.
- [ ] BPLTTC inputs and citations are used consistently.
- [ ] BP effects are not double-counted across incidence and case fatality.
- [ ] The diabetes and non-diabetes groups are mutually exclusive and reconcile to observed totals.
- [ ] Primary outcomes include quantitative 95% uncertainty intervals.
- [ ] Structural sensitivity analyses address the major modeling assumptions.
- [ ] Calibration and validation results are reported.
- [ ] The manuscript is health-only and the economic analysis is separated.
- [ ] Every number in the abstract, text, figures, tables, and supplement is generated from the same frozen output release.
- [ ] The R Markdown compiles from a clean environment without hard-coded user-specific paths.
- [ ] Coauthor comments are answered point by point.
- [ ] The GATHER checklist is complete.
- [ ] Final tracked changes and comments are resolved in the clean submission files.

## Immediate Next Action

Schedule a focused coauthor decision meeting using a one-page memo containing:

1. The three target-allocation options.
2. The proposed interpretation of BPLTTC RRs and case-fatality effects.
3. Confirmation that Paper 1 is health-only.
4. The proposed main regional histogram.

Once those four decisions are recorded, freeze the analysis specification and begin the deterministic rerun and validation sequence.
