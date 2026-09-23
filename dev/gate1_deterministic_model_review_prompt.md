# Claude Code prompt: Gate 1 deterministic-model review and PSA parameter register

Act as a senior health-modeling scientist, epidemiologist, and R code reviewer. Inspect the current repository and conduct a rigorous **Gate 1 review to freeze the deterministic model before implementing probabilistic sensitivity analysis (PSA) and 95% uncertainty intervals**.

## Scope

Review the complete deterministic pipeline, including:

- `code/00*.R`
- `code/01*.R`
- `code/02*.R`
- `code/03*.R`
- `code/04*.R`
- `code/05*.R`
- `code/06*.R`
- `code/07*.R`

Also inspect any directly referenced:

- input workbooks and processed data;
- mathematical documentation;
- tests;
- configuration files;
- reports, slides, and manuscript files when needed to verify how the model and policy scenarios are described;
- reference library or bibliography entries relevant to intervention effects.

Use the current working-tree versions as authoritative. Begin by recording the branch, commit, working-tree status, and any uncommitted changes. Do not assume earlier reviews remain accurate.

## Task boundaries

This is a **read-only review and deterministic-model freeze specification**.

Do not modify production scripts, inputs, reports, slides, manuscript files, or existing tests. The only files you may create are the two requested review deliverables in `dev/`.

You may run safe diagnostic commands, small deterministic tests, and restricted model checks if feasible. Do not launch the full multicountry model or a PSA. Clearly distinguish:

- findings verified directly from code or data;
- findings verified by running a diagnostic;
- inferences that have not been numerically tested;
- unresolved scientific decisions;
- recommended corrections.

Do not invent parameter values, confidence intervals, citations, covariance structures, or statistical distributions. If the repository lacks supporting evidence, record the item as unresolved.

The active intervention scope is limited to **blood-pressure control and statin treatment**. Sodium reduction and trans-fatty-acid (TFA) elimination are legacy components that have already been verified as not executed in the active intervention scenarios. Do not audit their effect evidence, include their parameters in the PSA register, or propose their reactivation. You may note their presence as legacy code, but treat them as out of scope. Confirm only that the active scenario call graph does not execute them.

## Primary objective

Determine whether the deterministic model is scientifically coherent, internally consistent, reproducible, and sufficiently frozen to serve as the central model for PSA.

Gate 1 must resolve, or explicitly identify the evidence or decision needed to resolve, the following eight areas.

### 1. Haemorrhagic-stroke incidence mismatch

Inspect the complete construction of baseline and intervention incidence for `hstroke`.

Specifically verify:

- which GBD relative-risk series is used to calculate the normalization factor `alpha`;
- which relative-risk series is used to construct `IR_bin`;
- whether either series is subsequently overwritten;
- whether the identity below holds before intervention effects are applied:

$$
\sum_b p_b IR_b = IR;
$$

- whether `eff_ir == 1` for `hstroke` when the intervention has zero incremental coverage or before intervention scale-up;
- whether any active BP-control or statin pathway unintentionally rescales haemorrhagic-stroke incidence;
- whether a previous correction is already present in the current working tree.

If the mismatch remains, specify the smallest scientifically coherent correction and a regression test. Do not implement it in this task.

### 2. BPLTTC 2021 versus Ettehad 2016 evidence

Inventory every BP-treatment effect source used across:

- Aim 1;
- Aim 2, if present in the reviewed pipeline;
- antihypertensive functions;
- documentation;
- reports, slides, and manuscript text.

For every source or parameter table, report:

- exact file and object;
- citation attributed to it;
- outcome;
- effect measure;
- BP reduction unit;
- baseline-SBP category;
- point estimate and available interval;
- model transition affected;
- whether the code value matches the cited evidence.

Determine whether Aim 1 and Aim 2 use different BP-effect evidence and whether the manuscript incorrectly labels BPLTTC-derived estimates as Ettehad estimates.

Recommend one primary evidence specification, but mark it as a scientific decision if the repository cannot resolve it.

### 3. Interpretation of the BPLTTC baseline-SBP strata

Critically evaluate whether the current model interprets BPLTTC estimates correctly.

Determine whether the BPLTTC Appendix S6 estimates are:

- treatment effects estimated separately within baseline-SBP strata; or
- marginal effects corresponding to sequential traversal of 10-mmHg SBP intervals.

Inspect how the model constructs `effect_size_nodiabetes` and `effect_size_diabetes`, including any cumulative products across BP categories.

Assess whether multiplying effects across baseline-SBP strata is scientifically justified. Clearly distinguish a baseline-risk subgroup effect from a dose-response increment.

Document:

- the current formula;
- its implicit causal and epidemiological assumptions;
- whether it is supported by the source;
- plausible alternative implementations;
- the recommended deterministic primary specification;
- any structural sensitivity analysis needed.

Do not silently accept the current cumulative construction simply because it reproduces the workbook.

### 4. Incidence effects versus case-fatality effects

Trace every antihypertensive effect applied to:

- disease incidence;
- case fatality;
- prevalent cases;
- mortality.

Determine whether trial effect estimates already include fatal and nonfatal cardiovascular events and whether adding separate case-fatality reductions may double count benefits.

For every case-fatality reduction parameter, identify:

- value;
- cause;
- exact code location;
- cited source;
- estimand;
- uncertainty information;
- whether it is applied to existing prevalent cases, incident cases, or both;
- whether it interacts with the baseline case-fatality trend.

Compare at least these potential deterministic specifications:

1. incidence effect only;
2. incidence plus current case-fatality effect;
3. another evidence-supported specification found in the repository.

Recommend the primary specification and define the others as structural sensitivity analyses where appropriate. If the evidence is insufficient, say so explicitly.

### 5. Statin point estimates and sources

Audit every statin-effect parameter, including:

- IHD incidence RR;
- ischaemic-stroke incidence RR;
- IHD case-fatality RR;
- ischaemic-stroke case-fatality RR;
- attributable fractions;
- the proportion of ischaemic stroke treated as atherosclerotic;
- adherence;
- baseline coverage;
- target coverage;
- scale-up start and target years.

Check whether any point estimate is accidentally taken from a confidence-limit value. In particular, verify whether `0.74` is intended as a point estimate or corresponds to the lower limit of an interval for a different estimate.

For each parameter, identify the exact supporting citation and whether the effect measure and modeled transition match. Mark unsupported values clearly. Recommend which statin parameters can remain in the deterministic primary model and which require correction or structural sensitivity analysis.

### 6. Harmonized 2026–2050 horizon

Audit all model, aggregation, and reporting code for:

- inclusion or exclusion of 2025;
- intervention start in 2026;
- target year;
- cumulative period;
- annual-average divisor;
- labels such as “2025–2050” or “2026–2050”;
- number of modeled years;
- years used for BAU and intervention subtraction.

The preferred reporting horizon is **2026–2050 inclusive**, which contains 25 years, unless the code or scientific protocol demonstrates otherwise.

List every mismatch and specify the required deterministic definition.

### 7. ASMR calculation

Audit every age-standardized mortality-rate calculation.

Verify:

- the numerator;
- the population denominator;
- whether population is incorrectly repeated or summed across causes;
- age grouping;
- treatment of the open-ended age group;
- standard-population source and weights;
- whether weights sum to one;
- whether the same method is used in the model report, slides, and manuscript;
- whether BAU and intervention ASMR use identical definitions.

Recommend one canonical function and provide its mathematical definition. Specify regression tests, including a check that population is counted once rather than once per cause.

### 8. Exact policy-scenario set

Inventory every scenario created or referenced across scripts and deliverables.

For each scenario, report:

- internal scenario ID;
- displayed label;
- interventions included;
- eligible population;
- baseline coverage;
- target coverage;
- start year;
- target year;
- post-target assumption;
- adherence;
- incidence effects;
- case-fatality effects;
- whether it is executed;
- whether it is reported;
- whether it is needed for PSA.

Pay particular attention to:

- `baseline`;
- `bp_no_diabetes_only`;
- `bp_diabetes_only`;
- `bp_combined`;
- `statins_only`;
- `all_interventions`, if this legacy ID is still used for the active BP-control-plus-statins scenario;
- any defined but unused scenario.

Recommend the exact frozen scenario set for the paper and identify scenarios that should be removed, renamed, retained only for validation, or added to the reported results. The reported intervention set must be limited to BP-control and statin scenarios. If `all_interventions` contains only BP control plus statins, recommend a clearer displayed label such as **Combined BP control and statin treatment**. If legacy sodium or TFA scenarios remain defined but are not called, classify them as inactive legacy code rather than active policy scenarios.

## Parameter register for future PSA

Create a comprehensive model-parameter register, not merely a list of parameters already known to have uncertainty intervals.

The register must cover at least:

- baseline incidence;
- baseline case fatality;
- background mortality;
- prevalence;
- BP-bin distributions;
- diabetes prevalence or diabetes share;
- baseline BP control;
- policy-target coverage;
- statin baseline and target coverage;
- intervention scale-up paths;
- BP incidence RRs;
- BP case-fatality reductions;
- statin incidence RRs;
- statin case-fatality RRs;
- statin attributable fractions;
- adherence;
- disability weights;
- life expectancy inputs;
- population projections;
- economic parameters if they are downstream of scripts 00–07 or required for later valuation;
- major structural assumptions that cannot properly be represented as ordinary probability distributions.

For each parameter, record:

- `parameter_id`;
- `parameter_family`;
- `human_readable_name`;
- `symbol`;
- `code_object`;
- `file`;
- `line_or_function`;
- `model_component`;
- `transition_affected`;
- `cause`;
- `age_dimension`;
- `sex_dimension`;
- `location_dimension`;
- `time_dimension`;
- `central_value_or_source`;
- `lower_bound`;
- `upper_bound`;
- `interval_level`;
- `effect_measure`;
- `scale`;
- `proposed_distribution`;
- `distribution_parameters`;
- `correlation_group`;
- `shared_draw_rule`;
- `source_citation`;
- `source_file`;
- `evidence_status`;
- `uncertainty_class`;
- `recommended_analysis`;
- `include_in_initial_psa`;
- `rationale`;
- `decision_required`;
- `notes`.

Use the following uncertainty classifications:

- `parameter`;
- `structural`;
- `policy`;
- `implementation`;
- `demographic`;
- `baseline_data`;
- `economic`;
- `fixed_definition`.

Use the following recommended-analysis categories:

- `PSA`;
- `DSA`;
- `structural_scenario`;
- `fixed`;
- `defer`;
- `unresolved`.

Only propose a probability distribution when its parameters can be derived from available evidence. For example:

- lognormal for positive RR parameters with a valid CI or SE;
- Beta only when the mean and variance, SE, CI, or effective sample size are available;
- coherent external draws for population or other correlated estimates;
- DSA or structural scenarios when no defensible probability distribution exists.

Use the correct formula for a 95% CI around an RR:

$$
SE\{\log(RR)\} =
\frac{\log(U)-\log(L)}{2\times1.96}.
$$

If the source interval is 99%, use the appropriate 99% critical value. If a 5-mmHg RR is transformed into a 10-mmHg RR by squaring, document:

$$
RR_{10}=RR_5^2,
\qquad
SE\{\log(RR_{10})\}=2SE\{\log(RR_5)\}.
$$

Do not assume independent sampling. Identify parameters that must share draws because they reuse the same evidence. Record missing covariance explicitly.

## PSA Parameter Freeze Gate for uncertainty intervals

In addition to the deterministic-model Gate 1 decision, build a formal **PSA Parameter Freeze Gate** for the active BP-control and statin interventions. This gate must convert the broad parameter inventory into a proposed, auditable parameter set for generating 95% uncertainty intervals.

The purpose is to decide which parameters are ready to be sampled, not to run the PSA. Review every candidate parameter and assign exactly one freeze decision:

- `INCLUDE_IN_INITIAL_PSA`;
- `FIX_IN_INITIAL_PSA`;
- `EVALUATE_BY_DSA`;
- `EVALUATE_AS_STRUCTURAL_SCENARIO`;
- `DEFER_PENDING_EVIDENCE`;
- `EXCLUDE_NOT_RELEVANT`.

A parameter may be classified as `INCLUDE_IN_INITIAL_PSA` only when all of the following are documented:

1. the parameter is used by an active BP-control or statin pathway;
2. its epidemiological estimand matches the modeled transition;
3. its central value has a traceable source;
4. its uncertainty can be parameterized from a reported CI, SE, variance, posterior draws, or another defensible evidence source;
5. its sampling scale and distribution are justified;
6. bounds and transformations preserve valid model states;
7. shared-estimate and correlation rules are specified;
8. the draw can be injected without changing the meaning of the deterministic model;
9. the parameter has a defined effect on at least one reported estimate.

For each candidate parameter, add these freeze fields to the parameter register:

- `psa_freeze_decision`;
- `freeze_status` (`READY`, `CONDITIONAL`, or `BLOCKED`);
- `blocking_reason`;
- `required_evidence_or_decision`;
- `distribution_fully_parameterized`;
- `validity_bounds`;
- `dependency_or_correlation_rule`;
- `draw_level` (global, cause, country, age, sex, year, or combination);
- `bau_recomputed_each_draw`;
- `affected_scenarios`;
- `affected_reported_estimates`;
- `expected_direction_of_effect`;
- `validation_test_required`;
- `freeze_owner`;
- `freeze_date`;
- `approval_status`.

Explicitly separate these possible uncertainty domains:

1. **Intervention-effect uncertainty**: BP and statin RRs and any supported case-fatality effects.
2. **Baseline-data uncertainty**: incidence, case fatality, background mortality, prevalence, BP distribution, diabetes share, and baseline treatment coverage.
3. **Implementation uncertainty**: adherence and scale-up performance.
4. **Policy uncertainty**: target coverage and target year.
5. **Demographic uncertainty**: population projections.
6. **Outcome-construction uncertainty**: disability weights and life expectancy used for DALYs.
7. **Structural uncertainty**: incidence-only versus incidence-plus-case-fatality effects, BP-stratum interpretation, effect combination, and cause mapping.

Recommend a clearly defined **initial PSA scope** and, separately, an **extended PSA scope**. The initial scope should prioritize parameters that are both influential and supported by defensible uncertainty evidence. Do not include a parameter merely because it is uncertain.

For each proposed initial-PSA parameter, specify the complete sampling rule, including:

- source estimate;
- source interval and its confidence level;
- transformation to the modeled scale;
- distribution;
- distribution parameters;
- truncation or no-truncation rule;
- correlation or shared-draw rule;
- model object receiving the draw;
- scenarios affected;
- expected validation behavior.

Define whether BAU can remain fixed. If any initial-PSA parameter affects baseline disease rates, prevalence, baseline coverage, or population, require a paired draw design:

$$
D_{\text{averted}}^{(d)} = D_{\text{BAU}}^{(d)} - D_{\text{scenario}}^{(d)}.
$$

If the initial PSA varies only intervention-effect parameters that do not affect BAU, document why BAU may be computed once and reused.

Create a mapping from each reported model-derived estimate to the sampled parameters that contribute to its UI. Classify the resulting interval as one of:

- `FULL_PARAMETER_UI` — all prespecified parameter uncertainties relevant to the estimand are sampled;
- `PARTIAL_PARAMETER_UI` — only a subset is sampled;
- `STRUCTURAL_FIXED` — no sampled parameter affects the quantity;
- `NO_UI_JUSTIFIED` — a fixed policy definition or other justified exemption;
- `UNRESOLVED`.

Do not describe an interval as comprehensive when important relevant parameters remain fixed. Recommend exact manuscript language for describing the scope of the 95% UIs.

The PSA Parameter Freeze Gate passes only if:

- every included parameter has a complete and reproducible sampling specification;
- no unsupported precision has been invented;
- reused source estimates share draws appropriately;
- missing covariance is disclosed and addressed through a sensitivity option;
- every sampled parameter has a code-injection point and validation test;
- the BAU handling rule is correct for the frozen parameter set;
- every reported model-derived estimate has a UI-coverage classification;
- all blocked parameters are explicitly assigned to DSA, structural analysis, deferral, or a fixed-input limitation.

If these conditions are not satisfied, conclude **“PSA Parameter Freeze Gate not passed”** and list the blocking evidence and decisions. It is acceptable for the deterministic-model Gate 1 to pass while the PSA Parameter Freeze Gate remains blocked.

## Required diagnostic tests

Where feasible, run or specify tests for:

1. Baseline reconstruction: $\sum_b p_bIR_b=IR$.
2. Zero-effect identity: intervention outputs equal BAU when all intervention effects are neutral.
3. Pre-scale-up identity: intervention and BAU rates are identical before 2026.
4. Scenario nesting: `bp_combined` contains both mutually exclusive hypertension subgroups exactly once.
5. Population conservation: population is not duplicated across causes.
6. ASMR denominator and weight checks.
7. Horizon: exactly 25 intervention years from 2026 through 2050.
8. Combined intervention algebra: identify whether effects are additive, multiplicative, or sequential and whether the model is sub-additive.
9. Reproducibility: repeated deterministic runs produce identical outputs.
10. Parameter provenance: every effect value appearing in code maps to exactly one evidence-register record or is explicitly marked unsupported.
11. PSA freeze completeness: every candidate parameter has one freeze decision and no included parameter lacks a complete sampling rule.
12. UI coverage: every reported model-derived estimate is classified as full, partial, fixed, exempt, or unresolved.

Do not declare a test passed unless it was actually executed. Otherwise label it “specified but not run.”

## Gate 1 decision framework

Assign each issue one status:

- `PASS`;
- `PASS WITH DOCUMENTATION CHANGE`;
- `FAIL — CODE DEFECT`;
- `FAIL — SCIENTIFIC DECISION REQUIRED`;
- `FAIL — EVIDENCE MISSING`;
- `NOT APPLICABLE`.

The deterministic model can be declared frozen only if:

- no unresolved code defects affect central estimates;
- all primary intervention effects have a documented source and estimand;
- the BP-stratum interpretation is scientifically approved;
- incidence and case-fatality effects do not create undocumented double counting;
- the scenario set and horizon are unambiguous;
- ASMR is correctly and consistently calculated;
- every primary model parameter has traceable provenance;
- central outputs can be reproduced deterministically.

If these conditions are not met, conclude **“Gate 1 not passed”** and provide a prioritized blocking-action list. Do not soften unresolved scientific issues into minor recommendations.

## Required deliverables

Create exactly these two files:

1. `dev/gate1_deterministic_model_review.md`
2. `dev/gate1_psa_parameter_register.xlsx`

### Markdown report structure

The Markdown report must contain:

1. Executive conclusion: Gate 1 passed or not passed.
2. Repository state and files inspected.
3. Deterministic pipeline map from scripts 00–07.
4. Findings for each of the eight required areas.
5. Verified defects.
6. Scientific interpretation issues.
7. Evidence and citation inconsistencies.
8. Frozen scenario-definition table.
9. Horizon and ASMR definitions.
10. Parameter-register summary.
11. PSA Parameter Freeze Gate conclusion.
12. Frozen initial-PSA parameter set.
13. Extended or deferred parameter set.
14. Mapping from reported estimates to UI-generating parameters.
15. Recommended manuscript language describing the UI scope.
16. Diagnostic tests executed and results.
17. Required regression tests.
18. Prioritized blocking actions.
19. Proposed freeze criteria.
20. Explicit list of questions requiring investigator or coauthor decisions.
21. Recommended next step for PSA implementation after both gates are passed.

For each finding, include exact file paths and line numbers or function names. Use concise status tags such as `[VERIFIED]`, `[TESTED]`, `[INFERRED]`, `[UNRESOLVED]`, and `[RECOMMENDATION]`.

### Excel workbook structure

Create a formatted `.xlsx` workbook with these sheets:

1. `README`
   - purpose;
   - review date;
   - branch and commit;
   - status definitions;
   - uncertainty classifications;
   - distribution rules;
   - instructions for updating the register.

2. `Gate1_Decisions`
   - one row for each of the eight Gate 1 issues;
   - current behavior;
   - evidence;
   - status;
   - recommended primary specification;
   - blocking action;
   - decision owner;
   - decision required.

3. `Parameter_Register`
   - the complete parameter-register columns listed above;
   - one parameter per row or one row per parameter-by-cause when values or evidence differ.

4. `PSA_Freeze_Gate`
   - one row per candidate parameter;
   - freeze decision and readiness status;
   - full sampling rule for included parameters;
   - blocking evidence or decision for conditional and blocked parameters;
   - affected scenarios and reported estimates;
   - BAU recomputation requirement;
   - required validation test and approval status.

5. `UI_Coverage_Map`
   - one row per reported estimate family;
   - active scenario and output metric;
   - contributing sampled parameters;
   - important fixed parameters;
   - UI classification;
   - proposed reporting note or limitation.

6. `Scenario_Definitions`
   - exact scenario IDs and all scenario components.

7. `Evidence_Map`
   - parameter-to-source and code-to-citation mapping;
   - flag mismatches, missing sources, and reused evidence.

8. `Diagnostics`
   - test ID;
   - test description;
   - executed or specified;
   - scope;
   - result;
   - tolerance;
   - evidence;
   - required follow-up.

9. `Change_List`
   - prioritized future code and documentation changes;
   - affected files;
   - scientific versus technical classification;
   - whether the change alters central estimates;
   - required regression test.

10. `Open_Questions`
   - questions requiring investigator, statistician, clinical expert, or coauthor decisions.

Format the workbook professionally:

- freeze header rows;
- enable filters;
- use readable column widths and wrapped text;
- use consistent status colors;
- preserve numeric cells as numeric values rather than formatted text;
- include no formulas that depend on unavailable external workbooks;
- ensure all sheet names and required columns are present.

After creating the workbook, reopen it programmatically and verify that:

- it is a valid `.xlsx`;
- every required sheet exists;
- headers are complete;
- row counts are plausible;
- numeric fields are stored as numeric values where applicable.

## Final response

After completing the review:

- state whether Gate 1 passed;
- state separately whether the PSA Parameter Freeze Gate passed;
- list the most important blocking findings;
- summarize the parameters frozen for the initial PSA and those assigned to DSA, structural analysis, or deferral;
- report which diagnostics were actually run;
- provide the exact paths to the two created files;
- confirm that no production code or existing deliverable was modified.

Do not implement PSA, generate uncertainty intervals, or edit the deterministic model during this task.
