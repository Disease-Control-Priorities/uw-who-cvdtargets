# Codex Prompt: Refactor Aim 2 BP-Control Scenarios by Diabetes Status

Review the full WHO CVD targets pipeline, focusing on:

- `data/processed/Scenarios.xlsx`
- `code/04_define_interventions.R`
- `code/06_run_scenarios_targets.R`
- Use `code/06_run_scenarios_multiple.R` only as a reference for existing diabetes-related code. Do not assume its current subgroup implementation is correct.

Then implement a transparent refactor of the Aim 2 blood-pressure-control scenario.

## Policy objective

The Aim 2 BP-control target is:

> By 2030, 150 million additional people with hypertension are controlled relative to baseline, using the 2025 population as the fixed denominator.

The 150 million must be the sum of:

1. Additional controlled people with hypertension and without diabetes.
2. Additional controlled people with hypertension and diabetes.

These two populations must be mutually exclusive and exhaustive within the hypertensive population. The “general” hypertension intervention must no longer include people with diabetes.

Retain the existing policy target of 80% BP control among people with diabetes by 2030 unless a clearly identified raw scenario input in `Scenarios.xlsx` specifies otherwise. The remaining number needed to reach 150 million must be assigned to hypertensive people without diabetes.

## Important findings to correct

1. `04_define_interventions.R` currently calculates the 150-million target and writes `htn_control_targets_by_loc.csv`, but a later “temporary” block reads calculated fields such as `htncov2_ambitious` from `Scenarios.xlsx` and overwrites that CSV. Remove this overwrite behavior.

2. `Scenarios.xlsx` currently calculates `htn_ctrl_diabetes` as overall `htncov2 + 0.15`. This does not preserve the observed overall control rate after splitting people by diabetes status.

3. The current general antihypertensive function applies a diabetes-weighted effect across the full hypertensive population. It therefore does not represent the population without diabetes.

4. The partial split in `06_run_scenarios_multiple.R` should not simply be copied. Its general and diabetes components are not fully mutually exclusive; the diabetes effect appears to be downscaled more than once, and multiplying the two aggregated subgroup effects introduces an inappropriate interaction term.

5. Script 04 currently collapses age-specific hypertension prevalence and diabetes data with `unique(..., by = c("location", "sex"))`. Do not select an arbitrary age row. Perform population weighting at the available location × sex × age level before aggregation.

## Required baseline decomposition

Let, for each relevant location–sex stratum:

- $H$ = number of people with hypertension in 2025.
- $w_D$ = share of hypertensive people with diabetes.
- $H_D = H w_D$ = hypertensive population with diabetes.
- $H_N = H(1-w_D)$ = hypertensive population without diabetes.
- $c$ = observed overall baseline hypertension-control rate.
- $\delta = 0.15$ = assumed diabetes versus non-diabetes control-rate difference.
- $c_D$ = baseline control among hypertensive people with diabetes.
- $c_N$ = baseline control among hypertensive people without diabetes.

Derive the subgroup control rates so that:

$$
c_D-c_N=0.15
$$

and

$$
c=(1-w_D)c_N+w_Dc_D.
$$

Therefore:

$$
c_N=c-w_D(0.15)
$$

and

$$
c_D=c+(1-w_D)(0.15).
$$

Do not use `c_D = c + 0.15`.

Verify explicitly that:

$$
H_Nc_N+H_Dc_D=Hc
$$

within a strict numerical tolerance.

All rates must be in $[0,1]$. Do not silently clamp values and thereby break the reconstruction identity. If the 15-percentage-point assumption is infeasible for a stratum, stop with an informative diagnostic listing the affected locations and strata, or implement a clearly documented bounded-gap rule that still preserves the observed overall rate.

## Diabetes denominator assumption

First determine what the existing `diabetes` variable represents.

If it is diabetes prevalence in the overall population rather than $P(D\mid H)$, do not silently label it as the share of hypertensive people with diabetes. If no joint hypertension–diabetes prevalence is available, retain the necessary simplifying assumption:

$$
P(D\mid H)=P(D),
$$

but make it explicit in:

- Variable names.
- Code comments.
- Diagnostics.
- The output audit table.

For example, use a name such as `diabetes_share_among_htn_assumed`.

Calculate the subgroup denominators using the 2025 population at the finest compatible location × sex × age level:

$$
H_j=N_{2025,j}\times p_{HTN,j}
$$

$$
H_{D,j}=H_j\times w_{D,j}
$$

$$
H_{N,j}=H_j\times(1-w_{D,j}).
$$

Aggregate only after these products have been calculated. Do not take unweighted means or arbitrary unique age rows.

## Target allocation in script 04

Refactor `04_define_interventions.R` so it explicitly computes the scale-up rates required for the Aim 2 target.

For the combined Aim 2 scenario:

1. Set the 2030 diabetes subgroup target to 80%, without reducing control in strata already above 80%:

   $$
   c_{D,2030}=\max(c_{D,2025},0.80).
   $$

2. Calculate additional controlled people with diabetes:

   $$
   \Delta_D=\sum H_D(c_{D,2030}-c_{D,2025}).
   $$

3. Calculate the residual target for hypertensive people without diabetes:

   $$
   R_N=150{,}000{,}000-\Delta_D.
   $$

4. Allocate $R_N$ across the non-diabetes subgroup using the current intended proportional scale-up approach. Solve for a global factor $s_N$:

   $$
   c_{N,2030,j}=\min(1,s_Nc_{N,2025,j})
   $$

   such that:

   $$
   \sum H_{N,j}(c_{N,2030,j}-c_{N,2025,j})=R_N.
   $$

   Because the target rates are capped at 1, use a transparent numerical root-solving procedure rather than assuming an uncapped closed-form factor.

5. Fail clearly if:

   - $\Delta_D>150$ million;
   - the non-diabetes population lacks sufficient remaining control capacity;
   - the root cannot be found;
   - required locations or population inputs are missing.

6. Confirm:

   $$
   \Delta_N+\Delta_D=150{,}000{,}000
   $$

   using the 2025 population and hypertensive-population denominators. Use a tight but practical tolerance, such as one person or $10^{-6}$ relative error.

The 2025 population must remain fixed for both the 2025 and 2030 controlled-person calculations. Do not use projected 2030 population to demonstrate achievement of the target.

## Scenario-based output from script 04

Replace the existing wide format—with scenario names embedded in columns such as `htncov2_aspirational`, `htncov2_ambitious`, and `htncov2_progress`—with a long, scenario-based input for script 06.

Produce one row per:

- `scenario_id`
- `location`
- `sex`, if supported by the source control data
- `subgroup`

Use unambiguous subgroup values such as:

- `htn_no_diabetes`
- `htn_diabetes`

The output should include at least:

- `scenario_id`
- `location`
- `sex`
- `subgroup`
- `baseline_year`
- `scaleup_start_year`
- `target_year`
- `population_2025`
- `htn_population_2025`
- `diabetes_share_among_htn_assumed`
- `baseline_control`
- `target_control`
- `annual_control_increment`
- `controlled_2025`
- `controlled_2030`
- `additional_controlled_2030`
- `allocation_method`

Continue writing a processed input such as `data/processed/htn_control_targets_by_loc.csv`, but make its structure scenario-based rather than using separate calculated target columns.

Also write a compact audit summary, for example `data/processed/htn_control_targets_summary.csv`, containing:

- Scenario target.
- Achieved total.
- Additional controlled without diabetes.
- Additional controlled with diabetes.
- Reconciliation difference.
- Diabetes target.
- Solved non-diabetes scale factor.
- Number of capped strata.
- Number of missing or excluded locations.

The raw policy assumptions may come from `Scenarios.xlsx`, but script 04—not spreadsheet-derived target coverage fields—must calculate all subgroup baselines, target rates, scale factors, and controlled-person counts.

Do not use `htn_ctrl_diabetes`, `controlled_2030_ambitious`, or similar calculated workbook columns as model inputs.

Preserve other existing scenarios only if they are still required downstream, but represent them as values of `scenario_id`, not as separate target columns.

## Required changes in `06_run_scenarios_targets.R`

Refactor script 06 to consume the new long scenario table.

### Scenario selection

Replace dynamic target-column logic such as `htn_target_col` with explicit selection by `scenario_id`.

Parallel jobs should be indexed by:

- `location`
- `scenario_id`

rather than by location × target-column name.

Preserve `htn_target_scenario` in the final model output if downstream scripts require it, but populate it with the scenario identifier rather than a column name.

### Mutually exclusive interventions

Use clear intervention names:

- `antihypertensive_no_diabetes`
- `antihypertensive_diabetes`

Define diagnostic scenarios such as:

- `baseline`
- `bp_no_diabetes_only`
- `bp_diabetes_only`
- `bp_combined`

The policy scenario for the 150-million target is `bp_combined`. The two “only” scenarios are useful decompositions, but neither alone should be described as achieving the full 150-million target.

### Applying subgroup effects

Prefer one transparent split-population antihypertensive function that accepts both subgroup baseline and target trajectories.

For each BP bin, age, sex, cause, location, and year:

1. Calculate the non-diabetes Ettehad effect using `effect_size_nodiabetes`.
2. Calculate the diabetes Ettehad effect using `effect_size_diabetes`.
3. Apply each effect only to its own population share.
4. Combine the two subgroup effects additively as a population mixture, not by multiplying two already aggregated effects.

Conceptually:

$$
IR^{new}_{bin}
=
IR_{bin}
\left[
(1-w_D)(1-e_{N,t})+
w_D(1-e_{D,t})
\right].
$$

Equivalently:

$$
IR^{new}_{bin}
=
IR_{bin}
\left[
1-(1-w_D)e_{N,t}-w_De_{D,t}
\right].
$$

For a subgroup-only diagnostic scenario, set the inactive subgroup’s incremental effect to zero.

Use the existing baseline-adjustment logic separately for each subgroup:

$$
e_{g,t}
=
\frac{E_g(c_{g,t}-c_{g,0})}
     {1-E_gc_{g,0}},
$$

where $g$ is diabetes or non-diabetes and $E_g$ is the appropriate subgroup-specific effect size.

For case fatality, weight the incremental coverage by mutually exclusive subgroup shares:

$$
\Delta c_t^{weighted}
=
(1-w_D)\Delta c_{N,t}
+
w_D\Delta c_{D,t}.
$$

Do not:

- Apply the general intervention to the diabetes share.
- Multiply the general and diabetes subgroup effects together.
- Multiply by diabetes prevalence more than once.
- Multiply by `raisedBP` again after effects have already been restricted to hypertensive BP bins.
- Use a diabetes-weighted average Ettehad effect for the non-diabetes intervention.

The final incidence and case-fatality effects should be calculated once from the mutually exclusive subgroup mixture.

### Scale-up trajectory

Use a transparent linear trajectory:

- Through 2025: subgroup control equals its baseline rate.
- 2026–2030: linear movement from baseline to the subgroup-specific target.
- From 2030 onward: hold the 2030 target constant.

Use the targets calculated by script 04. Script 06 must not recalculate the 150-million allocation or infer a new diabetes baseline.

## Required QA and tests

Add concise automated checks covering at least:

1. Hypertensive partition:

   $$
   H_N+H_D=H.
   $$

2. Baseline control reconstruction:

   $$
   H_Nc_N+H_Dc_D=Hc.
   $$

3. Baseline control gap equals 0.15 where feasible.

4. All baseline and target rates lie in $[0,1]$.

5. No missing location, sex, scenario, or subgroup keys.

6. Exactly one row per expected scenario × location × sex × subgroup key.

7. No negative subgroup population or additional-controlled count.

8. Combined additional controlled in 2030 equals 150 million at the 2025 population baseline.

9. The combined total equals the sum of diabetes and non-diabetes additions.

10. Control is unchanged through 2025, reaches the target in 2030, and remains constant after 2030.

11. Setting both subgroup targets equal to their baselines produces `eff_ir = 1` and `eff_cf = 1`.

12. When diabetes prevalence is zero, the combined calculation reduces to the non-diabetes calculation.

13. When diabetes prevalence is one, it reduces to the diabetes calculation.

14. The combined subgroup effect equals the explicit population-weighted mixture and contains no multiplicative cross-term.

15. No double application of diabetes prevalence or `raisedBP`.

Run representative tests for at least one low-diabetes country, one high-diabetes country, and the global reconciliation.

## File scope and preservation

Modify only:

- `code/04_define_interventions.R`
- `code/06_run_scenarios_targets.R`

You may add a narrowly scoped test file if the repository has an established testing location.

Do not modify:

- `code/06_run_scenarios_multiple.R`
- Other intervention logic.
- Baseline disease-model transitions.
- DALY or economic-valuation scripts.
- Unrelated statin, sodium, or TFA code.
- `Scenarios.xlsx`, unless absolutely necessary and clearly explained before doing so.

Preserve downstream output columns whenever possible. If a breaking schema change is unavoidable, identify every downstream reference before making the change and either provide a compatibility column or document the required follow-up.

## Completion report

After implementation, report:

1. Files changed.
2. The final formulas used.
3. The processed target-table schema.
4. Baseline controlled totals by subgroup.
5. Additional controlled by 2030 for each subgroup.
6. Their combined total and difference from 150 million.
7. The solved non-diabetes scale factor and number of capped strata.
8. Any locations excluded or missing inputs.
9. Tests run and their results.
10. Any assumptions that remain unresolved.

Do not report completion unless the 150-million reconciliation passes using the fixed 2025 population baseline.
