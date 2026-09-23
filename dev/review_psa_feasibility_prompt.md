# Claude Code Prompt: Review the Feasibility of a PSA-Based Uncertainty-Interval Production Pipeline

## Role

Act as a senior health decision-modeling researcher and R software reviewer. Review this repository to determine the feasibility, safest architecture, and minimum code changes needed to add a transparent, computationally efficient, and auditable probabilistic sensitivity analysis (PSA) that produces uncertainty intervals for the study's reported estimates.

This is a **review and design task only**. Do not implement or execute the PSA.

## Primary objective: uncertainty intervals in all estimate deliverables

The primary objective is **not merely to conduct a sensitivity analysis or add a PSA appendix**. The PSA must become the uncertainty engine for the study's estimate-delivery pipeline.

The three principal deliverables are:

- the Aim 1 executive slides;
- the full Aim 1 report; and
- the manuscript in `docs/who_cvd_targets_paper1.Rmd`.

The intended final state is that **every model-derived estimate presented in these deliverables is accompanied by an appropriate 95% uncertainty interval (UI)**. This includes estimates presented in:

- titles, subtitles, executive-summary statements, and inline text;
- headline global estimates;
- tables;
- figure values, trajectories, bars, and other plotted estimates;
- regional, income-group, age-sex, cause-specific, and country-level results;
- deaths, YLLs, YLDs, DALYs, rates, percentages, and economic values, where reported.

The review must therefore evaluate not only whether PSA draws can be run, but whether the entire chain from sampled parameter values to final presentation-ready 95% UIs can be built reliably. Treat completeness of UI coverage across the slides, report, and manuscript as a core acceptance criterion.

Where a particular display cannot reasonably show a UI directly—for example, a choropleth map—the review must propose a transparent companion presentation or reporting rule and identify the underlying estimate-level UI that must still be produced. Do not silently exempt estimates from uncertainty reporting.

Unless there is a compelling statistical reason otherwise, define the 95% UI as the 2.5th and 97.5th percentiles across valid PSA draws. The central deterministic result may remain the reported point estimate, with the PSA median retained as a diagnostic, provided this convention is applied consistently and explained.

## Repository scope to inspect

Begin at the repository root and read any applicable repository instructions, including `CLAUDE.md`, `CLAUDE`, `AGENTS.md`, or related instruction files.

Inspect, at minimum:

- All relevant files under `code/`, especially the model orchestrator, intervention definitions, baseline construction, scenario execution, DALY/output calculations, and economic valuation scripts.
- `scenarios/scenarios_aim1/aim1_executive_slides.Rmd`
- The principal Aim 1 report R Markdown file under `scenarios/scenarios_aim1/`. The requested filename may have been abbreviated or mistyped as `aim1_.Rmd`; locate the actual matching file, report its exact path, and explain how you resolved the ambiguity.
- `docs/who_cvd_targets_paper1.Rmd`
- Any configuration, tests, helper functions, or processed-input metadata directly needed to understand parameter flow and output generation.

The originally supplied path may contain the typo `scenarios/scenarions_aim1/`. Confirm the real directory and filenames from the repository rather than silently assuming them.

Do not inspect large raw data files unless their schema is essential to determine feasibility. Prefer tracing their use through the code.

## Hard constraints

- **Do not edit, create, move, rename, or delete any repository file.**
- **Do not run the R model.**
- **Do not execute any R script, R Markdown document, notebook, test, calibration, simulation, PSA draw, or economic valuation.**
- **Do not knit or render reports or slides.**
- **Do not install packages or change the environment.**
- **Do not generate model outputs.**
- Read-only repository inspection commands are allowed, such as listing files, searching text, viewing code, and checking Git status or history when helpful.
- Do not claim that a proposed design works empirically. Clearly distinguish findings verified from code inspection from recommendations that would require later implementation and testing.

## PSA scope to evaluate

For this first PSA, calibration and baseline epidemiological inputs are treated as structural/fixed. Evaluate uncertainty only in these intervention-effect parameter families:

1. **Blood-pressure-lowering effects**
   - Cause-specific relative risks or effect sizes for BP lowering.
   - Diabetes and non-diabetes effects where the current model distinguishes them.
   - Proposed sampling: joint or multivariate normal draws on the log-relative-risk scale when covariance is available.

2. **Blood-pressure case-fatality effects**
   - Current cause-specific reduction factors, including approximately:
     - IHD: 0.24
     - Ischaemic stroke: 0.36
     - Intracerebral haemorrhage: 0.76
     - Hypertensive heart disease: 0.20
   - Proposed sampling: beta distributions only when defensible confidence intervals, standard errors, or effective sample sizes can be identified.
   - If the repository contains only point assumptions without sufficient uncertainty evidence, recommend keeping these fixed in the PSA and evaluating them through deterministic sensitivity analysis instead.

3. **Statin effects**
   - Relative risks for IHD and ischaemic-stroke incidence.
   - Relative risks for IHD and ischaemic-stroke case fatality.
   - Proposed sampling: joint or multivariate normal draws on the log-relative-risk scale when covariance is available.

The same sampled evidence parameter must be applied consistently across all countries within a draw. Do not recommend independently resampling the same trial effect for each country.

Policy targets, target years, coverage definitions, the intervention-combination rule, and other structural assumptions should remain fixed in the primary PSA. Identify important structural alternatives separately as deterministic sensitivity analyses.

## Proposed computational design to assess

Assess the feasibility of the following architecture against the actual repository:

1. Preserve the current detailed country-level model output for a single central-estimate run. This remains the source for central report numbers, diagnostics, maps, and detailed decompositions.
2. Because the selected PSA parameters affect only interventions, assess whether the existing business-as-usual output can be calculated once and reused across all draws. Identify any code pathways that would invalidate this assumption.
3. Generate and save a master parameter-draw table before model execution, including `draw_id`, seed, all sampled parameter values, distribution metadata, and source identifiers.
4. For each draw and country:
   - run only the necessary intervention scenarios;
   - temporarily construct the detailed output;
   - calculate deaths and, where feasible, YLLs, YLDs, DALYs, and economic value before discarding age-specific detail;
   - reduce the output immediately to compact summaries needed by the reports;
   - remove the detailed country-draw object rather than saving or returning it to the main session.
5. Retain only report-relevant summaries, potentially separated into:
   - global totals by intervention and metric;
   - annual and cumulative trajectories;
   - cause-specific results;
   - age-sex results;
   - WHO-region results;
   - World Bank income-group results;
   - country-level results required for tables or maps;
   - any age-specific sufficient statistics required for ASMR, YLL, YLD, DALY, or economic calculations.
6. Write results incrementally in restartable batches, rather than retaining all draws in memory or creating a full detailed RDS file for every country-draw combination.
7. Maintain a manifest recording draw status, seed, start and completion times, code commit, expected and completed countries, warnings, failures, and validation outcomes.
8. Calculate 2.5th, 50th, and 97.5th percentiles from the compact PSA summaries, while retaining the deterministic central estimate as the reported point estimate unless there is a documented reason to report the PSA median.
9. Produce a reusable presentation-ready uncertainty-results layer from which the executive slides, full report, and manuscript obtain the central estimate, lower 95% UI, and upper 95% UI for every model-derived estimate they present.

## Questions the review must answer

### A. Current architecture and parameter flow

- Where are the three PSA parameter families currently defined, loaded, transformed, and applied?
- Which values are hard-coded, read from external files, calculated, or copied across scripts?
- Are there duplicate definitions or conflicting values across the model, reports, slides, and manuscript?
- Are the parameters accessible as function arguments, or would functions need to be refactored to avoid modifying global variables?
- Which functions and scripts form the actual central-run execution path?
- Are the no-diabetes and diabetes BP effects mutually exclusive population mixtures, multiplicative effects, or implemented differently in different parts of the repository?
- How are BP and statin effects combined in the all-interventions scenario?

### B. Reusing the baseline

- Can business-as-usual outputs be safely reused for every PSA draw when only the selected intervention-effect parameters vary?
- Does any intervention parameter currently leak into baseline construction, calibration, initialization, or target allocation?
- What exact baseline artefact or compact sufficient statistics should be cached?
- What validation would demonstrate that reuse is numerically identical to rerunning BAU?

### C. Memory and storage

- At what point are country outputs currently combined into a large in-memory object?
- Which report calculations require full single-year-age output, and which require only marginal summaries?
- Can DALY and economic calculations be converted into country-level functions and applied before discarding detailed output?
- What is the smallest sufficient output schema that reproduces every reported table, figure, map, scalar, and inline number?
- Would separate summary tables be safer and smaller than a single cross-classified cube?
- Recommend a storage format compatible with the repository and Windows/OneDrive workflow. Compare, as relevant, compact RDS batches, Parquet/Arrow, DuckDB, and CSV. Avoid introducing a dependency unless its benefit is material.
- Recommend a safe write strategy under parallel execution. Do not recommend simultaneous appends from multiple workers to the same CSV or RDS file.

### D. Parallelization and restartability

- Should parallelization occur across draws, countries within a draw, or batches of draws, given the current code?
- How can each worker return only compact summaries?
- What should happen when one country or one draw fails?
- How should completed draws be detected and skipped on restart?
- How should deterministic seeds be assigned so a specific draw can be reproduced independently of worker count or task ordering?

### E. Report integration

Create an inventory that maps each table, figure, map, inline result, and outcome in the two Aim 1 R Markdown files and `docs/who_cvd_targets_paper1.Rmd` to:

- its current source object;
- its necessary aggregation dimensions;
- whether the central detailed run remains necessary;
- the compact PSA summary table required for its uncertainty interval;
- whether a 95% uncertainty interval is appropriate;
- any report result that cannot be reproduced from the proposed compact summaries.

The inventory must be exhaustive enough to function as a **UI coverage audit**. Assign each estimate family a stable `estimate_id` or equivalent identifier and classify it as:

- `UI ready`: the proposed PSA summaries can generate its 95% UI;
- `requires additional retained dimensions`: the current proposed summaries are insufficient;
- `structural/fixed`: it is not a stochastic model estimate, with justification;
- `unresolved`: scientific or computational decisions are still required.

For every model-derived estimate, specify the intended presentation format, for example:

- inline text: `central estimate (95% UI lower–upper)`;
- table: central, lower, and upper columns or a combined formatted column;
- time series: central line plus 95% UI ribbon;
- bars or points: 95% UI error bars;
- maps: central-estimate map plus a companion uncertainty display or accessible country-level UI table;
- economic valuation: central estimate and 95% UI after propagating the same health-effect draws through valuation.

Pay particular attention to:

- deaths averted;
- annual and cumulative trajectories;
- results by intervention, cause, age, sex, region, income group, and country;
- ASMR;
- YLLs, YLDs, and DALYs;
- economic valuation;
- maps and top-country tables;
- inline manuscript and slide numbers.

### F. Statistical specification

- List every exact parameter proposed for this limited PSA, using its repository variable name and manuscript symbol or label.
- Report its current central value and source location.
- Identify whether lower and upper confidence limits or covariance information are already available in the repository.
- Recommend the distribution and parameterization formula, but do not invent missing confidence intervals or covariance matrices.
- For a relative risk with estimate `RR` and 95% confidence interval `(L, U)`, assess using:

  ```text
  log(RR_draw) ~ Normal(log(RR), SE_log^2)
  SE_log = [log(U) - log(L)] / 3.92
  ```

- Explain which effects need joint sampling and what to do if the required covariance matrix is unavailable.
- For beta-distributed case-fatality reduction factors, state exactly what additional evidence is needed to identify beta shape parameters. If this evidence is absent, recommend deterministic sensitivity analysis rather than an arbitrary beta precision.
- Distinguish parameter uncertainty from structural, policy, and implementation uncertainty.

### G. Auditability and validation

- Define the parameter-draw table schema.
- Define the compact result-table schemas and primary keys.
- Define the run-manifest schema.
- Recommend metadata required to reproduce the analysis: master seed, per-draw seed, Git commit, input versions, package versions, parameter sources, distributions, covariance assumptions, and scenario definitions.
- Propose validation tests for:
  - central-value draws reproducing the deterministic intervention results;
  - baseline reuse;
  - parameter bounds;
  - absence of missing or duplicate draw-country-scenario combinations;
  - correct application of one shared parameter draw across countries;
  - correct aggregation from country to region and global totals;
  - successful restart after an interrupted batch;
  - numerical stability of the 2.5th and 97.5th percentiles as the number of draws increases.
  - complete UI coverage across the executive slides, report, and manuscript;
  - identical `estimate_id`, scenario, time horizon, geography, population, cause, and metric definitions between central estimates and their UI bounds;
  - no model-derived point estimate appearing in a final deliverable without a matched lower and upper 95% UI, unless explicitly classified and justified as structural/fixed.

## Required deliverable

Return one structured review in Markdown with the following sections:

1. **Executive assessment**
   - Overall feasibility.
   - Main architectural conclusion.
   - Highest-risk obstacles.
   - Whether complete 95% UI coverage across the three deliverables is achievable from the existing model architecture.

2. **Files inspected**
   - Exact paths.
   - Purpose of each file.
   - Any requested path or filename that did not exist and the actual file used instead.

3. **Current model and reporting data flow**
   - From parameters to intervention functions to country outputs to reports.
   - Include a concise Mermaid flowchart if it materially improves clarity.

4. **Parameter audit**
   - Exact repository variables, central values, sources, transformations, uncertainty information, and duplication or inconsistencies.

5. **Feasibility of the streaming PSA architecture**
   - What can be adopted directly.
   - What must be refactored.
   - Whether BAU can be reused.

6. **Report-output dependency matrix**
   - One row per report table, figure, map, or inline-result family.
   - Current source, required dimensions, proposed compact PSA table, and limitations.
   - Required presentation form for the central estimate and 95% UI.
   - UI-coverage status and any missing information.

7. **Recommended data schemas**
   - Parameter draws.
   - Summary outputs.
   - Manifest and validation logs.
   - A presentation-ready estimate table keyed by stable `estimate_id`, with central estimate, PSA median, lower 95% UI, upper 95% UI, units, scenario, population, geography, time horizon, and metric metadata.
   - Show exact proposed column names and primary keys.

8. **Recommended orchestration design**
   - Draw generation.
   - Country execution.
   - Immediate summarization.
   - Safe incremental writes.
   - Restart logic.
   - Final uncertainty aggregation.
   - Provide pseudocode only, not executable implementation.

9. **Proposed file-change map**
   - Files that would likely need modification in a later implementation.
   - Proposed new files or functions.
   - Responsibility of each change.
   - Do not make the changes now.

10. **Validation and acceptance criteria**
    - Concrete tests and pass/fail criteria for a later implementation.
    - Include a deliverable-level UI completeness test covering all model-derived estimates in the slides, report, and manuscript.

11. **Open scientific or implementation decisions**
    - Missing uncertainty inputs.
    - Covariance assumptions.
    - PSA versus deterministic sensitivity choices.
    - Questions that must be resolved before coding.

12. **Recommended phased implementation plan**
    - Smallest safe proof of concept.
    - Scale-up to all countries and draws.
    - Construction of the presentation-ready estimate/UI layer.
    - Integration into the executive slides, full report, and manuscript.
    - A final UI coverage audit confirming that every model-derived estimate has a matched 95% UI or an explicit, justified exemption.
    - Expected computational and storage implications, expressed as reasoned estimates rather than measured benchmarks.

## Review standard

Be concrete and repository-specific. Cite file paths, function names, object names, and relevant line numbers where possible. Flag contradictions explicitly. Do not provide generic PSA advice unless it is tied directly to the current code. The final recommendation must be designed around production of presentation-ready 95% uncertainty intervals for all study estimates, not merely around demonstrating parameter sensitivity. Do not implement, execute, or benchmark the plan during this review.
