# Gate 1 review: deterministic-model freeze and PSA Parameter Freeze Gate

**Model:** WHO CVD targets model, Aim 1 pipeline (scripts 00–07). The active interventions are blood-pressure (BP) control and statin treatment.
**Review date:** 2026-09-23
**Companion workbook:** `dev/gate1_psa_parameter_register.xlsx` (10 sheets, 86 register rows)

**Status tags:**
- `[VERIFIED]` means read directly from code or data.
- `[TESTED]` means confirmed by a diagnostic that was actually run.
- `[INFERRED]` means reasoned but not numerically tested.
- `[UNRESOLVED]` means evidence or a decision is missing.
- `[RECOMMENDATION]` marks a proposed correction or specification. None of these has been implemented.

**Line references:**
- `06:` means `code/06_run_scenarios_multiple.R`.
- `06o:` means `code/06_run_scenarios_multiple_optimized.R`. It contains the same code, shifted by +11 to +56 lines.
- `06t:` means `code/06_run_scenarios_targets.R`.
- `M:` means `docs/who_cvd_targets_paper1.Rmd`.
- `R1:` means `scenarios/scenarios_aim1/aim1_report.Rmd`.

---

## 1. Executive conclusion

### Gate 1 (deterministic model freeze): **Gate 1 not passed.**

Seven of the eight Gate 1 areas fail. Four fail because a scientific decision or evidence is missing (G1-02 to G1-05). Three fail on verified code defects (G1-06 to G1-08), and the combined-scenario defect changes the headline result. Only G1-01 (haemorrhagic stroke) passes.

1. **The headline combined scenario is computed incorrectly** (`all_interventions`, displayed as "All Interventions"). `[TESTED]`
   - It still runs the legacy sodium pathway. That pathway returns `NA` incidence ratios for every row from 2026 onward.
   - `06:1502–1503` converts those `NA`s to 1. This silently erases all BP-control *incidence* effects in the combined scenario, in all 186 countries.
   - The same scenario also runs legacy trans-fat (TFA) elimination. That adds a real IHD case-fatality reduction in 126 countries.
   - The published 34.25 million deaths averted is therefore neither "BP + statins" nor all five interventions.
   - In three test countries, a correctly specified BP + statin scenario averts 6.6% to 32.7% more deaths than the reported figure.
2. **The scenario ASMR counts the population four times.** `[TESTED]` The aim1 report adds up `well + sick` across the four cause chains, so its ASMR is 4.0 times too low (131.7 vs 526.6 per 100,000 in 2019). The standard weights are also a decade-step approximation, not the WHO World Standard the captions claim.
3. **The BP-effect evidence and how it is used are not scientifically settled.**
   - Aim 1 uses BPLTTC 2021 stratum-specific estimates, but documents them as Ettehad 2016. Aim 2 uses Ettehad 2016.
   - The model multiplies the risk ratios of different baseline-SBP strata together. That treats subgroup (baseline-risk) estimates as dose–response increments, which the source design does not support.
4. **The case-fatality (CF) effects have no evidence base, and they drive a large share of the benefit.** `[TESTED]` These are the BP ρ values and the statin CF risk ratios.
   - The BP CF pathway accounts for 40–43% of BP deaths averted.
   - The statin CF pathway accounts for 82–86% of statin deaths averted.
   - The ρ values (0.24, 0.36, 0.76, 0.20) have no source.
5. **The statin IHD incidence RR of 0.74 is very likely a confidence limit, not a point estimate.** The adjacent code comment gives "CHD 0.80, 99% CI 0.74–0.87". The istroke incidence RR and both statin CF RRs have no source anywhere in the repository. The attributable fraction (AF) is the GBD high-fasting-plasma-glucose PAF, which does not match what it is used for.
6. **The horizon is labelled inconsistently.** Reported counts equal 2026–2050 because 2025 deaths averted are exactly zero. However, % of BAU uses a 2025–2050 denominator, so it is understated (4.93% vs 5.07%).

Things that do pass:
- The haemorrhagic-stroke fix in commit `c56782a` is correct for Aim 1. `[TESTED]`
- Pre-2026 identity, zero-effect identity, subgroup nesting and reproducibility all hold. `[TESTED]`
- The canonical script reproduces the saved outputs of the (untracked) optimized script exactly. `[TESTED]`

### PSA Parameter Freeze Gate: **PSA Parameter Freeze Gate not passed.**

- **Nothing is ready to sample.** None of the 86 candidate parameters meets all nine inclusion criteria.
- **The natural initial-PSA parameters are all blocked.** These are the BPLTTC outcome HRs and the statin incidence RRs.
  - BPLTTC HRs are blocked by the stratum-interpretation decision (G1-03) and by an unverifiable source: the appendix PDF is not in the repository and there is no BPLTTC bibliography entry.
  - Statin RRs are blocked by the missing or incorrect point estimates.
- **The sampling rules are already written.** A complete, conditional sampling specification is drafted (Section 12). It can be frozen once the blockers are resolved.
- **BAU can stay fixed.** The BAU scenario does not depend on any intervention-effect, GBD-RR or BP-distribution parameter, so it can be computed once for the proposed initial PSA. `[TESTED]` cell-level BAU invariance.

---

## 2. Repository state and files inspected

| Item | Value |
|---|---|
| Branch | `william` |
| HEAD | `c56782abfe889a6f00dd49238ca51e4f259d3a75`, "fix hstroke RRi bug" (2026-09-23 08:37) |
| Tree at review start | clean except untracked `code/06_run_scenarios_multiple_optimized.R`, `dev/gate1_deterministic_model_review_prompt.md`, `library/basios et al 2025 …pdf` |
| Changes made by the user during the review (09:18–09:24) | `code/00_run_model.R:112` now sources the untracked `06_run_scenarios_multiple_optimized.R`. Scripts 07/08 and `aim1_report.Rmd` were re-run. As a result, `output/08_*`, `output/paper/*`, `output/slides/*` and `scenarios/scenarios_aim1/aim1_results_tables.xlsx` are modified. |
| Changes made by the reviewer | None. The only new files are the two `dev/` deliverables. All diagnostics ran in the session scratchpad. |
| Model outputs reviewed | `output/out_model/model_output_<country>.rds`, 186 countries × 6 scenarios. Logs show these were produced on 2026-09-23 between 09:07 and 09:17, i.e. after HEAD, by the optimized script (10 cores). |
| Committed commit-level diff of the hstroke fix | `06:660–661`: `RRi_HSTROKE` is now carried into the bin table. The former overwrite `rris[, RRi_HSTROKE := RRi_ISTROKE]` was removed. |

**Files inspected:**
- **Code:** `code/00_run_model.R`, `01_utils.R`, `02_load_inputs.R`, `020`–`023` (headers and outputs), `03_clean_inputs.R`, `04_define_interventions.R`, `05_build_baseline.R`, `06_run_scenarios_multiple.R`, `06_run_scenarios_multiple_optimized.R` (full diff), `06_run_scenarios_targets.R` (effect sources and hstroke), `07_output_dalys.R`, `08_economic_value_calculation.R` (parameters).
- **Tests and reports:** `tests/test_aim2_bp_control.R`, `scenarios/scenarios_aim1/aim1_report.Rmd`.
- **Documents read via a line-referenced sweep:** `docs/who_cvd_targets_paper1.Rmd`, `docs/math-doc.Rmd`, `aim1_executive_slides.Rmd`, `aim2_report.Rmd`, `executive_slides_htn_targets.Rmd`, `docs/references.bib`.
- **Data, all read programmatically:**
  - `data/processed/ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx` (both sheets), `ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv`, `ettehad_rr_bp_reduction_effects.xlsx`, `ettehad_rr_bp_reduction_10mmHg.csv`
  - `htn_control_targets_by_loc.csv`, `htn_control_targets_summary.csv`
  - `statin_data.rds`, `af_statins.rds`, `bp_data6.csv`, `covfxn2.csv`, `adjustments2023_age.csv`, `tps_bgmx_cvd_forecasted.rds`, `tfa_policy_scenarios.rds`
  - `data/raw/IHME_GBD_2019_RELATIVE_RISKS_Y2020M10D15_HTN.xlsx`, `data/raw/GBD/IHME-GBD_2023_DATA-52d32656-1.csv` (header)
  - `output/paper/paper_scalars.rds`, `paper_dt_cumul.rds`, `output/slides/sl_*.rds`
- **Earlier reviews:** `dev/psa_feasibility_review.md` and `dev/who_cvd_targets_paper1_coauthor_revision_work_plan.md` were read only to check whether their claims still hold; they were not treated as authoritative. Their hstroke description predates the fix and is now out of date for Aim 1.

**Canonical versus optimized script.** `00_run_model.R` now sources an untracked file. The optimized file differs from the tracked file only in context reuse, `ncores = 10`, a pilot option and error reporting. `[VERIFIED]` by diff; the scenario list and run call are identical. Its outputs equal the canonical script's cell by cell (max abs diff 0 for Colombia, China and Nigeria). `[TESTED]` Even so, the production path is not under version control. See change C09.

---

## 3. Deterministic pipeline map (00–07)

| Step | Script | Role | Status |
|---|---|---|---|
| 00 | `00_run_model.R` | Sets paths and flags (`run_CF_trend_80 = TRUE`, …); sources 01–08 | Sources the untracked optimized 06 (line 112) |
| 01 | `01_utils.R` | Old `get.bp.prob` (overridden in 06), TFA helper, age groups | Legacy |
| 02 | `02_load_inputs.R` → `020`–`023` | WHO GHE deaths, GBD rates, transition probabilities, Lee–Carter BG.mx and CF forecasts | Outputs are pre-computed in `data/processed` |
| 03 | `03_clean_inputs.R` | Empty stub | — |
| 04 | `04_define_interventions.R` | 150M BP-target split by diabetes status → `htn_control_targets_by_loc.csv`; statin target table (not used by the model) | 43/43 QA checks pass `[TESTED]` |
| 05 | `05_build_baseline.R` | `b_rates` = calibrated rates + UNWPP population + COVID + BG.mx trend + 80% CF trend. IR is held at its 2019 value for 2020–2050. `data.in` = BP distribution | Reads processed data only |
| 06 | `06_run_scenarios_multiple(_optimized).R` | `project.all()`: BP split → (legacy sodium) → combine → (legacy TFA) → statins → 3-state transitions 2017–2050 | Defects G1-08 |
| 07 | `07_output_dalys.R` | YLD = sick × DW; YLL = dead × WPP LE; `year >= 2026` | Horizon correct |
| report | `aim1_report.Rmd` → `output/paper`, `output/slides` | Deaths averted, ASMR, tables | Defects G1-06, G1-07 |

How the rates are built for each (country, age, sex, cause, year):

1. **Bin incidence** (`calculate_baseline_incidence_gbd`, 06:629–683):
   - $p_b$ comes from $N(\mu,\sigma)$.
   - $RR_b = RR_{GBD}^{(m_b-120)/10}$.
   - $\alpha=\sum_b p_bRR_b$.
   - $IR_b = RR_b\,IR/\alpha$.
2. **BP incidence effect** (06:167–213):
   - $E^{\Delta}_g = E_g (c_{g,t}-c_{g,0})/(1-E_g c_{g,0})$ for $g\in\{N,D\}$, applied in bins ≥140.
   - $IR'_b = IR_b[1-(1-w_D)E^\Delta_N-w_DE^\Delta_D]$.
   - $\varepsilon_{IR}=\sum_b p_b IR'_b/IR$.
3. **BP CF effect** (06:215–240): $\varepsilon_{CF}=1-\rho_c\,\overline{\Delta c}$, where $\overline{\Delta c}$ is the mixture-weighted incremental control averaged over the hypertensive bins.
4. **Statins** (06:1072–1257), ages ≥40:
   - $\varepsilon^{st}_{IR}=1-AF(1-RR_{IR})\Delta U/(1-(1-RR_{IR})U_0)$.
   - $\varepsilon^{st}_{CF}=1-\psi(1-RR_{CF})\Delta U/(1-(1-RR_{CF})U_0)$, with $\psi=0.6$ for istroke and 1 for IHD.
5. **Combination.** BP subgroups are additive on the rate scale. BP and statins are multiplicative and sequential. The clamps are IR, CF ≤ 0.99.
6. **Transitions** (06:1588–1630): independent Well→Sick→Dead chains per cause, sharing one population through `all.mx`.

---

## 4. Findings for the eight Gate 1 areas

### 4.1 Haemorrhagic-stroke incidence construction — **PASS** (Aim 1 active pipeline)

- **Normalizer.** `[VERIFIED]` `alpha` for `hstroke` uses `RRi_HSTROKE` (06:650), which comes from the GBD "Intracerebral hemorrhage" age-specific RR per 10 mmHg (06:280–286, 641–643).
- **Bin incidence.** `[VERIFIED]` `IR_bin` for `hstroke` also uses `RRi_HSTROKE` (06:660–661, 680; 06o:672). The former overwrite with `RRi_ISTROKE` was removed in `c56782a`. Nothing later overwrites either series in Aim 1.
- **Residual defect in Aim 2.** `[VERIFIED]` `06t:638` still contains `rris[, RRi_HSTROKE := RRi_ISTROKE]`. Aim 2 is not sourced by `00_run_model.R`, but the defect must be fixed before Aim 2 is reused (C10).
- **Identity.** `[TESTED]` $\sum_b p_bIR_b=IR$ holds for all causes, with max relative deviation 2.2×10⁻¹⁶ (Colombia, China, Nigeria; all ages, sexes and years).
- **Size of the old error.** `[TESTED]` Emulating the pre-fix construction gives $\sum_b p_bIR_b/IR$ between 0.78 and 0.999 (mean 0.876–0.880). Before the fix, every BP scenario therefore reduced hstroke incidence spuriously by about 12%, even at zero coverage.
- **No spurious effect before scale-up.** `[TESTED]` `eff_ir` = 1 for hstroke in all years ≤2025, across 186 countries (max deviation 3.3×10⁻¹⁶). `eff_ir` is also exactly 1 at zero incremental coverage (both subgroups inactive, or targets equal to baselines; max deviation 2.2×10⁻¹⁶).
- **Other pathways.** `[TESTED]` No active BP or statin pathway rescales hstroke. Statins do not touch hstroke. However, the combined scenario *erases* the BP hstroke incidence effect (`eff_ir` = 1 from 2026; see 4.8).
- **Regression test.** `[RECOMMENDATION]` Add T01 permanently (Section 17). Also add a check that `RRi` for hstroke equals the GBD ICH series in both `alpha` and `IR_bin`.

### 4.2 BPLTTC 2021 versus Ettehad 2016 — **FAIL — SCIENTIFIC DECISION REQUIRED**

Inventory of BP-effect sources:

| Where | File / object | Attributed citation | Outcome → cause | Measure | Strata | Transition | Matches cited evidence? |
|---|---|---|---|---|---|---|---|
| Aim 1 incidence (06:474–475; 06o:485) | `ettehad_rr_bp_reduction_effects_bplttc_2021.xlsx` Sheet1 → `ETIHAD_RR_BIN` | Workbook "Source" column: "mmc1 (9).pdf, Figure S6" (BPLTTC 2021 appendix). Objects are named "Ettehad". | IHD→ihd; Stroke→istroke **and** hstroke; Heart failure→hhd (and inactive aod) | HR per 5 mmHg squared to RR per 10 mmHg; 95% CI squared | <120, 120–129, 130–139, 140–149, 150–159, 160–169, ≥170 | Well→Sick | Internally consistent `[TESTED]`: Sheet1 = Appendix S6, HR5² = RR10 (max diff 0), cumulative products reproduce the effect sizes (1×10⁻¹⁶). **Not verifiable against the publication** `[UNRESOLVED]`: the PDF is not in the library and the bib has no BPLTTC entry. |
| Aim 1 legacy sodium (06:319) | `ettehad_rr_bp_reduction_10mmHg_bplttc_2021.csv` → `ETIHAD_RR` | none | as above | RR per 10 mmHg | 7 BPLTTC strata, but the mapping at 06:349–357 expects Ettehad strata | Well→Sick (sodium) | **Broken**: 16 of 28 rows have `NA` `bp_cat` `[TESTED]` (see 4.8) |
| Aim 2 (06t:304, 459) | `ettehad_rr_bp_reduction_10mmHg.csv`, `ettehad_rr_bp_reduction_effects.xlsx` | Ettehad 2016 | CHD, Stroke, HF | RR per 10 mmHg (e.g., CHD 140–149: 0.80, 0.69–0.94) | <130, 130–139, 140–149, 150–159, ≥160 | Well→Sick | Yes (Aim 2 only; not executed) |
| BP CF (06:233–236) | `cf_etihad` ρ = 0.24/0.36/0.76/0.20 (/0.047 aod) | none in code; manuscript: Ettehad 2016 (M:802) | — | "CF reduction per unit control" | — | Sick→Dead | **No**. Ettehad reports event RRs, not case-fatality reductions `[INFERRED]` |
| Manuscript | M:126, 157, 164, 174, 696, 753, 802–803, 844, 938–942 | Ettehad 2016 | — | "diabetes-weighted Ettehad effect" | — | — | **Mislabelled** `[VERIFIED]`: Aim 1 uses BPLTTC. The methods text also describes the old diabetes-weighted algorithm, not the current split-population mixture. |
| Math doc | `docs/math-doc.Rmd:73–79, 600` | Ettehad | — | $w_{DM}$ default 0.1 | — | — | Outdated |
| Slides | `aim1_executive_slides.Rmd:251–256, 489, 1301` | Ettehad 2016 | — | split mixture (correct algorithm) | — | — | Mislabelled citation |

Point estimates and 95% CIs used (RR per 10 mmHg) are in the Parameter_Register sheet, rows `BE-*` (21 rows), and in Section 12.

**Conclusions:**
1. `[VERIFIED]` Aim 1 and Aim 2 use different BP-effect evidence.
2. `[VERIFIED]` The manuscript, math doc and slides label BPLTTC-derived Aim 1 estimates as Ettehad 2016.
3. There are two outcome-mapping assumptions. `[VERIFIED]` Total stroke is applied to both ischaemic stroke and ICH. `[INFERRED]` Incident heart failure is applied to HHD, which is an estimand mismatch.

`[RECOMMENDATION]` Adopt **BPLTTC 2021** as the single primary source for both aims, with Ettehad 2016 as a sensitivity analysis. BPLTTC is an individual-participant-data meta-analysis with per-5-mmHg estimates for the exact outcomes, stratified by baseline SBP down to <120. This is a **scientific decision** (Q01). The article and appendix must be added to `library/` and `docs/references.bib`, and the Appendix S6 values verified, before any BP parameter can be frozen.

### 4.3 Interpretation of the BPLTTC baseline-SBP strata — **FAIL — SCIENTIFIC DECISION REQUIRED**

**The current formula** `[VERIFIED]` (workbook Sheet1; consumed at 06:175–179). For hypertensive bin $b$ with ordered bins $k=140\text{–}149,\dots,b$:

$$E_N(b)=1-\prod_{k=140\text{–}149}^{b}RR_{10}(k),\qquad E_D(b)=1-RR_{10}(130\text{–}139)\prod_{k=140\text{–}149}^{b}RR_{10}(k),$$

where 170–179 and 180+ both use the ≥170 stratum. Examples:
- IHD, 180+ (no diabetes): $1-0.8836\cdot0.8281\cdot0.7569\cdot0.8836^2=0.568$.
- Non-hypertensive bins carry the placeholder value $E=1$ (a 100% reduction). It is never used, because only bins ≥140 are eligible (06:195), but it is a latent hazard (C13).

**Implicit assumptions:**
1. A newly controlled person moves from their bin midpoint to 130–139 (no diabetes) or 120–129 (diabetes). This implies $\Delta_b=m_b-135$ or $m_b-125$.
2. Lowering SBP from bin $k$ to bin $k-1$ carries the HR that BPLTTC estimated for *participants whose baseline SBP was in stratum $k$*.
3. Effects of successive 10-mmHg steps multiply, i.e. they are log-additive.
4. The effect is immediate and does not attenuate.

**What BPLTTC estimates.** `[INFERRED]` from the published design; confirm against the source, which is not in the repository. Each stratum estimate is a randomized treatment effect per 5 mmHg SBP reduction, estimated *within a baseline-SBP subgroup*. It is an effect-modification (baseline-risk subgroup) estimate. It is not the marginal effect of traversing that 10-mmHg interval of achieved SBP.
- Someone starting at 175 mmHg and treated to 135 mmHg never becomes a "150–159 baseline" participant.
- Applying the 150–159 subgroup HR to the 160→150 segment therefore conflates a baseline-risk subgroup with a dose–response increment.
- The cumulative product is supported only under a piecewise log-linear dose–response by achieved SBP. The source does not estimate that.
- BPLTTC's headline interpretation is that proportional risk reductions per fixed SBP reduction are broadly similar across baseline strata. `[INFERRED]` This favours a pooled per-mmHg estimate.

**Alternatives tested** `[TESTED]` (effect sizes computed from the repository's Appendix S6 values; "pooled" is an inverse-variance pool across the seven strata computed here, **not** the published overall estimate):

| cause | bin | current $E_N$ | current $E_D$ | own-stratum $E_N$ | own-stratum $E_D$ | derived-pooled $E_N$ | derived-pooled $E_D$ |
|---|---|---|---|---|---|---|---|
| IHD | 140–149 | 0.116 | 0.186 | 0.116 | 0.219 | 0.142 | 0.265 |
| IHD | 160–169 | 0.446 | 0.490 | 0.566 | 0.672 | 0.369 | 0.459 |
| IHD | 180+ | 0.568 | 0.601 | 0.461 | 0.524 | 0.536 | 0.602 |
| Stroke | 150–159 | 0.520 | 0.594 | 0.649 | 0.792 | 0.388 | 0.521 |
| Stroke | 180+ | 0.767 | 0.803 | 0.651 | 0.718 | 0.707 | 0.771 |
| HF→HHD | 140–149 | 0.098 | 0.348 | 0.098 | 0.185 | 0.242 | 0.425 |
| HF→HHD | 160–169 | 0.658 | 0.753 | 0.882 | 0.942 | 0.564 | 0.669 |

The own-stratum column is $1-RR_{10}(b)^{\Delta_b/10}$. The derived-pooled column is $1-\overline{RR}_{10}^{\Delta_b/10}$, with $\overline{RR}_{10}$ = 0.858 (IHD), 0.782 (stroke), 0.758 (HF). The full table is in `stratum_alternatives` (Diagnostics T15).

Impact on bp_combined deaths averted, 2026–2050 `[TESTED]`:
- Own-stratum: +3.6% in each of Colombia, China and Nigeria.
- Derived-pooled: −1.9% (Colombia), −3.5% (China), −1.8% (Nigeria).
- The aggregate is fairly robust because the bin-level differences offset, but bin-level effects differ by up to 0.23. Own-stratum log-linear raises noisy single-stratum estimates to powers up to 6 (for example, HF 160–169 at HR5 = 0.70), which yields implausible 88–94% reductions.

`[RECOMMENDATION]` Primary deterministic specification:
- Use a **log-linear dose–response with the outcome-specific pooled BPLTTC HR per 5 mmHg**: $RR_b = HR_5^{\Delta_b/5}$.
- Take the pooled HR and CI from the BPLTTC main analysis. They are not in the repository and must be extracted.
- Define $\Delta_b$ by an approved achieved-SBP rule (Q03).

Structural sensitivity analyses:
1. Own-stratum log-linear.
2. The current cumulative traversal.
3. An alternative achieved-SBP rule.

Status: requires investigator, statistician and clinical co-author approval (Q02, Q03).

### 4.4 Incidence effects versus case-fatality effects — **FAIL — EVIDENCE MISSING**

Trace `[VERIFIED]`:

| Effect | Value(s) | Cause | Code | Cited source | Estimand | Uncertainty | Applied to | Interaction with CF trend |
|---|---|---|---|---|---|---|---|---|
| BP → incidence | $E_N,E_D$ (4.3) | all four | 06:190–213 | BPLTTC (mislabelled Ettehad) | Fatal+non-fatal events per SBP reduction | 95% CI available | Well→Sick, hypertensive bins | none |
| BP → CF | ρ = 0.24 (ihd), 0.36 (istroke), 0.76 (hstroke), 0.20 (hhd); 0.047 (aod, inactive) | per cause | 06:215–240 | **none** (object named `cf_etihad`; manuscript M:802 cites Ettehad) | "Proportional CF reduction per unit incremental control" — not reported by any cited source | none | **All prevalent cases** (single Sick compartment: incident and existing), not restricted to hypertensives. $\overline{\Delta c}$ is a *normalised* mean over hypertensive bins, not multiplied by P(hypertensive \| sick). | Multiplies the BAU CF, which already contains the 80% secular-decline trend (05:359–363) |
| Statins → incidence | RR 0.74 (ihd), 0.80 (istroke) × AF | ihd, istroke | 06:1184–1227 | CTT (see 4.5) | per 1 mmol/L LDL-C | none | Well→Sick, ages ≥40 | none |
| Statins → CF | RR 0.80 (ihd); 0.96 × ψ 0.60 (istroke) | ihd, istroke | 06:1184–1227 | none specific | Event RR used as CF multiplier | none | All prevalent cases ≥40, no AF | Multiplies BAU CF |
| TFA → CF (legacy) | effect by TFA intake | ihd | 06:1026–1029 | legacy | — | — | Executed only in `all_interventions` | — |

**Double-counting analysis:**
- `[INFERRED]` BPLTTC and CTT outcomes include fatal *and* non-fatal events. The incidence pathway already credits prevented fatal first events through fewer cases.
- An additional CF reduction applied to the acute fatality of events that still occur is not supported by BP-lowering trials. Chronic BP control does not reduce the fatality of an event once it has occurred.
- A CF effect is defensible only as *secondary prevention*: fewer recurrent fatal events among people with established disease. It would then need a secondary-prevention estimand, e.g. BPLTTC secondary-prevention CV death, or CTT CHD death in participants with prior CHD. It would also need to be restricted to treated, eligible prevalent cases.
- The current ρ values have no source, and 0.76 for ICH is implausibly large (a 76% CF reduction at full control).

**Size of the CF pathways** `[TESTED]` (bp_combined and statins_only, cumulative deaths averted 2026–2050):

| Country | BP IR+CF | BP IR-only | BP CF-only | share from CF | Statin IR+CF | Statin IR-only | share from CF |
|---|---|---|---|---|---|---|---|
| Colombia | 198,144 | 119,111 | 82,603 | 40% | 168,078 | 27,460 | 84% |
| China | 4,885,318 | 2,874,154 | 2,035,900 | 41% | 6,283,468 | 879,932 | 86% |
| Nigeria | 215,974 | 123,873 | 93,352 | 43% | 275,494 | 48,362 | 82% |

The "share from CF" is 1 − IR-only/IR+CF.

**Specifications compared:**
1. **Incidence only.** Directly supported by trial event reductions.
2. **Incidence plus current CF** (current model). Unsupported ρ; high risk of double counting.
3. **Incidence plus an evidence-based secondary-prevention CF effect.** Applied only to prevalent cases who are hypertensive (BP) or have established ASCVD (statins). Parameterized from BPLTTC secondary-prevention CV death or CTT secondary-prevention estimates. The repository holds Appendix S6 "Cardiovascular death" by stratum, but for primary and secondary prevention combined, so option 3 is `[UNRESOLVED]` until the right estimates are extracted.

`[RECOMMENDATION]`
- **BP:** primary = incidence only (1); structural sensitivity = current CF (2) and, once parameterized, (3).
- **Statins:** primary = incidence only, *after* the AF issue in 4.5 is corrected; structural sensitivity = a secondary-prevention pathway. Because statins' primary-prevention effect is currently shrunk by the AF while the CF effect is not, the statin result is dominated by an unsourced pathway.
- The evidence is insufficient to support the current CF specification as primary. This is a scientific decision (Q04, Q06).

### 4.5 Statin point estimates and sources — **FAIL — EVIDENCE MISSING**

| Parameter | Code value | Location | Supporting citation | Estimand vs modeled transition | Finding |
|---|---|---|---|---|---|
| IHD incidence RR | 0.74 | 06:1089 (06o:1103) | Comment 06:1058–1062: "RR coronary heart disease 0·80, 99% CI 0·74–0·87", "Source: Trials 2015", DOI 10.1016/S0140-6736(10)61350-5 (CTT 2010, bib `Baigent2010IntensiveLDLLowering` by title). Manuscript cites Fulcher 2015 (CTT). | RR per 1 mmol/L LDL-C, applied unscaled per treated person × AF | `[VERIFIED]` **0.74 equals the lower 99% limit of the CHD estimate quoted next to it. It is not a point estimate.** Replacing it with 0.80 lowers statin deaths averted by 3.2% (COL), 1.9% (CHN) and 2.4% (NGA) `[TESTED]`. |
| Ischaemic-stroke incidence RR | 0.80 | 06:1090 | none specific | RR | **Unsupported** |
| IHD CF RR | 0.80 | 06:1091 | Same value as the CHD event RR in the comment | Event RR applied as a CF multiplier to all prevalent IHD ≥40 | **Estimand mismatch** (4.4) |
| Ischaemic-stroke CF RR | 0.96 | 06:1092 | none anywhere | — | **Unsupported** |
| Attributable fraction | country-specific `af_statins` (IHD mean 0.157, range 0.073–0.294; istroke mean 0.196); defaults 0.1497/0.1161 | 06:1054, 1098–1099, 1164–1167 | "GBD 2021 risk factor attribution, High Fasting Plasma glucose" (06:1051–1052, 1066–1068) | Multiplies the incidence effect | **Estimand mismatch.** The high-FPG PAF is neither the share of incidence preventable by statins nor the share of events in the statin-eligible population. It appears to proxy "diabetes-targeted" statin use (comment 06:1043). |
| Proportion of ischaemic stroke that is atherosclerotic | 0.60 | 06:1078 default, 06:1548 hard-coded; 04:839 | Petty 1999 (bib entry; M:266) | Scales istroke CF effect | Citation present; derivation of 0.60 undocumented `[UNRESOLVED]` |
| Adherence | **1 / 1 active** (06:2075–2076). 0.575 / 0.664 are defined at 06:1965–1966 but never used. | — | none. The library has Basios et al. 2025 (adherence) but it is not linked. | Multiplies coverage | Documentation contradicts itself: "full adherence" (M:114, 794) vs "adherence-adjusted" (M:298; slides S1:476) |
| Baseline coverage | `statins_current` in year 2024 (median 0.040, mean 0.077, range 0.0003–0.368) | 06:1387–1395 | "FDC_coverage_data_statints_pp.csv" (04:772; not in repository) | $U_0$ | Provenance incomplete; script 04 labels it "2025" |
| Target coverage | **0.50** (run call 06:2072) | also 0.50 in the parameter block (06:1962); 0.60 function default (06:1079); 0.6 in `statin_data.rds`; **0.80** in `statins_control_targets_by_loc.csv` (04:925, not consumed) | WHO target | — | Active value is clear, but the files disagree |
| Start / target year | 2026 / **2030** (06:2073–2074) | Parameter block says 2026 / 2050 (unused) | — | 20% of increment in 2026 (06:397, 404), full by 2030, constant to 2050 | Dead parameter block contradicts the active call |
| Eligibility | age ≥40 (06:1223) | — | policy | Tracking columns `eff_ir/eff_cf` are modified for ages <40 (06:1233–1234) although the rates are not | Minor tracking defect |

**Face-validity concern.** `[TESTED]` Global statins_only (21.2M) averts more deaths than bp_combined (20.7M). 84–86% of the statin benefit comes from the unsourced CF pathway.

`[RECOMMENDATION]`
- **Can stay:** target coverage 0.50, start 2026, target 2030, ages ≥40, and the scale-up formula. These are policy definitions.
- **Must be corrected before freeze:**
  - IHD incidence RR: a verified point estimate and CI, with an explicit outcome and a per-mmol/L scaling decision.
  - Ischaemic-stroke incidence RR: sourced.
  - AF: replaced with a defined eligible-event share, or removed with an explicit eligibility model.
  - CF RRs: sourced with a secondary-prevention estimand, or moved to a structural scenario.
  - Adherence: decided (recommend 1 for ITT-based effects, with DSA once a source for 0.575/0.664 is documented).
- **DSA:** ψ = 0.60 and baseline coverage.

### 4.6 Harmonized 2026–2050 horizon — **FAIL — CODE DEFECT** (reporting layer; counts unaffected)

Model years `[TESTED]`:
- Outputs cover 2017–2050, i.e. 34 years.
- The transition loop is nominally 2018–2058 (06:1588), but `b_rates` stops at 2050 (05:181), so 2051–2058 are no-ops. The log message "Projecting from 2017 to 2058" (06:1586) is misleading.

Intervention timing `[VERIFIED]`:
- BP fraction = $\min(1,\max(0,(t-2025)/5))$ (06:24–26), giving 0.2 in 2026 and 1.0 in 2030.
- Statins use $(t-2026+1)/5$ (06:397–404), also 0.2 in 2026.
- Both are held constant from 2030 to 2050.
- `[TESTED]` Deaths averted in 2025 are exactly 0 for every scenario.

Mismatches:

| Location | What it does | Consequence |
|---|---|---|
| `07_output_dalys.R:6, 166` | `year >= 2026` | Correct (2026–2050, 25 years) |
| R1:336 `study_years` | from `year >= 2025` → "2025–2050"; saved in `paper_scalars$study_years` | Label mismatch |
| R1:789–1320 (Tables 1–5, figures) | sums from `year >= 2025`, labels "2025–2050", divides by 25 | Counts numerically equal 2026–2050 (2025 averted = 0); labels wrong |
| R1:1007, 1038, 1081, 1368–1603 | "2026–2050" | Internal inconsistency within one report |
| R1:1672 `bau_total_deaths` | BAU deaths from 2025 (694.27M) | **% of BAU understated**: combined 4.93% vs 5.07%; bp_combined 2.98% vs 3.06%; statins 3.06% vs 3.14% `[TESTED]` |
| M:93 | manual override `study_years <- "2026-2050"` | Hides the upstream label |
| `06:1826–1829` `calculate_cumulative_impact` | default `start_year = 2025` (unused helper) | Latent |
| math-doc:34–35, 95 | $T_{start}$ default 2025, fraction without +1 | Outdated |
| slides S1:1234 | $\min[(t-2026)/4,1]$ | Contradicts code (gives 0 in 2026) |
| aim2_report | "2025–2050", `/25` over 2025–2050 sums | Stale |

`[RECOMMENDATION]` Deterministic definition:
- The reporting horizon is calendar years **2026–2050 inclusive (25 years)**.
- Every cumulative sum, BAU denominator and average uses `year %in% 2026:2050`.
- Annual average = cumulative / 25.
- The intervention fraction is $f(t)=\min(1,\max(0,(t-2025)/5))$ for BP and statins.
- Labels must be generated from the same constant.

### 4.7 ASMR calculation — **FAIL — CODE DEFECT**

Audit of `calculate_asmr` (R1:143–170) and its two uses:

| Check | By-cause ASMR (R1:745–757) | Scenario ASMR (R1:1331–1339; `sl_dt_asmr.rds`; `aim1_asmr_interventions.png`; slides; manuscript Fig. S4) |
|---|---|---|
| Numerator | deaths of that cause, summed over locations and sexes | deaths summed over 4 causes (correct for all-CVD) |
| Denominator | Σ(well + sick) of that cause chain — population once | **Σ over 4 cause chains of (well + sick) ≈ 4 × population** `[TESTED]`: ratio 3.92–3.98 per country; report ASMR 131.7 vs 526.6 per 100,000 (2019, same denominator counted once) |
| Denominator timing | well + sick = pop − all.mx, i.e. end-of-year survivors, not start-of-year or mid-year population (480.1 per 100,000 with start-of-year pop) | same |
| Age grouping | single years 20–95; 95 = 95+ | same |
| Open age group | 95+ weight = pooled weights for 95–100 | same |
| Standard population | Hard-coded decade-step weights (0.013 × 10 years, 0.014 × 10, …) renormalised over 20–95+; after renormalisation the ≥20 shares are 20s 21.3%, 30s 19.6%, 40s 18.0%, 50s 14.7%, 60s 11.5%, 70s 8.2%, 80s 4.9%, 90+ 1.8%. **Captioned "WHO World Standard Population (2000–2025)" but not the published table.** `[INFERRED]`: the published 5-year WHO weights put much less weight on ages ≥80; verify against Ahmad et al. 2001 (no bib entry). | same |
| Weights sum to 1 | Yes after renormalisation `[TESTED]` | Yes |
| BAU vs scenario definition | identical | identical, so relative differences keep their direction but absolute levels are ~4× too low |
| Same method in slides/manuscript | slides reuse `asmr_int`; manuscript cites the figure | aim2_report has the same ×4 defect (aim2_report:881, 1388) |

`[RECOMMENDATION]` Canonical function, for scenario $s$, year $t$, cause set $C$, and 5-year age groups $g\in\{20\text{–}24,\dots,85+\}$:

$$\mathrm{ASMR}_{s,t}=10^5\sum_{g} w_g\,\frac{\sum_{a\in g}\sum_{\ell,x}\sum_{c\in C} D_{\ell,x,c,s,t,a}}{\sum_{a\in g}\sum_{\ell,x}\left(P_{\ell,x,c_0,s,t,a}-\tfrac12 A_{\ell,x,c_0,s,t,a}\right)},\qquad \sum_g w_g=1,$$

where:
- $D$ = `dead`.
- $P$ = `pop` (start-of-year population of the stratum), read from **one** reference cause chain $c_0$, so population is counted once. `pop` is identical across cause rows (max relative difference 1.8×10⁻¹³) `[TESTED]`.
- $A$ = `all.mx` (all-cause deaths in the stratum).
- $w_g$ = the official WHO World Standard 5-year weights for ages ≥20, renormalised.

The same function must serve cause-specific ASMR ($C=\{c\}$), all-CVD ASMR, BAU and every scenario, in the report, slides and manuscript.

Regression tests (Section 17):
- Population counted once: the denominator equals Σ pop of one chain.
- The four cause-specific ASMRs sum to the all-CVD ASMR. The current code fails this test.
- Σw = 1.
- A constant-rate synthetic population returns that rate.
- BAU ASMR equals scenario ASMR for t ≤ 2025.

### 4.8 Exact policy-scenario set — **FAIL — CODE DEFECT**

Executed scenarios `[VERIFIED]` 06:1754–1776; `[TESTED]` all six are present in every country output:

| scenario_id | Displayed label | Interventions | Executed | Reported | Issue |
|---|---|---|---|---|---|
| `baseline` | Business-as-Usual | none | Yes | Yes | — |
| `bp_no_diabetes_only` | HTN Control (No Diabetes) | BP, no-diabetes subgroup | Yes | Yes (07, R1). **Dropped from manuscript Table 1** because `int_order` (M:85–90) lists the nonexistent "HTN Control (General)" `[VERIFIED]` | label |
| `bp_diabetes_only` | HTN Control (Diabetes) | BP, diabetes subgroup | Yes | Yes | — |
| `bp_combined` | — | both BP subgroups (the 150M policy) | Yes | **No** (07:143–146; R1:313–316) | The policy scenario is not reported |
| `statins_only` | Improved Statin Uptake | statins | Yes | Yes | parameters (4.5) |
| `all_interventions` | All Interventions | BP (both) + **sodium + TFA** + statins | Yes | **Yes (headline)** | **Defective** |

How the `all_interventions` defect arises `[TESTED]`:
1. The legacy `ETIHAD_RR` mapping (06:345–379) expects Ettehad strata ("<130", ">=160"). The BPLTTC CSV (06:319) uses seven different strata, so 16 of 28 rows get `bp_cat = NA`.
2. With `salteff = 0`, `sbp_reduction = 0`. But `rr_per_10mmhg` is `NA` in 5 of 8 bins, and `NA × 0 = NA` (06:878–883). As a result, `IR_new` is `NA` and `eff_ir_salt` is `NA` for **100%** of rows from 2026 on (T13).
3. `eff_ir = eff_ir_bp × eff_ir_salt` is `NA`, and 06:1503 then sets `eff_ir := 1`.
4. **Every BP incidence effect is erased.** `eff_ir(all)` equals `eff_ir(statins_only)` for ihd/istroke and 1 for hhd/hstroke, in every 2026+ row of all 186 countries (T14).
5. TFA elimination (`tfa_target_tfa = 0`, from 2028) reduces IHD CF in 126 countries (mean factor 0.966 in 2030; minimum 0.853).
6. The reported cause split exposes the problem: HHD deaths averted = −11,705 and hstroke = 0.96M. For comparison, bp_combined gives HHD 2.94M and hstroke 4.91M.
7. A correct BP + statin scenario (tested in the harness) averts 359,840 vs 316,125 reported (Colombia, +13.8%), 11.09M vs 10.40M (China, +6.6%) and 488,270 vs 368,035 (Nigeria, +32.7%).

**Legacy components.** Sodium and TFA are not "not executed" in the active scenario set. Both run inside `all_interventions`: sodium produces `NA`s, and TFA has a real effect. They are legacy code that currently contaminates the headline scenario and must be removed. Their evidence was not audited, as instructed.

The recommended frozen set is in Section 8.

---

## 5. Verified defects

| ID | Defect | Location | Affects central estimates? | Evidence |
|---|---|---|---|---|
| D1 | Combined scenario erases BP incidence effects (NA → 1) | 06:1484–1503 with 06:345–379, 857–894 | **Yes (headline)** | T13, T14 `[TESTED]` |
| D2 | Combined scenario includes TFA elimination (IHD CF) | 06:1761–1763, 1521–1533; run call 06:2082–2083 | **Yes (headline)** | T14 `[TESTED]` |
| D3 | Scenario ASMR denominator counts population 4× | R1:1331–1336; aim2_report:881, 1388 | ASMR (reported figure) | T05/T06 `[TESTED]` |
| D4 | ASMR weights mislabelled as WHO World Standard | R1:143–157, 777, 1354 | ASMR levels | `[VERIFIED]` |
| D5 | % of BAU uses 2025–2050 denominator | R1:1672 and dependants | Reported percentages (−2.7% relative) | T07 `[TESTED]` |
| D6 | Manuscript `int_order` label "HTN Control (General)" drops the no-diabetes row | M:85–90, 317 | Reported table content | `[VERIFIED]` |
| D7 | Statin IHD incidence RR is the lower 99% CI limit | 06:1089 | Yes (−1.9% to −3.2% statin deaths if 0.80) | `[VERIFIED]` + T15 |
| D8 | Aim 2 hstroke RR overwrite persists | 06t:638 | Aim 2 only (not executed) | `[VERIFIED]` |
| D9 | Tracking columns `eff_ir/eff_cf` differ from applied ratios (TFA on all causes; statins <40) | 06:1029, 1233–1234 | No (columns dropped downstream) | T19 `[TESTED]` |
| D10 | Negative initial `well` (42 rows; 2017; ages 93–95; IHD) | 06:1569–1575 | Negligible | T18 `[TESTED]` |
| D11 | Production path (`00_run_model.R:112`) sources an untracked script | `00_run_model.R` | Reproducibility | `[VERIFIED]`; outputs identical (T09) |
| D12 | Dead parameter block contradicts the active run call (adherence 0.575/0.664; statin target year 2050) | 06:1952–1966 vs 2072–2083 | No (but misleading) | `[VERIFIED]` |
| D13 | Script 04 statin table reads `C:/Users/.../WHO-CVD/Scenarios.xlsx` outside the repo and writes an 80% target not used by the model | 04:739, 925 | No | `[VERIFIED]` |
| D14 | Placeholder effect_size = 1 for non-hypertensive bins in the effect workbook | workbook Sheet1 | No (bins excluded) | `[VERIFIED]` |

## 6. Scientific interpretation issues

1. **BPLTTC strata used as dose–response increments** (4.3). Decision needed on how to use the strata.
2. **CF effects** for BP (ρ) and statins may double-count trial event reductions. They are applied to all prevalent cases, not only hypertensive ones (4.4).
3. **Statin AF** (high-FPG PAF) does not match the quantity it is used for. The incidence effect is also not scaled by the LDL-C reduction achieved (4.5).
4. **Achieved SBP** after control is implicit (to 130–139 or 120–129) and undocumented.
5. **Diabetes assumptions.**
   - The diabetes share among hypertensives is assumed equal to population diabetes prevalence (04:283–302; 06:145–152).
   - A 15-percentage-point diabetes–non-diabetes control gap is assumed without a source (04:35–67).
6. **Baseline control input.** Control from `bp_data6.csv` (file Year = 2015) is used as the 2025 baseline.
7. **Baseline trends.**
   - Incidence is held at 2019 levels to 2050 (05:178–183).
   - 80% of the secular CF decline is treated as exogenous without a source (05:359–363).
   - Some forecast strata show CF increases of up to +107% by 2050, which needs a face-validity review.
8. **Outcome mapping.** Heart-failure evidence is applied to HHD, and total-stroke evidence to both stroke types.
9. **Combination claims.** The manuscript says the combined benefit "exceeds the sum" (M:256, 270, 1063). This contradicts the model's multiplicative (sub-additive) algebra. `[TESTED]` The correct BP + statins result is 98.3–99.3% of the sum of its components.

## 7. Evidence and citation inconsistencies

- BPLTTC 2021 is used but never cited. There is no bib entry, and the appendix ("mmc1 (9).pdf") is not in `library/`. Ettehad 2016 is cited instead (M:126, 157, 164, 802–803, 844).
- The `Ettehad2016` bib entry has a malformed `doi` field (a supplement path is appended).
- The statin comment cites "Trials 2015" but gives the DOI of CTT 2010. The manuscript cites Fulcher 2015. None of the three is in `library/`.
- There is no source anywhere for ρ_CF, statin istroke RR 0.80, statin CF RR 0.96, the 0.8 CF-trend share or the 15-pp control gap.
- The manuscript's parameter table lists statin AF defaults "0.15/0.12" and "GBD 2021" without naming the risk factor (high fasting plasma glucose).
- Adherence is described as "full" in M:114/794 but "adjusted" in M:298 and slides S1:476.
- The ASMR is labelled "WHO World Standard Population (2000–2025)", but there is no reference and the weights differ.
- The manuscript methods (M:938–1004) describe the retired diabetes-weighted BP algorithm. Only the slides describe the current split-population mixture.
- M:844 labels "GBD 2023: Chong et al.", but Chong 2024 is not a GBD collaborator paper.
- The title says "190 countries"; the outputs contain 186.

## 8. Frozen scenario-definition table (recommended)

| scenario_id (frozen) | Label | Interventions | Eligible population | Baseline coverage | Target | Start / target | After target | Adherence | Incidence effects | CF effects | Role |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `baseline` | Business-as-usual | none | adults 20–95 | embedded in rates | — | — | BAU trends (05) | — | — | — | Reference; fixed in initial PSA |
| `bp_no_diabetes_only` | BP control: hypertensives without diabetes | BP (no-DM) | hypertensives without diabetes, SBP ≥140 bins | $c_N$ (weighted 0.157) | 83.76M additional (weighted 0.222) | 2026 / 2030 | constant to 2050 | implicit (control achieved) | per G1-02/03 decision | per G1-04 decision | Component (reported) |
| `bp_diabetes_only` | BP control: hypertensives with diabetes | BP (DM) | hypertensives with diabetes | $c_D$ (0.323) | 66.24M additional (0.453; cap 0.80) | 2026 / 2030 | constant | implicit | as above | as above | Component (reported) |
| `bp_combined` | BP control: 150 million additional controlled | BP (both) | all hypertensives | $c_N,c_D$ | 150M (mode `diabetes_capped_to_target`) | 2026 / 2030 | constant | implicit | additive mixture | as above | **Primary BP policy scenario — report it** |
| `statins_only` | Statin treatment scale-up | statins | adults ≥40 (definition per Q19) | 2024 `statins_current` | 50% | 2026 / 2030 | constant | per Q08 (1 recommended) | corrected RRs | per G1-04 | Primary statin scenario |
| `bp_statins_combined` (new; replaces `all_interventions`) | **Combined BP control and statin treatment** | BP (both) + statins | as components | as components | 150M + 50% | 2026 / 2030 | constant | as components | BP × statins (sequential) | as components | Headline combined scenario |

Changes relative to the current set:
- **Remove** `all_interventions`, together with its sodium and TFA components.
- **Rename** display labels as shown in the table.
- **Report** `bp_combined`.
- **Keep for validation only:** the check that `bp_combined` equals the sum of the two subgroups on the rate scale, and the Aim 2 scenario IDs (inactive).
- **Classify as inactive legacy code** the sodium and TFA functions and the stale `htncov2_*` labels in the Aim 2 deliverables.

## 9. Horizon and ASMR definitions (frozen proposal)

- **Horizon:**
  - $\mathcal{T}=\{2026,\dots,2050\}$, $|\mathcal{T}|=25$.
  - Deaths averted: $A_s=\sum_{t\in\mathcal{T}}(D_{BAU,t}-D_{s,t})$.
  - Annual mean: $A_s/25$.
  - % of BAU: $A_s/\sum_{t\in\mathcal{T}}D_{BAU,t}$.
  - Intervention fraction: $f(t)=\min(1,\max(0,(t-2025)/5))$, which gives 0 in 2025 and 1 from 2030.
- **ASMR:** the canonical formula in 4.7 (population counted once, mid-year denominator, WHO World Standard 5-year weights for ages ≥20 renormalised to 1). The same function is used everywhere.

## 10. Parameter-register summary

Sheet `Parameter_Register` has 86 rows × 50 columns: the 33 required fields, the 16 freeze fields, and `proposed_scope_when_unblocked`.

| Family | Rows | Freeze decisions |
|---|---|---|
| Baseline epidemiology and structural baseline assumptions (BL-01…11) | 11 | DEFER 4, FIX 4, STRUCTURAL 3 |
| BP distribution and GBD RR (BP-01…04) | 4 | DSA 2, FIX 1, DEFER 1 |
| Diabetes share, baseline control, policy, scale-up (DC-, PO-) | 13 | FIX 7, DSA 4, DEFER 1, STRUCTURAL 1 |
| BPLTTC stratum HRs (BE-IHD/STK/HF × 7 strata) | 21 | DEFER 15 (used), EXCLUDE 6 (unused under current spec) |
| BP structural (BE-STRUCT-01…06) and BP CF ρ (BCF-*) | 12 | STRUCTURAL 4, FIX 2, DEFER 4, EXCLUDE 2 (aod, Ettehad Aim 2) |
| Statins (ST-01…11) | 11 | DEFER 4, DSA 4, STRUCTURAL 2, FIX 1 |
| Combination / legacy (CO-01…03) | 3 | FIX 1, EXCLUDE 2 |
| Population, DW, LE, discounting, ASMR weights (DE-, OC-) | 5 | STRUCTURAL 2, DSA 2, FIX 1 |
| Economic (EC-01…04) and model structure (SM-01…02) | 6 | DSA 4, FIX 2 |
| **Total** | **86** | DEFER 29, FIX 19, DSA 16, STRUCTURAL 12, EXCLUDE 10, **INCLUDE 0** |

Freeze status: READY 32, CONDITIONAL 30, BLOCKED 24.

`Evidence_Map` has 17 records. It flags 6 unsupported values, 5 estimand or citation mismatches, 2 reused-evidence records and 1 out-of-scope-but-executed record.

## 11. PSA Parameter Freeze Gate conclusion — **not passed**

Gate criteria and status:

| Criterion | Met? | Why not |
|---|---|---|
| Every included parameter has a complete, reproducible sampling specification | Vacuous (0 included) | The intended initial parameters are blocked |
| No unsupported precision invented | Yes | No distribution is proposed without repository evidence |
| Reused estimates share draws | Specified | Stroke → istroke and hstroke; HF → hhd; strata shared between diabetes and non-diabetes products; CTT shared across IHD pathways |
| Missing covariance disclosed, with a sensitivity option | Specified | Cross-outcome covariance within a BPLTTC stratum is missing (sensitivity: perfect correlation); cross-age covariance for GBD RRs is missing |
| Every sampled parameter has an injection point and validation test | Partially | `project.all()` has no argument for an alternative effect table or statin RRs; one is needed (C03, C05) |
| BAU handling correct | Yes | BAU invariant `[TESTED]` |
| Every reported estimate has a UI class | Yes | `UI_Coverage_Map` (16 families) |
| Every blocked parameter assigned to DSA, structural, defer or fixed | Yes | See register |

The gate fails because the parameters that matter most are blocked:

| Blocked parameter | Blocking reason | What is needed |
|---|---|---|
| BPLTTC HRs | Source not in repository (criterion 3); estimand use depends on G1-03 (criterion 2) | Add and verify the source; approve the specification |
| Statin incidence RRs | Invalid or unsourced point estimates | Extract verified CTT estimates |
| BP and statin CF effects | No evidence base | Evidence, or a structural decision |

## 12. Frozen initial-PSA parameter set

**Currently frozen: none.** The proposed initial set below is ready to freeze once G1-02, G1-03, G1-05 and Q16–Q17 are resolved.

**P-I1. BP-lowering HRs (BPLTTC 2021).**
- **Source:** HR per 5 mmHg with its 95% CI.
- **Transformation:** $\log RR_{10}=2\log HR_5$; $SE(\log RR_{10})=2\,SE(\log HR_5)=(\ln U_{10}-\ln L_{10})/(2\times1.96)$.
- **Distribution:** $\log RR_{10}\sim N(\ln RR_{10},SE^2)$. No truncation (harmful draws are allowed); assert the incidence multiplier stays > 0.
- **Draw level and sharing:** one global draw per iteration.
  - The Stroke draw is shared by istroke and hstroke; the HF draw by hhd.
  - The same stratum draw feeds both the no-diabetes and diabetes effects.
  - Strata are independent of each other (distinct participants).
  - Cross-outcome covariance is missing; the sensitivity analysis assumes perfect correlation across outcomes within a stratum.
- **Injection:** rebuild `ETIHAD_RR_BIN` per draw according to the approved specification, and pass it through a new `etihad_rr_table` argument of `project.all()`.
- **Scenarios:** all BP scenarios and the combined scenario.
- **Validation:** an RR = 1 draw gives `eff_ir` = 1; the median-draw run reproduces the deterministic run; the identity T01 holds.
- **If the pooled specification is approved (recommended):** only three parameters (IHD, stroke, HF pooled HR5), taken from the BPLTTC main paper. These values are not in the repository.
- **If a stratum-based specification is approved:** the 15 parameters below, from the repository's Appendix S6.

| Outcome | Stratum | HR per 5 mmHg (95% CI) | RR per 10 mmHg (95% CI) | meanlog | sdlog |
|---|---|---|---|---|---|
| Ischaemic heart disease | 130–139 (diabetes step) | 0.96 (0.88–1.04) | 0.9216 (0.7744–1.0816) | −0.0816 | 0.0852 |
| Ischaemic heart disease | 140–149 | 0.94 (0.87–1.02) | 0.8836 (0.7569–1.0404) | −0.1238 | 0.0812 |
| Ischaemic heart disease | 150–159 | 0.91 (0.83–0.99) | 0.8281 (0.6889–0.9801) | −0.1886 | 0.0899 |
| Ischaemic heart disease | 160–169 | 0.87 (0.80–0.95) | 0.7569 (0.6400–0.9025) | −0.2785 | 0.0877 |
| Ischaemic heart disease | ≥170 | 0.94 (0.88–1.01) | 0.8836 (0.7744–1.0201) | −0.1238 | 0.0703 |
| Stroke | 130–139 | 0.92 (0.83–1.02) | 0.8464 (0.6889–1.0404) | −0.1668 | 0.1052 |
| Stroke | 140–149 | 0.90 (0.82–0.99) | 0.8100 (0.6724–0.9801) | −0.2107 | 0.0961 |
| Stroke | 150–159 | 0.77 (0.65–0.92) | 0.5929 (0.4225–0.8464) | −0.5227 | 0.1772 |
| Stroke | 160–169 | 0.86 (0.79–0.94) | 0.7396 (0.6241–0.8836) | −0.3016 | 0.0887 |
| Stroke | ≥170 | 0.90 (0.84–0.97) | 0.8100 (0.7056–0.9409) | −0.2107 | 0.0734 |
| Heart failure | 130–139 | 0.85 (0.75–0.97) | 0.7225 (0.5625–0.9409) | −0.3250 | 0.1312 |
| Heart failure | 140–149 | 0.95 (0.85–1.07) | 0.9025 (0.7225–1.1449) | −0.1026 | 0.1174 |
| Heart failure | 150–159 | 0.88 (0.78–1.00) | 0.7744 (0.6084–1.0000) | −0.2557 | 0.1268 |
| Heart failure | 160–169 | 0.70 (0.61–0.80) | 0.4900 (0.3721–0.6400) | −0.7133 | 0.1383 |
| Heart failure | ≥170 | 0.89 (0.79–1.00) | 0.7921 (0.6241–1.0000) | −0.2331 | 0.1203 |

**P-I2. Statin incidence RRs** (IHD; ischaemic stroke).
- **Source:** verified CTT point estimates and CIs, with the correct confidence level. For a 99% CI, $SE=(\ln U-\ln L)/(2\times2.576)$.
- **Illustration only, if the comment's CHD estimate is verified:** meanlog = ln 0.80 = −0.2231; sdlog = (ln 0.87 − ln 0.74)/5.152 = 0.0314.
- **Distribution:** lognormal, global draw. Shared with any IHD statin CF effect that is based on the same CTT estimate.
- **Injection:** new arguments for the RRs in `calculate_statins_impact`.
- **Validation:** RR = 1 gives `eff_ir` = 1.

**P-I3 (only if G1-04 retains CF effects).** Evidence-based secondary-prevention RRs, parameterized as in P-I1 and P-I2.

**BAU rule.** BAU is computed once and reused. None of P-I1 to P-I3 enters the BAU path.
- `project.all` with `interventions = character(0)` calls no intervention function.
- Bin allocation is normalised.
- `[TESTED]` BAU deaths are identical cell by cell across all structural variants.

## 13. Extended or deferred parameter set

- **Extended PSA (paired BAU and scenario draws required where marked):**
  - GBD RR per 10 mmHg (BP-04; lognormal from the file's 95% UI; one z per cause; BAU invariant).
  - Mean SBP (BP-01; normal from Lower95/Upper95 once the source is confirmed; country-level shared z; BAU invariant).
  - Baseline IR, CF, BG.mx and prevalence (BL-01, 03, 07, 08; GBD draws plus recalibration; **paired**).
  - Population (DE-01; WPP variants as scenarios; **paired**).
  - DW (OC-01).
  - Statin baseline coverage (ST-10).
  - AF (ST-05, once its estimand is defined).
  - ψ (ST-07).
  - BP CF evidence (BCF-*, if sourced).
- **DSA:** SD of SBP; P(D|H); 15-pp gap; baseline control; scale-up path; adherence; ψ; health discounting; economic parameters (elasticity, VSL ratio and floor, discount rate, GNI/SSP).
- **Structural scenarios:**
  - BP stratum interpretation (3 options).
  - Achieved-SBP rule.
  - BP CF inclusion (3 specifications) and statin CF inclusion.
  - LDL-C scaling.
  - Outcome mapping.
  - CF-trend exogenous share (0.8 vs 1.0).
  - Incidence trend.
  - Allocation mode.
  - LE source (WPP vs GBD reference).
  - Effect lag.
- **Fixed:** policy definitions (150M, 0.80 cap, 50% statins, years, age ≥40), bin structure, calibration factors, rate caps and model structure.

## 14. Mapping from reported estimates to UI-generating parameters

The full map is in sheet `UI_Coverage_Map`. Under the proposed initial PSA (P-I1 and P-I2; BAU fixed):

| Reported estimate family | Contributing sampled parameters | Important fixed | Class |
|---|---|---|---|
| Cumulative/annual deaths averted; by cause, region, income, country, age, sex; % of BAU | P-I1, P-I2 | baseline rates, population, BP distribution, GBD RR, CF effects, AF, adherence | PARTIAL_PARAMETER_UI |
| DALYs, YLLs, YLDs averted | P-I1, P-I2 | + DW, LE | PARTIAL_PARAMETER_UI |
| Scenario ASMR (after the fix) | P-I1, P-I2 | + standard weights | PARTIAL_PARAMETER_UI |
| Economic value | P-I1, P-I2 | + economic parameters (DSA) | PARTIAL_PARAMETER_UI |
| BAU deaths, BAU ASMR, BAU trends | none | all baseline inputs | STRUCTURAL_FIXED |
| Structural sensitivity results | n/a | — | STRUCTURAL_FIXED (report as ranges) |
| 150M target tables, statin coverage, country count | none | policy definitions | NO_UI_JUSTIFIED |

In the current state, every model-derived estimate is **UNRESOLVED** because no parameter is ready.

## 15. Recommended manuscript language for the UI scope

> "Ninety-five percent uncertainty intervals (UIs) were obtained from [N] Monte Carlo draws. Each draw sampled the relative effects of blood-pressure lowering (BPLTTC 2021, log-normal distributions derived from the published 95% confidence intervals) and of statin therapy (CTT, log-normal distributions derived from the published confidence intervals). The same draw was applied in all countries, and each UI is the 2.5th–97.5th percentile of the resulting distribution. Business-as-usual projections were held fixed. The UIs therefore reflect uncertainty in intervention effect sizes only. They do not include uncertainty in baseline disease rates, population projections, blood-pressure distributions, baseline treatment coverage, adherence, disability weights or life expectancy. They also exclude structural uncertainty, such as how baseline-blood-pressure-stratified trial effects are applied or whether case-fatality effects are included. We examined these in deterministic sensitivity and scenario analyses (Supplementary Tables X–Y). The UIs should be read as partial parameter uncertainty intervals and are likely to understate total uncertainty."

Do not call these UIs "comprehensive" or describe them as "reflecting uncertainty in all model inputs".

## 16. Diagnostic tests executed and results

All tests below were **executed** unless marked otherwise. The harness is in the session scratchpad: the 05 text is evaluated with a location filter, and the canonical 06 definitions are evaluated without the cluster. Nothing was written to the repository.

| Test | Scope | Result |
|---|---|---|
| T01 Baseline reconstruction Σp_b IR_b = IR | COL, CHN, NGA; all causes | **PASS** (2.2×10⁻¹⁶). Pre-fix emulation 0.78–0.999 |
| T02 Zero-effect identity | 3 countries (BP both inactive; baseline-ID targets) | **PASS** (2.2×10⁻¹⁶). Statin zero-effect: *specified but not run* |
| T03 Pre-scale-up identity (≤2025) | 186 countries | **PASS** (max \|Δdead\| 1.5×10⁻¹¹; all eff = 1) |
| T04 Nesting: bp_combined = both subgroups once | 186 countries | **PASS** (rate-scale additivity 5.6×10⁻¹⁶; deaths 20.697M vs 20.657M sum, +0.19%) |
| T05 Population not duplicated across causes | 186 countries | Model **PASS** (pop identical across causes); **report FAIL** (Σ over causes = 3.92–3.98 × pop) |
| T06 ASMR denominator and weights | global | **FAIL** (×4.0); weights sum to 1 |
| T07 Horizon = 25 years 2026–2050 | outputs + report | Outputs **PASS** (2025 averted = 0); report **FAIL** (labels, BAU denominator) |
| T08 Combined algebra | 3 countries | BP subgroups additive; BP × statins multiplicative. Corrected combined / sum = 98.3% (COL), 99.3% (CHN), 99.3% (NGA): **sub-additive** |
| T09 Reproducibility | 3 countries × 6 scenarios | **PASS** (identical repeats; canonical = saved optimized-script outputs, max diff 0) |
| T10 Parameter provenance | all effect values | **FAIL** (see Evidence_Map) |
| T11 PSA freeze completeness | 86 rows | Completeness **PASS**; 0 rows includable → gate **not passed** |
| T12 UI coverage | 16 families | **PASS** (all classified) |
| T13 Sodium path inert at salteff = 0 | 3 countries | **FAIL** (100% NA from 2026) |
| T14 Combined scenario keeps BP incidence effects | 186 countries | **FAIL** (erased everywhere; TFA active in 126 countries) |
| T15 Structural variants | 3 countries | BP CF share 40–43%; statin CF share 82–86%; stratum options −3.5% to +3.6%; statin RR 0.80: −1.9% to −3.2% |
| T16 Existing `tests/test_aim2_bp_control.R` | 43 checks | **PASS** |
| T17 Artefacts vs outputs | `paper_scalars.rds` | **PASS** (34,251,262; 6,965,353; 13,691,704; 21,215,663 match), but they inherit D1/D2 |
| T18 Non-negative states | 186 countries | **FAIL (minor)**, 42 rows |
| T19 Tracking columns = applied ratios | 186 countries | **FAIL (minor)** |
| T20 BAU invariance under structural variants | 3 countries | **PASS** (cell-level identical) |

The full multi-country model was **not** re-run, and no PSA was run.

## 17. Required regression tests

To be added in `tests/test_gate1_deterministic.R` (C16):

1. **R01 identity:** for each cause, $\max|\sum_b p_bIR_b/IR-1|<10^{-12}$. Also assert that hstroke `RRi` equals the GBD ICH RR in both `alpha` and `IR_bin`.
2. **R02 zero effect:**
   - BP targets = baselines → `eff_ir` = `eff_cf` = 1.
   - Statin target = baseline → `eff` = 1.
   - Any effect RR = 1 → `eff` = 1.
3. **R03 pre-2026:** every scenario equals BAU for t ≤ 2025 (states and eff).
4. **R04 nesting:** $(\varepsilon_{comb}-1)=(\varepsilon_N-1)+(\varepsilon_D-1)$ on IR and CF; the target table has exactly both subgroups per key.
5. **R05 combined scenario:** $\varepsilon_{IR}^{BP+st}=\varepsilon_{IR}^{BP}\varepsilon_{IR}^{st}$ row-wise; no `NA` in `eff_*` (the code must stop on `NA`); no sodium or TFA function is called.
6. **R06 population once:** the ASMR denominator equals Σ pop of one chain; Σ over the four cause-specific ASMRs equals the all-CVD ASMR; Σw = 1; the constant-rate test passes.
7. **R07 horizon:** reported years = 2026:2050 (25); BAU denominators use the same years; labels come from one constant.
8. **R08 reproducibility:** two runs are identical; the canonical and optimized scripts are identical.
9. **R09 provenance:** every numeric effect constant in 06 appears in the register with an `evidence_status` other than blank.
10. **R10 non-negativity:** all states ≥ 0, including initial states.
11. **R11 PSA hooks (future):** a median draw reproduces the deterministic run, and BAU is unchanged across draws.

## 18. Prioritized blocking actions

1. **(P1, code)** Replace `all_interventions` with `bp_statins_combined` (BP both + statins). Make `NA` effects fatal. Remove sodium and TFA from every reported scenario. Re-run and re-knit. (C01, C02)
2. **(P1, science)** Decide the BP evidence source and the stratum interpretation (Q01–Q03). Add the BPLTTC article and appendix to `library/` and `references.bib`, and verify the Appendix S6 values. (C03, C17)
3. **(P1, science)** Decide the BP and statin CF pathways (Q04, Q06). Recommended: incidence-only primary. (C04)
4. **(P1, science)** Correct the statin parameters: the 0.74 point estimate, the istroke RR source, the AF estimand, LDL scaling and adherence (Q05–Q08). (C05)
5. **(P1, code)** Implement the canonical ASMR and regenerate the ASMR figures and slides. (C06)
6. **(P2, code)** Harmonize the horizon to 2026–2050 in reports, including BAU denominators and labels. (C07)
7. **(P1, docs)** Revise the manuscript, math doc and slides:
   - BPLTTC citation and split-mixture methods.
   - `int_order` label.
   - Report `bp_combined`.
   - Combined-scenario definition and label.
   - Remove the "greater than sum" claims.
   - Adherence wording.
   (C08)
8. **(P2, repro)** Commit the optimized script (or revert `00`), and add the regression tests. (C09, C16)
9. **(P2)** Port the fixes to Aim 2 or retire it. Resolve the script 04 statin-table external path. (C10, C15)

## 19. Proposed freeze criteria

The deterministic model is frozen when **all** of the following hold:

1. D1–D7 are fixed and R01–R10 pass on a full 186-country run.
2. The investigators have signed off on Q01–Q07 and Q11–Q13 in `Open_Questions`, with decisions recorded in `Gate1_Decisions`.
3. Every effect constant has an `Evidence_Map` record with a verified source file in `library/` and a bib entry.
4. The frozen scenario set of Section 8 is implemented, and the labels are identical across 07, the report, slides and manuscript.
5. The horizon and ASMR definitions of Section 9 are implemented through single shared functions or constants.
6. The production script is committed, and a tagged commit reproduces the reported artefacts exactly (T09, T17).

The PSA Parameter Freeze Gate passes when P-I1 and P-I2 (and P-I3 if retained) move to `INCLUDE_IN_INITIAL_PSA` / `READY` with `nine_criteria_met = Yes`, and the correlation assumptions (Q17) and the initial scope (Q16) are approved.

## 20. Questions requiring investigator or coauthor decisions

See sheet `Open_Questions` (Q01–Q20). The blocking questions are:

- **Q01** Primary BP evidence source for both aims (BPLTTC 2021 vs Ettehad 2016).
- **Q02** How to use the BPLTTC baseline-SBP strata: pooled log-linear, own-stratum, or current cumulative.
- **Q03** Achieved SBP after control, in both subgroups.
- **Q04** Whether BP control has CF effects, and which evidence and which patients.
- **Q05** Statin IHD incidence RR (point estimate, outcome, CI level) and LDL-C scaling.
- **Q06** Sources for the statin istroke and CF RRs, and whether to retain the statin CF pathway.
- **Q07** The statin AF estimand.
- **Q11** Frozen scenario set and labels, including reporting `bp_combined`.
- **Q12** Horizon 2026–2050 with matching BAU denominators.
- **Q13** ASMR standard population and denominator.
- **Q16–Q17** Initial PSA scope and correlation assumptions.

The non-blocking questions cover:

- **Q08** Adherence.
- **Q09** The derivation of ψ.
- **Q10** P(D|H) and the 15-pp control gap.
- **Q14** The LE source and DALY discounting.
- **Q15** The 80% exogenous CF trend.
- **Q18** The allocation mode.
- **Q19** The statin target denominator.
- **Q20** The future of Aim 2.

## 21. Recommended next step for PSA implementation (after both gates pass)

1. Refactor `project.all()` so every sampled effect is an explicit argument, keeping the defaults equal to the frozen deterministic values:
   - `etihad_rr_table`
   - statin RRs
   - CF parameters, if retained
2. Write a draw generator from the frozen `PSA_Freeze_Gate` rows, with the seed recorded, the shared-draw rules above, and one global draw per iteration. It produces a draw table.
3. Compute BAU once per country. Run the intervention scenarios per draw.
4. Store country × scenario × year × cause × age-group totals per draw, rather than the full state arrays.
5. Aggregate within each draw before taking percentiles.
6. Validate:
   - The median draw is close to the deterministic run.
   - An RR = 1 draw is neutral.
   - BAU is identical across draws.
   - The correlation sensitivity analyses run.
7. Pilot on the three test countries before the full run.
8. Report partial parameter UIs, using the Section 15 language, alongside the DSA and structural-scenario tables.
