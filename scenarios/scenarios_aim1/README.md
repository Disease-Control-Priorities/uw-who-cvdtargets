
# 🌍 UW - WHO CVD Targets 2030

## Modelling the Burden of Disease Impact of Cardiovascular Risk-Factor Control

------------------------------------------------------------------------

## 📌 Overview

This repository contains the modelling framework, data pipelines, and
analytical code used to evaluate the **population health and
burden-of-disease impact of achieving the World Health Organization
(WHO) cardiovascular disease (CVD) risk-factor control targets by
2030**, as endorsed in the 2025 global political declaration on
noncommunicable diseases (NCDs).

The project evaluates progress toward four priority WHO targets:

-   **150 million more people with hypertension under control by 2030**
-   **≥50% of eligible individuals receiving lipid-lowering therapy and
    counselling**
-   **80% blood pressure control among people with diagnosed diabetes**
-   **80% availability of validated automated blood pressure devices in
    public and private facilities**

The repository supports two companion scientific outputs:

1.  **Projected Health Outcomes of Achieving WHO Targets for
    Cardiovascular Risk-Factor Control: A Modelling Study**
2.  **How Achieving ≥50% National Blood Pressure Control Contributes to
    the WHO 150-Million Target by 2030**

------------------------------------------------------------------------

## 🎯 Scientific Motivation

Cardiovascular disease remains the leading cause of death globally.
Although cost-effective prevention and treatment strategies exist, major
gaps persist in:

-   Hypertension diagnosis and control\
-   Lipid-lowering therapy coverage\
-   Integrated diabetes and cardiovascular care\
-   Health-system readiness (e.g., validated BP devices)

The 2025 WHO political declaration reaffirmed ambitious but measurable
targets. However, the **burden-of-disease implications of achieving
these targets---individually and jointly---remain insufficiently
quantified**.

This project addresses:

-   How many heart attacks and strokes could be prevented by 2030?
-   How many CVD deaths could be averted?
-   How many life-years could be gained?
-   How would disability-adjusted life years (DALYs) change under full
    target achievement?
-   How does achieving ≥50% national hypertension control translate into
    progress toward the global 150-million target?

------------------------------------------------------------------------

## 🧠 Modelling Framework

The repository implements a population-based modelling framework
integrating:

-   Demographic projections\
-   Risk-factor prevalence and control rates\
-   Treatment coverage expansion scenarios\
-   Clinical relative risk reductions\
-   Health-system capacity constraints\
-   Cause-specific incidence and mortality modelling\
-   Burden-of-disease metrics (YLLs, YLDs, DALYs)

### Key Features

-   Scenario-based scale-up modelling (baseline vs.WHO target
    attainment)
-   Care cascade modelling (diagnosis → treatment → control)
-   Joint modelling of overlapping targets (hypertension, lipids,
    diabetes)
-   Device availability as a scale-up constraint
-   Estimation of:
    -   CVD events averted\
    -   CVD deaths averted\
    -   Life-years gained\
    -   Years of Life Lost (YLLs) averted\
    -   Years Lived with Disability (YLDs) averted\
    -   Disability-Adjusted Life Years (DALYs) averted

------------------------------------------------------------------------

## 🗂 Repository Structure

/data Raw and processed epidemiological and demographic inputs

/code Risk-factor projections Target scale-up algorithms Event and
mortality modelling Burden-of-disease calculations

/scenarios WHO target scenarios Sensitivity analyses Country-level
projections

/outputs Tables and figures for manuscript submission

/docs Methods documentation Assumptions and parameter sources

------------------------------------------------------------------------

## 🔬 Analytical Strategy

### 1️⃣ Baseline Estimation

-   Estimate prevalence of hypertension, diabetes, and hyperlipidemia\
-   Estimate current treatment and control coverage\
-   Establish baseline CVD incidence and mortality\
-   Compute baseline YLLs, YLDs, and DALYs

### 2️⃣ Target Scenario Construction

-   Scale coverage to WHO target thresholds by 2030\
-   Incorporate health-system readiness constraints\
-   Model improvements along the care cascade

### 3️⃣ Health and Burden Impact Estimation

-   Apply relative risk reductions to CVD incidence\
-   Estimate avoided myocardial infarctions and strokes\
-   Estimate avoided CVD deaths\
-   Aggregate life-years gained\
-   Calculate reductions in YLLs, YLDs, and DALYs

------------------------------------------------------------------------

## 📊 Outputs

The repository generates:

-   Country-level and regional projections
-   Contribution of each country toward the 150-million global target
-   Decomposition of impact by target component
-   Estimates of events, deaths, life-years, and DALYs averted
-   Publication-ready tables and figures

------------------------------------------------------------------------

## 🌎 Policy Relevance

This repository provides a transparent and reproducible framework to:

-   Quantify progress toward WHO NCD commitments
-   Assess reductions in cardiovascular burden of disease
-   Support accountability monitoring toward 2030
-   Identify the most impactful points of intervention in the CVD care
    cascade

------------------------------------------------------------------------

## 🔁 Reproducibility

All analyses are fully reproducible using:

-   **R** (primary modelling environment)
-   Version-controlled scenario definitions
-   Documented parameter sources
-   Structured output generation for manuscript submission

------------------------------------------------------------------------

## 📜 License

Specify license here (e.g., MIT, GPL-3.0, etc.)

------------------------------------------------------------------------

## 📚 Citation

If using this framework or code, please cite the associated
manuscript(s) once published.
