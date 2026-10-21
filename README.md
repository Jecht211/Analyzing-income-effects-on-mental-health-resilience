# Analyzing Income Effects on Mental Health Resilience
### Multivariate Statistical Analysis of Population Health Surveillance Data (BRFSS)

---

## Executive Summary
This project investigates the relationship between socioeconomic stratification—specifically household income brackets—and population-level mental health resilience. Utilizing a massive national cohort of **~335,000 clean records** from the CDC’s **Behavioral Risk Factor Surveillance System (BRFSS)**, this analysis deploys robust multivariate regression pipelines in **R** to isolate pure income effects from confounding variables including age, education, employment status, and baseline somatic conditions.

The final models establish that while income acts as a powerful structural buffer against psychological distress, its protective qualities operate non-linearly, encountering clear plateaus and inversion points when facing chronic physical degradation.

---

## Data Infrastructure & Variable Mapping
The data engineering pipeline ingests, cleans, and standardizes heterogeneous public health survey records into high-integrity data structures. Alphanumeric features were mapped onto clean, standardized naming conventions:

*   **`MENTHLTH` → `mh_days`:** Mapped `88` (None) to `0`. Programmatically removed uninformative inputs (`77`, `99`), maintaining a **98.2% data completeness rate**.
*   **`PHYSHLTH` → `ph_days`:** Mapped `88` (None) to `0`. Filtered missing vectors to preserve a **97.9% data completeness rate**.
*   **`EXERANY2` → `exercise`:** Converted raw indicators into a clean binary factor structure (`Yes` / `No`).
*   **`INCOME3` → `income`:** Consolidated 11 highly fragmented raw brackets down to 9 distinct economic tiers, ranging from Tier 1 (<\$15k) to Tier 9 (\$200k+).

> **Data Governance Note:** Dropping missing income data generated an initial 22% missingness threshold. To mitigate selection bias, a **comparative correlation matrix analysis** was executed evaluating the dataset with missing rows *included* versus *excluded*. Because cross-variable coefficients remained statistically identical, complete-case filtration was mathematically validated, preserving a robust baseline of **~335,000 high-integrity rows**.

---

## Core Visual Exploration Findings (Figures 1–8 Takeaways)

*   **Cognitive Anchoring & Rounding Bias:** Analyzing non-zero data tracks reveals a severe rounding bias. Instead of precise days, respondents consistently round experiences to weekly anchors or clean 5-day increments in this exact descending density order: **30 → 5 → 10 → 15 → 20 → 7 → 25 days**.
*   **The Behavioral Exercise Buffer:** Inactive individuals experience an average of **50% more poor mental health days** per month compared to active peers, establishing personal exercise as a massive protective asset.
*   **Somatic-Psychological Interdependence:** Poor mental health days track tightly with physical illness across the 0-to-29-day spectrum. However, artificial spikes occur at 14 and 21 days due to **weekly rounding bias**.
*   **The 30-Day Disability Adaptation Paradox:** At exactly 30 days of continuous physical illness, the mean mental distress metric **drops significantly**, breaking the linear trend. This provides strong empirical confirmation of a longitudinal coping mechanism: individuals managing permanent, long-term disabilities show significant psychological adaptation, decoupling chronic physical limitations from acute mental health degradation.

---

## Model Evolution & Statistical Optimization

To isolate the predictive dynamics of socioeconomic shielding, the project iterated through three major modeling milestones:

1.  **Model 1 (Baseline OLS Linear Regression):** Achieved an **Adjusted R² of 0.1042** (p < 0.001), confirming that income, somatic health, and exercise exert powerful, independent baseline pressures on psychological wellness.
2.  **Model 4 (Socioeconomic Interaction OLS):** Elevating explanatory power to an **Adjusted R² of 0.1081**. Uncovered a crucial non-linear dynamic: moving up wealth brackets shields individuals from somatic distress, but this protective coefficient completely **plateaus at the highest wealth tiers**, proving that financial liquidity cannot entirely override chronic biological degradation.
3.  **Model 9 (Parsimonious Binary Logistic Regression - Final GLM Engine):** Model 9 optimized data fit with an **AIC of 352,134**. Modeling a chronic distress threshold (≥ 5 days/month) exposed two critical inflection points:
    *   **The \$100k Shield Paradox:** For healthy cohorts, clearing the \$200k+ tier cuts the odds of distress by **52%**. However, when interacting with active physical sickness, the **under-\$100k bracket shields mental distress best**. Past \$100k, distress metrics flex upward slightly due to high-stakes corporate deadlines and upper-stratification anxieties.
    *   **The Exercise Strain Inversion:** Physical exercise ceases to protect against mental distress after **five days of physical illness**. Past this 5-day threshold, forcing physical exertion on a compromised biological system correlates with an *increase* in distress odds due to severe physiological strain.

---

## Persona Simulations & Project Limitations

### Programmatic Probability Predictions (Model 9 Logit Outputs)
*   **Persona 1 (Baseline Low-Income):** *Low Income (<\$25K), Perfect Physical Health, Active Exercise.* Establishes our baseline population control, exhibiting a **28% baseline probability** of crossing into chronic mental distress.
*   **Persona 2 (Compounding At-Risk):** *Middle Income (<\$100K), Moderately Ill (18 poor physical days), No Exercise.* Stripping behavioral habits while compounding somatic illness drives up the probability of chronic distress aggressively.
*   **Persona 3 (High-Income Resilience):** *High Income (\$200K+), Severely Ill (26 poor physical days), Active Exercise.* Visualizes the socioeconomic cushioning effect; material resources combined with active exercise help insulate the individual, maintaining a lower distress probability than an unbuffered peer.

### Project Boundaries & Constraints
*   **The 10% Variance Explanatory Ceiling:** Model features collectively account for **~10.8% of total variance**. The remaining ~89% is driven entirely by unmeasured external parameters (genetic baselines, acute trauma) that a high-level surveillance survey cannot isolate.
*   **The Aggregation Fallacy:** The model is highly reliable for forecasting broad public health shifts across broad demographic segments. However, due to high structural variance and subjective volatility in self-reported data, applying this model to predict a single individual’s mental health horizon will yield a **high margin of error**.
