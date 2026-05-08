# Project: Disaggregating ESG and International Tourism Demand

An academic paper examining how disaggregated ESG (Environmental, Social, Governance) factors are associated with international tourist arrivals across 27 European countries from 2000 to 2023. Core contribution: **against composite ESG scores**, the paper uses a theory-driven sparse specification to recover stable within-country signals that kitchen-sink models suppress through multicollinearity.

Working title: *"Disaggregating ESG: Heterogeneous Determinants of International Tourism Demand in Europe, 2000-2023"*
Target journals: *Tourism Economics* (primary), *Journal of Sustainable Tourism* (alternative). Word limit ~9,000.

**Current version: `paper_v3.qmd`** (referee response to v2 evaluation). See *v2→v3 changes* section below.

---

## Research Questions

1. Which ESG factors are robustly associated with international arrivals after absorbing country and year fixed effects?
2. Are these effects heterogeneous between transition (lower-income) and frontier (higher-income) European economies?
3. How sensitive are conclusions to specification choices, and what does that imply for the ESG-tourism literature?

---

## Five Hypotheses

- **H1** – Corruption control (`gov_corruption`) → positive within-country conditional association with arrivals
- **H2** – Air quality / inverse PM2.5 (`env_pm25_exp`) → positive within-country conditional association
- **H3** – Internet penetration (`soc_internet_users`) → positive within-country conditional association (has Granger precedence → can carry stronger language)
- **H4** – Inverse unemployment (`soc_unempl_ilo`) → positive within-country conditional association
- **H5** – Associations in H1–H4 are larger in transition economies; internet may show saturation in frontier economies (but differential significance may also reflect lower identifying variation — see Table 22)

---

## Three Theoretical Channels

1. **Reputation** — ESG failures/improvements shape destination perception in source markets
2. **Supply quality** — better governance/environment/digital infrastructure raises the tourism product
3. **Institutional friction** — stable governance and digital infrastructure lower travel transaction costs

---

## Main Empirical Model

Two-way fixed effects (TWFE) panel estimated with `plm` in R:

```
log(arrivals_it) = α_i + λ_t + β1·corruption_it + β2·PM2.5_it + β3·internet_it + β4·unemployment_it + β5·GDP_per_capita_it + ε_it
```

- **α_i** country FE: absorb geography, culture, climate, persistent destination image
- **λ_t** year FE: absorb COVID, global cycle, exchange rates, common annual shocks
- Standard errors clustered at country level (wild cluster bootstrap for few-cluster inference, B=9999); Driscoll-Kraay as sensitivity
- Fixed effects preferred over random effects on **substantive grounds** (time-invariant country traits correlated with regressors); Mundlak test p=0.110 does NOT reject RE — this is correctly noted in v3 (was misreported in v2)
- `feols` (fixest) used for country-specific time trends and interaction robustness; `plm` for main tables

---

## Theory-Sparse Specification — The Five Core Regressors

| R variable | Original code | Definition | Source |
|---|---|---|---|
| `gov_corruption` | G4 | Control of corruption (WGI estimate, std normal units) | Worldwide Governance Indicators |
| `env_pm25_exp` | E7 | Population-weighted PM2.5 exposure (μg/m³) | World Bank WDI |
| `soc_internet_users` | S10 | Internet users (% of population) | World Bank WDI |
| `soc_unempl_ilo` | S42 | Unemployment, total (% labour force, ILO modelled) | ILO |
| `gov_gdp_capita` | G8 | GDP per capita (current USD ÷ 10,000) — control only | World Bank WDI |

---

## Why Theory-Sparse Over Kitchen-Sink

`gov_rule_law` (G21) and `gov_corruption` (G4) share ~62% of within-country variance. In the full 15-regressor governance spec, `gov_corruption` flips sign and loses significance the moment `gov_rule_law` enters. The sparse spec recovers a positive, significant corruption coefficient. This is the paper's central methodological demonstration — **Table 11** (sequential governance build-up).

---

## Outcome Variables (four, estimated in parallel for robustness)

| R variable | Definition |
|---|---|
| `log_arrivals` | log(tour_arrivals) — **primary** (no +1: zero observations confirmed absent) |
| `log_nights` | log(tour_nights) |
| `log_arrival_pop` | log(tour_arrival_pop) — arrivals per capita |
| `log_night_area` | log(tour_night_density) — overnight stays per km² |

Note: v1 had a mislabelling of the per-capita and per-area outcomes; v2 corrected it; v3 retains v2 definitions. The `+1` offset was dropped in v3 — no zero-arrivals observations exist in the panel.

---

## Income Heterogeneity Split

Countries split at **median mean-GDP-per-capita** over 2000–2023 (≈ USD 24,075). Borderline countries (Czech Republic, Slovenia, Slovakia, Greece, Portugal) are subject to sensitivity analysis (Table 21: one-at-a-time reclassification). Alternative baselines using 2000 and 2023 GDP per capita are reported in Table 23.

**Interpretation caveat (v3):** Differential significance in Table 6 between income bins may reflect lower within-country variation in the high-income subsample rather than structural saturation. Table 22 quantifies this via within-country SD by income bin; where the low-to-high ratio exceeds 1.5, saturation cannot be distinguished from reduced identifying variation.

---

## Full Variable Universe (74 ESG indicators)

Naming convention: `env_*` (8 vars), `soc_*` (45 vars), `gov_*` (21 vars), plus `tour_*` outcomes.

**Environmental (env_):** co2_gdp, co2_capita, nd_gain_idx, pm25_exp, renew_elec, renew_cons *(E2 and E5 dropped)*

**Social (soc_):** age_dependency, edu_compul_yrs, female_mgrs, broadband, gini_index, immunizations (dpt/hepb/measles), mort_infant/maternal/general/road/poison/u5/neonatal, internet_users, life_expect, edu_low_sec, net_migration, open_defec, water/sanit indicators, poverty, edu_duration, women_parl, health_exp, gpi indicators, youth_idle, suicide_rate, unempl variants (gen/ilo/nat/y_ilo/y_nat)

**Governance (gov_):** cpi_aop, cpi_eop, inflation, corruption, account_bal, export_price, gdp_capita, gdp_const, gdp_growth, gdp_ppp variants, effectiveness, pol_stability, real_gdp_gr, reg_quality, rd_expend, researchers, rule_law, terms_trade, voice_acc, reer

---

## Data Cleaning Pipeline (7 steps)

1. Rename with prefix convention
2. Verify country, year, observation counts
3. Drop variables >60% missing or zero-variance
4. Impute via 4-pass cascade: (a) linear interpolation within-country ≤3yr gaps, (b) edge fill ≤2yr, (c) year-median across countries, (d) global-median fallback
5. Winsorize ESG vars at 1st/99th percentile (outcomes and IDs untouched)
6. Build four outcome variables using plain `log()` (no +1 offset)
7. Validate + persist `dta_clean`; `stopifnot(min(tour_arrivals) > 0)` guard added

**After step 6:** `gov_gdp_capita` is scaled to USD 10,000 units for regression interpretability (income classification is computed from unscaled values first, then scaling applied before `pdata` creation).

Imputation provenance tracked per cell: original / linear_interp / edge_fill / year_median / global_median.
**Key concern**: year-median imputation imports cross-country variance into within-country gaps — addressed by strict-subsample rerun (Table 16). Note: strict subsample drops only ~27 observations (global-median cells on `env_pm25_exp`); does not strip linear interpolation or edge fill.

---

## Robustness Battery (v3)

### Core tables (v2 and v3)
- **Table 3** — main TWFE estimates + wild cluster bootstrap p-values (Rademacher, B=9999) ← *bootstrap added in v3*
- **Table 4** — four outcome variables
- **Table 5** — COVID period sensitivity: full / excl-2020 / excl-2021 / excl-2022 / excl-2020-22 ← *single-year exclusions added in v3*
- **Table 6** — stratified by income bin (excl. COVID); saturation/low-power caveat added in v3
- **Table 7** — formal interaction spec + joint Wald test for all 4 interaction terms ← *joint test added in v3*
- **Table 8** — PCA pillar composites (gov_pc1 not significant — explained by pillar heterogeneity, not contradiction)
- **Table 9** — elastic net (α=0.5, λ.1se, 10-fold CV), cross-sectional importance only
- **Table 10** — random forest importance; CV R² ≈ TWFE within R² noted explicitly in v3
- **Table 11** — sequential governance build-up (the corruption flip story)
- **Table 12** — within-country pairwise correlations among governance vars
- **Table 13** — theory-sparse vs. kitchen-sink side-by-side
- **Table 14** — imputation provenance rates per headline regressor
- **Table 15** — within-country distinct-value counts (flags thin identification)
- **Table 16** — strict subsample (~27 obs dropped, bounds global-median on env_pm25_exp only)
- **Table 17** — unwinsorized sensitivity

### New tables added in v3 (referee response)
- **Table 18** — country-specific linear time trends (`feols`, `Country[year_num]`)
- **Table 19** — quadratic internet saturation test; implied turning point reported
- **Table 20** — leave-one-country-out coefficient stability (27 leave-outs per variable)
- **Table 21** — borderline country reclassification sensitivity (5 countries, one at a time)
- **Table 22** — within-country SD by income bin; ratio flags saturation vs. low-power ambiguity
- **Table 23** — income classification robustness: mean-2000-2023 / year-2000 / year-2023 baselines
- **Table 24** — lagged regressors (L0, L1, L2) sensitivity

### Appendices
- **Table A1** — country bin assignment (GDP per capita in original USD for display)
- **Table A2** — Granger causality tests; results differentiate causal language by regressor in v3
- **Table A3** — panel unit root tests (IPS, LLC); now populated in v3 using matrix input to `purtest`
- **Appendix D** — Mundlak test; interpretation corrected in v3 (p=0.110 ≠ favours FE)

---

## Key Empirical Findings (headline)

- **gov_corruption**: positive, significant within-country conditional association; suppressed when gov_rule_law enters kitchen-sink spec; **no Granger precedence** — framed as co-movement, not causal driver
- **env_pm25_exp**: negative association (higher PM2.5 = fewer tourists), robust; **bidirectional Granger** — coefficient mixes structural and reverse channels; interpret with caution
- **soc_internet_users**: positive, robust; **clean unidirectional Granger** — strongest basis for conditional-causal language; saturation pattern in frontier economies possibly confounded with lower variation
- **soc_unempl_ilo**: negative association; **no Granger precedence** — framed as co-movement
- All four associations concentrate in lower-income transition economies

---

## Identification Assumptions and Known Concerns

- Conditional exogeneity of within-country ESG variation after absorbing FEs — **not fully defensible** without instruments; paper uses associational language for 3 of 4 regressors
- Granger evidence (Table A2) differentiates regressors: internet has temporal precedence; corruption and unemployment do not; PM2.5 has bidirectional feedback
- Slow-moving regressors limit within-country variation in frontier economies (flagged in distinct-value diagnostics, Table 15 and Table 22)
- Reverse causality partially bounded by lagged regressors (Table 24) and country-specific trends (Table 18)
- Imputation contamination from year-median pass — bounded (not eliminated) by strict subsample (Table 16)
- Cross-sectional dependence — Pesaran CD test reported in Section 5.3; Driscoll-Kraay SE reported as sensitivity
- Few-cluster inference — wild cluster bootstrap (Table 3) addresses 27-cluster asymptotic concern
- Sample restricted to Europe — limits external validity

---

## File Structure

| File | Purpose |
|---|---|
| `paper_v3.qmd` | **Current main paper** — referee response version, all v3 changes applied |
| `paper_v2.qmd` | Prior version (v2); do not edit |
| `paper_v1.qmd` | Earlier version; do not edit |
| `dta_.xlsx` | Raw data (country × year, 74 ESG + tourism vars) |
| `dta_clean.csv` / `dta_clean.rds` | Cleaned data artefacts (from prior runs; v3 regenerates at compile time) |
| `esg_multimethod.R` | Standalone multimethod analysis script |
| `analysis.R` | Standalone analysis script |
| `prepare data.R` | Data preparation script |
| `variables.txt` | Stata-style variable labels (original codes G1–G23, S1–S45, E3–E9) |
| `present results*.qmd/html` | Earlier presentation versions |
| `output/` | Persisted artefacts: CSV tables + RDS model objects |
| `ESG_Tourism_Results.RData` | Saved R workspace |

---

## R Packages

`tidyverse`, `readxl`, `plm`, `lmtest`, `sandwich`, `fixest`, `fwildclusterboot`, `randomForest`, `glmnet`, `zoo`, `broom`, `knitr`, `kableExtra`, `car`, `purrr`, `here`

`fwildclusterboot` added in v3 for wild cluster bootstrap inference (27-cluster panel).

---

## What Is Still Outstanding

- Abstract — to be drafted after Section 6 results are finalised
- Literature gap paragraph (Section 1) — co-authors
- Full Literature Review (Section 2, five subsections) — co-authors
- Discussion (Section 7) — co-authors; must differentiate causal language by regressor (see Granger framing in Section 5.2)
- Conclusion (Section 8) — co-authors
- References — co-authors
- Substantive interpretation paragraphs within Section 6 — co-authors
- Verify `paper_v3.qmd` renders without errors (especially: `fwildclusterboot` installation, `feols` country-time-trends, unit root `purtest` matrix input, `boottest` API)

---

## v2 → v3 Changes (Referee Response)

### Priority 1 — Code corrections
| Change | Location | Why |
|---|---|---|
| `log(x+1)` → `log(x)` in `build_outcomes()` | `winsorize-and-derive` chunk | No zero-arrivals obs; +1 was operationally irrelevant and breaks elasticity interpretation |
| `stopifnot(min(tour_arrivals) > 0)` guard added | `winsorize-and-derive` chunk | Explicit validation before taking log |
| `gov_gdp_capita / 10000` scaling | After income classification in `winsorize-and-derive` | Coefficient was reporting as `0.0000`; $10k unit makes it interpretable |
| p-values: `0.0000` → `<0.0001` via `fmt_p()` | `extract_coef_table()` in setup | Standard publication practice |
| Mundlak interpretation corrected | Appendix D prose | p=0.110 does NOT favour FE; test cannot reject RE |
| `fwildclusterboot` added to packages | Setup chunk | Required for wild bootstrap |

### Priority 2 — New analyses
| Table | Analysis | Addresses |
|---|---|---|
| T3 extended | Wild cluster bootstrap p-values (Rademacher, B=9999) | Few-cluster inference (N=27) |
| T5 extended | Single-year COVID exclusions (2020, 2021, 2022) | Binary COVID sensitivity too coarse |
| T7 extended | Joint Wald test for 4 interaction terms | Test whether heterogeneity exists jointly |
| T18 | Country-specific linear time trends (`feols`) | Country-specific trend confounders |
| T19 | Quadratic internet saturation + turning point | Formal saturation test for H5 |
| T20 | Leave-one-country-out stability (27 leave-outs) | Single influential country concern |
| T21 | Borderline country reclassification (5 countries) | Income bin robustness (promised in v2, missing) |
| T22 | Within-country SD by income bin + ratio | Distinguish saturation from low identifying variation |
| T23 | Income classification: 2000 and 2023 baselines | Mean-period classification robustness |
| T24 | Lagged regressors (L1, L2) | Timing and reverse causality |
| A3 populated | Panel unit root tests (IPS, LLC via matrix input) | Was empty in v2 |
| Pesaran CD test | Cross-sectional dependence in Section 5.3 | Justify Driscoll-Kraay as primary/sensitivity |

### Priority 3 — Text/framing
| Change | Location |
|---|---|
| Causal language differentiated by regressor (internet = causal language OK; corruption/unemployment = conditional association; PM2.5 = bidirectional feedback) | Section 5.2 new paragraph; Sections 1, 6, 7, 8 search-replaced |
| Cross-method section reframed: elastic net/RF estimate cross-sectional importance, TWFE estimates within-country gradient — divergence is informative, not contradictory | Section 6.5 |
| PCA gov_pc1 insignificance explained: pillar heterogeneity dilutes PC1; gov_corruption is representative of reform trajectory, not unique channel | Section 6.5 |
| Saturation/low-power ambiguity stated explicitly; Table 22 cross-reference | Section 6.4 and Table 6 prose |
| Strict subsample framing corrected: bounds only global-median on env_pm25_exp (~27 obs) | Section 6.7 |
| Variable reconciliation note added | End of Section 4.4 |
| RF CV R² ≈ TWFE within R² noted | Section 6.5 after Table 10 |
| FE preference on substantive grounds (not Mundlak) | Section 5.3 |

---

## How to Work on This Project

### Before starting any non-trivial task
Plan first. State what you intend to change and why before touching `paper_v3.qmd` or any R script. For tasks touching the data pipeline or model specifications, write out the steps explicitly and confirm before executing.

### Verification means
- R code changes: confirm the chunk runs without error and the output table looks correct
- Paper edits: do not silently alter econometric specifications, variable definitions, or hypothesis numbering — these are load-bearing
- Never change a model specification (regressors, FE structure, SE clustering) without flagging it explicitly

### Elegance for this project means
- Tidy, readable R: prefer `mutate/summarise` pipelines over loops where natural
- One chunk per logical step, named with the label convention already in use (`tbl-*`, `fig-*`)
- Do not add regressors, robustness checks, or new tables beyond what is requested — scope creep in a paper is dangerous
- Prefer editing existing chunks over adding new ones

### Self-improvement
After any correction: update `tasks/lessons.md` with the pattern so the same mistake is not repeated.

### Subagents
Use subagents for literature searches, parallel robustness explorations, or any task that would bloat the main context window with large data outputs.

### The one rule that overrides everything
**Do not alter `dta_clean` or the imputation pipeline without explicit instruction.** Everything downstream depends on it. If something looks wrong in the data, flag it — do not silently fix it.
