![](esg.jpg)

# Disaggregating ESG: Heterogeneous Determinants of International Tourism Demand in Europe, 2000-2023

This repository hosts the final version of the working paper *Disaggregating ESG: Heterogeneous Determinants of International Tourism Demand in Europe, 2000-2023*. The paper studies how environmental, social, and governance factors are associated with international tourist arrivals in a panel of 27 European countries observed annually from 2000 to 2023. It pushes back on the use of composite ESG scores and instead estimates a theory-driven sparse specification, complemented by a long-run cointegration analysis, a Common Correlated Effects estimator, multiple-imputation uncertainty propagation, and a cross-method comparison with elastic net, random forest, and PCA.

## Read or download the paper

- [HTML (final version, recommended)](https://raw.githack.com/lusiki/ESG-turism/main/paper_final.html)
- [PDF download](paper_final.pdf)
- [Word (.docx) download](paper_final.docx)

## Research questions

1. Which ESG factors are robustly associated with international arrivals once country and year fixed effects absorb time-invariant country traits and common annual shocks?
2. Are these associations heterogeneous between transition (lower-income) and frontier (higher-income) European economies?
3. How sensitive are the conclusions to specification, identification framework (within-country deviations versus long-run cointegrating relationships), and the treatment of cross-sectional dependence?

## Contribution

- **Substantive.** Governance reform (corruption control) co-moves with international arrivals along persistent country reform trajectories; the within-country deviation channel does not survive country-specific time trends. Air quality, internet penetration, and unemployment carry within-country associations of the expected sign. Residuals show substitution-style cross-sectional dependence after year fixed effects, consistent with destination competition within Europe.
- **Methodological.** The within-country versus long-run identification distinction matters quantitatively for ESG-tourism analysis. Pooled cross-sectional learners (PCA, elastic net, random forest) and within-country causal estimators answer different questions and accordingly highlight different ESG dimensions. Within-pillar redundancy among WGI governance indicators suppresses the corruption signal in kitchen-sink specifications and motivates the theory-sparse approach.

## Methods

The empirical strategy combines several complementary estimators:

- **Two-way fixed effects (TWFE)** with country and year fixed effects, cluster-robust standard errors, and wild cluster bootstrap inference (Webb 6-point, B = 9999) for few-cluster reliability
- **Driscoll-Kraay** standard errors as the cross-sectional-dependence-robust sensitivity
- **Country-specific linear time trends** to separate long-run reform trajectories from within-country deviations
- **Pooled Mean Group / cointegration analysis** for the long-run governance-tourism relationship
- **Common Correlated Effects (CCE)** estimator for cross-sectional dependence
- **Multiple imputation (mice)** to propagate imputation uncertainty into inference
- **Heterogeneity analysis** comparing transition and frontier European economies, with within-country variance diagnostics to separate saturation from low identifying variation
- **Cross-method comparison** with PCA pillar composites, elastic net, and random forest, treated as cross-sectional importance benchmarks rather than competing causal estimators

## Theory-sparse core specification

| R variable        | Definition                                              | Source |
|-------------------|---------------------------------------------------------|--------|
| `gov_corruption`  | Control of corruption (WGI estimate)                    | Worldwide Governance Indicators |
| `env_pm25_exp`    | Population-weighted PM2.5 exposure (μg/m³)              | World Bank WDI |
| `soc_internet_users` | Internet users (% of population)                      | World Bank WDI |
| `soc_unempl_ilo`  | Unemployment, total (% labour force, ILO modelled)      | ILO |
| `gov_gdp_capita`  | GDP per capita (USD, control variable)                   | World Bank WDI |

## Headline findings

- **Governance (corruption control).** Positive co-movement with international arrivals identified off persistent country reform trajectories. The signal does not survive the inclusion of country-specific linear time trends and is therefore read as a slow-moving institutional-trajectory result, not a within-country structural deviation result.
- **Air quality (inverse PM2.5).** Negative within-country association with arrivals; bidirectional Granger evidence implies a simultaneous-feedback structure, so the coefficient is interpreted as co-movement rather than a structural elasticity.
- **Digital infrastructure (internet penetration).** Positive within-country association with arrivals and clean unidirectional Granger precedence; this carries the strongest temporal grounding among the four headline regressors.
- **Labour market (inverse unemployment).** Negative within-country conditional association; Granger fails in both directions, so no temporal ordering is imposed.
- **Heterogeneity.** Associations concentrate in transition economies. The frontier-economy saturation reading is qualified: where the within-country standard-deviation ratio (low/high) exceeds 1.5, saturation is observationally indistinguishable from reduced identifying variation.

## Data

- 27 European countries, 24 years (2000–2023), 648 country-year observations
- 74 candidate ESG indicators across environmental (8), social (45), and governance (21) pillars, drawn from World Bank WDI, Eurostat, OECD, and the Worldwide Governance Indicators
- Four outcome variables: log arrivals (primary), log overnight stays, log arrivals per capita, log overnight stays per square kilometre
- Cleaning pipeline imputes via a four-pass cascade (linear interpolation, edge fill, year-median, global-median fallback), with imputation provenance tracked per cell and a strict-subsample sensitivity reported

## Repository contents

| Path | Description |
|------|-------------|
| `paper_final.qmd` | Source for the final paper (Quarto / R) |
| `paper_final.html` | Rendered HTML (linked above) |
| `paper_final.pdf` | Rendered PDF |
| `paper_final.docx` | Rendered Word document |
| `dta_.xlsx`, `dta_clean.csv`, `dta_clean.rds` | Raw and cleaned data |
| `analysis.R`, `esg_multimethod.R`, `prepare data.R` | Standalone analysis and data preparation scripts |
| `output/` | Persisted tables and model objects produced during render |
| `variables.txt` | Variable code dictionary |

## Limitations

- Slow-moving regressors limit within-country variation in frontier economies
- Imputation contamination from the year-median pass is bounded, not eliminated, by the strict-subsample rerun
- Sample restricted to 27 European countries; external validity beyond Europe is not claimed
- Reverse causality cannot be fully ruled out for environmental and labour market regressors

## Citation

```
[LST] (2025). Disaggregating ESG: Heterogeneous Determinants of
International Tourism Demand in Europe, 2000-2023. Working Paper.
```
