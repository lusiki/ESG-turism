



![](esg.jpg)




# ESG Factors and International Tourism



A multi-method panel analysis examining how Environmental, Social, and Governance factors influence international tourist arrivals across 27 countries over 24 years (2000-2023).

## Key Findings

**Governance quality is the strongest ESG driver of tourism**, with effects 10 times larger in developing countries than developed nations.

| Factor | Effect on Tourist Arrivals | p-value |
|--------|---------------------------|---------|
| Corruption control | +0.7% per unit improvement | 0.005 |
| Air pollution (PM2.5) | -2.3% per μg/m³ | 0.039 |
| Internet access | +0.5% per percentage point | 0.005 |
| Unemployment | -1.0% per percentage point | 0.032 |

## Methods

The analysis employs six complementary approaches:

- **Principal Component Analysis** for dimension reduction and ESG index creation
- **Random Forest** for variable importance and prediction (R² = 97.8%)
- **Two-Way Fixed Effects** for causal inference, controlling for country and year effects
- **Elastic Net** for variable selection among 74 candidate predictors
- **Heterogeneity Analysis** comparing effects in developed vs. developing countries
- **Spatial Analysis** testing for regional spillover effects

The Two-Way Fixed Effects model is the primary specification for causal claims.

## Heterogeneity Results

ESG effects differ substantially by development level:

| Variable | Developing Countries | Developed Countries |
|----------|---------------------|---------------------|
| Corruption control | +1.0% (significant) | -0.1% (not significant) |
| Internet users | +0.8% (significant) | -0.3% (significant) |
| PM2.5 exposure | -4.3% (significant) | -1.7% (not significant) |
| Life expectancy | not significant | +7.8% (significant) |

Developing countries benefit most from governance and environmental improvements. Developed countries benefit more from health and quality-of-life indicators.

## Policy Implications

**For developing countries:** Prioritize anti-corruption efforts, air quality improvements, and internet infrastructure expansion.

**For developed countries:** Focus on maintaining institutional quality and improving health and quality-of-life indicators.

## Data

- 27 countries, 24 years, 648 observations
- 74 ESG variables across Environment (8), Social (45), and Governance (21) dimensions
- Dependent variable: log of international tourist arrivals


## Limitations

- Heavy data imputation prevented first-differences estimation
- Natural attractions and marketing variables not included
- Potential reverse causality (tourism may improve ESG outcomes)
- Sample limited to 27 primarily European countries

## Citation

```
[LST] (2025). ESG Factors and International Tourism: 
A Multi-Method Panel Analysis. Working Paper.

```

## Comprehensive empirical report is avaliable [here](https://raw.githack.com/lusiki/ESG-turism/main/present%20results_.html)
## Comprehensive empirical report2 is avaliable [here](https://raw.githack.com/lusiki/ESG-turism/main/present%20results%20v2.html)







