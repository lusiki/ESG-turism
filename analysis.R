# =============================================================================
# ESG-Tourism Panel Data Analysis
# Comprehensive analysis with FULL SUMMARIES for all components
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# -----------------------------------------------------------------------------

required_packages <- c(
  "plm",           # Panel data models
  "lmtest",        # Diagnostic tests
  "sandwich",      # Robust standard errors
  "randomForest",  # Random Forest
  "glmnet",        # Elastic Net / LASSO
  "caret",         # ML utilities
  "dplyr",         # Data manipulation
  "tidyr",         # Data reshaping
  "MASS"           # Stepwise AIC
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

invisible(sapply(required_packages, install_if_missing))

# Fix MASS::select masking dplyr::select
select <- dplyr::select

# -----------------------------------------------------------------------------
# 2. DATA PREPARATION
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 80), "\n")
cat("2. DATA PREPARATION\n")
cat(strrep("=", 80), "\n")

# Define variable groups based on prefixes
env_vars <- c("env_co2_cba", "env_co2_gdp", "env_co2_capita", "env_co2_pba",
              "env_nd_gain_idx", "env_pm25_exp", "env_renew_elec", "env_renew_cons")

soc_vars <- c("soc_age_dependency", "soc_edu_compul_yrs", "soc_female_mgrs",
              "soc_broadband", "soc_gini_index", "soc_immun_dpt", "soc_immun_hepb",
              "soc_immun_measles", "soc_mort_infant", "soc_internet_users",
              "soc_life_expect", "soc_edu_low_sec", "soc_mort_maternal",
              "soc_mort_general", "soc_mort_road", "soc_mort_poison",
              "soc_mort_u5_tot", "soc_mort_neonatal", "soc_net_migration",
              "soc_open_defec", "soc_water_basic", "soc_sanit_basic",
              "soc_water_safe", "soc_pov_190", "soc_pov_national",
              "soc_sanit_improved", "soc_women_parl", "soc_health_exp",
              "soc_gpi_primary", "soc_youth_idle", "soc_suicide_rate",
              "soc_unempl_gen", "soc_unempl_ilo")

gov_vars <- c("gov_cpi_aop", "gov_inflation", "gov_corruption",
              "gov_gdp_capita", "gov_gdp_const", "gov_gdp_growth",
              "gov_effectiveness", "gov_pol_stability", "gov_real_gdp_gr",
              "gov_reg_quality", "gov_rd_expend", "gov_researchers",
              "gov_rule_law", "gov_terms_trade", "gov_voice_acc", "gov_reer")

all_esg_vars <- c(env_vars, soc_vars, gov_vars)

# Create dependent variables (log transformations)
dta_clean <- dta_clean %>%
  mutate(
    log_arrivals = log(tour_arrivals + 1),
    log_nights = log(tour_nights + 1),
    log_night_pop = log(tour_night_pop + 0.01),
    log_night_area = log(tour_night_density + 0.01)
  )

# Filter to available variables
available_esg <- intersect(all_esg_vars, names(dta_clean))
available_env <- intersect(env_vars, names(dta_clean))
available_soc <- intersect(soc_vars, names(dta_clean))
available_gov <- intersect(gov_vars, names(dta_clean))

cat("\nAvailable Environment variables:", length(available_env), "\n")
cat("Available Social variables:", length(available_soc), "\n")
cat("Available Governance variables:", length(available_gov), "\n")

# Create analysis dataset
dta_analysis <- dta_clean %>%
  dplyr::select(Country, Year, log_arrivals, log_nights, log_night_pop, log_night_area,
                all_of(available_esg), gov_gdp_capita) %>%
  na.omit()

cat("\nAnalysis dataset:", nrow(dta_analysis), "observations\n")
cat("Countries:", length(unique(dta_analysis$Country)), "\n")
cat("Years:", min(dta_analysis$Year), "-", max(dta_analysis$Year), "\n")

# --- DATA PREPARATION SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("DATA PREPARATION SUMMARY\n")
cat(strrep("*", 80), "\n")
cat("\nTotal observations:", nrow(dta_analysis), "\n")
cat("Number of countries:", length(unique(dta_analysis$Country)), "\n")
cat("Time period:", min(dta_analysis$Year), "-", max(dta_analysis$Year), "\n")
cat("Panel balance: ", nrow(dta_analysis) / length(unique(dta_analysis$Country)), " years per country (avg)\n")
cat("\nDependent Variables:\n")
cat("  - log_arrivals: log(tourist arrivals + 1)\n")
cat("  - log_nights: log(tourist nights + 1)\n")
cat("  - log_night_pop: log(nights per capita + 0.01)\n")
cat("  - log_night_area: log(nights per km² + 0.01)\n")
cat("\nIndependent Variables:\n")
cat("  - Environment:", length(available_env), "variables\n")
cat("  - Social:", length(available_soc), "variables\n")
cat("  - Governance:", length(available_gov), "variables\n")
cat("  - Total ESG:", length(available_esg), "variables\n")

# =============================================================================
# 3. PCA - DIMENSION REDUCTION
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("3. PRINCIPAL COMPONENT ANALYSIS (PCA)\n")
cat(strrep("=", 80), "\n")

pca_summary_list <- list()

run_pca <- function(data, vars, dimension_name) {
  pca_data <- data %>% dplyr::select(all_of(vars)) %>% na.omit()

  if (ncol(pca_data) < 2) {
    cat("Not enough variables for", dimension_name, "PCA\n")
    return(NULL)
  }

  pca_scaled <- scale(pca_data)
  pca_scaled <- pca_scaled[, apply(pca_scaled, 2, sd, na.rm = TRUE) > 0]
  pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)

  cat("\n", strrep("-", 60), "\n")
  cat(dimension_name, "PCA RESULTS\n")
  cat(strrep("-", 60), "\n")

  # Variance explained - all components
  var_exp <- summary(pca_result)$importance
  cat("\nVariance Explained (All Components):\n")
  print(round(var_exp, 4))

  # All loadings for PC1, PC2, PC3
  cat("\nPC1 Loadings (all variables, sorted):\n")
  loadings_pc1 <- sort(pca_result$rotation[, 1], decreasing = TRUE)
  print(round(loadings_pc1, 4))

  if (ncol(pca_result$rotation) >= 2) {
    cat("\nPC2 Loadings (all variables, sorted):\n")
    loadings_pc2 <- sort(pca_result$rotation[, 2], decreasing = TRUE)
    print(round(loadings_pc2, 4))
  }

  if (ncol(pca_result$rotation) >= 3) {
    cat("\nPC3 Loadings (all variables, sorted):\n")
    loadings_pc3 <- sort(pca_result$rotation[, 3], decreasing = TRUE)
    print(round(loadings_pc3, 4))
  }

  # Eigenvalues
  eigenvalues <- pca_result$sdev^2
  cat("\nEigenvalues:\n")
  print(round(eigenvalues, 4))

  return(list(
    pca = pca_result,
    pc1_scores = pca_result$x[, 1],
    var_explained = var_exp[2, 1],
    cumulative_var = var_exp[3, ],
    eigenvalues = eigenvalues,
    loadings = pca_result$rotation
  ))
}

# Run PCA for each dimension
env_pca <- run_pca(dta_analysis, available_env, "ENVIRONMENT")
soc_pca <- run_pca(dta_analysis, available_soc, "SOCIAL")
gov_pca <- run_pca(dta_analysis, available_gov, "GOVERNANCE")

# Add PC1 scores to dataset
if (!is.null(env_pca)) {
  dta_analysis$env_pc1 <- NA
  env_complete <- complete.cases(dta_analysis[, available_env])
  dta_analysis$env_pc1[env_complete] <- env_pca$pc1_scores
}

if (!is.null(soc_pca)) {
  dta_analysis$soc_pc1 <- NA
  soc_complete <- complete.cases(dta_analysis[, available_soc])
  dta_analysis$soc_pc1[soc_complete] <- soc_pca$pc1_scores
}

if (!is.null(gov_pca)) {
  dta_analysis$gov_pc1 <- NA
  gov_complete <- complete.cases(dta_analysis[, available_gov])
  dta_analysis$gov_pc1[gov_complete] <- gov_pca$pc1_scores
}

# --- PCA SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("PCA SUMMARY\n")
cat(strrep("*", 80), "\n")

cat("\n┌─────────────────────────────────────────────────────────────────────────────┐\n")
cat("│ DIMENSION      │ PC1 Var%  │ PC2 Var%  │ PC3 Var%  │ Cum Var (3PC) │ N Vars │\n")
cat("├─────────────────────────────────────────────────────────────────────────────┤\n")

if (!is.null(env_pca)) {
  cat(sprintf("│ Environment    │ %8.2f%% │ %8.2f%% │ %8.2f%% │ %12.2f%% │ %6d │\n",
              env_pca$var_explained * 100,
              summary(env_pca$pca)$importance[2, 2] * 100,
              summary(env_pca$pca)$importance[2, 3] * 100,
              env_pca$cumulative_var[3] * 100,
              length(available_env)))
}
if (!is.null(soc_pca)) {
  cat(sprintf("│ Social         │ %8.2f%% │ %8.2f%% │ %8.2f%% │ %12.2f%% │ %6d │\n",
              soc_pca$var_explained * 100,
              summary(soc_pca$pca)$importance[2, 2] * 100,
              summary(soc_pca$pca)$importance[2, 3] * 100,
              soc_pca$cumulative_var[3] * 100,
              length(available_soc)))
}
if (!is.null(gov_pca)) {
  cat(sprintf("│ Governance     │ %8.2f%% │ %8.2f%% │ %8.2f%% │ %12.2f%% │ %6d │\n",
              gov_pca$var_explained * 100,
              summary(gov_pca$pca)$importance[2, 2] * 100,
              summary(gov_pca$pca)$importance[2, 3] * 100,
              gov_pca$cumulative_var[3] * 100,
              length(available_gov)))
}
cat("└─────────────────────────────────────────────────────────────────────────────┘\n")

cat("\nTop 3 PC1 Loadings per Dimension:\n")
if (!is.null(env_pca)) {
  top3_env <- head(sort(abs(env_pca$loadings[, 1]), decreasing = TRUE), 3)
  cat("  Environment:", paste(names(top3_env), collapse = ", "), "\n")
}
if (!is.null(soc_pca)) {
  top3_soc <- head(sort(abs(soc_pca$loadings[, 1]), decreasing = TRUE), 3)
  cat("  Social:", paste(names(top3_soc), collapse = ", "), "\n")
}
if (!is.null(gov_pca)) {
  top3_gov <- head(sort(abs(gov_pca$loadings[, 1]), decreasing = TRUE), 3)
  cat("  Governance:", paste(names(top3_gov), collapse = ", "), "\n")
}

# =============================================================================
# 4. RANDOM FOREST - VARIABLE IMPORTANCE
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("4. RANDOM FOREST - VARIABLE IMPORTANCE\n")
cat(strrep("=", 80), "\n")

rf_data <- dta_analysis %>%
  dplyr::select(log_arrivals, log_nights, all_of(available_esg)) %>%
  na.omit()

cat("\nRandom Forest sample size:", nrow(rf_data), "\n")

run_rf_importance <- function(data, dv, predictors) {
  set.seed(42)
  formula_rf <- as.formula(paste(dv, "~", paste(predictors, collapse = " + ")))

  rf_model <- randomForest(
    formula_rf,
    data = data,
    ntree = 500,
    importance = TRUE,
    na.action = na.omit
  )

  importance_df <- data.frame(
    Variable = rownames(importance(rf_model)),
    IncMSE = importance(rf_model)[, "%IncMSE"],
    IncNodePurity = importance(rf_model)[, "IncNodePurity"]
  ) %>%
    arrange(desc(IncMSE))

  return(list(model = rf_model, importance = importance_df))
}

# RF for log_arrivals
cat("\n", strrep("-", 60), "\n")
cat("RANDOM FOREST: log_arrivals\n")
cat(strrep("-", 60), "\n")
rf_arrivals <- run_rf_importance(rf_data, "log_arrivals", available_esg)
cat("\nModel Performance:\n")
cat("  R-squared:", round(rf_arrivals$model$rsq[500], 4), "\n")
cat("  MSE:", round(rf_arrivals$model$mse[500], 4), "\n")
cat("  % Var explained:", round(rf_arrivals$model$rsq[500] * 100, 2), "%\n")
cat("\nVariable Importance Rankings (%IncMSE) - ALL VARIABLES:\n")
print(rf_arrivals$importance, row.names = FALSE)

# RF for log_nights
cat("\n", strrep("-", 60), "\n")
cat("RANDOM FOREST: log_nights\n")
cat(strrep("-", 60), "\n")
rf_nights <- run_rf_importance(rf_data, "log_nights", available_esg)
cat("\nModel Performance:\n")
cat("  R-squared:", round(rf_nights$model$rsq[500], 4), "\n")
cat("  MSE:", round(rf_nights$model$mse[500], 4), "\n")
cat("  % Var explained:", round(rf_nights$model$rsq[500] * 100, 2), "%\n")
cat("\nVariable Importance Rankings (%IncMSE) - ALL VARIABLES:\n")
print(rf_nights$importance, row.names = FALSE)

# Combined importance
rf_combined <- rf_arrivals$importance %>%
  rename(IncMSE_arrivals = IncMSE, NodePurity_arrivals = IncNodePurity) %>%
  left_join(
    rf_nights$importance %>%
      rename(IncMSE_nights = IncMSE, NodePurity_nights = IncNodePurity),
    by = "Variable"
  ) %>%
  mutate(
    Avg_IncMSE = (IncMSE_arrivals + IncMSE_nights) / 2,
    Avg_NodePurity = (NodePurity_arrivals + NodePurity_nights) / 2
  ) %>%
  arrange(desc(Avg_IncMSE))

# --- RANDOM FOREST SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("RANDOM FOREST SUMMARY\n")
cat(strrep("*", 80), "\n")

cat("\nModel Performance Comparison:\n")
cat("┌────────────────────────────────────────────────────────┐\n")
cat("│ Dependent Variable │ R-squared │    MSE    │ N trees  │\n")
cat("├────────────────────────────────────────────────────────┤\n")
cat(sprintf("│ log_arrivals       │   %6.4f  │ %9.4f │    500   │\n",
            rf_arrivals$model$rsq[500], rf_arrivals$model$mse[500]))
cat(sprintf("│ log_nights         │   %6.4f  │ %9.4f │    500   │\n",
            rf_nights$model$rsq[500], rf_nights$model$mse[500]))
cat("└────────────────────────────────────────────────────────┘\n")

cat("\nCOMBINED VARIABLE IMPORTANCE (sorted by average %IncMSE):\n")
print(rf_combined, row.names = FALSE)

cat("\nTop 10 Most Important Variables (by Avg %IncMSE):\n")
top10_rf <- head(rf_combined, 10)
for (i in 1:nrow(top10_rf)) {
  cat(sprintf("  %2d. %s (Avg: %.2f, Arrivals: %.2f, Nights: %.2f)\n",
              i, top10_rf$Variable[i], top10_rf$Avg_IncMSE[i],
              top10_rf$IncMSE_arrivals[i], top10_rf$IncMSE_nights[i]))
}

cat("\nImportance by ESG Dimension (Avg %IncMSE):\n")
rf_combined$Dimension <- case_when(
  rf_combined$Variable %in% available_env ~ "Environment",
  rf_combined$Variable %in% available_soc ~ "Social",
  rf_combined$Variable %in% available_gov ~ "Governance",
  TRUE ~ "Other"
)
dimension_importance <- rf_combined %>%
  group_by(Dimension) %>%
  summarise(
    Mean_IncMSE = mean(Avg_IncMSE, na.rm = TRUE),
    Max_IncMSE = max(Avg_IncMSE, na.rm = TRUE),
    N_vars = n()
  ) %>%
  arrange(desc(Mean_IncMSE))
print(dimension_importance, row.names = FALSE)

# =============================================================================
# 5. ELASTIC NET - VARIABLE SELECTION
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("5. ELASTIC NET - VARIABLE SELECTION (Alpha = 0.5)\n")
cat(strrep("=", 80), "\n")

run_elastic_net <- function(data, dv, predictors, alpha = 0.5) {
  X <- as.matrix(data[, predictors])
  y <- data[[dv]]

  complete_cases <- complete.cases(X, y)
  X <- X[complete_cases, ]
  y <- y[complete_cases]
  X_scaled <- scale(X)

  set.seed(42)
  cv_model <- cv.glmnet(X_scaled, y, alpha = alpha, nfolds = 10)
  final_model <- glmnet(X_scaled, y, alpha = alpha, lambda = cv_model$lambda.min)

  # Get all coefficients (including zeros)
  coefs <- coef(final_model)
  all_coef_df <- data.frame(
    Variable = rownames(coefs)[-1],
    Coefficient = as.numeric(coefs)[-1]
  ) %>%
    arrange(desc(abs(Coefficient)))

  # Non-zero coefficients only
  nonzero_coef_df <- all_coef_df %>%
    filter(Coefficient != 0)

  return(list(
    model = final_model,
    cv_model = cv_model,
    lambda_min = cv_model$lambda.min,
    lambda_1se = cv_model$lambda.1se,
    selected_vars = nonzero_coef_df$Variable,
    coefficients = nonzero_coef_df,
    all_coefficients = all_coef_df
  ))
}

# Elastic Net for log_arrivals
cat("\n", strrep("-", 60), "\n")
cat("ELASTIC NET: log_arrivals\n")
cat(strrep("-", 60), "\n")
en_arrivals <- run_elastic_net(rf_data, "log_arrivals", available_esg)
cat("\nCross-Validation Results:\n")
cat("  Optimal lambda (min):", round(en_arrivals$lambda_min, 6), "\n")
cat("  Lambda (1se):", round(en_arrivals$lambda_1se, 6), "\n")
cat("  Variables selected:", length(en_arrivals$selected_vars), "out of", length(available_esg), "\n")
cat("\nSelected Variables with Non-Zero Coefficients:\n")
print(en_arrivals$coefficients, row.names = FALSE)
cat("\nAll Coefficients (including zeros):\n")
print(en_arrivals$all_coefficients, row.names = FALSE)

# Elastic Net for log_nights
cat("\n", strrep("-", 60), "\n")
cat("ELASTIC NET: log_nights\n")
cat(strrep("-", 60), "\n")
en_nights <- run_elastic_net(rf_data, "log_nights", available_esg)
cat("\nCross-Validation Results:\n")
cat("  Optimal lambda (min):", round(en_nights$lambda_min, 6), "\n")
cat("  Lambda (1se):", round(en_nights$lambda_1se, 6), "\n")
cat("  Variables selected:", length(en_nights$selected_vars), "out of", length(available_esg), "\n")
cat("\nSelected Variables with Non-Zero Coefficients:\n")
print(en_nights$coefficients, row.names = FALSE)
cat("\nAll Coefficients (including zeros):\n")
print(en_nights$all_coefficients, row.names = FALSE)

# --- ELASTIC NET SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("ELASTIC NET SUMMARY\n")
cat(strrep("*", 80), "\n")

cat("\nModel Comparison:\n")
cat("┌──────────────────────────────────────────────────────────────────┐\n")
cat("│ DV             │ Lambda (min) │ Lambda (1se) │ Vars Selected    │\n")
cat("├──────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│ log_arrivals   │ %12.6f │ %12.6f │ %3d / %3d        │\n",
            en_arrivals$lambda_min, en_arrivals$lambda_1se,
            length(en_arrivals$selected_vars), length(available_esg)))
cat(sprintf("│ log_nights     │ %12.6f │ %12.6f │ %3d / %3d        │\n",
            en_nights$lambda_min, en_nights$lambda_1se,
            length(en_nights$selected_vars), length(available_esg)))
cat("└──────────────────────────────────────────────────────────────────┘\n")

cat("\nVariable Selection Comparison:\n")
all_en_vars <- union(en_arrivals$selected_vars, en_nights$selected_vars)
en_comparison <- data.frame(Variable = all_en_vars) %>%
  left_join(en_arrivals$coefficients %>% rename(Coef_arrivals = Coefficient), by = "Variable") %>%
  left_join(en_nights$coefficients %>% rename(Coef_nights = Coefficient), by = "Variable") %>%
  mutate(
    Selected_Arrivals = ifelse(!is.na(Coef_arrivals), "Yes", "No"),
    Selected_Nights = ifelse(!is.na(Coef_nights), "Yes", "No"),
    Selected_Both = ifelse(Selected_Arrivals == "Yes" & Selected_Nights == "Yes", "Yes", "No")
  ) %>%
  arrange(desc(Selected_Both), desc(abs(Coef_arrivals)))

print(en_comparison, row.names = FALSE)

cat("\nVariables Selected for BOTH DVs:\n")
common_vars <- intersect(en_arrivals$selected_vars, en_nights$selected_vars)
if (length(common_vars) > 0) {
  for (v in common_vars) {
    coef_arr <- en_arrivals$coefficients$Coefficient[en_arrivals$coefficients$Variable == v]
    coef_ngt <- en_nights$coefficients$Coefficient[en_nights$coefficients$Variable == v]
    cat(sprintf("  - %s (arrivals: %.4f, nights: %.4f)\n", v, coef_arr, coef_ngt))
  }
} else {
  cat("  None\n")
}

cat("\nVariables Selected ONLY for log_arrivals:\n")
only_arrivals <- setdiff(en_arrivals$selected_vars, en_nights$selected_vars)
if (length(only_arrivals) > 0) cat("  ", paste(only_arrivals, collapse = ", "), "\n") else cat("  None\n")

cat("\nVariables Selected ONLY for log_nights:\n")
only_nights <- setdiff(en_nights$selected_vars, en_arrivals$selected_vars)
if (length(only_nights) > 0) cat("  ", paste(only_nights, collapse = ", "), "\n") else cat("  None\n")

# =============================================================================
# 6. PANEL FIXED EFFECTS MODELS
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("6. PANEL FIXED EFFECTS MODELS\n")
cat(strrep("=", 80), "\n")

pdata <- pdata.frame(dta_analysis, index = c("Country", "Year"))

# Preselected variables (15 IVs)
preselected_vars <- c(
  "env_co2_capita", "env_renew_elec", "env_renew_cons", "env_pm25_exp", "env_nd_gain_idx",
  "soc_life_expect", "soc_internet_users", "soc_gini_index", "soc_broadband", "soc_health_exp",
  "gov_gdp_capita", "gov_corruption", "gov_pol_stability", "gov_effectiveness", "gov_rule_law"
)
preselected_vars <- intersect(preselected_vars, names(pdata))

pca_vars <- c("env_pc1", "soc_pc1", "gov_pc1")
pca_vars <- intersect(pca_vars, names(pdata))

dep_vars <- c("log_arrivals", "log_nights", "log_night_pop", "log_night_area")

cat("\nPreselected variables (", length(preselected_vars), "):\n")
cat(paste(preselected_vars, collapse = ", "), "\n")

# Storage for all results
all_panel_results <- list()
panel_summary_table <- data.frame()

run_panel_models <- function(pdata, dv, ivs, model_name) {
  results <- list()
  formula_str <- paste(dv, "~", paste(ivs, collapse = " + "))
  formula_panel <- as.formula(formula_str)

  cat("\n", strrep("-", 70), "\n")
  cat("MODEL:", model_name, "| DV:", dv, "\n")
  cat("Formula:", formula_str, "\n")
  cat(strrep("-", 70), "\n")

  # Initialize summary row with ALL columns to avoid rbind errors
  summary_row <- data.frame(
    DV = dv,
    Variable_Set = model_name,
    N_IVs = length(ivs),
    Pooled_R2 = NA_real_,
    Country_FE_R2 = NA_real_,
    TwoWay_FE_R2 = NA_real_,
    RE_R2 = NA_real_,
    N_obs = NA_integer_,
    Hausman_p = NA_real_,
    Hausman_Decision = NA_character_,
    stringsAsFactors = FALSE
  )

  # Pooled OLS
  tryCatch({
    pooled <- plm(formula_panel, data = pdata, model = "pooling")
    results$pooled <- pooled
    cat("\n[POOLED OLS]\n")
    cat("R²:", round(summary(pooled)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(pooled)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(pooled), "\n")
    cat("\nCoefficients:\n")
    print(summary(pooled)$coefficients)
    summary_row$Pooled_R2 <- round(summary(pooled)$r.squared[1], 4)
  }, error = function(e) {
    cat("Pooled OLS failed:", e$message, "\n")
  })

  # Country FE
  tryCatch({
    fe_country <- plm(formula_panel, data = pdata, model = "within", effect = "individual")
    results$fe_country <- fe_country
    cat("\n[COUNTRY FIXED EFFECTS]\n")
    cat("R²:", round(summary(fe_country)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(fe_country)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(fe_country), "\n")
    cat("\nCoefficients:\n")
    print(summary(fe_country)$coefficients)
    summary_row$Country_FE_R2 <- round(summary(fe_country)$r.squared[1], 4)
  }, error = function(e) {
    cat("Country FE failed:", e$message, "\n")
  })

  # Two-Way FE
  tryCatch({
    fe_twoway <- plm(formula_panel, data = pdata, model = "within", effect = "twoways")
    results$fe_twoway <- fe_twoway
    cat("\n[TWO-WAY FIXED EFFECTS] ⭐ PREFERRED\n")
    cat("R²:", round(summary(fe_twoway)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(fe_twoway)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(fe_twoway), "\n")
    cat("\nCoefficients:\n")
    print(summary(fe_twoway)$coefficients)
    summary_row$TwoWay_FE_R2 <- round(summary(fe_twoway)$r.squared[1], 4)
    summary_row$N_obs <- nobs(fe_twoway)
  }, error = function(e) {
    cat("Two-Way FE failed:", e$message, "\n")
  })

  # Random Effects
  tryCatch({
    re <- plm(formula_panel, data = pdata, model = "random")
    results$re <- re
    cat("\n[RANDOM EFFECTS]\n")
    cat("R²:", round(summary(re)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(re)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(re), "\n")
    cat("\nCoefficients:\n")
    print(summary(re)$coefficients)
    summary_row$RE_R2 <- round(summary(re)$r.squared[1], 4)
  }, error = function(e) {
    cat("Random Effects failed:", e$message, "\n")
  })

  # Hausman test
  if (!is.null(results$fe_country) && !is.null(results$re)) {
    tryCatch({
      hausman <- phtest(results$fe_country, results$re)
      cat("\n[HAUSMAN TEST]\n")
      cat("Chi-squared:", round(hausman$statistic, 4), "\n")
      cat("df:", hausman$parameter, "\n")
      cat("p-value:", round(hausman$p.value, 4), "\n")
      cat("Conclusion:", ifelse(hausman$p.value < 0.05, "Use Fixed Effects", "Random Effects acceptable"), "\n")
      summary_row$Hausman_p <- round(hausman$p.value, 4)
      summary_row$Hausman_Decision <- ifelse(hausman$p.value < 0.05, "FE", "RE ok")
    }, error = function(e) {
      cat("Hausman test failed:", e$message, "\n")
    })
  }

  results$summary_row <- summary_row
  return(results)
}

# PRESELECTED VARIABLES
cat("\n", strrep("=", 80), "\n")
cat("VARIABLE SET: PRESELECTED (15 IVs)\n")
cat(strrep("=", 80), "\n")

for (dv in dep_vars) {
  model_name <- paste0("preselected_", gsub("log_", "", dv))
  result <- run_panel_models(pdata, dv, preselected_vars, "Preselected")
  all_panel_results[[model_name]] <- result
  panel_summary_table <- rbind(panel_summary_table, result$summary_row)
}

# PCA INDICES
cat("\n", strrep("=", 80), "\n")
cat("VARIABLE SET: PCA INDICES (3 IVs)\n")
cat(strrep("=", 80), "\n")

pdata_pca <- pdata[complete.cases(pdata[, pca_vars]), ]
cat("Observations with complete PCA scores:", nrow(pdata_pca), "\n")

for (dv in dep_vars) {
  model_name <- paste0("pca_", gsub("log_", "", dv))
  result <- run_panel_models(pdata_pca, dv, pca_vars, "PCA")
  all_panel_results[[model_name]] <- result
  panel_summary_table <- rbind(panel_summary_table, result$summary_row)
}

# =============================================================================
# 7. STEPWISE AIC SELECTION
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("7. STEPWISE AIC VARIABLE SELECTION\n")
cat(strrep("=", 80), "\n")

stepwise_panel_selection <- function(data, dv, candidate_vars) {
  model_data <- data %>%
    dplyr::select(all_of(c(dv, candidate_vars, "Country", "Year"))) %>%
    na.omit()

  if (nrow(model_data) < 50) {
    cat("Insufficient observations\n")
    return(NULL)
  }

  null_formula <- as.formula(paste(dv, "~ 1"))
  full_formula <- as.formula(paste(dv, "~", paste(candidate_vars, collapse = " + ")))
  null_model <- lm(null_formula, data = model_data)

  tryCatch({
    step_model <- step(null_model,
                       scope = list(lower = null_formula, upper = full_formula),
                       direction = "forward",
                       trace = 0)

    selected_vars <- names(coef(step_model))[-1]

    return(list(
      selected_vars = selected_vars,
      aic = AIC(step_model),
      bic = BIC(step_model),
      n_vars = length(selected_vars),
      model = step_model,
      r_squared = summary(step_model)$r.squared,
      adj_r_squared = summary(step_model)$adj.r.squared
    ))
  }, error = function(e) {
    cat("Stepwise failed:", e$message, "\n")
    return(NULL)
  })
}

stepwise_results <- list()
stepwise_summary <- data.frame()

for (dv in dep_vars) {
  cat("\n", strrep("-", 60), "\n")
  cat("STEPWISE AIC:", dv, "\n")
  cat(strrep("-", 60), "\n")

  result <- stepwise_panel_selection(dta_analysis, dv, available_esg)

  if (!is.null(result)) {
    stepwise_results[[dv]] <- result

    cat("\nStepwise Selection Results:\n")
    cat("  Number of variables selected:", result$n_vars, "\n")
    cat("  AIC:", round(result$aic, 2), "\n")
    cat("  BIC:", round(result$bic, 2), "\n")
    cat("  R²:", round(result$r_squared, 4), "\n")
    cat("  Adj R²:", round(result$adj_r_squared, 4), "\n")

    cat("\nSelected variables (in order of selection):\n")
    for (i in seq_along(result$selected_vars)) {
      cat(sprintf("  %2d. %s\n", i, result$selected_vars[i]))
    }

    cat("\nOLS Coefficients (stepwise model):\n")
    print(summary(result$model)$coefficients)

    # Store summary
    stepwise_summary <- rbind(stepwise_summary, data.frame(
      DV = dv,
      N_vars = result$n_vars,
      AIC = round(result$aic, 2),
      BIC = round(result$bic, 2),
      R2 = round(result$r_squared, 4),
      Adj_R2 = round(result$adj_r_squared, 4)
    ))

    # Run Two-Way FE with selected variables
    if (length(result$selected_vars) > 0) {
      cat("\n[TWO-WAY FE WITH STEPWISE VARIABLES]\n")
      model_name <- paste0("stepwise_", gsub("log_", "", dv))
      fe_result <- run_panel_models(pdata, dv, result$selected_vars, "Stepwise")
      all_panel_results[[model_name]] <- fe_result
      panel_summary_table <- rbind(panel_summary_table, fe_result$summary_row)
    }
  }
}

# --- STEPWISE SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("STEPWISE AIC SUMMARY\n")
cat(strrep("*", 80), "\n")

cat("\nModel Fit Comparison:\n")
print(stepwise_summary, row.names = FALSE)

cat("\nVariables Selected per DV:\n")
for (dv in names(stepwise_results)) {
  cat("\n", dv, "(", stepwise_results[[dv]]$n_vars, "variables):\n")
  cat("  ", paste(stepwise_results[[dv]]$selected_vars, collapse = ", "), "\n")
}

# Common variables across all DVs
if (length(stepwise_results) > 1) {
  common_stepwise <- Reduce(intersect, lapply(stepwise_results, function(x) x$selected_vars))
  cat("\nVariables selected for ALL DVs:\n")
  if (length(common_stepwise) > 0) {
    cat("  ", paste(common_stepwise, collapse = ", "), "\n")
  } else {
    cat("  None\n")
  }
}

# =============================================================================
# 8. HETEROGENEITY ANALYSIS - BY DEVELOPMENT LEVEL
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("8. HETEROGENEITY ANALYSIS - BY DEVELOPMENT LEVEL\n")
cat(strrep("=", 80), "\n")

# Split by median GDP
median_gdp <- median(dta_analysis$gov_gdp_capita, na.rm = TRUE)
cat("\nMedian GDP per capita:", round(median_gdp, 2), "\n")

dta_analysis <- dta_analysis %>%
  group_by(Country) %>%
  mutate(avg_gdp = mean(gov_gdp_capita, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(developed = ifelse(avg_gdp >= median_gdp, 1, 0))

country_groups <- dta_analysis %>%
  dplyr::select(Country, developed, avg_gdp) %>%
  distinct() %>%
  arrange(desc(avg_gdp))

cat("\n", strrep("-", 60), "\n")
cat("COUNTRY CLASSIFICATION\n")
cat(strrep("-", 60), "\n")
cat("\nHigh-income countries (", sum(country_groups$developed == 1), "):\n")
print(country_groups %>% filter(developed == 1) %>% dplyr::select(Country, avg_gdp), n = 50)
cat("\nLow/Middle-income countries (", sum(country_groups$developed == 0), "):\n")
print(country_groups %>% filter(developed == 0) %>% dplyr::select(Country, avg_gdp), n = 50)

dta_high <- dta_analysis %>% filter(developed == 1)
dta_low <- dta_analysis %>% filter(developed == 0)

pdata_high <- pdata.frame(dta_high, index = c("Country", "Year"))
pdata_low <- pdata.frame(dta_low, index = c("Country", "Year"))

heterogeneity_results <- list()
heterogeneity_summary <- data.frame()

for (dv in c("log_arrivals", "log_nights")) {
  cat("\n", strrep("-", 70), "\n")
  cat("HETEROGENEITY ANALYSIS: ", dv, "\n")
  cat(strrep("-", 70), "\n")

  formula_str <- paste(dv, "~", paste(preselected_vars, collapse = " + "))
  formula_panel <- as.formula(formula_str)

  # High income group
  cat("\n[HIGH-INCOME COUNTRIES - TWO-WAY FE]\n")
  tryCatch({
    fe_high <- plm(formula_panel, data = pdata_high, model = "within", effect = "twoways")
    heterogeneity_results[[paste0(dv, "_high")]] <- fe_high
    cat("R²:", round(summary(fe_high)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(fe_high)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(fe_high), "\n")
    cat("\nCoefficients:\n")
    print(summary(fe_high)$coefficients)

    heterogeneity_summary <- rbind(heterogeneity_summary, data.frame(
      DV = dv, Group = "High-income",
      R2 = round(summary(fe_high)$r.squared[1], 4),
      N_obs = nobs(fe_high)
    ))
  }, error = function(e) cat("High-income model failed:", e$message, "\n"))

  # Low/Middle income group
  cat("\n[LOW/MIDDLE-INCOME COUNTRIES - TWO-WAY FE]\n")
  tryCatch({
    fe_low <- plm(formula_panel, data = pdata_low, model = "within", effect = "twoways")
    heterogeneity_results[[paste0(dv, "_low")]] <- fe_low
    cat("R²:", round(summary(fe_low)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(fe_low)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(fe_low), "\n")
    cat("\nCoefficients:\n")
    print(summary(fe_low)$coefficients)

    heterogeneity_summary <- rbind(heterogeneity_summary, data.frame(
      DV = dv, Group = "Low/Mid-income",
      R2 = round(summary(fe_low)$r.squared[1], 4),
      N_obs = nobs(fe_low)
    ))
  }, error = function(e) cat("Low/Mid-income model failed:", e$message, "\n"))
}

# Interaction models
cat("\n", strrep("=", 80), "\n")
cat("INTERACTION MODELS (var + var:developed)\n")
cat(strrep("=", 80), "\n")

pdata_full <- pdata.frame(dta_analysis, index = c("Country", "Year"))
interaction_summary <- list()

for (dv in c("log_arrivals", "log_nights")) {
  cat("\n", strrep("-", 70), "\n")
  cat("INTERACTION MODEL:", dv, "\n")
  cat(strrep("-", 70), "\n")

  main_effects <- paste(preselected_vars, collapse = " + ")
  interactions <- paste(paste0(preselected_vars, ":developed"), collapse = " + ")
  formula_interact <- as.formula(paste(dv, "~", main_effects, "+", interactions))

  tryCatch({
    interact_model <- plm(formula_interact, data = pdata_full, model = "within", effect = "twoways")
    heterogeneity_results[[paste0(dv, "_interact")]] <- interact_model

    cat("\nModel Fit:\n")
    cat("R²:", round(summary(interact_model)$r.squared[1], 4), "\n")
    cat("Adj R²:", round(summary(interact_model)$r.squared[2], 4), "\n")
    cat("Observations:", nobs(interact_model), "\n")

    cat("\nAll Coefficients:\n")
    print(summary(interact_model)$coefficients)

    # Extract interaction coefficients
    coefs <- summary(interact_model)$coefficients
    interaction_rows <- grep(":developed", rownames(coefs))

    if (length(interaction_rows) > 0) {
      cat("\n[INTERACTION TERMS ONLY]\n")
      interaction_coefs <- coefs[interaction_rows, , drop = FALSE]
      print(interaction_coefs)

      # Store significant interactions
      sig_int <- interaction_coefs[interaction_coefs[, "Pr(>|t|)"] < 0.1, , drop = FALSE]
      interaction_summary[[dv]] <- list(
        all = interaction_coefs,
        significant = sig_int
      )

      if (nrow(sig_int) > 0) {
        cat("\nSignificant interactions (p < 0.1):\n")
        print(sig_int)
      } else {
        cat("\nNo significant interactions at p < 0.1\n")
      }

      # Marginal effects interpretation
      cat("\nMarginal Effects Interpretation:\n")
      for (i in 1:nrow(interaction_coefs)) {
        var_name <- gsub(":developed", "", rownames(interaction_coefs)[i])
        coef_val <- interaction_coefs[i, "Estimate"]
        p_val <- interaction_coefs[i, "Pr(>|t|)"]
        sig_star <- ifelse(p_val < 0.01, "***", ifelse(p_val < 0.05, "**", ifelse(p_val < 0.1, "*", "")))

        cat(sprintf("  %s: Effect differs by %.4f for high-income countries %s\n",
                    var_name, coef_val, sig_star))
      }
    }
  }, error = function(e) cat("Interaction model failed:", e$message, "\n"))
}

# --- HETEROGENEITY SUMMARY ---
cat("\n", strrep("*", 80), "\n")
cat("HETEROGENEITY ANALYSIS SUMMARY\n")
cat(strrep("*", 80), "\n")

cat("\nModel Fit by Income Group:\n")
if (nrow(heterogeneity_summary) > 0) {
  print(heterogeneity_summary, row.names = FALSE)
} else {
  cat("No heterogeneity results available\n")
}

cat("\nCoefficient Comparison (High vs Low/Mid Income):\n")
for (dv in c("log_arrivals", "log_nights")) {
  cat("\n", dv, ":\n")
  cat(strrep("-", 50), "\n")

  high_model <- heterogeneity_results[[paste0(dv, "_high")]]
  low_model <- heterogeneity_results[[paste0(dv, "_low")]]

  if (!is.null(high_model) && !is.null(low_model)) {
    high_coefs <- summary(high_model)$coefficients
    low_coefs <- summary(low_model)$coefficients

    common_vars_het <- intersect(rownames(high_coefs), rownames(low_coefs))

    comparison_df <- data.frame(
      Variable = common_vars_het,
      High_Coef = round(high_coefs[common_vars_het, "Estimate"], 4),
      High_pval = round(high_coefs[common_vars_het, "Pr(>|t|)"], 4),
      Low_Coef = round(low_coefs[common_vars_het, "Estimate"], 4),
      Low_pval = round(low_coefs[common_vars_het, "Pr(>|t|)"], 4)
    ) %>%
      mutate(
        High_sig = ifelse(High_pval < 0.01, "***", ifelse(High_pval < 0.05, "**", ifelse(High_pval < 0.1, "*", ""))),
        Low_sig = ifelse(Low_pval < 0.01, "***", ifelse(Low_pval < 0.05, "**", ifelse(Low_pval < 0.1, "*", ""))),
        Direction_Same = ifelse(sign(High_Coef) == sign(Low_Coef), "Yes", "No")
      )

    print(comparison_df, row.names = FALSE)
  } else {
    cat("  Models not available for comparison\n")
  }
}

cat("\nSignificant Interaction Effects Summary:\n")
if (length(interaction_summary) > 0) {
  for (dv in names(interaction_summary)) {
    cat("\n", dv, ":\n")
    if (!is.null(interaction_summary[[dv]]$significant) && nrow(interaction_summary[[dv]]$significant) > 0) {
      print(interaction_summary[[dv]]$significant)
    } else {
      cat("  No significant interactions (p < 0.1)\n")
    }
  }
} else {
  cat("  No interaction models estimated\n")
}

# =============================================================================
# 9. COMPREHENSIVE PANEL MODEL SUMMARY
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("9. COMPREHENSIVE PANEL MODEL SUMMARY\n")
cat(strrep("=", 80), "\n")

cat("\nFull Panel Model Comparison Table:\n")
print(panel_summary_table, row.names = FALSE)

cat("\nR² Comparison by Model Specification:\n")
r2_wide <- panel_summary_table %>%
  dplyr::select(DV, Variable_Set, TwoWay_FE_R2, Country_FE_R2, Pooled_R2, RE_R2) %>%
  arrange(DV, Variable_Set)
print(r2_wide, row.names = FALSE)

cat("\nBest Performing Variable Set (by Two-Way FE R²):\n")
best_by_dv <- panel_summary_table %>%
  filter(!is.na(TwoWay_FE_R2)) %>%
  group_by(DV) %>%
  filter(TwoWay_FE_R2 == max(TwoWay_FE_R2, na.rm = TRUE)) %>%
  dplyr::select(DV, Variable_Set, TwoWay_FE_R2, N_IVs) %>%
  ungroup()
print(best_by_dv, row.names = FALSE)

# =============================================================================
# 10. FINAL COMPREHENSIVE SUMMARY
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("10. FINAL COMPREHENSIVE SUMMARY\n")
cat(strrep("=", 80), "\n")

cat("\n┌────────────────────────────────────────────────────────────────────────────┐\n")
cat("│                           ANALYSIS OVERVIEW                                │\n")
cat("├────────────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│ Total Observations: %-5d    Countries: %-3d    Years: %d-%d          │\n",
            nrow(dta_analysis), length(unique(dta_analysis$Country)),
            min(dta_analysis$Year), max(dta_analysis$Year)))
cat(sprintf("│ Total ESG Variables: %-3d (Env: %d, Soc: %d, Gov: %d)                       │\n",
            length(available_esg), length(available_env), length(available_soc), length(available_gov)))
cat("└────────────────────────────────────────────────────────────────────────────┘\n")

cat("\n[1] PCA DIMENSION REDUCTION\n")
cat("────────────────────────────\n")
if (!is.null(env_pca)) cat(sprintf("  Environment PC1: %.1f%% variance explained\n", env_pca$var_explained * 100))
if (!is.null(soc_pca)) cat(sprintf("  Social PC1: %.1f%% variance explained\n", soc_pca$var_explained * 100))
if (!is.null(gov_pca)) cat(sprintf("  Governance PC1: %.1f%% variance explained\n", gov_pca$var_explained * 100))

cat("\n[2] RANDOM FOREST VARIABLE IMPORTANCE\n")
cat("──────────────────────────────────────\n")
cat(sprintf("  Model R² - Arrivals: %.4f, Nights: %.4f\n",
            rf_arrivals$model$rsq[500], rf_nights$model$rsq[500]))
cat("  Top 5 predictors (by avg %IncMSE):\n")
for (i in 1:5) {
  cat(sprintf("    %d. %s (%.2f)\n", i, rf_combined$Variable[i], rf_combined$Avg_IncMSE[i]))
}

cat("\n[3] ELASTIC NET VARIABLE SELECTION\n")
cat("───────────────────────────────────\n")
cat(sprintf("  Variables selected - Arrivals: %d, Nights: %d\n",
            length(en_arrivals$selected_vars), length(en_nights$selected_vars)))
cat(sprintf("  Variables selected for BOTH: %d\n", length(common_vars)))
if (length(common_vars) > 0) {
  cat("    ", paste(head(common_vars, 5), collapse = ", "),
      ifelse(length(common_vars) > 5, "...", ""), "\n")
}

cat("\n[4] STEPWISE AIC SELECTION\n")
cat("──────────────────────────\n")
for (dv in names(stepwise_results)) {
  cat(sprintf("  %s: %d variables (R²=%.4f)\n",
              dv, stepwise_results[[dv]]$n_vars, stepwise_results[[dv]]$r_squared))
}

cat("\n[5] PANEL MODEL COMPARISON (Two-Way FE R²)\n")
cat("──────────────────────────────────────────\n")
twoway_summary <- panel_summary_table %>%
  filter(!is.na(TwoWay_FE_R2)) %>%
  dplyr::select(DV, Variable_Set, TwoWay_FE_R2) %>%
  pivot_wider(names_from = Variable_Set, values_from = TwoWay_FE_R2)
print(twoway_summary, row.names = FALSE)

cat("\n[6] HETEROGENEITY ANALYSIS\n")
cat("──────────────────────────\n")
if (nrow(heterogeneity_summary) > 0) {
  cat("  R² by income group:\n")
  print(heterogeneity_summary, row.names = FALSE)
}

cat("\n  Significant interaction effects (p < 0.1):\n")
total_sig_interactions <- 0
if (length(interaction_summary) > 0) {
  for (dv in names(interaction_summary)) {
    if (!is.null(interaction_summary[[dv]]$significant)) {
      n_sig <- nrow(interaction_summary[[dv]]$significant)
      total_sig_interactions <- total_sig_interactions + n_sig
      if (n_sig > 0) {
        cat(sprintf("    %s: %d significant interactions\n", dv, n_sig))
        cat("      Variables:", paste(gsub(":developed", "", rownames(interaction_summary[[dv]]$significant)), collapse = ", "), "\n")
      }
    }
  }
}
if (total_sig_interactions == 0) {
  cat("    No significant interactions found\n")
}

cat("\n", strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80), "\n")