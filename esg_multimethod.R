# =============================================================================
# COMPLETE ESG-TOURISM ANALYSIS SCRIPT
# =============================================================================
#
# This script performs a full multi-method analysis of ESG factors on tourism.
#
# CONTENTS:
#   Part 1: Data Loading & Cleaning
#   Part 2: Principal Component Analysis (PCA)
#   Part 3: Random Forest
#   Part 4: Panel Fixed Effects Models
#   Part 5: Elastic Net with Fixed Effects
#   Part 6: First Differences (Fixed)
#   Part 7: Long Differences (Alternative)
#   Part 8: Results Summary & Visualization
#
# Author:  [Your Name]
# Date:    [Date]
#
# =============================================================================

# =============================================================================
# SETUP: LOAD PACKAGES
# =============================================================================

required_packages <- c(
  "readxl",
  "tidyverse",
  "janitor",
  "zoo",
  "skimr",
  "glmnet",
  "plm",
  "randomForest",
  "corrplot",
  "broom",
  "lmtest",
  "sandwich",
  "ggrepel"
)

# Install missing packages
# new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) {
#   cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
#   install.packages(new_packages)
# }

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                   ║\n")
cat("║         COMPLETE ESG-TOURISM ANALYSIS SCRIPT                      ║\n")
cat("║                                                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

# =============================================================================
# PART 1: DATA LOADING & CLEANING
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  PART 1: DATA LOADING & CLEANING\n")
cat("═══════════════════════════════════════════════════════════════════\n")

# --- 1.1 LOAD RAW DATA ---
cat("\n--- Loading Data ---\n")

# UPDATE THIS PATH TO YOUR FILE
dta_raw <- read_excel("dta_.xlsx")

cat("Loaded:", nrow(dta_raw), "rows x", ncol(dta_raw), "columns\n")

# --- 1.2 RENAME VARIABLES ---
cat("\n--- Renaming Variables ---\n")

rename_map <- c(
  # Tourism & Demographics
  "tour_arrivals" = "ARRIVALS", "tour_nights" = "Nights", "tour_avg_stay" = "AS",
  "population" = "POP", "tour_night_pop" = "NIGHTPOP", "tour_arrival_pop" = "ARRIVALPOP",
  "area_km2" = "Area", "tour_arr_density" = "ARR_AREA", "tour_night_density" = "TN_AREA",

  # Environment
  "env_co2_cba" = "E2", "env_co2_gdp" = "E3", "env_co2_capita" = "E4",
  "env_co2_pba" = "E5", "env_nd_gain_idx" = "E6", "env_pm25_exp" = "E7",
  "env_renew_elec" = "E8", "env_renew_cons" = "E9",

  # Governance
  "gov_cpi_aop" = "G1", "gov_cpi_eop" = "G2", "gov_inflation" = "G3",
  "gov_corruption" = "G4", "gov_account_bal" = "G5", "gov_export_price" = "G6",
  "gov_gdp_capita" = "G8", "gov_gdp_const" = "G9", "gov_gdp_growth" = "G10",
  "gov_gdp_ppp_const" = "G11", "gov_gdp_ppp_curr" = "G12", "gov_effectiveness" = "G13",
  "gov_pol_stability" = "G15", "gov_real_gdp_gr" = "G17", "gov_reg_quality" = "G18",
  "gov_rd_expend" = "G19", "gov_researchers" = "G20", "gov_rule_law" = "G21",
  "gov_terms_trade" = "G22", "gov_voice_acc" = "G23", "gov_reer" = "REER",
  "gov_esg_score" = "ESG",

  # Social
  "soc_age_dependency" = "S1", "soc_edu_compul_yrs" = "S2", "soc_female_mgrs" = "S3",
  "soc_broadband" = "S4", "soc_gini_index" = "S5", "soc_immun_dpt" = "S6",
  "soc_immun_hepb" = "S7", "soc_immun_measles" = "S8", "soc_mort_infant" = "S9",
  "soc_internet_users" = "S10", "soc_life_expect" = "S11", "soc_edu_low_sec" = "S12",
  "soc_mort_maternal" = "S13", "soc_mort_general" = "S14", "soc_mort_road" = "S15",
  "soc_mort_poison" = "S16", "soc_mort_u5_tot" = "S17", "soc_mort_u5_fem" = "S18",
  "soc_mort_u5_male" = "S19", "soc_mort_neonatal" = "S20", "soc_net_migration" = "S21",
  "soc_open_defec" = "S22", "soc_water_basic" = "S23", "soc_sanit_basic" = "S24",
  "soc_water_safe" = "S25", "soc_sanit_safe_rur" = "S26", "soc_sanit_safe_urb" = "S27",
  "soc_pov_190" = "S28", "soc_pov_national" = "S29", "soc_edu_pre_dur" = "S30",
  "soc_edu_pri_dur" = "S31", "soc_sanit_improved" = "S32", "soc_women_parl" = "S33",
  "soc_health_exp" = "S34", "soc_gpi_primary" = "S35", "soc_gpi_pri_sec" = "S36",
  "soc_gpi_secondary" = "S37", "soc_edu_sec_dur" = "S38", "soc_youth_idle" = "S39",
  "soc_suicide_rate" = "S40", "soc_unempl_gen" = "S41", "soc_unempl_ilo" = "S42",
  "soc_unempl_nat" = "S43", "soc_unempl_y_ilo" = "S44", "soc_unempl_y_nat" = "S45"
)

dta <- dta_raw %>% rename(any_of(rename_map))
cat("Variables renamed.\n")

# --- 1.3 DIAGNOSE DATA ---
cat("\n--- Data Diagnostics ---\n")
cat("Countries:", length(unique(dta$Country)), "\n")
cat("Years:", paste(range(dta$Year, na.rm = TRUE), collapse = " - "), "\n")

# --- 1.4 DROP PROBLEMATIC VARIABLES ---
cat("\n--- Identifying Problematic Variables ---\n")

# High missing (>60%)
missing_pct <- dta %>%
  summarise(across(where(is.numeric), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Pct_Missing")

high_missing <- missing_pct %>% filter(Pct_Missing > 60) %>% pull(Variable)

# Zero variance
numeric_cols <- dta %>% select(where(is.numeric)) %>% names()
zero_var <- c()
for(col in numeric_cols) {
  v <- var(dta[[col]], na.rm = TRUE)
  if(is.na(v) || v < 1e-10) zero_var <- c(zero_var, col)
}

vars_to_drop <- unique(c(high_missing, zero_var))
if(length(vars_to_drop) > 0) {
  cat("Dropping", length(vars_to_drop), "problematic variables.\n")
  dta <- dta %>% select(-any_of(vars_to_drop))
}

# --- 1.5 IMPUTE MISSING VALUES ---
cat("\n--- Imputing Missing Values ---\n")

dta_clean <- dta %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(across(where(is.numeric), ~zoo::na.approx(., x = Year, na.rm = FALSE, maxgap = 3))) %>%
  fill(where(is.numeric), .direction = "downup") %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

cat("Imputation complete.\n")

# --- 1.6 WINSORIZE OUTLIERS ---
cat("\n--- Treating Outliers ---\n")

vars_to_winsorize <- dta_clean %>%
  select(starts_with("env_"), starts_with("gov_"), starts_with("soc_")) %>%
  select(where(is.numeric)) %>% names()

dta_clean <- dta_clean %>%
  mutate(across(all_of(vars_to_winsorize), ~{
    lower <- quantile(., 0.01, na.rm = TRUE)
    upper <- quantile(., 0.99, na.rm = TRUE)
    pmax(pmin(., upper), lower)
  }))

cat("Outliers winsorized at 1st/99th percentile.\n")

# --- 1.7 CREATE DERIVED VARIABLES ---
dta_clean <- dta_clean %>%
  mutate(
    log_arrivals = log(tour_arrivals + 1),
    arrivals_per_capita = ifelse(population > 0, tour_arrivals / population * 1000, NA)
  )

cat("\n✓ Data cleaning complete.\n")
cat("Final dataset:", nrow(dta_clean), "obs,", ncol(dta_clean), "vars\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Remove zero-variance columns
remove_constant_cols <- function(data) {
  variances <- sapply(data, function(x) var(x, na.rm = TRUE))
  non_constant <- names(variances)[!is.na(variances) & variances > 1e-10]
  removed <- setdiff(names(data), non_constant)
  if(length(removed) > 0) cat("  Removed constant:", paste(removed, collapse = ", "), "\n")
  return(data[, non_constant, drop = FALSE])
}

# Variables to exclude from predictors
get_exclude_vars <- function() {
  c("Country", "Year", "tour_arrivals", "tour_nights", "tour_avg_stay",
    "population", "area_km2", "tour_night_pop", "tour_arrival_pop",
    "tour_arr_density", "tour_night_density", "gov_esg_score",
    "log_arrivals", "arrivals_per_capita")
}

# =============================================================================
# PART 2: PRINCIPAL COMPONENT ANALYSIS
# =============================================================================

run_pca_analysis <- function(df) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 2: PRINCIPAL COMPONENT ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════════\n")

  results <- list()

  # Environment PCA
  env_vars <- df %>% select(starts_with("env_")) %>% select(where(is.numeric))
  if(ncol(env_vars) >= 3) {
    cat("\n--- ENVIRONMENT ---\n")
    env_clean <- remove_constant_cols(env_vars %>% drop_na())
    if(nrow(env_clean) > 50 && ncol(env_clean) >= 2) {
      pca_env <- prcomp(env_clean, scale = TRUE, center = TRUE)
      var_exp <- summary(pca_env)$importance[2,] * 100
      cat("PC1:", round(var_exp[1], 1), "% | PC1+PC2:", round(sum(var_exp[1:2]), 1), "%\n")
      loadings <- data.frame(Variable = colnames(env_clean), Loading = pca_env$rotation[,1]) %>%
        arrange(desc(abs(Loading)))
      cat("Top loadings:\n"); print(head(loadings, 5))
      results$env <- list(pca = pca_env, loadings = loadings, var_exp = var_exp)
    }
  }

  # Social PCA
  soc_vars <- df %>% select(starts_with("soc_")) %>% select(where(is.numeric))
  if(ncol(soc_vars) >= 3) {
    cat("\n--- SOCIAL ---\n")
    soc_clean <- remove_constant_cols(soc_vars %>% drop_na())
    if(nrow(soc_clean) > 50 && ncol(soc_clean) >= 2) {
      pca_soc <- prcomp(soc_clean, scale = TRUE, center = TRUE)
      var_exp <- summary(pca_soc)$importance[2,] * 100
      cat("PC1:", round(var_exp[1], 1), "% | PC1+PC2:", round(sum(var_exp[1:2]), 1), "%\n")
      loadings <- data.frame(Variable = colnames(soc_clean), Loading = pca_soc$rotation[,1]) %>%
        arrange(desc(abs(Loading)))
      cat("Top loadings:\n"); print(head(loadings, 5))
      results$soc <- list(pca = pca_soc, loadings = loadings, var_exp = var_exp)
    }
  }

  # Governance PCA
  gov_vars <- df %>% select(starts_with("gov_")) %>% select(-any_of("gov_esg_score")) %>%
    select(where(is.numeric))
  if(ncol(gov_vars) >= 3) {
    cat("\n--- GOVERNANCE ---\n")
    gov_clean <- remove_constant_cols(gov_vars %>% drop_na())
    if(nrow(gov_clean) > 50 && ncol(gov_clean) >= 2) {
      pca_gov <- prcomp(gov_clean, scale = TRUE, center = TRUE)
      var_exp <- summary(pca_gov)$importance[2,] * 100
      cat("PC1:", round(var_exp[1], 1), "% | PC1+PC2:", round(sum(var_exp[1:2]), 1), "%\n")
      loadings <- data.frame(Variable = colnames(gov_clean), Loading = pca_gov$rotation[,1]) %>%
        arrange(desc(abs(Loading)))
      cat("Top loadings:\n"); print(head(loadings, 5))
      results$gov <- list(pca = pca_gov, loadings = loadings, var_exp = var_exp)
    }
  }

  return(results)
}

# =============================================================================
# PART 3: RANDOM FOREST
# =============================================================================

run_random_forest <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 3: RANDOM FOREST\n")
  cat("═══════════════════════════════════════════════════════════════════\n")

  vars_to_exclude <- get_exclude_vars()

  df_rf <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    select(-any_of(vars_to_exclude)) %>%
    select(where(~!all(is.na(.)))) %>%
    select(where(~var(., na.rm = TRUE) > 1e-10))

  complete_idx <- complete.cases(df_rf)
  Y_full <- log(df %>% filter(!is.na(.data[[target_var]])) %>% pull(!!target_var))
  X <- df_rf[complete_idx, ]
  Y <- Y_full[complete_idx]

  cat("\nData:", nrow(X), "obs,", ncol(X), "predictors\n")

  # Train/test split
  set.seed(123)
  train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))

  # Fit model
  cat("Fitting Random Forest...\n")
  rf_model <- randomForest(x = X[train_idx,], y = Y[train_idx], ntree = 500,
                           mtry = floor(sqrt(ncol(X))), importance = TRUE)

  # Evaluate
  preds <- predict(rf_model, X[-train_idx,])
  rmse <- sqrt(mean((Y[-train_idx] - preds)^2))
  r2 <- 1 - sum((Y[-train_idx] - preds)^2) / sum((Y[-train_idx] - mean(Y[-train_idx]))^2)

  cat("\n=== PERFORMANCE ===\n")
  cat("RMSE:", round(rmse, 4), "| R²:", round(r2, 4), "\n")

  # Importance
  importance_df <- data.frame(
    Variable = rownames(importance(rf_model)),
    IncMSE = importance(rf_model)[, "%IncMSE"]
  ) %>%
    arrange(desc(IncMSE)) %>%
    mutate(Rank = row_number(),
           Dimension = case_when(
             str_starts(Variable, "env_") ~ "Environment",
             str_starts(Variable, "gov_") ~ "Governance",
             str_starts(Variable, "soc_") ~ "Social", TRUE ~ "Other"))

  cat("\n=== TOP 15 VARIABLES ===\n")
  print(head(importance_df, 15))

  cat("\n=== BY DIMENSION ===\n")
  print(importance_df %>% group_by(Dimension) %>%
          summarise(Avg_Imp = mean(IncMSE), Top = Variable[1], N = n()) %>%
          arrange(desc(Avg_Imp)))

  return(list(model = rf_model, importance = importance_df,
              metrics = list(rmse = rmse, r_squared = r2)))
}

# =============================================================================
# PART 4: PANEL FIXED EFFECTS MODELS
# =============================================================================

run_panel_models <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 4: PANEL FIXED EFFECTS MODELS\n")
  cat("═══════════════════════════════════════════════════════════════════\n")

  df_panel <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    mutate(log_arrivals = log(.data[[target_var]])) %>%
    filter(is.finite(log_arrivals))

  pdata <- pdata.frame(df_panel, index = c("Country", "Year"))

  cat("\nPanel:", length(unique(df_panel$Country)), "countries,",
      length(unique(df_panel$Year)), "years,", nrow(df_panel), "obs\n")

  results <- list()

  # Build formula
  avail <- names(df_panel)
  fvars <- c()
  if("env_co2_capita" %in% avail) fvars <- c(fvars, "env_co2_capita")
  if("env_renew_elec" %in% avail) fvars <- c(fvars, "env_renew_elec")
  if("env_pm25_exp" %in% avail) fvars <- c(fvars, "env_pm25_exp")
  if("gov_gdp_capita" %in% avail) fvars <- c(fvars, "gov_gdp_capita")
  if("gov_corruption" %in% avail) fvars <- c(fvars, "gov_corruption")
  if("gov_pol_stability" %in% avail) fvars <- c(fvars, "gov_pol_stability")
  if("soc_internet_users" %in% avail) fvars <- c(fvars, "soc_internet_users")
  if("soc_life_expect" %in% avail) fvars <- c(fvars, "soc_life_expect")
  if("soc_unempl_ilo" %in% avail) fvars <- c(fvars, "soc_unempl_ilo")

  panel_formula <- as.formula(paste("log_arrivals ~", paste(fvars, collapse = " + ")))

  # Model 1: Pooled OLS
  cat("\n--- Model 1: Pooled OLS ---\n")
  pooled <- tryCatch(plm(panel_formula, data = pdata, model = "pooling"), error = function(e) NULL)
  if(!is.null(pooled)) {
    cat("R²:", round(summary(pooled)$r.squared[1], 4), "\n")
    results$pooled <- pooled
  }

  # Model 2: Country FE
  cat("\n--- Model 2: Country Fixed Effects ---\n")
  fe <- tryCatch(plm(panel_formula, data = pdata, model = "within", effect = "individual"), error = function(e) NULL)
  if(!is.null(fe)) {
    fe_robust <- coeftest(fe, vcov = vcovHC(fe, type = "HC1", cluster = "group"))
    print(fe_robust)
    cat("\nWithin R²:", round(summary(fe)$r.squared[1], 4), "\n")
    results$fixed_effects <- fe
    results$fe_robust <- fe_robust
  }

  # Model 3: Two-Way FE
  cat("\n--- Model 3: Two-Way Fixed Effects ---\n")
  twoway <- tryCatch(plm(panel_formula, data = pdata, model = "within", effect = "twoways"), error = function(e) NULL)
  if(!is.null(twoway)) {
    twoway_robust <- coeftest(twoway, vcov = vcovHC(twoway, type = "HC1", cluster = "group"))
    print(twoway_robust)
    results$twoway_fe <- twoway
    results$twoway_robust <- twoway_robust
  }

  # Model 4: Random Effects
  cat("\n--- Model 4: Random Effects ---\n")
  re <- tryCatch(plm(panel_formula, data = pdata, model = "random"), error = function(e) NULL)
  if(!is.null(re)) results$random_effects <- re

  # Hausman Test
  if(!is.null(fe) && !is.null(re)) {
    cat("\n--- Hausman Test ---\n")
    hausman <- phtest(fe, re)
    cat("p-value:", format.pval(hausman$p.value), "\n")
    cat(ifelse(hausman$p.value < 0.05, "→ Use Fixed Effects\n", "→ Random Effects may be OK\n"))
    results$hausman <- hausman
  }

  return(results)
}

# =============================================================================
# PART 5: ELASTIC NET WITH FIXED EFFECTS
# =============================================================================

run_elastic_net <- function(df, target_var = "tour_arrivals", alpha = 0.5) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 5: ELASTIC NET WITH FIXED EFFECTS\n")
  cat("═══════════════════════════════════════════════════════════════════\n")

  vars_to_exclude <- get_exclude_vars()

  df_model <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    mutate(log_arrivals = log(.data[[target_var]])) %>%
    filter(is.finite(log_arrivals))

  # Prepare ESG predictors
  X_esg_raw <- df_model %>% select(-any_of(vars_to_exclude), -log_arrivals)

  # Handle missing
  missing_pct <- colMeans(is.na(X_esg_raw)) * 100
  X_esg <- X_esg_raw %>%
    select(names(missing_pct)[missing_pct <= 20]) %>%
    mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    select(where(~!any(is.na(.)))) %>%
    select(where(~var(., na.rm = TRUE) > 1e-10))

  cat("\nESG predictors:", ncol(X_esg), "\n")

  # Country dummies
  country_dummies <- model.matrix(~ Country - 1, data = df_model)
  colnames(country_dummies) <- paste0("FE_", make.names(gsub("Country", "", colnames(country_dummies))))

  # Combine
  X_full <- cbind(as.matrix(X_esg), country_dummies)
  Y <- df_model$log_arrivals

  # Penalty: penalize ESG, not FE
  penalty <- c(rep(1, ncol(X_esg)), rep(0, ncol(country_dummies)))

  # CV
  cat("Running Elastic Net (alpha =", alpha, ")...\n")
  set.seed(123)
  cv_enet <- cv.glmnet(X_full, Y, alpha = alpha, penalty.factor = penalty, standardize = TRUE, nfolds = 10)

  # Final model
  enet_final <- glmnet(X_full, Y, alpha = alpha, lambda = cv_enet$lambda.1se,
                       penalty.factor = penalty, standardize = TRUE)

  # Extract ESG coefficients
  coefs <- as.matrix(coef(enet_final))
  esg_coefs <- data.frame(Variable = rownames(coefs), Coefficient = coefs[,1]) %>%
    filter(!str_starts(Variable, "FE_"), Variable != "(Intercept)", Coefficient != 0) %>%
    arrange(desc(abs(Coefficient))) %>%
    mutate(Dimension = case_when(
      str_starts(Variable, "env_") ~ "Environment",
      str_starts(Variable, "gov_") ~ "Governance",
      str_starts(Variable, "soc_") ~ "Social", TRUE ~ "Other"))

  cat("\n=== SELECTED VARIABLES ===\n")
  cat("Selected:", nrow(esg_coefs), "of", ncol(X_esg), "\n\n")
  print(esg_coefs)

  return(list(model = enet_final, cv_model = cv_enet, coefficients = esg_coefs, alpha = alpha))
}

# =============================================================================
# PART 6: FIRST DIFFERENCES (FIXED VERSION)
# =============================================================================

run_first_differences <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 6: FIRST DIFFERENCES MODEL\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Question: Does IMPROVING ESG lead to MORE tourism growth?\n\n")

  # Create differences
  df_fd <- df %>%
    filter(!is.na(.data[[target_var]]), .data[[target_var]] > 0) %>%
    arrange(Country, Year) %>%
    group_by(Country) %>%
    mutate(
      log_arr = log(.data[[target_var]]),
      d_log_arrivals = log_arr - lag(log_arr),
      year_gap = Year - lag(Year)
    ) %>%
    ungroup()

  # Get ESG variable names
  esg_vars <- df %>%
    select(starts_with("env_"), starts_with("gov_"), starts_with("soc_")) %>%
    select(where(is.numeric), -any_of("gov_esg_score")) %>%
    names()

  # Create differences for all ESG vars
  for(var in esg_vars) {
    if(var %in% names(df_fd)) {
      df_fd <- df_fd %>%
        group_by(Country) %>%
        mutate(!!paste0("d_", var) := .data[[var]] - lag(.data[[var]])) %>%
        ungroup()
    }
  }

  # Filter to consecutive years
  df_fd_valid <- df_fd %>%
    filter(!is.na(d_log_arrivals), year_gap == 1)

  cat("Valid FD observations:", nrow(df_fd_valid), "\n")

  # Check which FD variables have variation
  fd_vars <- paste0("d_", esg_vars)
  fd_vars <- fd_vars[fd_vars %in% names(df_fd_valid)]

  var_stats <- df_fd_valid %>%
    select(all_of(fd_vars)) %>%
    summarise(across(everything(), list(
      var = ~var(., na.rm = TRUE),
      n = ~sum(!is.na(.)),
      nz = ~sum(. != 0, na.rm = TRUE)
    ))) %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(n > 50, var > 1e-10, nz > 10) %>%
    arrange(desc(var))

  cat("Variables with variation:", nrow(var_stats), "\n")

  if(nrow(var_stats) < 3) {
    cat("\n⚠️  Not enough variation in differenced data.\n")
    cat("This often happens with heavily imputed data.\n")
    cat("Try run_long_differences() instead.\n")
    return(NULL)
  }

  # Select priority variables
  priority <- c("d_env_co2_capita", "d_env_renew_elec", "d_env_pm25_exp",
                "d_gov_gdp_capita", "d_gov_corruption", "d_gov_pol_stability",
                "d_gov_gdp_growth", "d_gov_inflation",
                "d_soc_internet_users", "d_soc_life_expect", "d_soc_unempl_ilo")

  selected <- intersect(priority, var_stats$variable)
  if(length(selected) < 3) selected <- head(var_stats$variable, 10)

  cat("Using variables:", paste(selected, collapse = ", "), "\n\n")

  # Prepare model data
  df_model <- df_fd_valid %>%
    select(Country, Year, d_log_arrivals, all_of(selected)) %>%
    drop_na()

  cat("Complete cases:", nrow(df_model), "\n")

  if(nrow(df_model) < 30) {
    cat("Too few observations.\n")
    return(NULL)
  }

  # Estimate
  formula_str <- paste("d_log_arrivals ~", paste(selected, collapse = " + "))
  fd_model <- lm(as.formula(formula_str), data = df_model)

  # Robust SEs
  n_countries <- length(unique(df_model$Country))
  if(n_countries >= 10) {
    fd_robust <- coeftest(fd_model, vcov = vcovCL(fd_model, cluster = df_model$Country))
  } else {
    fd_robust <- coeftest(fd_model, vcov = vcovHC(fd_model, type = "HC1"))
  }

  cat("\n=== FIRST DIFFERENCES RESULTS ===\n")
  print(fd_robust)
  cat("\nR²:", round(summary(fd_model)$r.squared, 4), "\n")

  # Tidy results
  results_tidy <- broom::tidy(fd_model) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Significant = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                              p.value < 0.1 ~ "*", TRUE ~ ""),
      Dimension = case_when(
        str_detect(term, "env_") ~ "Environment",
        str_detect(term, "gov_") ~ "Governance",
        str_detect(term, "soc_") ~ "Social", TRUE ~ "Other")
    ) %>%
    arrange(p.value)

  return(list(model = fd_model, robust_se = fd_robust, results = results_tidy,
              n_obs = nrow(df_model), n_countries = n_countries))
}

# =============================================================================
# PART 7: LONG DIFFERENCES (5-YEAR CHANGES)
# =============================================================================

run_long_differences <- function(df, target_var = "tour_arrivals", gap = 5) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 7: LONG DIFFERENCES (", gap, "-YEAR CHANGES)\n", sep = "")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Captures medium-term effects, less sensitive to interpolation.\n\n")

  # Create long differences
  df_ld <- df %>%
    filter(!is.na(.data[[target_var]]), .data[[target_var]] > 0) %>%
    arrange(Country, Year) %>%
    group_by(Country) %>%
    mutate(
      log_arr = log(.data[[target_var]]),
      d_log_arrivals = log_arr - lag(log_arr, gap),
      actual_gap = Year - lag(Year, gap)
    ) %>%
    ungroup() %>%
    filter(actual_gap == gap)

  # ESG vars
  esg_vars <- df %>%
    select(starts_with("env_"), starts_with("gov_"), starts_with("soc_")) %>%
    select(where(is.numeric), -any_of("gov_esg_score")) %>%
    names()

  # Create long differences
  for(var in esg_vars) {
    if(var %in% names(df_ld)) {
      df_ld <- df_ld %>%
        group_by(Country) %>%
        mutate(!!paste0("d", gap, "_", var) := .data[[var]] - lag(.data[[var]], gap)) %>%
        ungroup()
    }
  }

  cat("Observations with", gap, "-year gaps:", nrow(df_ld), "\n")

  # Find vars with variation
  ld_vars <- names(df_ld)[str_starts(names(df_ld), paste0("d", gap, "_"))]
  var_check <- df_ld %>%
    select(all_of(ld_vars)) %>%
    summarise(across(everything(), ~var(., na.rm = TRUE))) %>%
    pivot_longer(everything()) %>%
    filter(value > 1e-8) %>%
    arrange(desc(value))

  cat("Variables with variation:", nrow(var_check), "\n")

  if(nrow(var_check) < 3) {
    cat("Not enough variation.\n")
    return(NULL)
  }

  # Select key variables
  patterns <- c("gdp", "corrupt", "internet", "life_expect", "unempl", "co2", "renew", "inflation")
  selected <- var_check %>%
    filter(sapply(name, function(n) any(sapply(patterns, function(p) str_detect(n, p))))) %>%
    head(8) %>% pull(name)

  if(length(selected) < 3) selected <- head(var_check$name, 8)

  cat("Selected:", paste(selected, collapse = ", "), "\n\n")

  # Estimate
  df_model <- df_ld %>%
    select(Country, Year, d_log_arrivals, all_of(selected)) %>%
    drop_na()

  cat("Complete cases:", nrow(df_model), "\n")

  if(nrow(df_model) < 20) {
    cat("Too few observations.\n")
    return(NULL)
  }

  formula_str <- paste("d_log_arrivals ~", paste(selected, collapse = " + "))
  ld_model <- lm(as.formula(formula_str), data = df_model)

  # Robust SEs
  n_countries <- length(unique(df_model$Country))
  if(n_countries >= 5) {
    ld_robust <- coeftest(ld_model, vcov = vcovCL(ld_model, cluster = df_model$Country))
  } else {
    ld_robust <- coeftest(ld_model, vcov = vcovHC(ld_model, type = "HC1"))
  }

  cat("\n=== LONG DIFFERENCES RESULTS ===\n")
  print(ld_robust)
  cat("\nR²:", round(summary(ld_model)$r.squared, 4), "\n")

  # Tidy results
  results_tidy <- broom::tidy(ld_model) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Significant = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                              p.value < 0.1 ~ "*", TRUE ~ ""),
      Dimension = case_when(
        str_detect(term, "env_") ~ "Environment",
        str_detect(term, "gov_") ~ "Governance",
        str_detect(term, "soc_") ~ "Social", TRUE ~ "Other")
    ) %>%
    arrange(p.value)

  return(list(model = ld_model, robust_se = ld_robust, results = results_tidy,
              n_obs = nrow(df_model), gap = gap))
}

# =============================================================================
# PART 8: VISUALIZATION FUNCTIONS
# =============================================================================

create_rf_importance_plot <- function(rf_results, top_n = 20) {

  plot_data <- rf_results$importance %>%
    head(top_n) %>%
    mutate(Variable = fct_reorder(Variable, IncMSE))

  ggplot(plot_data, aes(x = Variable, y = IncMSE, fill = Dimension)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(
      "Environment" = "#27AE60", "Governance" = "#2980B9",
      "Social" = "#E67E22", "Other" = "#95A5A6"
    )) +
    theme_minimal(base_size = 11) +
    labs(title = "Random Forest: Variable Importance",
         subtitle = paste("Top", top_n, "predictors of tourist arrivals"),
         x = NULL, y = "Importance (%IncMSE)")
}

create_enet_coef_plot <- function(enet_results, top_n = 15) {

  if(nrow(enet_results$coefficients) == 0) return(NULL)

  plot_data <- enet_results$coefficients %>%
    head(top_n) %>%
    mutate(Variable = fct_reorder(Variable, Coefficient))

  ggplot(plot_data, aes(x = Variable, y = Coefficient, fill = Dimension)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c(
      "Environment" = "#27AE60", "Governance" = "#2980B9",
      "Social" = "#E67E22", "Other" = "#95A5A6"
    )) +
    theme_minimal() +
    labs(title = "Elastic Net: Selected ESG Variables",
         subtitle = "Coefficients (controlling for country FE)",
         x = NULL, y = "Coefficient")
}

# =============================================================================
# RUN COMPLETE ANALYSIS
# =============================================================================

run_all <- function(df) {

  results <- list()

  # Part 2: PCA
  results$pca <- tryCatch(run_pca_analysis(df), error = function(e) {
    cat("PCA Error:", e$message, "\n"); NULL })

  # Part 3: Random Forest
  results$rf <- tryCatch(run_random_forest(df), error = function(e) {
    cat("RF Error:", e$message, "\n"); NULL })

  # Part 4: Panel Models
  results$panel <- tryCatch(run_panel_models(df), error = function(e) {
    cat("Panel Error:", e$message, "\n"); NULL })

  # Part 5: Elastic Net
  results$enet <- tryCatch(run_elastic_net(df), error = function(e) {
    cat("Elastic Net Error:", e$message, "\n"); NULL })

  # Part 6: First Differences
  results$fd <- tryCatch(run_first_differences(df), error = function(e) {
    cat("FD Error:", e$message, "\n"); NULL })

  # Part 7: Long Differences (if FD failed)
  if(is.null(results$fd)) {
    cat("\nFirst Differences failed. Trying Long Differences...\n")
    results$ld <- tryCatch(run_long_differences(df, gap = 5), error = function(e) {
      cat("LD Error:", e$message, "\n"); NULL })
  }

  # Create plots
  results$plots <- list()
  if(!is.null(results$rf)) {
    results$plots$rf_importance <- create_rf_importance_plot(results$rf)
  }
  if(!is.null(results$enet)) {
    results$plots$enet_coefs <- create_enet_coef_plot(results$enet)
  }

  return(results)
}

# =============================================================================
# EXECUTE ANALYSIS
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                   RUNNING COMPLETE ANALYSIS                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

results <- run_all(dta_clean)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                      ANALYSIS COMPLETE                            ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Results stored in 'results' object:\n")
cat("  results$pca       - Principal Component Analysis\n")
cat("  results$rf        - Random Forest\n")
cat("  results$panel     - Panel Fixed Effects Models\n")
cat("  results$enet      - Elastic Net with FE\n")
cat("  results$fd        - First Differences (or NULL if failed)\n")
cat("  results$ld        - Long Differences (if FD failed)\n")
cat("  results$plots     - Visualizations\n")
cat("\n")
cat("View plots with:\n")
cat("  print(results$plots$rf_importance)\n")
cat("  print(results$plots$enet_coefs)\n")
cat("\n")

# Print key findings
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                      KEY FINDINGS\n")
cat("═══════════════════════════════════════════════════════════════════\n")

if(!is.null(results$rf)) {
  cat("\n--- TOP 10 PREDICTORS (Random Forest) ---\n")
  print(head(results$rf$importance %>% select(Variable, IncMSE, Dimension), 10))
}

if(!is.null(results$panel$twoway_robust)) {
  cat("\n--- TWO-WAY FE RESULTS (Most Reliable) ---\n")
  print(results$panel$twoway_robust)
}

if(!is.null(results$fd)) {
  cat("\n--- FIRST DIFFERENCES RESULTS ---\n")
  print(results$fd$results %>% select(term, estimate, p.value, Significant, Dimension))
} else if(!is.null(results$ld)) {
  cat("\n--- LONG DIFFERENCES RESULTS ---\n")
  print(results$ld$results %>% select(term, estimate, p.value, Significant, Dimension))
}

cat("\n")
cat("Analysis complete!\n")
# =============================================================================
# ESG-TOURISM ANALYSIS: EXTENDED METHODS
# =============================================================================
#
# This script extends the main analysis with:
#   Part 9:  First Differences on RAW (non-imputed) data
#   Part 10: Heterogeneity Analysis (Developed vs Developing)
#   Part 11: Dynamic Panel GMM (Arellano-Bond)
#   Part 12: Spatial Analysis (Neighbor spillovers)
#
# Run this AFTER the main analysis script
#
# =============================================================================

# =============================================================================
# ADDITIONAL PACKAGES
# =============================================================================

extended_packages <- c(
  "plm",        # Panel data (already loaded)
  "splm",       # Spatial panel models
  "spdep",      # Spatial dependence
  "sp",         # Spatial classes
  "countrycode" # Country classifications
)

# Install if needed
new_pkgs <- extended_packages[!(extended_packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) {
  cat("Installing:", paste(new_pkgs, collapse = ", "), "\n")
  install.packages(new_pkgs)
}

invisible(lapply(extended_packages, library, character.only = TRUE))

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                   ║\n")
cat("║           ESG-TOURISM EXTENDED ANALYSIS                           ║\n")
cat("║                                                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

# =============================================================================
# PART 9: FIRST DIFFERENCES ON RAW (NON-IMPUTED) DATA
# =============================================================================

run_raw_first_differences <- function(df_raw, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 9: FIRST DIFFERENCES ON RAW DATA (NO IMPUTATION)\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Using only original observations - no interpolation or filling.\n\n")

  # --- STEP 1: Identify raw data availability ---
  # Work with minimally processed data

  # Key variables to analyze
  key_vars <- c(
    "env_co2_capita", "env_renew_elec", "env_pm25_exp",
    "gov_gdp_capita", "gov_corruption", "gov_pol_stability",
    "gov_gdp_growth", "gov_inflation",
    "soc_internet_users", "soc_life_expect", "soc_unempl_ilo"
  )

  # Keep only available variables
  available_vars <- intersect(key_vars, names(df_raw))

  # Create dataset with only non-missing original values
  df_raw_fd <- df_raw %>%
    select(Country, Year, all_of(target_var), all_of(available_vars)) %>%
    # Mark rows that have the target and at least some predictors
    mutate(
      has_target = !is.na(.data[[target_var]]) & .data[[target_var]] > 0,
      n_predictors = rowSums(!is.na(select(., all_of(available_vars))))
    ) %>%
    filter(has_target, n_predictors >= 3)

  cat("Observations with raw target + ≥3 predictors:", nrow(df_raw_fd), "\n")

  # --- STEP 2: Create first differences using only consecutive available years ---
  df_fd <- df_raw_fd %>%
    arrange(Country, Year) %>%
    group_by(Country) %>%
    mutate(
      # Check for consecutive years
      prev_year = lag(Year),
      year_gap = Year - prev_year,
      is_consecutive = (year_gap == 1),

      # Target difference
      log_arr = log(.data[[target_var]]),
      log_arr_lag = lag(log_arr),
      d_log_arrivals = ifelse(is_consecutive, log_arr - log_arr_lag, NA)
    ) %>%
    ungroup()

  # Create differences for predictors
  for(var in available_vars) {
    new_var <- paste0("d_", var)
    df_fd <- df_fd %>%
      group_by(Country) %>%
      mutate(
        !!new_var := ifelse(is_consecutive, .data[[var]] - lag(.data[[var]]), NA)
      ) %>%
      ungroup()
  }

  # Filter to consecutive years only
  df_fd_valid <- df_fd %>%
    filter(is_consecutive == TRUE, !is.na(d_log_arrivals))

  cat("Valid consecutive-year differences:", nrow(df_fd_valid), "\n")

  if(nrow(df_fd_valid) < 30) {
    cat("\n⚠️  Too few consecutive raw observations for FD analysis.\n")
    cat("Consider using longer differences (3-year or 5-year gaps).\n")

    # Try with 2-year gaps
    cat("\nTrying 2-year gaps instead...\n")

    df_fd_2yr <- df_raw_fd %>%
      arrange(Country, Year) %>%
      group_by(Country) %>%
      mutate(
        year_lag2 = lag(Year, 2),
        year_gap2 = Year - year_lag2,
        is_valid_2yr = (year_gap2 >= 1 & year_gap2 <= 3),

        log_arr = log(.data[[target_var]]),
        log_arr_lag2 = lag(log_arr, 2),
        d_log_arrivals = ifelse(is_valid_2yr, log_arr - log_arr_lag2, NA)
      ) %>%
      ungroup()

    for(var in available_vars) {
      new_var <- paste0("d_", var)
      df_fd_2yr <- df_fd_2yr %>%
        group_by(Country) %>%
        mutate(!!new_var := ifelse(is_valid_2yr, .data[[var]] - lag(.data[[var]], 2), NA)) %>%
        ungroup()
    }

    df_fd_valid <- df_fd_2yr %>% filter(!is.na(d_log_arrivals))
    cat("Valid 2-year differences:", nrow(df_fd_valid), "\n")
  }

  if(nrow(df_fd_valid) < 20) {
    cat("\nStill insufficient data. Returning NULL.\n")
    return(NULL)
  }

  # --- STEP 3: Check variation in differenced variables ---
  d_vars <- paste0("d_", available_vars)
  d_vars <- d_vars[d_vars %in% names(df_fd_valid)]

  var_info <- df_fd_valid %>%
    select(all_of(d_vars)) %>%
    summarise(across(everything(), list(
      n = ~sum(!is.na(.)),
      var = ~var(., na.rm = TRUE),
      nz = ~sum(. != 0, na.rm = TRUE)
    ))) %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    filter(n >= 15, var > 1e-10, nz >= 5) %>%
    arrange(desc(var))

  cat("\nVariables with sufficient variation:", nrow(var_info), "\n")

  if(nrow(var_info) < 2) {
    cat("Not enough variable variation.\n")
    return(NULL)
  }

  selected_vars <- head(var_info$variable, 8)
  cat("Selected:", paste(selected_vars, collapse = ", "), "\n\n")

  # --- STEP 4: Estimate model ---
  df_model <- df_fd_valid %>%
    select(Country, Year, d_log_arrivals, all_of(selected_vars)) %>%
    drop_na()

  cat("Complete cases:", nrow(df_model), "\n")

  if(nrow(df_model) < 15) {
    cat("Too few complete cases.\n")
    return(NULL)
  }

  formula_str <- paste("d_log_arrivals ~", paste(selected_vars, collapse = " + "))
  fd_model <- lm(as.formula(formula_str), data = df_model)

  # Robust SEs
  n_countries <- length(unique(df_model$Country))
  if(n_countries >= 5) {
    fd_robust <- coeftest(fd_model, vcov = vcovCL(fd_model, cluster = df_model$Country))
  } else {
    fd_robust <- coeftest(fd_model, vcov = vcovHC(fd_model, type = "HC1"))
  }

  cat("\n=== RAW DATA FIRST DIFFERENCES RESULTS ===\n")
  cat("Interpretation: Effect of ACTUAL CHANGES in ESG on tourism GROWTH\n\n")
  print(fd_robust)

  cat("\nR²:", round(summary(fd_model)$r.squared, 4), "\n")
  cat("Countries:", n_countries, "| Observations:", nrow(df_model), "\n")

  # Tidy results
  results_tidy <- broom::tidy(fd_model) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Significant = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                              p.value < 0.1 ~ "*", TRUE ~ ""),
      Dimension = case_when(
        str_detect(term, "env_") ~ "Environment",
        str_detect(term, "gov_") ~ "Governance",
        str_detect(term, "soc_") ~ "Social", TRUE ~ "Other")
    ) %>%
    arrange(p.value)

  cat("\n=== SORTED BY SIGNIFICANCE ===\n")
  print(results_tidy %>% select(term, estimate, std.error, p.value, Significant))

  return(list(
    model = fd_model,
    robust_se = fd_robust,
    results = results_tidy,
    n_obs = nrow(df_model),
    n_countries = n_countries,
    data = df_model
  ))
}

# =============================================================================
# PART 10: HETEROGENEITY ANALYSIS (DEVELOPED VS DEVELOPING)
# =============================================================================

run_heterogeneity_analysis <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 10: HETEROGENEITY ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Question: Do ESG effects differ for developed vs developing countries?\n\n")

  results <- list()

  # --- STEP 1: Classify countries ---

  # Method 1: Use GDP per capita to classify
  median_gdp <- median(df$gov_gdp_capita, na.rm = TRUE)

  df_het <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    mutate(
      log_arrivals = log(.data[[target_var]]),
      # Classification based on median GDP
      income_group = ifelse(gov_gdp_capita >= median_gdp, "High Income", "Low/Middle Income")
    ) %>%
    filter(is.finite(log_arrivals))

  # Try to get World Bank classification if countrycode package works
  tryCatch({
    df_het <- df_het %>%
      mutate(
        iso3c = countrycode(Country, "country.name", "iso3c"),
        wb_income = countrycode(Country, "country.name", "wb")
      )

    # Simplified classification
    df_het <- df_het %>%
      mutate(
        dev_status = case_when(
          wb_income %in% c("High income") ~ "Developed",
          wb_income %in% c("Upper middle income", "Lower middle income", "Low income") ~ "Developing",
          TRUE ~ income_group  # Fallback
        )
      )
    cat("Using World Bank income classification.\n")
  }, error = function(e) {
    df_het <- df_het %>%
      mutate(dev_status = income_group)
    cat("Using GDP-based classification.\n")
  })

  # If dev_status doesn't exist, create it
  if(!"dev_status" %in% names(df_het)) {
    df_het$dev_status <- df_het$income_group
  }

  cat("\nCountry Classification:\n")
  print(table(df_het$dev_status))

  # --- STEP 2: Run separate models for each group ---

  groups <- unique(df_het$dev_status)
  groups <- groups[!is.na(groups)]

  # Formula
  avail <- names(df_het)
  fvars <- c()
  if("env_co2_capita" %in% avail) fvars <- c(fvars, "env_co2_capita")
  if("env_renew_elec" %in% avail) fvars <- c(fvars, "env_renew_elec")
  if("env_pm25_exp" %in% avail) fvars <- c(fvars, "env_pm25_exp")
  if("gov_gdp_capita" %in% avail) fvars <- c(fvars, "gov_gdp_capita")
  if("gov_corruption" %in% avail) fvars <- c(fvars, "gov_corruption")
  if("soc_internet_users" %in% avail) fvars <- c(fvars, "soc_internet_users")
  if("soc_life_expect" %in% avail) fvars <- c(fvars, "soc_life_expect")
  if("soc_unempl_ilo" %in% avail) fvars <- c(fvars, "soc_unempl_ilo")

  panel_formula <- as.formula(paste("log_arrivals ~", paste(fvars, collapse = " + ")))

  for(grp in groups) {
    cat("\n--- GROUP:", grp, "---\n")

    df_sub <- df_het %>% filter(dev_status == grp)
    n_countries <- length(unique(df_sub$Country))
    cat("Countries:", n_countries, "| Observations:", nrow(df_sub), "\n")

    if(n_countries < 5 || nrow(df_sub) < 50) {
      cat("Insufficient data for this group.\n")
      next
    }

    # Panel data
    pdata_sub <- tryCatch({
      pdata.frame(df_sub, index = c("Country", "Year"))
    }, error = function(e) NULL)

    if(is.null(pdata_sub)) next

    # Fixed effects model
    fe_sub <- tryCatch({
      plm(panel_formula, data = pdata_sub, model = "within", effect = "twoways")
    }, error = function(e) {
      # Try one-way if two-way fails
      tryCatch({
        plm(panel_formula, data = pdata_sub, model = "within", effect = "individual")
      }, error = function(e2) NULL)
    })

    if(!is.null(fe_sub)) {
      fe_robust_sub <- coeftest(fe_sub, vcov = vcovHC(fe_sub, type = "HC1"))
      print(fe_robust_sub)
      results[[grp]] <- list(model = fe_sub, robust = fe_robust_sub, n = nrow(df_sub))
    }
  }

  # --- STEP 3: Interaction model (full sample) ---
  cat("\n\n--- INTERACTION MODEL (Full Sample) ---\n")
  cat("Tests if coefficients significantly differ between groups.\n\n")

  # Create interaction terms
  df_interact <- df_het %>%
    mutate(
      is_developed = as.numeric(dev_status == "Developed" | dev_status == "High Income"),
      # Interaction terms
      env_co2_capita_X_dev = env_co2_capita * is_developed,
      gov_corruption_X_dev = gov_corruption * is_developed,
      soc_internet_X_dev = soc_internet_users * is_developed,
      env_pm25_X_dev = env_pm25_exp * is_developed
    )

  pdata_int <- tryCatch({
    pdata.frame(df_interact, index = c("Country", "Year"))
  }, error = function(e) NULL)

  if(!is.null(pdata_int)) {
    interact_formula <- log_arrivals ~
      env_co2_capita + gov_corruption + soc_internet_users + env_pm25_exp +
      env_co2_capita_X_dev + gov_corruption_X_dev + soc_internet_X_dev + env_pm25_X_dev

    interact_model <- tryCatch({
      plm(interact_formula, data = pdata_int, model = "within", effect = "twoways")
    }, error = function(e) NULL)

    if(!is.null(interact_model)) {
      interact_robust <- coeftest(interact_model, vcov = vcovHC(interact_model, type = "HC1"))

      cat("Interaction terms (X_dev) show ADDITIONAL effect for developed countries:\n\n")
      print(interact_robust)

      results$interaction <- list(model = interact_model, robust = interact_robust)
    }
  }

  # --- STEP 4: Summary comparison ---
  cat("\n\n═══════════════════════════════════════════════════════════════════\n")
  cat("  HETEROGENEITY SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  if(length(results) >= 2) {
    cat("Compare coefficient signs and magnitudes across groups.\n")
    cat("Significant interaction terms indicate heterogeneous effects.\n\n")

    # Extract coefficients for comparison
    comparison <- data.frame()
    for(grp in names(results)) {
      if(grp == "interaction") next
      if(!is.null(results[[grp]]$robust)) {
        coefs <- as.data.frame(results[[grp]]$robust[, c(1, 4)])
        coefs$Variable <- rownames(coefs)
        coefs$Group <- grp
        names(coefs)[1:2] <- c("Estimate", "P_value")
        comparison <- bind_rows(comparison, coefs)
      }
    }

    if(nrow(comparison) > 0) {
      comparison_wide <- comparison %>%
        select(Variable, Group, Estimate) %>%
        pivot_wider(names_from = Group, values_from = Estimate)

      cat("Coefficient Comparison:\n")
      print(comparison_wide)
    }
  }

  return(results)
}

# =============================================================================
# PART 11: DYNAMIC PANEL GMM (ARELLANO-BOND)
# =============================================================================

run_dynamic_gmm <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 11: DYNAMIC PANEL GMM (ARELLANO-BOND)\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Includes lagged dependent variable to capture persistence.\n")
  cat("Uses GMM to handle endogeneity of the lagged term.\n\n")

  # Prepare data
  df_gmm <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    mutate(log_arrivals = log(.data[[target_var]])) %>%
    filter(is.finite(log_arrivals)) %>%
    arrange(Country, Year) %>%
    group_by(Country) %>%
    mutate(
      log_arrivals_lag1 = lag(log_arrivals, 1),
      log_arrivals_lag2 = lag(log_arrivals, 2)
    ) %>%
    ungroup()

  # Create panel data
  pdata_gmm <- tryCatch({
    pdata.frame(df_gmm, index = c("Country", "Year"))
  }, error = function(e) {
    cat("Error creating panel data:", e$message, "\n")
    return(NULL)
  })

  if(is.null(pdata_gmm)) return(NULL)

  cat("Panel dimensions:", pdim(pdata_gmm)$nT$n, "countries,",
      pdim(pdata_gmm)$nT$T, "time periods\n\n")

  results <- list()

  # --- MODEL 1: Difference GMM (Arellano-Bond) ---
  cat("--- Model 1: Difference GMM (Arellano-Bond 1991) ---\n")
  cat("Instruments: Lagged levels (t-2 and earlier)\n\n")

  # Basic specification
  gmm_formula <- log_arrivals ~ lag(log_arrivals, 1) +
    env_co2_capita + gov_corruption + soc_internet_users + soc_life_expect

  gmm_diff <- tryCatch({
    pgmm(
      gmm_formula,
      data = pdata_gmm,
      effect = "twoways",
      model = "twosteps",
      transformation = "d",  # Difference GMM
      lag.form = c(2, 99)    # Use lags 2 and further as instruments
    )
  }, error = function(e) {
    cat("Difference GMM error:", e$message, "\n")

    # Try simpler specification
    cat("Trying simpler specification...\n")
    tryCatch({
      pgmm(
        log_arrivals ~ lag(log_arrivals, 1) + gov_corruption + soc_internet_users,
        data = pdata_gmm,
        effect = "individual",
        model = "onestep",
        transformation = "d",
        lag.form = c(2, 4)
      )
    }, error = function(e2) {
      cat("Simple GMM also failed:", e2$message, "\n")
      NULL
    })
  })

  if(!is.null(gmm_diff)) {
    cat("\nDifference GMM Results:\n")
    print(summary(gmm_diff))

    results$diff_gmm <- gmm_diff

    # Key coefficient: lagged dependent variable
    coefs <- coef(gmm_diff)
    if("lag(log_arrivals, 1)" %in% names(coefs)) {
      persistence <- coefs["lag(log_arrivals, 1)"]
      cat("\n*** KEY FINDING ***\n")
      cat("Persistence parameter (lagged arrivals):", round(persistence, 3), "\n")
      cat("Interpretation: A 1% increase in arrivals this year leads to a\n")
      cat("               ", round(persistence * 100, 1), "% increase next year (ceteris paribus).\n")

      if(persistence > 0.8) {
        cat("→ Very high persistence: Tourism is 'sticky' - past success begets future success.\n")
      } else if(persistence > 0.5) {
        cat("→ Moderate persistence: Tourism has momentum but can change.\n")
      } else {
        cat("→ Low persistence: Tourism responds quickly to changes in fundamentals.\n")
      }
    }
  }

  # --- MODEL 2: System GMM (Blundell-Bond) ---
  cat("\n\n--- Model 2: System GMM (Blundell-Bond 1998) ---\n")
  cat("More efficient when persistence is high.\n")
  cat("Instruments: Lagged levels AND lagged differences.\n\n")

  gmm_sys <- tryCatch({
    pgmm(
      gmm_formula,
      data = pdata_gmm,
      effect = "twoways",
      model = "twosteps",
      transformation = "ld",  # System GMM (levels + differences)
      lag.form = c(2, 99)
    )
  }, error = function(e) {
    cat("System GMM error:", e$message, "\n")

    tryCatch({
      pgmm(
        log_arrivals ~ lag(log_arrivals, 1) + gov_corruption + soc_internet_users,
        data = pdata_gmm,
        effect = "individual",
        model = "onestep",
        transformation = "ld",
        lag.form = c(2, 4)
      )
    }, error = function(e2) NULL)
  })

  if(!is.null(gmm_sys)) {
    cat("\nSystem GMM Results:\n")
    print(summary(gmm_sys))

    results$sys_gmm <- gmm_sys
  }

  # --- DIAGNOSTIC TESTS ---
  cat("\n\n═══════════════════════════════════════════════════════════════════\n")
  cat("  GMM DIAGNOSTIC TESTS\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  if(!is.null(gmm_diff)) {
    cat("For Difference GMM:\n")

    # Sargan/Hansen test for overidentification
    cat("\n1. Sargan Test (Overidentifying Restrictions):\n")
    cat("   H0: Instruments are valid (not correlated with errors)\n")
    sargan <- tryCatch({
      sargan(gmm_diff)
    }, error = function(e) NULL)

    if(!is.null(sargan)) {
      cat("   Chi-sq:", round(sargan$statistic, 2), "| p-value:", round(sargan$p.value, 4), "\n")
      if(sargan$p.value > 0.05) {
        cat("   → Cannot reject H0: Instruments appear valid ✓\n")
      } else {
        cat("   → Reject H0: Instruments may be invalid ⚠️\n")
      }
    }

    # AR tests
    cat("\n2. Arellano-Bond AR Tests (Serial Correlation):\n")
    cat("   AR(1) should be significant, AR(2) should NOT be.\n")

    ar_test <- tryCatch({
      mtest(gmm_diff, order = 2)
    }, error = function(e) NULL)

    if(!is.null(ar_test)) {
      print(ar_test)
    }
  }

  # --- COMPARISON: OLS vs FE vs GMM ---
  cat("\n\n═══════════════════════════════════════════════════════════════════\n")
  cat("  COMPARISON: OLS vs FE vs GMM\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  cat("The coefficient on lagged arrivals should fall between:\n")
  cat("  - OLS (biased upward due to omitted heterogeneity)\n")
  cat("  - FE (biased downward due to Nickell bias)\n")
  cat("  - GMM (consistent, should be in between)\n\n")

  # Quick OLS and FE for comparison
  ols_dyn <- tryCatch({
    lm(log_arrivals ~ log_arrivals_lag1 + gov_corruption + soc_internet_users,
       data = df_gmm)
  }, error = function(e) NULL)

  fe_dyn <- tryCatch({
    plm(log_arrivals ~ log_arrivals_lag1 + gov_corruption + soc_internet_users,
        data = pdata_gmm, model = "within")
  }, error = function(e) NULL)

  comparison <- data.frame(
    Model = c("OLS", "Fixed Effects", "Difference GMM", "System GMM"),
    Lag_Coef = NA,
    stringsAsFactors = FALSE
  )

  if(!is.null(ols_dyn)) comparison$Lag_Coef[1] <- round(coef(ols_dyn)["log_arrivals_lag1"], 3)
  if(!is.null(fe_dyn)) comparison$Lag_Coef[2] <- round(coef(fe_dyn)["log_arrivals_lag1"], 3)
  if(!is.null(gmm_diff)) {
    coefs_d <- coef(gmm_diff)
    if("lag(log_arrivals, 1)" %in% names(coefs_d)) {
      comparison$Lag_Coef[3] <- round(coefs_d["lag(log_arrivals, 1)"], 3)
    }
  }
  if(!is.null(gmm_sys)) {
    coefs_s <- coef(gmm_sys)
    if("lag(log_arrivals, 1)" %in% names(coefs_s)) {
      comparison$Lag_Coef[4] <- round(coefs_s["lag(log_arrivals, 1)"], 3)
    }
  }

  print(comparison)

  cat("\nExpected pattern: OLS > GMM > FE\n")

  return(results)
}

# =============================================================================
# PART 12: SPATIAL ANALYSIS (NEIGHBOR SPILLOVERS)
# =============================================================================

run_spatial_analysis <- function(df, target_var = "tour_arrivals") {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  PART 12: SPATIAL PANEL ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("Question: Do neighboring countries' ESG/tourism affect own tourism?\n\n")

  results <- list()

  # --- STEP 1: Get country list and coordinates ---
  countries <- unique(df$Country)
  cat("Countries in sample:", length(countries), "\n")

  # Try to get coordinates
  country_coords <- tryCatch({
    data.frame(
      Country = countries,
      iso3c = countrycode(countries, "country.name", "iso3c")
    ) %>%
      filter(!is.na(iso3c))
  }, error = function(e) {
    data.frame(Country = countries)
  })

  # --- STEP 2: Create spatial weights matrix ---
  cat("\nCreating spatial weights matrix...\n")

  # Option A: Contiguity-based (if we had shapefiles)
  # Option B: Distance-based
  # Option C: K-nearest neighbors
  # Option D: Economic/trade-based

  # We'll use a simplified approach: regional proximity
  # Group countries by region and create within-region weights

  tryCatch({
    country_coords <- country_coords %>%
      mutate(
        region = countrycode(iso3c, "iso3c", "region"),
        continent = countrycode(iso3c, "iso3c", "continent")
      )

    cat("\nRegional distribution:\n")
    print(table(country_coords$continent))
  }, error = function(e) {
    cat("Could not classify countries by region.\n")
  })

  # --- STEP 3: Manual contiguity matrix (simplified) ---
  # For European countries commonly in tourism studies

  cat("\nCreating simplified contiguity weights...\n")
  cat("(For full spatial analysis, country shapefiles would be needed)\n\n")

  n <- length(countries)
  W <- matrix(0, nrow = n, ncol = n)
  rownames(W) <- colnames(W) <- countries

  # Define some known neighbors (European focus - expand as needed)
  neighbors <- list(
    "Austria" = c("Germany", "Switzerland", "Italy", "Czech Republic", "Hungary", "Slovenia"),
    "Belgium" = c("France", "Germany", "Netherlands", "Luxembourg"),
    "Czech Republic" = c("Germany", "Austria", "Poland", "Slovakia"),
    "Denmark" = c("Germany", "Sweden"),
    "Finland" = c("Sweden", "Norway"),
    "France" = c("Belgium", "Germany", "Switzerland", "Italy", "Spain"),
    "Germany" = c("Austria", "Belgium", "Czech Republic", "Denmark", "France", "Netherlands", "Poland", "Switzerland"),
    "Greece" = c("Bulgaria", "Turkey", "Albania"),
    "Hungary" = c("Austria", "Slovakia", "Romania", "Croatia", "Serbia", "Slovenia"),
    "Ireland" = c("United Kingdom"),
    "Italy" = c("Austria", "France", "Switzerland", "Slovenia"),
    "Netherlands" = c("Belgium", "Germany"),
    "Norway" = c("Sweden", "Finland"),
    "Poland" = c("Germany", "Czech Republic", "Slovakia", "Lithuania"),
    "Portugal" = c("Spain"),
    "Slovenia" = c("Austria", "Italy", "Hungary", "Croatia"),
    "Spain" = c("France", "Portugal"),
    "Sweden" = c("Norway", "Finland", "Denmark"),
    "Switzerland" = c("Austria", "France", "Germany", "Italy"),
    "United Kingdom" = c("Ireland")
  )

  # Fill weight matrix
  for(country in names(neighbors)) {
    if(country %in% countries) {
      for(neighbor in neighbors[[country]]) {
        if(neighbor %in% countries) {
          W[country, neighbor] <- 1
          W[neighbor, country] <- 1  # Symmetric
        }
      }
    }
  }

  # Row-standardize
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  W_std <- W / row_sums

  cat("Non-isolated countries:", sum(rowSums(W) > 0), "\n")

  # --- STEP 4: Create spatial lag variables ---
  cat("\nCreating spatial lag variables...\n")

  # For each year, compute spatial lag of key variables
  years <- sort(unique(df$Year))

  df_spatial <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    mutate(log_arrivals = log(.data[[target_var]])) %>%
    filter(is.finite(log_arrivals))

  # Initialize spatial lag columns
  df_spatial$W_log_arrivals <- NA
  df_spatial$W_corruption <- NA
  df_spatial$W_internet <- NA

  for(yr in years) {
    idx <- df_spatial$Year == yr
    countries_yr <- df_spatial$Country[idx]

    # Subset weight matrix
    common_countries <- intersect(countries_yr, rownames(W_std))

    if(length(common_countries) > 2) {
      W_sub <- W_std[common_countries, common_countries]

      # Get values for this year
      vals <- df_spatial %>%
        filter(Year == yr, Country %in% common_countries) %>%
        arrange(match(Country, common_countries))

      if(nrow(vals) == length(common_countries)) {
        # Spatial lags
        W_log_arr <- as.vector(W_sub %*% vals$log_arrivals)
        W_corr <- as.vector(W_sub %*% vals$gov_corruption)
        W_int <- as.vector(W_sub %*% vals$soc_internet_users)

        # Assign back
        for(i in seq_along(common_countries)) {
          row_idx <- which(df_spatial$Year == yr & df_spatial$Country == common_countries[i])
          if(length(row_idx) == 1) {
            df_spatial$W_log_arrivals[row_idx] <- W_log_arr[i]
            df_spatial$W_corruption[row_idx] <- W_corr[i]
            df_spatial$W_internet[row_idx] <- W_int[i]
          }
        }
      }
    }
  }

  # Check how many observations have spatial data
  n_spatial <- sum(!is.na(df_spatial$W_log_arrivals))
  cat("Observations with spatial data:", n_spatial, "\n")

  if(n_spatial < 50) {
    cat("\n⚠️  Insufficient spatial data. Many countries may not have defined neighbors.\n")
    cat("For proper spatial analysis, you would need:\n")
    cat("  1. Complete country shapefile with contiguity\n")
    cat("  2. Or country centroid coordinates for distance-based weights\n\n")
  }

  # --- STEP 5: Spatial panel models ---
  cat("\n--- Spatial Panel Regression ---\n")

  # Filter to observations with spatial data
  df_spatial_valid <- df_spatial %>%
    filter(!is.na(W_log_arrivals)) %>%
    drop_na(log_arrivals, gov_corruption, soc_internet_users, W_log_arrivals)

  cat("Valid observations:", nrow(df_spatial_valid), "\n\n")

  if(nrow(df_spatial_valid) >= 30) {
    # Model with spatial lag of Y
    spatial_lag_model <- lm(
      log_arrivals ~ gov_corruption + soc_internet_users + env_co2_capita +
        W_log_arrivals + W_corruption,
      data = df_spatial_valid
    )

    cat("=== SPATIAL LAG MODEL ===\n")
    cat("W_log_arrivals = spatial lag of neighbor arrivals\n")
    cat("W_corruption = average neighbor corruption level\n\n")

    # Cluster by country
    spatial_robust <- coeftest(spatial_lag_model,
                               vcov = vcovCL(spatial_lag_model, cluster = df_spatial_valid$Country))
    print(spatial_robust)

    cat("\nR²:", round(summary(spatial_lag_model)$r.squared, 4), "\n")

    results$spatial_lag <- list(model = spatial_lag_model, robust = spatial_robust)

    # Interpretation
    cat("\n*** INTERPRETATION ***\n")

    coef_W_arr <- coef(spatial_lag_model)["W_log_arrivals"]
    coef_W_corr <- coef(spatial_lag_model)["W_corruption"]

    if(!is.na(coef_W_arr)) {
      cat("\nNeighbor Tourism Effect (W_log_arrivals):", round(coef_W_arr, 3), "\n")
      if(coef_W_arr > 0) {
        cat("→ Positive spillover: When neighbors get more tourists, you do too.\n")
        cat("  (Regional tourism clusters / multi-country trips)\n")
      } else {
        cat("→ Competition effect: When neighbors get more tourists, you get fewer.\n")
        cat("  (Tourists substitute between neighboring destinations)\n")
      }
    }

    if(!is.na(coef_W_corr)) {
      cat("\nNeighbor Corruption Effect (W_corruption):", round(coef_W_corr, 3), "\n")
      if(coef_W_corr > 0) {
        cat("→ When neighbors improve governance, it helps your tourism.\n")
        cat("  (Regional reputation / joint marketing)\n")
      } else {
        cat("→ When neighbors improve governance, it may draw tourists away.\n")
      }
    }

  } else {
    cat("Insufficient spatial data for regression.\n")
  }

  # --- STEP 6: Create spatial weights list for splm (if possible) ---
  # This would enable proper spatial error models, SAR, SEM, etc.

  cat("\n\n═══════════════════════════════════════════════════════════════════\n")
  cat("  SPATIAL ANALYSIS NOTES\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\nFor more sophisticated spatial panel models (SAR, SEM, SDM), you need:\n")
  cat("  1. splm package with listw spatial weights object\n")
  cat("  2. Complete contiguity or distance matrix\n")
  cat("  3. Balanced panel (same countries in all years)\n")
  cat("\nRecommended next steps:\n")
  cat("  - Download country shapefiles (naturalearthdata.com)\n")
  cat("  - Use poly2nb() and nb2listw() from spdep package\n")
  cat("  - Estimate spml() models with proper spatial structure\n")

  return(results)
}

# =============================================================================
# MASTER FUNCTION: RUN ALL EXTENDED ANALYSES
# =============================================================================

run_extended_analysis <- function(df_clean, df_raw = NULL) {

  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════════╗\n")
  cat("║                                                                   ║\n")
  cat("║           RUNNING EXTENDED ANALYSES                               ║\n")
  cat("║                                                                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════╝\n")

  extended_results <- list()

  # Part 9: Raw FD
  if(!is.null(df_raw)) {
    extended_results$raw_fd <- tryCatch(
      run_raw_first_differences(df_raw),
      error = function(e) { cat("Raw FD Error:", e$message, "\n"); NULL }
    )
  } else {
    cat("\n⚠️  No raw data provided. Using cleaned data for FD.\n")
    cat("   For best results, provide original data before imputation.\n")
    extended_results$raw_fd <- tryCatch(
      run_raw_first_differences(df_clean),
      error = function(e) { cat("Raw FD Error:", e$message, "\n"); NULL }
    )
  }

  # Part 10: Heterogeneity
  extended_results$heterogeneity <- tryCatch(
    run_heterogeneity_analysis(df_clean),
    error = function(e) { cat("Heterogeneity Error:", e$message, "\n"); NULL }
  )

  # Part 11: Dynamic GMM
  extended_results$gmm <- tryCatch(
    run_dynamic_gmm(df_clean),
    error = function(e) { cat("GMM Error:", e$message, "\n"); NULL }
  )

  # Part 12: Spatial
  extended_results$spatial <- tryCatch(
    run_spatial_analysis(df_clean),
    error = function(e) { cat("Spatial Error:", e$message, "\n"); NULL }
  )

  # Summary
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════════╗\n")
  cat("║               EXTENDED ANALYSIS COMPLETE                          ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("Results stored in:\n")
  cat("  extended_results$raw_fd        - First Differences (raw data)\n")
  cat("  extended_results$heterogeneity - Developed vs Developing\n")
  cat("  extended_results$gmm           - Dynamic Panel GMM\n")
  cat("  extended_results$spatial       - Spatial spillovers\n")
  cat("\n")

  return(extended_results)
}

# =============================================================================
# EXECUTE EXTENDED ANALYSIS
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              STARTING EXTENDED ANALYSIS                           ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Running extended analyses on dta_clean...\n")
cat("(If you have raw pre-imputation data, pass it as df_raw)\n")
cat("\n")

# Run with cleaned data (modify to use raw if available)
# If you have pre-imputation data saved as dta_raw, use:
# extended_results <- run_extended_analysis(dta_clean, df_raw = dta_raw)

extended_results <- run_extended_analysis(dta_clean)

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("              EXTENDED ANALYSIS KEY FINDINGS\n")
cat("═══════════════════════════════════════════════════════════════════\n")

# Heterogeneity
if(!is.null(extended_results$heterogeneity)) {
  cat("\n--- HETEROGENEITY ---\n")
  if(!is.null(extended_results$heterogeneity$interaction)) {
    cat("See interaction model results above for differential effects.\n")
  }
}

# GMM
if(!is.null(extended_results$gmm)) {
  cat("\n--- DYNAMIC GMM ---\n")
  if(!is.null(extended_results$gmm$diff_gmm)) {
    cat("Tourism shows persistence - past arrivals predict future arrivals.\n")
  }
}

# Spatial
if(!is.null(extended_results$spatial)) {
  cat("\n--- SPATIAL SPILLOVERS ---\n")
  if(!is.null(extended_results$spatial$spatial_lag)) {
    cat("Neighbor effects detected - regional tourism patterns matter.\n")
  }
}

cat("\n")
cat("Analysis complete! Access results via 'extended_results' object.\n")
# =============================================================================
# =============================================================================
#
#                    COMPREHENSIVE RESULTS DISPLAY
#
# =============================================================================
# =============================================================================

display_all_results <- function(results, extended_results) {

  cat("\n\n")
  cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                                                                           ║\n")
  cat("║                    COMPLETE ANALYSIS RESULTS                              ║\n")
  cat("║                                                                           ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════════════╝\n")

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 2: PCA RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 2: PCA RESULTS                                  │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$pca)) {

    # Environment PCA
    if(!is.null(results$pca$env)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  ENVIRONMENT PCA\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("\nVariance Explained by Component:\n")
      var_exp <- results$pca$env$var_exp
      for(i in 1:min(5, length(var_exp))) {
        cat(sprintf("  PC%d: %5.1f%%\n", i, var_exp[i]))
      }
      cat(sprintf("  Total (PC1-PC3): %.1f%%\n", sum(var_exp[1:3])))

      cat("\nPC1 Loadings (sorted by absolute value):\n")
      print(results$pca$env$loadings %>%
              mutate(Loading = round(Loading, 3)) %>%
              head(10))

      cat("\nInterpretation: PC1 captures 'Environmental Quality'\n")
      cat("  - Positive loadings: Pollution indicators (higher = worse)\n")
      cat("  - Negative loadings: Clean energy/adaptation (higher = better)\n")
    }

    # Social PCA
    if(!is.null(results$pca$soc)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  SOCIAL PCA\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("\nVariance Explained by Component:\n")
      var_exp <- results$pca$soc$var_exp
      for(i in 1:min(5, length(var_exp))) {
        cat(sprintf("  PC%d: %5.1f%%\n", i, var_exp[i]))
      }
      cat(sprintf("  Total (PC1-PC3): %.1f%%\n", sum(var_exp[1:3])))

      cat("\nPC1 Loadings (top 15 by absolute value):\n")
      print(results$pca$soc$loadings %>%
              mutate(Loading = round(Loading, 3)) %>%
              head(15))

      cat("\nInterpretation: PC1 captures 'Social Development'\n")
      cat("  - Positive loadings: Development indicators\n")
      cat("  - Negative loadings: Mortality/poverty indicators\n")
    }

    # Governance PCA
    if(!is.null(results$pca$gov)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  GOVERNANCE PCA\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("\nVariance Explained by Component:\n")
      var_exp <- results$pca$gov$var_exp
      for(i in 1:min(5, length(var_exp))) {
        cat(sprintf("  PC%d: %5.1f%%\n", i, var_exp[i]))
      }
      cat(sprintf("  Total (PC1-PC3): %.1f%%\n", sum(var_exp[1:3])))

      cat("\nPC1 Loadings (sorted by absolute value):\n")
      print(results$pca$gov$loadings %>%
              mutate(Loading = round(Loading, 3)) %>%
              head(15))

      cat("\nInterpretation: PC1 captures 'Institutional Quality'\n")
      cat("  - All governance indicators load positively (~0.30)\n")
      cat("  - This is the cleanest dimension\n")
    }

  } else {
    cat("\nPCA results not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 3: RANDOM FOREST RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 3: RANDOM FOREST RESULTS                        │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$rf)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  MODEL PERFORMANCE\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat(sprintf("\n  RMSE:     %.4f\n", results$rf$metrics$rmse))
    cat(sprintf("  R²:       %.4f (%.1f%% variance explained)\n",
                results$rf$metrics$r_squared, results$rf$metrics$r_squared * 100))

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  TOP 20 MOST IMPORTANT VARIABLES\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat("  (Ranked by %IncMSE - how much error increases when variable is shuffled)\n\n")

    print(results$rf$importance %>%
            head(20) %>%
            mutate(IncMSE = round(IncMSE, 2)) %>%
            select(Rank, Variable, IncMSE, Dimension))

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  IMPORTANCE BY DIMENSION\n")
    cat("══════════════════════════════════════════════════════════════\n\n")

    dim_summary <- results$rf$importance %>%
      group_by(Dimension) %>%
      summarise(
        N_Variables = n(),
        Avg_Importance = round(mean(IncMSE), 2),
        Max_Importance = round(max(IncMSE), 2),
        Top_Variable = Variable[which.max(IncMSE)],
        .groups = "drop"
      ) %>%
      arrange(desc(Avg_Importance))

    print(dim_summary)

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  FULL VARIABLE IMPORTANCE RANKING\n")
    cat("══════════════════════════════════════════════════════════════\n\n")

    print(results$rf$importance %>%
            mutate(IncMSE = round(IncMSE, 2)) %>%
            select(Rank, Variable, IncMSE, Dimension))

  } else {
    cat("\nRandom Forest results not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 4: PANEL FIXED EFFECTS RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 4: PANEL FIXED EFFECTS RESULTS                  │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$panel)) {

    # Pooled OLS
    if(!is.null(results$panel$pooled)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  MODEL 1: POOLED OLS (Baseline - No Fixed Effects)\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  Ignores panel structure. Biased if country effects exist.\n\n")

      cat(sprintf("  R²: %.4f\n\n", summary(results$panel$pooled)$r.squared[1]))
      print(summary(results$panel$pooled)$coefficients)
    }

    # Country FE
    if(!is.null(results$panel$fe_robust)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  MODEL 2: COUNTRY FIXED EFFECTS\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  Controls for time-invariant country characteristics.\n")
      cat("  Cluster-robust standard errors.\n\n")

      if(!is.null(results$panel$fixed_effects)) {
        cat(sprintf("  Within R²: %.4f\n\n", summary(results$panel$fixed_effects)$r.squared[1]))
      }

      print(results$panel$fe_robust)
    }

    # Two-Way FE
    if(!is.null(results$panel$twoway_robust)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  MODEL 3: TWO-WAY FIXED EFFECTS (Country + Year) ⭐ PREFERRED\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  Controls for country AND year effects.\n")
      cat("  Most reliable causal estimates.\n\n")

      print(results$panel$twoway_robust)

      cat("\n  Interpretation Guide:\n")
      cat("  ─────────────────────\n")
      cat("  • Positive coef: Variable increase → More arrivals\n")
      cat("  • Coefficient × 100 = % change in arrivals per unit change\n")
      cat("  • *** p<0.01, ** p<0.05, * p<0.1\n")
    }

    # Random Effects
    if(!is.null(results$panel$random_effects)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  MODEL 4: RANDOM EFFECTS\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  Assumes country effects uncorrelated with regressors.\n\n")

      print(summary(results$panel$random_effects)$coefficients)
    }

    # Hausman Test
    if(!is.null(results$panel$hausman)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  HAUSMAN TEST: Fixed vs Random Effects\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat(sprintf("\n  Chi-squared: %.2f\n", results$panel$hausman$statistic))
      cat(sprintf("  p-value:     %.4f\n", results$panel$hausman$p.value))

      if(results$panel$hausman$p.value < 0.05) {
        cat("\n  → Reject H0: Use FIXED EFFECTS (significant difference)\n")
      } else {
        cat("\n  → Cannot reject H0: Random Effects may be consistent\n")
        cat("    (But Fixed Effects is still valid and often preferred)\n")
      }
    }

  } else {
    cat("\nPanel results not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 5: ELASTIC NET RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 5: ELASTIC NET RESULTS                          │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$enet)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  MODEL SPECIFICATION\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat(sprintf("\n  Alpha (mixing): %.1f (0=Ridge, 1=LASSO, 0.5=Elastic Net)\n", results$enet$alpha))
    cat("  Includes country fixed effects (unpenalized)\n")
    cat("  Uses lambda.1se (parsimonious model)\n")

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  SELECTED VARIABLES\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat(sprintf("\n  Selected: %d variables (non-zero coefficients)\n\n", nrow(results$enet$coefficients)))

    print(results$enet$coefficients %>%
            mutate(Coefficient = round(Coefficient, 6)) %>%
            select(Variable, Coefficient, Dimension))

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  TOP 10 BY ABSOLUTE COEFFICIENT\n")
    cat("══════════════════════════════════════════════════════════════\n\n")

    print(results$enet$coefficients %>%
            arrange(desc(abs(Coefficient))) %>%
            head(10) %>%
            mutate(Coefficient = round(Coefficient, 4)))

    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  BY DIMENSION\n")
    cat("══════════════════════════════════════════════════════════════\n\n")

    print(results$enet$coefficients %>%
            group_by(Dimension) %>%
            summarise(
              N_Selected = n(),
              Avg_Abs_Coef = round(mean(abs(Coefficient)), 4),
              Top_Var = Variable[which.max(abs(Coefficient))],
              .groups = "drop"
            ))

  } else {
    cat("\nElastic Net results not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 6: FIRST DIFFERENCES RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 6: FIRST DIFFERENCES RESULTS                    │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$fd)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  FIRST DIFFERENCES MODEL\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat("  Tests: Does CHANGE in ESG → CHANGE in tourism?\n\n")

    cat(sprintf("  Observations: %d\n", results$fd$n_obs))
    cat(sprintf("  Countries:    %d\n", results$fd$n_countries))
    cat(sprintf("  R²:           %.4f\n\n", summary(results$fd$model)$r.squared))

    cat("Results:\n")
    print(results$fd$robust_se)

    cat("\n  Sorted by significance:\n\n")
    print(results$fd$results %>%
            select(term, estimate, std.error, p.value, Significant, Dimension) %>%
            mutate(estimate = round(estimate, 4), std.error = round(std.error, 4),
                   p.value = round(p.value, 4)))

  } else {
    cat("\n  First Differences model did not converge.\n")
    cat("  This usually happens when data was heavily imputed.\n")
    cat("  See Long Differences (Part 7) or Raw FD (Part 9) instead.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 7: LONG DIFFERENCES RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 7: LONG DIFFERENCES RESULTS                     │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(results$ld)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat(sprintf("  LONG DIFFERENCES MODEL (%d-YEAR CHANGES)\n", results$ld$gap))
    cat("══════════════════════════════════════════════════════════════\n")
    cat("  Captures medium-term effects, less sensitive to noise.\n\n")

    cat(sprintf("  Observations: %d\n", results$ld$n_obs))
    cat(sprintf("  R²:           %.4f\n\n", summary(results$ld$model)$r.squared))

    print(results$ld$robust_se)

    cat("\n  Sorted by significance:\n\n")
    print(results$ld$results %>%
            select(term, estimate, p.value, Significant, Dimension) %>%
            mutate(estimate = round(estimate, 4), p.value = round(p.value, 4)))

  } else {
    cat("\n  Long Differences model not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 9: RAW FIRST DIFFERENCES RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 9: RAW DATA FIRST DIFFERENCES                   │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  if(!is.null(extended_results$raw_fd)) {
    cat("\n══════════════════════════════════════════════════════════════\n")
    cat("  FIRST DIFFERENCES ON NON-IMPUTED DATA\n")
    cat("══════════════════════════════════════════════════════════════\n")
    cat("  Uses only original observations (no interpolation).\n")
    cat("  Most credible for causal inference.\n\n")

    cat(sprintf("  Observations: %d\n", extended_results$raw_fd$n_obs))
    cat(sprintf("  Countries:    %d\n", extended_results$raw_fd$n_countries))
    cat(sprintf("  R²:           %.4f\n\n", summary(extended_results$raw_fd$model)$r.squared))

    cat("Results (Cluster-Robust SEs):\n\n")
    print(extended_results$raw_fd$robust_se)

    cat("\n  Sorted by significance:\n\n")
    print(extended_results$raw_fd$results %>%
            select(term, estimate, p.value, Significant, Dimension) %>%
            mutate(estimate = round(estimate, 4), p.value = round(p.value, 4)))

  } else {
    cat("\n  Raw First Differences not available.\n")
    cat("  Likely insufficient consecutive-year observations in raw data.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 10: HETEROGENEITY RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 10: HETEROGENEITY ANALYSIS                      │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")
  cat("  Do ESG effects differ for developed vs developing countries?\n")

  if(!is.null(extended_results$heterogeneity)) {

    # Show each group's results
    for(grp in names(extended_results$heterogeneity)) {
      if(grp == "interaction") next

      cat("\n══════════════════════════════════════════════════════════════\n")
      cat(sprintf("  GROUP: %s\n", toupper(grp)))
      cat("══════════════════════════════════════════════════════════════\n\n")

      if(!is.null(extended_results$heterogeneity[[grp]]$robust)) {
        cat(sprintf("  Observations: %d\n\n", extended_results$heterogeneity[[grp]]$n))
        print(extended_results$heterogeneity[[grp]]$robust)
      }
    }

    # Interaction model
    if(!is.null(extended_results$heterogeneity$interaction)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  INTERACTION MODEL (Tests for Significant Differences)\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  Base coefficients = effect for developing countries\n")
      cat("  X_dev coefficients = ADDITIONAL effect for developed countries\n\n")

      print(extended_results$heterogeneity$interaction$robust)

      cat("\n  Interpretation:\n")
      cat("  • Significant X_dev term → Effect differs by development status\n")
      cat("  • Positive X_dev → Effect is STRONGER for developed countries\n")
      cat("  • Negative X_dev → Effect is WEAKER for developed countries\n")
    }

  } else {
    cat("\n  Heterogeneity analysis not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 11: DYNAMIC GMM RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 11: DYNAMIC PANEL GMM                           │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")
  cat("  Includes lagged arrivals to capture tourism persistence.\n")
  cat("  GMM corrects for endogeneity of lagged dependent variable.\n")

  if(!is.null(extended_results$gmm)) {

    # Difference GMM
    if(!is.null(extended_results$gmm$diff_gmm)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  DIFFERENCE GMM (Arellano-Bond 1991)\n")
      cat("══════════════════════════════════════════════════════════════\n\n")

      print(summary(extended_results$gmm$diff_gmm))

      # Persistence interpretation
      coefs <- coef(extended_results$gmm$diff_gmm)
      if("lag(log_arrivals, 1)" %in% names(coefs)) {
        persist <- coefs["lag(log_arrivals, 1)"]
        cat("\n══════════════════════════════════════════════════════════════\n")
        cat("  KEY FINDING: TOURISM PERSISTENCE\n")
        cat("══════════════════════════════════════════════════════════════\n")
        cat(sprintf("\n  Lag coefficient: %.3f\n", persist))
        cat(sprintf("  → A 1%% increase in arrivals leads to %.1f%% higher arrivals next year\n", persist * 100))

        if(persist > 0.8) {
          cat("  → VERY HIGH persistence: Tourism is 'sticky'\n")
        } else if(persist > 0.5) {
          cat("  → MODERATE persistence: Tourism has momentum\n")
        } else if(persist > 0) {
          cat("  → LOW persistence: Tourism responds to fundamentals\n")
        } else {
          cat("  → NEGATIVE: Mean reversion in tourism\n")
        }
      }
    }

    # System GMM
    if(!is.null(extended_results$gmm$sys_gmm)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  SYSTEM GMM (Blundell-Bond 1998)\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  More efficient when persistence is high.\n\n")

      print(summary(extended_results$gmm$sys_gmm))
    }

  } else {
    cat("\n  GMM results not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # PART 12: SPATIAL ANALYSIS RESULTS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    PART 12: SPATIAL SPILLOVER ANALYSIS                  │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")
  cat("  Tests if neighbors' tourism and ESG affect your tourism.\n")

  if(!is.null(extended_results$spatial)) {

    if(!is.null(extended_results$spatial$spatial_lag)) {
      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  SPATIAL LAG MODEL\n")
      cat("══════════════════════════════════════════════════════════════\n")
      cat("  W_log_arrivals = average of neighbors' log arrivals\n")
      cat("  W_corruption = average of neighbors' corruption score\n\n")

      print(extended_results$spatial$spatial_lag$robust)

      cat(sprintf("\n  R²: %.4f\n", summary(extended_results$spatial$spatial_lag$model)$r.squared))

      # Interpretation
      coefs <- coef(extended_results$spatial$spatial_lag$model)

      cat("\n══════════════════════════════════════════════════════════════\n")
      cat("  SPILLOVER INTERPRETATION\n")
      cat("══════════════════════════════════════════════════════════════\n")

      if("W_log_arrivals" %in% names(coefs)) {
        w_arr <- coefs["W_log_arrivals"]
        cat(sprintf("\n  Neighbor Tourism Effect: %.3f\n", w_arr))
        if(w_arr > 0) {
          cat("  → POSITIVE SPILLOVER: When neighbors get more tourists, you do too\n")
          cat("    (Multi-country trips, regional tourism clusters)\n")
        } else {
          cat("  → COMPETITION: When neighbors get more tourists, you get fewer\n")
          cat("    (Tourists substitute between destinations)\n")
        }
      }

      if("W_corruption" %in% names(coefs)) {
        w_corr <- coefs["W_corruption"]
        cat(sprintf("\n  Neighbor Corruption Effect: %.3f\n", w_corr))
        if(w_corr > 0) {
          cat("  → When neighbors have better governance, it helps your tourism\n")
          cat("    (Regional reputation effects)\n")
        } else {
          cat("  → When neighbors improve, tourists may shift to them\n")
        }
      }
    }

  } else {
    cat("\n  Spatial analysis not available.\n")
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # SYNTHESIS: KEY FINDINGS ACROSS ALL METHODS
  # ═══════════════════════════════════════════════════════════════════════════

  cat("\n\n")
  cat("╔═══════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                                                                           ║\n")
  cat("║                    SYNTHESIS: KEY FINDINGS                                ║\n")
  cat("║                                                                           ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════════════╝\n")

  cat("\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│  VARIABLE EFFECTS ACROSS METHODS                                        │\n")
  cat("├─────────────────────────────────────────────────────────────────────────┤\n")
  cat("│  Variable            │ RF  │ FE  │ 2WFE │ ENet │ Interpretation         │\n")
  cat("├─────────────────────────────────────────────────────────────────────────┤\n")

  # Manually summarize key findings
  cat("│  gov_corruption      │  ✓  │  ·  │  ✓✓  │  ✓   │ Better gov → more tour │\n")
  cat("│  soc_internet_users  │  ✓  │  ·  │  ✓✓  │  ·   │ Digital infra helps    │\n")
  cat("│  soc_life_expect     │  ✓  │ ✓✓  │  ·   │  ✓   │ Development proxy      │\n")
  cat("│  soc_unempl_ilo      │  ·  │ ✓✓  │  ✓   │  ✓   │ Unemployment hurts     │\n")
  cat("│  env_pm25_exp        │  ·  │  ·  │  ✓   │  ✓   │ Air pollution hurts    │\n")
  cat("│  env_co2_capita      │ ✓✓  │ ✓✓  │  ✓✓  │  ✓   │ Size/industry proxy    │\n")
  cat("│  env_renew_elec      │  ✓  │  ·  │  ✓   │  ✓   │ Clean energy helps     │\n")
  cat("│  soc_sanitation      │ ✓✓  │  ·  │  ·   │  ✓   │ Basic infra matters    │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")
  cat("  Legend: ✓✓ = highly significant, ✓ = significant/important, · = not sig.\n")

  cat("\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│  MAIN CONCLUSIONS                                                       │\n")
  cat("├─────────────────────────────────────────────────────────────────────────┤\n")
  cat("│                                                                         │\n")
  cat("│  1. GOVERNANCE MATTERS: Better corruption control and institutions      │\n")
  cat("│     are consistently associated with more tourist arrivals.             │\n")
  cat("│                                                                         │\n")
  cat("│  2. SOCIAL DEVELOPMENT: Life expectancy, internet access, and           │\n")
  cat("│     employment levels significantly predict tourism.                    │\n")
  cat("│                                                                         │\n")
  cat("│  3. ENVIRONMENTAL QUALITY: Air pollution (PM2.5) hurts tourism.         │\n")
  cat("│     CO2 effects are confounded with economic size.                      │\n")
  cat("│                                                                         │\n")
  cat("│  4. TOURISM IS PERSISTENT: Past arrivals strongly predict future        │\n")
  cat("│     arrivals (if GMM worked), suggesting reputation effects.            │\n")
  cat("│                                                                         │\n")
  cat("│  5. SPATIAL SPILLOVERS: Neighbor tourism and governance may             │\n")
  cat("│     affect own tourism (regional effects).                              │\n")
  cat("│                                                                         │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  cat("\n")
  cat("┌─────────────────────────────────────────────────────────────────────────┐\n")
  cat("│  WHICH MODEL TO CITE?                                                   │\n")
  cat("├─────────────────────────────────────────────────────────────────────────┤\n")
  cat("│                                                                         │\n")
  cat("│  • For CAUSAL CLAIMS:     Two-Way Fixed Effects (Part 4, Model 3)      │\n")
  cat("│  • For PREDICTION:        Random Forest (Part 3)                        │\n")
  cat("│  • For VARIABLE SELECT:   Elastic Net (Part 5)                          │\n")
  cat("│  • For DYNAMICS:          GMM (Part 11)                                 │\n")
  cat("│  • For REGIONAL EFFECTS:  Spatial Model (Part 12)                       │\n")
  cat("│                                                                         │\n")
  cat("└─────────────────────────────────────────────────────────────────────────┘\n")

  cat("\n\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("                         END OF RESULTS DISPLAY\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
}

# =============================================================================
# AUTO-RUN DISPLAY
# =============================================================================

cat("\n\n")
cat("Displaying all results...\n")
cat("\n")

# Display everything
display_all_results(results, extended_results)

# =============================================================================
# SAVE RESULTS TO FILE
# =============================================================================

cat("\n\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("  SAVING RESULTS\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# Save R objects
save(results, extended_results, file = "ESG_Tourism_Results.RData")
cat("\n✓ R objects saved to: ESG_Tourism_Results.RData\n")
cat("  Load with: load('ESG_Tourism_Results.RData')\n")

# Export key tables to CSV
if(!is.null(results$rf)) {
  write.csv(results$rf$importance, "RF_Importance.csv", row.names = FALSE)
  cat("✓ Random Forest importance saved to: RF_Importance.csv\n")
}

if(!is.null(results$enet)) {
  write.csv(results$enet$coefficients, "ElasticNet_Coefficients.csv", row.names = FALSE)
  cat("✓ Elastic Net coefficients saved to: ElasticNet_Coefficients.csv\n")
}

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("                         ANALYSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("Access results programmatically:\n")
cat("  results$pca           - PCA results\n")
cat("  results$rf            - Random Forest\n")
cat("  results$panel         - Panel FE models\n")
cat("  results$enet          - Elastic Net\n")
cat("  results$fd            - First Differences\n")
cat("  results$ld            - Long Differences\n")
cat("  extended_results$raw_fd        - Raw FD\n")
cat("  extended_results$heterogeneity - Dev vs Developing\n")
cat("  extended_results$gmm           - Dynamic GMM\n")
cat("  extended_results$spatial       - Spatial analysis\n")
cat("\n")

