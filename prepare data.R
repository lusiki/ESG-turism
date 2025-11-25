
# =============================================================================
# SCRIPT 1: DATA LOADING, RENAMING & CLEANING
# =============================================================================
#
# Purpose: Load raw ESG-Tourism data, rename variables, and clean thoroughly
# Output:  A clean dataset (dta_clean) ready for analysis
#
# Author:  [Luka Sikic]
# Date:    [11/25/2025]
#
# =============================================================================

# --- LOAD REQUIRED PACKAGES ---
required_packages <- c(
  "readxl",     # Read Excel files
  "tidyverse", # Data manipulation
  "janitor",    # Clean column names
  "zoo",        # Time series interpolation
  "skimr"       # Data summaries
)

# Install if missing
#new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
#if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(required_packages, library, character.only = TRUE)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║     SCRIPT 1: DATA LOADING, RENAMING & CLEANING              ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")

# =============================================================================
# STEP 1: LOAD RAW DATA
# =============================================================================

cat("\n--- STEP 1: Loading Data ---\n")

# UPDATE THIS PATH TO YOUR FILE LOCATION
dta_raw <- read_excel("dta_.xlsx")

cat("Loaded:", nrow(dta_raw), "rows x", ncol(dta_raw), "columns\n")

# =============================================================================
# STEP 2: RENAME VARIABLES
# =============================================================================

cat("\n--- STEP 2: Renaming Variables ---\n")

rename_map <- c(
  # 1. DEPENDENT VARIABLES (TOURISM & DEMOGRAPHICS)
  "tour_arrivals"      = "ARRIVALS",
  "tour_nights"        = "Nights",
  "tour_avg_stay"      = "AS",
  "population"         = "POP",
  "tour_night_pop"     = "NIGHTPOP",
  "tour_arrival_pop"   = "ARRIVALPOP",
  "area_km2"           = "Area",
  "tour_arr_density"   = "ARR_AREA",
  "tour_night_density" = "TN_AREA",

  # 2. ENVIRONMENT (E Variables)
  "env_co2_cba"        = "E2",
  "env_co2_gdp"        = "E3",
  "env_co2_capita"     = "E4",
  "env_co2_pba"        = "E5",
  "env_nd_gain_idx"    = "E6",
  "env_pm25_exp"       = "E7",
  "env_renew_elec"     = "E8",
  "env_renew_cons"     = "E9",

  # 3. GOVERNANCE & ECONOMY (G Variables)
  "gov_cpi_aop"        = "G1",
  "gov_cpi_eop"        = "G2",
  "gov_inflation"      = "G3",
  "gov_corruption"     = "G4",
  "gov_account_bal"    = "G5",
  "gov_export_price"   = "G6",
  "gov_gdp_capita"     = "G8",
  "gov_gdp_const"      = "G9",
  "gov_gdp_growth"     = "G10",
  "gov_gdp_ppp_const"  = "G11",
  "gov_gdp_ppp_curr"   = "G12",
  "gov_effectiveness"  = "G13",
  "gov_pol_stability"  = "G15",
  "gov_real_gdp_gr"    = "G17",
  "gov_reg_quality"    = "G18",
  "gov_rd_expend"      = "G19",
  "gov_researchers"    = "G20",
  "gov_rule_law"       = "G21",
  "gov_terms_trade"    = "G22",
  "gov_voice_acc"      = "G23",
  "gov_reer"           = "REER",
  "gov_esg_score"      = "ESG",

  # 4. SOCIAL (S Variables)
  "soc_age_dependency" = "S1",
  "soc_edu_compul_yrs" = "S2",
  "soc_female_mgrs"    = "S3",
  "soc_broadband"      = "S4",
  "soc_gini_index"     = "S5",
  "soc_immun_dpt"      = "S6",
  "soc_immun_hepb"     = "S7",
  "soc_immun_measles"  = "S8",
  "soc_mort_infant"    = "S9",
  "soc_internet_users" = "S10",
  "soc_life_expect"    = "S11",
  "soc_edu_low_sec"    = "S12",
  "soc_mort_maternal"  = "S13",
  "soc_mort_general"   = "S14",
  "soc_mort_road"      = "S15",
  "soc_mort_poison"    = "S16",
  "soc_mort_u5_tot"    = "S17",
  "soc_mort_u5_fem"    = "S18",
  "soc_mort_u5_male"   = "S19",
  "soc_mort_neonatal"  = "S20",
  "soc_net_migration"  = "S21",
  "soc_open_defec"     = "S22",
  "soc_water_basic"    = "S23",
  "soc_sanit_basic"    = "S24",
  "soc_water_safe"     = "S25",
  "soc_sanit_safe_rur" = "S26",
  "soc_sanit_safe_urb" = "S27",
  "soc_pov_190"        = "S28",
  "soc_pov_national"   = "S29",
  "soc_edu_pre_dur"    = "S30",
  "soc_edu_pri_dur"    = "S31",
  "soc_sanit_improved" = "S32",
  "soc_women_parl"     = "S33",
  "soc_health_exp"     = "S34",
  "soc_gpi_primary"    = "S35",
  "soc_gpi_pri_sec"    = "S36",
  "soc_gpi_secondary"  = "S37",
  "soc_edu_sec_dur"    = "S38",
  "soc_youth_idle"     = "S39",
  "soc_suicide_rate"   = "S40",
  "soc_unempl_gen"     = "S41",
  "soc_unempl_ilo"     = "S42",
  "soc_unempl_nat"     = "S43",
  "soc_unempl_y_ilo"   = "S44",
  "soc_unempl_y_nat"   = "S45"
)

dta <- dta_raw %>%
  rename(any_of(rename_map))

cat("Variables renamed successfully.\n")

# =============================================================================
# STEP 3: INITIAL DIAGNOSTICS
# =============================================================================

cat("\n--- STEP 3: Initial Diagnostics ---\n")

cat("\nPanel Structure:\n")
cat("  Countries:", length(unique(dta$Country)), "\n")
cat("  Years:", paste(range(dta$Year, na.rm = TRUE), collapse = " - "), "\n")
cat("  Total obs:", nrow(dta), "\n")

# Check target variable
cat("\nTarget Variable (tour_arrivals):\n")
cat("  Available:", sum(!is.na(dta$tour_arrivals)), "\n")
cat("  Missing:", sum(is.na(dta$tour_arrivals)), "\n")

# Count variables by dimension
n_env <- sum(str_starts(names(dta), "env_"))
n_gov <- sum(str_starts(names(dta), "gov_"))
n_soc <- sum(str_starts(names(dta), "soc_"))
n_tour <- sum(str_starts(names(dta), "tour_"))

cat("\nVariables by Dimension:\n")
cat("  Environment:", n_env, "\n")
cat("  Governance:", n_gov, "\n")
cat("  Social:", n_soc, "\n")
cat("  Tourism:", n_tour, "\n")

# =============================================================================
# STEP 4: IDENTIFY PROBLEMATIC VARIABLES
# =============================================================================

cat("\n--- STEP 4: Identifying Problematic Variables ---\n")

# 4a. Variables with >60% missing (will be dropped)
missing_pct <- dta %>%
  summarise(across(where(is.numeric), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Pct_Missing") %>%
  arrange(desc(Pct_Missing))

high_missing_vars <- missing_pct %>% filter(Pct_Missing > 60) %>% pull(Variable)

if(length(high_missing_vars) > 0) {
  cat("\nVariables with >60% missing (will be DROPPED):\n")
  print(filter(missing_pct, Pct_Missing > 60))
} else {
  cat("\nNo variables with >60% missing. Good!\n")
}

# 4b. Zero-variance columns (will cause PCA to fail)
numeric_cols <- dta %>% select(where(is.numeric)) %>% names()
zero_var_cols <- c()

for(col in numeric_cols) {
  v <- var(dta[[col]], na.rm = TRUE)
  if(is.na(v) || v < 1e-10) {
    zero_var_cols <- c(zero_var_cols, col)
  }
}

if(length(zero_var_cols) > 0) {
  cat("\nZero-variance columns (will be DROPPED):\n")
  cat(" ", paste(zero_var_cols, collapse = ", "), "\n")
} else {
  cat("\nNo zero-variance columns. Good!\n")
}

# =============================================================================
# STEP 5: DROP PROBLEMATIC VARIABLES
# =============================================================================

cat("\n--- STEP 5: Dropping Problematic Variables ---\n")

vars_to_drop <- unique(c(high_missing_vars, zero_var_cols))

if(length(vars_to_drop) > 0) {
  cat("Dropping", length(vars_to_drop), "variables:\n")
  cat(" ", paste(head(vars_to_drop, 10), collapse = ", "))
  if(length(vars_to_drop) > 10) cat(" ... and", length(vars_to_drop) - 10, "more")
  cat("\n")

  dta <- dta %>% select(-any_of(vars_to_drop))
} else {
  cat("No variables to drop.\n")
}

# =============================================================================
# STEP 6: IMPUTE MISSING VALUES
# =============================================================================

cat("\n--- STEP 6: Imputing Missing Values ---\n")

# Strategy:
# 1. Within-country time interpolation (max 3-year gaps)
# 2. Forward/backward fill for edges (max 2 years)
# 3. Year-specific median for remaining gaps

dta_clean <- dta %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%

  # Step 6a: Linear interpolation within country (max 3-year gap)
  mutate(across(
    where(is.numeric),
    ~zoo::na.approx(., x = Year, na.rm = FALSE, maxgap = 3)
  )) %>%

  # Step 6b: Forward/backward fill for edges
  fill(where(is.numeric), .direction = "downup") %>%

  ungroup() %>%

  # Step 6c: Year-specific median for remaining missing
  group_by(Year) %>%
  mutate(across(
    where(is.numeric),
    ~ifelse(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  ungroup()

# Check remaining missing
remaining_missing <- sum(is.na(dta_clean %>% select(where(is.numeric))))
cat("Missing values after imputation:", remaining_missing, "\n")

# Final fallback: global median
if(remaining_missing > 0) {
  dta_clean <- dta_clean %>%
    mutate(across(
      where(is.numeric),
      ~ifelse(is.na(.), median(., na.rm = TRUE), .)
    ))
  cat("Applied global median fallback.\n")
}

# =============================================================================
# STEP 7: OUTLIER TREATMENT (WINSORIZATION)
# =============================================================================

cat("\n--- STEP 7: Treating Outliers ---\n")

# Winsorize at 1st and 99th percentile
# Exclude identifiers and tourism targets from winsorization

vars_to_winsorize <- dta_clean %>%
  select(starts_with("env_"), starts_with("gov_"), starts_with("soc_")) %>%
  select(where(is.numeric)) %>%
  names()

dta_clean <- dta_clean %>%
  mutate(across(
    all_of(vars_to_winsorize),
    ~{
      lower <- quantile(., 0.01, na.rm = TRUE)
      upper <- quantile(., 0.99, na.rm = TRUE)
      pmax(pmin(., upper), lower)
    }
  ))

cat("Outliers winsorized at 1st/99th percentile for", length(vars_to_winsorize), "variables.\n")

# =============================================================================
# STEP 8: CREATE DERIVED VARIABLES
# =============================================================================

cat("\n--- STEP 8: Creating Derived Variables ---\n")

dta_clean <- dta_clean %>%
  mutate(
    # Log of arrivals (useful for regression)
    log_arrivals = log(tour_arrivals + 1),

    # Arrivals per capita (if not already present)
    arrivals_per_capita = ifelse(population > 0, tour_arrivals / population * 1000, NA),

    # Tourism intensity (arrivals per km2)
    arrivals_per_km2 = ifelse(area_km2 > 0, tour_arrivals / area_km2, NA)
  )

cat("Created: log_arrivals, arrivals_per_capita, arrivals_per_km2\n")

# =============================================================================
# STEP 9: FINAL VALIDATION
# =============================================================================

cat("\n--- STEP 9: Final Validation ---\n")

cat("\nFinal Dataset Structure:\n")
cat("  Rows:", nrow(dta_clean), "\n")
cat("  Columns:", ncol(dta_clean), "\n")
cat("  Countries:", length(unique(dta_clean$Country)), "\n")
cat("  Years:", paste(range(dta_clean$Year), collapse = " - "), "\n")

# Check for any remaining issues
final_missing <- dta_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0)

if(nrow(final_missing) > 0) {
  cat("\nWarning: Some missing values remain:\n")
  print(final_missing)
} else {
  cat("\nNo missing values in numeric columns. ✓\n")
}

# Check variance
final_zero_var <- dta_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~var(., na.rm = TRUE))) %>%
  pivot_longer(everything()) %>%
  filter(is.na(value) | value < 1e-10)

if(nrow(final_zero_var) > 0) {
  cat("\nWarning: Zero-variance columns remain:\n")
  print(final_zero_var)
} else {
  cat("No zero-variance columns. ✓\n")
}

# =============================================================================
# STEP 10: SAVE CLEAN DATA
# =============================================================================

cat("\n--- STEP 10: Saving Clean Data ---\n")

# Save as RDS (preserves R data types)
saveRDS(dta_clean, "dta_clean.rds")
cat("Saved: dta_clean.rds\n")

# Also save as CSV for portability
write_csv(dta_clean, "dta_clean.csv")
cat("Saved: dta_clean.csv\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║                    CLEANING COMPLETE                         ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("Clean dataset stored in: dta_clean\n")
cat("\n")
cat("Next step: Run Script 2 (Analysis) with:\n")
cat("  source('02_esg_tourism_analysis.R')\n")
cat("\n")

# Print quick summary
cat("--- Quick Summary ---\n")
skim_summary <- dta_clean %>%
  select(tour_arrivals, starts_with("env_"), starts_with("gov_"), starts_with("soc_")) %>%
  select(1:10) %>%  # First 10 variables
  skim()

print(skim_summary)
# CHECK THE NEW STRUCTURE
colnames(dta)

# This produces a rich text summary in your console
skim(dta)






