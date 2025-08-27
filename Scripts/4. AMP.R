# =====================================
# Script: AMP Method Lifetime Risk Calculator
# Author: Ke Pang
# Date: 2025-08-12
# Purpose: Calculate lifetime risk of cancer using Adjusted for Multiple Primaries (AMP) method
# Reference: Sasieni P, Shelton J, Ormiston-Smith N, et al. Br J Cancer 2011;105(3):460-5
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
CANCER_TYPE <- "colorectum"  # Options: "colorectum" or "rectum"
DISTRIBUTION_ASSUMPTION <- "Poisson"  # Options: "Binomial" or "Poisson"

# File paths
INPUT_FILE <- file.path("Data", CANCER_TYPE, paste0("2022_", CANCER_TYPE, "_data_all_regions.csv")) 
OUTPUT_FILE <- file.path("Data", paste0("lifetime_risk_", CANCER_TYPE, ".csv"))

#2. Print configuration ----
cat("=== AMP LIFETIME RISK CALCULATION ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Distribution Assumption:", DISTRIBUTION_ASSUMPTION, "\n")
cat("Input File:", INPUT_FILE, "\n")
cat("Output File:", OUTPUT_FILE, "\n")
cat("=====================================\n\n")

#3. File validation ----
cat("Checking file availability...\n")
if (!file.exists(INPUT_FILE)) {
  stop("Input file not found: ", INPUT_FILE)
}
cat("Input file found! ✓\n\n")

#4. Data loading and validation ----
cat("Loading data...\n")
data <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)
cat("Data loaded:", nrow(data), "records\n")

# Validate required columns
required_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Ri", "Di", "Ni", "Mi")
missing_cols <- setdiff(required_cols, colnames(data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Sort data by region and age
data <- data %>%
  arrange(ISOcode, AgeStart)

cat("Data validation passed ✓\n\n")

# ##0-14
# data_collapsed <- data %>%
#   group_by(ISOcode) %>%
#   # 先挑出前三个年龄段
#   slice(1:3) %>%
#   summarise(
#     AgeStart = 0,
#     AgeEnd = 14,
#     Ri = sum(Ri, na.rm = TRUE),
#     Di = sum(Di, na.rm = TRUE),
#     Ni = sum(Ni, na.rm = TRUE),
#     Mi = sum(Mi, na.rm = TRUE),
#     .groups = "drop"
#   )
# data <- data %>%
#   group_by(ISOcode) %>%
#   mutate(row_id = row_number()) %>%
#   filter(row_id > 3) %>%
#   select(-row_id) %>%
#   bind_rows(data_collapsed) %>%
#   arrange(ISOcode, AgeStart)
# 

#5. AMP method calculation function ----
calculate_amp_lifetime_risk <- function(region_data, distribution = "Poisson") {
  
  # Prepare data
  region_data <- region_data %>%
    arrange(AgeStart) %>%
    mutate(
      # Age interval width
      wi = case_when(
        AgeEnd == 999 ~ 5,  
        TRUE ~ AgeEnd - AgeStart + 1
      ),
      # Check for problematic values
      valid_data = (Ri + Mi - Di) > 0 & Ni > 0
    ) %>%
    filter(valid_data)  # Remove invalid rows
  
  if (nrow(region_data) == 0) {
    return(data.frame(
      ISOcode = unique(region_data$ISOcode)[1],
      lifetime_risk = NA,
      variance = NA,
      std_error = NA,
      ci_lower = NA,
      ci_upper = NA,
      note = "No valid data"
    ))
  }
  
  n_groups <- nrow(region_data)
  
  # Initialize vectors for calculations
  lambda_c <- numeric(n_groups)      # Cancer incidence rate
  S0_star <- numeric(n_groups)       # Probability of being alive and cancer-free
  Sx <- numeric(n_groups)           # Survival probability term
  
  # Variance components
  var_lambda_c <- numeric(n_groups)
  var_S0_star <- numeric(n_groups)
  var_Sx <- numeric(n_groups)
  
  # Calculate cumulative hazard for S0_star
  cumulative_hazard <- 0
  
  for (i in 1:n_groups) {
    # Extract values for current age group
    Ri <- region_data$Ri[i]
    Di <- region_data$Di[i]
    Mi <- region_data$Mi[i]
    Ni <- region_data$Ni[i]
    wi <- region_data$wi[i]
    
    # Calculate lambda_c (cancer incidence rate)
    denominator <- Ri + Mi - Di
    lambda_c[i] <- Ri / denominator
    
    # Calculate S0_star (probability of being alive and cancer-free)
    if (i == 1) {
      S0_star[i] <- 1  # At birth, probability is 1
    } else {
      S0_star[i] <- exp(-cumulative_hazard)
    }
    
    # Calculate Sx term
    if (region_data$AgeEnd[i] == 999) {
      # Special case for final age group (85+)
      Sx[i] <- 1  # For final age group, this becomes 1
    } else {
      # Regular age groups
      hazard_rate <- (wi/Ni) * denominator
      Sx[i] <- 1 - exp(-hazard_rate)
    }
    
    # Update cumulative hazard for next iteration
    cumulative_hazard <- cumulative_hazard + denominator/Ni
    
    # Calculate variance components based on distribution assumption
    pci <- lambda_c[i]
    poi <- denominator / Ni
    
    if (distribution == "Binomial") {
      # Binomial variance formulas
      var_lambda_c[i] <- (pci * (1 - pci)) / denominator
      
      # Variance of log(S0_star)
      if (i == 1) {
        var_log_S0_star <- 0
      } else {
        var_log_S0_star <- sum(sapply(1:(i-1), function(j) {
          poj <- (region_data$Ri[j] + region_data$Mi[j] - region_data$Di[j]) / region_data$Ni[j]
          (poj * (1 - poj)) / region_data$Ni[j]
        }))
      }
      var_S0_star[i] <- (S0_star[i])^2 * var_log_S0_star
      
      # Variance of Sx
      if (region_data$AgeEnd[i] == 999) {
        var_Sx[i] <- 0  # No variance for final age group
      } else {
        pxi <- poi
        Sx_prime <- exp(-(wi/Ni) * denominator)
        var_log_Sx_prime <- (wi^2 * pxi * (1 - pxi)) / Ni
        var_Sx[i] <- (Sx_prime^2) * var_log_Sx_prime
      }
      
    } else {  # Poisson
      # Poisson variance formulas
      var_lambda_c[i] <- pci / denominator
      
      # Variance of log(S0_star)
      if (i == 1) {
        var_log_S0_star <- 0
      } else {
        var_log_S0_star <- sum(sapply(1:(i-1), function(j) {
          poj <- (region_data$Ri[j] + region_data$Mi[j] - region_data$Di[j]) / region_data$Ni[j]
          poj / region_data$Ni[j]
        }))
      }
      var_S0_star[i] <- (S0_star[i])^2 * var_log_S0_star
      
      # Variance of Sx
      if (region_data$AgeEnd[i] == 999) {
        var_Sx[i] <- 0  # No variance for final age group
      } else {
        pxi <- poi
        Sx_prime <- exp(-(wi/Ni) * denominator)
        var_log_Sx_prime <- (wi^2 * pxi) / Ni
        var_Sx[i] <- (Sx_prime^2) * var_log_Sx_prime
      }
    }
  }
  
  # Calculate lifetime risk S
  S <- sum(lambda_c * S0_star * Sx)
  
  # Calculate variance using the complex formula from the document
  total_variance <- 0
  
  for (i in 1:n_groups) {
    # Expected values
    E_lambda_c <- lambda_c[i]
    E_S0_star <- S0_star[i]
    E_Sx <- Sx[i]
    
    # Variance components
    Var_lambda_c <- var_lambda_c[i]
    Var_S0_star <- var_S0_star[i]
    Var_Sx <- var_Sx[i]
    
    # Calculate E(λc²(S0*)²Sx²) using independence assumption
    E_lambda_c_sq_S0_star_sq_Sx_sq <- 
      (Var_lambda_c + E_lambda_c^2) * 
      (Var_S0_star + E_S0_star^2) * 
      (Var_Sx + E_Sx^2)
    
    # Calculate [E(λcS0*Sx)]²
    E_lambda_c_S0_star_Sx_sq <- (E_lambda_c * E_S0_star * E_Sx)^2
    
    # Add to total variance
    total_variance <- total_variance + 
      E_lambda_c_sq_S0_star_sq_Sx_sq - E_lambda_c_S0_star_Sx_sq
  }
  
  # Calculate confidence intervals
  std_error <- sqrt(total_variance)
  ci_lower <- max(0, S - 1.96 * std_error)  # Ensure non-negative
  ci_upper <- min(1, S + 1.96 * std_error)  # Ensure not > 1
  
  return(data.frame(
    ISOcode = region_data$ISOcode[1],
    lifetime_risk = S,
    variance = total_variance,
    std_error = std_error,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_age_groups = n_groups,
    note = "Success"
  ))
}

#6. Calculate lifetime risk for all regions ----
cat("Calculating lifetime risk using AMP method...\n")

# Get unique regions
regions <- unique(data$ISOcode)
cat("Processing", length(regions), "regions...\n")

# Calculate for each region
results <- data %>%
  group_by(ISOcode) %>%
  do(calculate_amp_lifetime_risk(., distribution = DISTRIBUTION_ASSUMPTION)) %>%
  ungroup()

cat("Calculations completed for", nrow(results), "regions\n\n")

#7. Results summary ----
cat("=== LIFETIME RISK RESULTS ===\n")

# Show summary statistics
valid_results <- results %>% filter(!is.na(lifetime_risk))

if (nrow(valid_results) > 0) {
  cat("Valid results:", nrow(valid_results), "regions\n")
  cat("Mean lifetime risk:", round(mean(valid_results$lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
  cat("Median lifetime risk:", round(median(valid_results$lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
  cat("Range:", round(min(valid_results$lifetime_risk, na.rm = TRUE) * 100, 4), "% -", 
      round(max(valid_results$lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
} else {
  cat("No valid results calculated\n")
}

# Show failed calculations
failed_results <- results %>% filter(is.na(lifetime_risk))
if (nrow(failed_results) > 0) {
  cat("Failed calculations:", nrow(failed_results), "regions\n")
}

cat("=============================\n\n")

#8. Save results ----
cat("Saving results...\n")

# Create output directory if needed
if (!dir.exists(dirname(OUTPUT_FILE))) {
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

# Add percentage columns and format results
final_results <- results %>%
  mutate(
    lifetime_risk_percent = round(lifetime_risk * 100, 6),
    std_error_percent = round(std_error * 100, 6),
    ci_lower_percent = round(ci_lower * 100, 6),
    ci_upper_percent = round(ci_upper * 100, 6),
    cancer_type = CANCER_TYPE,
    distribution_method = DISTRIBUTION_ASSUMPTION
  ) %>%
  select(ISOcode, cancer_type, distribution_method, n_age_groups,
         lifetime_risk, lifetime_risk_percent, 
         variance, std_error, std_error_percent,
         ci_lower, ci_upper, ci_lower_percent, ci_upper_percent, note) %>%
  arrange(desc(lifetime_risk))

# Save results
write.csv(final_results, OUTPUT_FILE, row.names = FALSE)

cat("Results saved to:", OUTPUT_FILE, "\n")

# Show top 10 results
cat("\nTop 10 regions by lifetime risk:\n")
print(head(final_results %>% 
             filter(!is.na(lifetime_risk)) %>%
             select(ISOcode, lifetime_risk_percent, ci_lower_percent, ci_upper_percent), 
           10))

