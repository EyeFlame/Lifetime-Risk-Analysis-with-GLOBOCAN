# =====================================
# Script: AMP Method Dual Lifetime Risk Calculator
# Author: Ke Pang (Modified)
# Date: 2025-08-28
# Purpose: Calculate both LR of Developing (LRI) and LR of Dying (LRM) using AMP method
# Reference: Sasieni P, Shelton J, Ormiston-Smith N, et al. Br J Cancer 2011;105(3):460-5
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
CANCER_TYPE <- "colon"  # Options: "colorectum" or "rectum"
DISTRIBUTION_ASSUMPTION <- "Poisson"  # Options: "Binomial" or "Poisson"
RISK_TYPE <- "both"  # Options: "LRI", "LRM", or "both"

# File paths
INPUT_FILE <- file.path("Data", CANCER_TYPE, paste0("2022_", CANCER_TYPE, "_data_all_regions.csv")) 
OUTPUT_FILE <- file.path("Data", paste0("lifetime_risk_", CANCER_TYPE, "_", tolower(RISK_TYPE), ".csv"))

#2. Print configuration ----
cat("=== AMP DUAL LIFETIME RISK CALCULATION ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Distribution Assumption:", DISTRIBUTION_ASSUMPTION, "\n")
cat("Risk Type:", RISK_TYPE, "\n")
cat("  - LRI: Lifetime Risk of Developing (Incidence)\n")
cat("  - LRM: Lifetime Risk of Dying (Mortality)\n")
cat("Input File:", INPUT_FILE, "\n")
cat("Output File:", OUTPUT_FILE, "\n")
cat("==========================================\n\n")

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

#5. Enhanced AMP method calculation function ----
calculate_amp_lifetime_risk <- function(region_data, distribution = "Poisson", risk_type = "both") {
  
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
      valid_data_LRI = (Ri + Mi - Di) > 0 & Ni > 0,  # For LRI calculation
      valid_data_LRM = Mi > 0 & Ni > 0               # For LRM calculation
    )
  
  # Create separate datasets for LRI and LRM calculations
  region_data_LRI <- region_data %>% filter(valid_data_LRI)
  region_data_LRM <- region_data %>% filter(valid_data_LRM)
  
  # Function to calculate single risk type
  calculate_single_risk <- function(data_subset, calc_type) {
    if (nrow(data_subset) == 0) {
      return(list(
        lifetime_risk = NA,
        variance = NA,
        std_error = NA,
        ci_lower = NA,
        ci_upper = NA,
        n_age_groups = 0,
        note = paste("No valid data for", calc_type)
      ))
    }
    
    n_groups <- nrow(data_subset)
    
    # Initialize vectors for calculations
    lambda_c <- numeric(n_groups)      # Cancer rate (incidence or mortality)
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
      Ri <- data_subset$Ri[i]
      Di <- data_subset$Di[i]
      Mi <- data_subset$Mi[i]
      Ni <- data_subset$Ni[i]
      wi <- data_subset$wi[i]
      
      # Calculate lambda_c based on risk type
      if (calc_type == "LRI") {
        # For LRI: use incidence data
        denominator <- Ri + Mi - Di
        lambda_c[i] <- Ri / denominator
      } else {
        # For LRM: use mortality data
        denominator <- Mi
        lambda_c[i] <- Di / denominator
      }
      
      # Calculate S0_star (probability of being alive and cancer-free)
      if (i == 1) {
        S0_star[i] <- 1  # At birth, probability is 1
      } else {
        S0_star[i] <- exp(-cumulative_hazard)
      }
      
      # Calculate Sx term
      if (data_subset$AgeEnd[i] == 999) {
        # Special case for final age group (85+)
        Sx[i] <- 1  # For final age group, this becomes 1
      } else {
        # Regular age groups
        hazard_rate <- (wi/Ni) * denominator
        Sx[i] <- 1 - exp(-hazard_rate)
      }
      
      # Update cumulative hazard for next iteration
      if (calc_type == "LRI") {
        cumulative_hazard <- cumulative_hazard + (Ri + Mi - Di)/Ni
      } else {
        cumulative_hazard <- cumulative_hazard + Mi/Ni
      }
      
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
            if (calc_type == "LRI") {
              poj <- (data_subset$Ri[j] + data_subset$Mi[j] - data_subset$Di[j]) / data_subset$Ni[j]
            } else {
              poj <- data_subset$Mi[j] / data_subset$Ni[j]
            }
            (poj * (1 - poj)) / data_subset$Ni[j]
          }))
        }
        var_S0_star[i] <- (S0_star[i])^2 * var_log_S0_star
        
        # Variance of Sx
        if (data_subset$AgeEnd[i] == 999) {
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
            if (calc_type == "LRI") {
              poj <- (data_subset$Ri[j] + data_subset$Mi[j] - data_subset$Di[j]) / data_subset$Ni[j]
            } else {
              poj <- data_subset$Mi[j] / data_subset$Ni[j]
            }
            poj / data_subset$Ni[j]
          }))
        }
        var_S0_star[i] <- (S0_star[i])^2 * var_log_S0_star
        
        # Variance of Sx
        if (data_subset$AgeEnd[i] == 999) {
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
    
    return(list(
      lifetime_risk = S,
      variance = total_variance,
      std_error = std_error,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      n_age_groups = n_groups,
      note = "Success"
    ))
  }
  
  # Calculate based on requested risk type
  result <- data.frame(ISOcode = region_data$ISOcode[1])
  
  if (risk_type %in% c("LRI", "both")) {
    lri_result <- calculate_single_risk(region_data_LRI, "LRI")
    result$LRI_lifetime_risk <- lri_result$lifetime_risk
    result$LRI_variance <- lri_result$variance
    result$LRI_std_error <- lri_result$std_error
    result$LRI_ci_lower <- lri_result$ci_lower
    result$LRI_ci_upper <- lri_result$ci_upper
    result$LRI_n_age_groups <- lri_result$n_age_groups
    result$LRI_note <- lri_result$note
  }
  
  if (risk_type %in% c("LRM", "both")) {
    lrm_result <- calculate_single_risk(region_data_LRM, "LRM")
    result$LRM_lifetime_risk <- lrm_result$lifetime_risk
    result$LRM_variance <- lrm_result$variance
    result$LRM_std_error <- lrm_result$std_error
    result$LRM_ci_lower <- lrm_result$ci_lower
    result$LRM_ci_upper <- lrm_result$ci_upper
    result$LRM_n_age_groups <- lrm_result$n_age_groups
    result$LRM_note <- lrm_result$note
  }
  
  return(result)
}

#6. Calculate lifetime risk for all regions ----
cat("Calculating lifetime risk using AMP method...\n")
cat("Risk type(s) to calculate:", RISK_TYPE, "\n")

# Get unique regions
regions <- unique(data$ISOcode)
cat("Processing", length(regions), "regions...\n")

# Calculate for each region
results <- data %>%
  group_by(ISOcode) %>%
  do(calculate_amp_lifetime_risk(., distribution = DISTRIBUTION_ASSUMPTION, risk_type = RISK_TYPE)) %>%
  ungroup()

cat("Calculations completed for", nrow(results), "regions\n\n")

#7. Results summary ----
cat("=== LIFETIME RISK RESULTS ===\n")

# Show summary statistics for LRI
if ("LRI_lifetime_risk" %in% colnames(results)) {
  valid_lri <- results %>% filter(!is.na(LRI_lifetime_risk))
  
  if (nrow(valid_lri) > 0) {
    cat("--- LRI (Lifetime Risk of Developing) ---\n")
    cat("Valid results:", nrow(valid_lri), "regions\n")
    cat("Mean LRI:", round(mean(valid_lri$LRI_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
    cat("Median LRI:", round(median(valid_lri$LRI_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
    cat("Range:", round(min(valid_lri$LRI_lifetime_risk, na.rm = TRUE) * 100, 4), "% -", 
        round(max(valid_lri$LRI_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n\n")
  } else {
    cat("--- LRI (Lifetime Risk of Developing) ---\n")
    cat("No valid LRI results calculated\n\n")
  }
}

# Show summary statistics for LRM
if ("LRM_lifetime_risk" %in% colnames(results)) {
  valid_lrm <- results %>% filter(!is.na(LRM_lifetime_risk))
  
  if (nrow(valid_lrm) > 0) {
    cat("--- LRM (Lifetime Risk of Dying) ---\n")
    cat("Valid results:", nrow(valid_lrm), "regions\n")
    cat("Mean LRM:", round(mean(valid_lrm$LRM_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
    cat("Median LRM:", round(median(valid_lrm$LRM_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n")
    cat("Range:", round(min(valid_lrm$LRM_lifetime_risk, na.rm = TRUE) * 100, 4), "% -", 
        round(max(valid_lrm$LRM_lifetime_risk, na.rm = TRUE) * 100, 4), "%\n\n")
  } else {
    cat("--- LRM (Lifetime Risk of Dying) ---\n")
    cat("No valid LRM results calculated\n\n")
  }
}

cat("==============================\n\n")

#8. Save results ----
cat("Saving results...\n")

# Create output directory if needed
if (!dir.exists(dirname(OUTPUT_FILE))) {
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

# Prepare final results with percentage columns
final_results <- results %>%
  mutate(
    cancer_type = CANCER_TYPE,
    distribution_method = DISTRIBUTION_ASSUMPTION,
    risk_calculation = RISK_TYPE
  )

# Add percentage columns based on what was calculated
if ("LRI_lifetime_risk" %in% colnames(results)) {
  final_results <- final_results %>%
    mutate(
      LRI_lifetime_risk_percent = round(LRI_lifetime_risk * 100, 6),
      LRI_std_error_percent = round(LRI_std_error * 100, 6),
      LRI_ci_lower_percent = round(LRI_ci_lower * 100, 6),
      LRI_ci_upper_percent = round(LRI_ci_upper * 100, 6)
    )
}

if ("LRM_lifetime_risk" %in% colnames(results)) {
  final_results <- final_results %>%
    mutate(
      LRM_lifetime_risk_percent = round(LRM_lifetime_risk * 100, 6),
      LRM_std_error_percent = round(LRM_std_error * 100, 6),
      LRM_ci_lower_percent = round(LRM_ci_lower * 100, 6),
      LRM_ci_upper_percent = round(LRM_ci_upper * 100, 6)
    )
}

# Reorder columns for better readability
col_order <- c("ISOcode", "cancer_type", "distribution_method", "risk_calculation")

if ("LRI_lifetime_risk" %in% colnames(final_results)) {
  col_order <- c(col_order, 
                 "LRI_lifetime_risk", "LRI_lifetime_risk_percent",
                 "LRI_variance", "LRI_std_error", "LRI_std_error_percent",
                 "LRI_ci_lower", "LRI_ci_upper", "LRI_ci_lower_percent", "LRI_ci_upper_percent",
                 "LRI_n_age_groups", "LRI_note")
}

if ("LRM_lifetime_risk" %in% colnames(final_results)) {
  col_order <- c(col_order,
                 "LRM_lifetime_risk", "LRM_lifetime_risk_percent", 
                 "LRM_variance", "LRM_std_error", "LRM_std_error_percent",
                 "LRM_ci_lower", "LRM_ci_upper", "LRM_ci_lower_percent", "LRM_ci_upper_percent",
                 "LRM_n_age_groups", "LRM_note")
}

final_results <- final_results %>%
  select(all_of(col_order))

# Sort by LRI if available, otherwise by LRM
if ("LRI_lifetime_risk" %in% colnames(final_results)) {
  final_results <- final_results %>% arrange(desc(LRI_lifetime_risk))
} else if ("LRM_lifetime_risk" %in% colnames(final_results)) {
  final_results <- final_results %>% arrange(desc(LRM_lifetime_risk))
}

# Save results
write.csv(final_results, OUTPUT_FILE, row.names = FALSE)

cat("Results saved to:", OUTPUT_FILE, "\n")

# Show top 10 results
cat("\nTop 10 regions by lifetime risk:\n")
if ("LRI_lifetime_risk" %in% colnames(final_results)) {
  cat("--- LRI Results ---\n")
  print(head(final_results %>% 
               filter(!is.na(LRI_lifetime_risk)) %>%
               select(ISOcode, LRI_lifetime_risk_percent, LRI_ci_lower_percent, LRI_ci_upper_percent), 
             10))
}

if ("LRM_lifetime_risk" %in% colnames(final_results)) {
  cat("\n--- LRM Results ---\n")
  print(head(final_results %>% 
               filter(!is.na(LRM_lifetime_risk)) %>%
               select(ISOcode, LRM_lifetime_risk_percent, LRM_ci_lower_percent, LRM_ci_upper_percent), 
             10))
}

cat("\n=== CALCULATION COMPLETED ===\n")