# =====================================
# Script: Multi-Regional Cancer and Population Data Merger
# Author: Ke Pang
# Date: 2025-08-26
# Purpose: Merge cancer incidence/mortality from multiple regional levels with population/mortality data
# Output: Final dataset with ISOcode, AgeStart, AgeEnd, Ri, Di, Ni, Mi from all regional levels
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
CANCER_TYPE <- "colorectum"  # Options: "colorectum", "rectum", "lip-oral-cavity", etc.

# Regional levels to process (add more as needed)
REGIONAL_LEVELS <- c("countries", "subregions", "HDI")

# File paths
POPULATION_MORTALITY_FILE <- "Data/population data from UN/popu_mort_2022_HDI.csv"

# Output file
OUTPUT_FILE <- file.path("Data", CANCER_TYPE, paste0("2022_", CANCER_TYPE, "_data_all_regions.csv"))

#2. Print configuration ----
cat("=== MULTI-REGIONAL DATA MERGING CONFIGURATION ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Regional Levels:", paste(REGIONAL_LEVELS, collapse = ", "), "\n")
cat("Population/Mortality File:", POPULATION_MORTALITY_FILE, "\n")
cat("Output File:", OUTPUT_FILE, "\n")
cat("=================================================\n\n")

#3. File validation ----
cat("Checking file availability...\n")

# Check population mortality file
if (!file.exists(POPULATION_MORTALITY_FILE)) {
  stop("Population mortality file not found: ", POPULATION_MORTALITY_FILE)
}
cat("  Population/mortality file found ✓\n")

# Check cancer data files for each regional level
available_levels <- c()
cancer_files <- list()

for (level in REGIONAL_LEVELS) {
  inc_file <- file.path("Data", CANCER_TYPE, paste0("age_specific_incidence_", level, ".csv"))
  mort_file <- file.path("Data", CANCER_TYPE, paste0("age_specific_mortality_", level, ".csv"))
  
  if (file.exists(inc_file) && file.exists(mort_file)) {
    available_levels <- c(available_levels, level)
    cancer_files[[level]] <- list(incidence = inc_file, mortality = mort_file)
    cat("  ", level, "files found ✓\n")
  } else {
    cat("  ", level, "files missing - skipping this level\n")
    if (!file.exists(inc_file)) cat("    Missing:", inc_file, "\n")
    if (!file.exists(mort_file)) cat("    Missing:", mort_file, "\n")
  }
}

if (length(available_levels) == 0) {
  stop("No complete cancer data files found for any regional level.")
}

cat("Available regional levels:", paste(available_levels, collapse = ", "), "\n\n")

#4. Data loading ----
cat("Loading population and mortality data...\n")
pop_mort_data <- read.csv(POPULATION_MORTALITY_FILE, stringsAsFactors = FALSE)
cat("  Population/Mortality data loaded:", nrow(pop_mort_data), "records\n\n")

cat("Loading cancer data from all regional levels...\n")
all_cancer_data <- list()

for (level in available_levels) {
  cat("  Processing", level, "level...\n")
  
  # Load incidence data
  inc_data <- read.csv(cancer_files[[level]]$incidence, stringsAsFactors = FALSE)
  cat("    Incidence data:", nrow(inc_data), "records\n")
  
  # Load mortality data
  mort_data <- read.csv(cancer_files[[level]]$mortality, stringsAsFactors = FALSE)
  cat("    Mortality data:", nrow(mort_data), "records\n")
  
  # Merge incidence and mortality for this level
  level_cancer_data <- merge(
    inc_data[, c("ISOcode", "AgeStart", "AgeEnd", "Ri")],
    mort_data[, c("ISOcode", "AgeStart", "AgeEnd", "Di")],
    by = c("ISOcode", "AgeStart", "AgeEnd"),
    all = TRUE
  )
  
  # Add regional level identifier
  level_cancer_data$RegionalLevel <- level
  
  all_cancer_data[[level]] <- level_cancer_data
  cat("    Merged", level, "data:", nrow(level_cancer_data), "records\n")
}

#5. Combine all regional levels ----
cat("\nCombining cancer data from all regional levels...\n")

# Combine all cancer data
combined_cancer_data <- do.call(rbind, all_cancer_data)

# Remove duplicate entries (in case same ISOcode appears in multiple levels)
# Keep the first occurrence (prioritize by order in REGIONAL_LEVELS)
combined_cancer_data <- combined_cancer_data %>%
  arrange(match(RegionalLevel, available_levels)) %>%  # Order by priority
  distinct(ISOcode, AgeStart, AgeEnd, .keep_all = TRUE) %>%
  select(-RegionalLevel)  # Remove the identifier column

cat("  Combined cancer data:", nrow(combined_cancer_data), "records\n")
cat("  Unique regions:", length(unique(combined_cancer_data$ISOcode)), "\n")

#6. Data validation ----
cat("\nValidating data structure...\n")

# Check required columns
required_pop_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Ni", "Mi")
required_cancer_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Ri", "Di")

# Validate population/mortality data
missing_pop_cols <- setdiff(required_pop_cols, colnames(pop_mort_data))
if (length(missing_pop_cols) > 0) {
  stop("Missing columns in population/mortality data: ", paste(missing_pop_cols, collapse = ", "))
}

# Validate combined cancer data
missing_cancer_cols <- setdiff(required_cancer_cols, colnames(combined_cancer_data))
if (length(missing_cancer_cols) > 0) {
  stop("Missing columns in combined cancer data: ", paste(missing_cancer_cols, collapse = ", "))
}

cat("Data structure validation passed ✓\n")

#7. Data merging ----
cat("\nMerging population and cancer datasets...\n")

# Merge population/mortality data with combined cancer data
# Keep only rows that have both cancer data AND population data
final_data <- merge(
  pop_mort_data[, c("ISOcode", "AgeStart", "AgeEnd", "Ni", "Mi")],
  combined_cancer_data[, c("ISOcode", "AgeStart", "AgeEnd", "Ri", "Di")],
  by = c("ISOcode", "AgeStart", "AgeEnd"),
  all.x = FALSE,  # Don't keep population data without cancer data
  all.y = FALSE   # Don't keep cancer data without population data
)

cat("  Final merge completed:", nrow(final_data), "records\n")

# Remove any rows with missing values in either cancer or population data
final_data <- final_data %>%
  filter(
    !is.na(Ri) & !is.na(Di) & !is.na(Ni) & !is.na(Mi) &
      Ri >= 0 & Di >= 0 & Ni > 0 & Mi >= 0  # Keep only valid positive values
  )

cat("  Filtered to rows with complete data:", nrow(final_data), "records\n")

#8. Data quality checks ----
cat("\nPerforming data quality checks...\n")

# Since we've already filtered for complete data, check for any remaining issues
missing_pop_mort <- sum(is.na(final_data$Ni) | is.na(final_data$Mi))
missing_cancer <- sum(is.na(final_data$Ri) | is.na(final_data$Di))

if (missing_pop_mort > 0) cat("  Warning:", missing_pop_mort, "records with missing population/mortality data\n")
if (missing_cancer > 0) cat("  Warning:", missing_cancer, "records with missing cancer data\n")

# Check for any remaining negative values
negative_ri <- sum(final_data$Ri < 0, na.rm = TRUE)
negative_di <- sum(final_data$Di < 0, na.rm = TRUE)
negative_ni <- sum(final_data$Ni <= 0, na.rm = TRUE)  # Population should be > 0
negative_mi <- sum(final_data$Mi < 0, na.rm = TRUE)

if (negative_ri > 0) cat("  Warning:", negative_ri, "negative Ri values\n")
if (negative_di > 0) cat("  Warning:", negative_di, "negative Di values\n")
if (negative_ni > 0) cat("  Warning:", negative_ni, "non-positive Ni values\n")
if (negative_mi > 0) cat("  Warning:", negative_mi, "negative Mi values\n")

# Check coverage - all records should now have both cancer and population data
total_regions <- length(unique(final_data$ISOcode))

cat("  All", nrow(final_data), "records have both cancer and population data\n")
cat("  Unique regions in final dataset:", total_regions, "\n")

# Sort final data
final_data <- final_data %>%
  arrange(ISOcode, AgeStart, AgeEnd) %>%
  select(ISOcode, AgeStart, AgeEnd, Ri, Di, Ni, Mi)  # Ensure column order

cat("  Data quality checks completed ✓\n")

#9. Save results ----
cat("\nSaving final dataset...\n")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(OUTPUT_FILE))) {
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

# Save final merged data
write.csv(final_data, OUTPUT_FILE, row.names = FALSE)

#10. Summary ----
cat("\n=== MULTI-REGIONAL MERGING COMPLETE ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Processed Regional Levels:", paste(available_levels, collapse = ", "), "\n")
cat("Total records:", nrow(final_data), "\n")
cat("Regions/Countries:", length(unique(final_data$ISOcode)), "\n")
cat("Age groups:", length(unique(paste(final_data$AgeStart, final_data$AgeEnd, sep="-"))), "\n")
cat("Age range:", min(final_data$AgeStart), "-", max(final_data$AgeEnd), "\n")
cat("Output saved to:", OUTPUT_FILE, "\n")
cat("======================================\n")

# Show sample of results
cat("\nSample of final dataset:\n")
print(head(final_data, 15))

# Show summary statistics
cat("\nSummary statistics:\n")
cat("Incidence (Ri): Min =", min(final_data$Ri), ", Max =", max(final_data$Ri), ", Mean =", round(mean(final_data$Ri), 2), "\n")
cat("Cancer Mortality (Di): Min =", min(final_data$Di), ", Max =", max(final_data$Di), ", Mean =", round(mean(final_data$Di), 2), "\n")
cat("Population (Ni): Min =", min(final_data$Ni, na.rm = TRUE), ", Max =", max(final_data$Ni, na.rm = TRUE), ", Mean =", round(mean(final_data$Ni, na.rm = TRUE), 2), "\n")
cat("All-cause Mortality (Mi): Min =", min(final_data$Mi, na.rm = TRUE), ", Max =", max(final_data$Mi, na.rm = TRUE), ", Mean =", round(mean(final_data$Mi, na.rm = TRUE), 2), "\n")

# Show data coverage - all regions now have both cancer and population data
cat("\nData coverage summary:\n")
cat("All regions have both cancer and population data:", nrow(final_data), "records\n")

# Show breakdown by data availability
non_zero_cancer <- sum(final_data$Ri > 0 | final_data$Di > 0)
zero_cancer <- nrow(final_data) - non_zero_cancer

cat("Records with non-zero cancer data:", non_zero_cancer, "\n")
cat("Records with zero cancer data:", zero_cancer, "\n")

# Show ISOcode ranges to identify different regional levels
cat("\nISOcode ranges in final dataset:\n")
iso_summary <- final_data %>%
  group_by(ISOcode) %>%
  summarise(
    records = n(),
    has_cancer = sum(Ri > 0 | Di > 0) > 0,
    .groups = "drop"
  ) %>%
  arrange(ISOcode)

cat("Countries (1-999):", sum(iso_summary$ISOcode <= 999), "\n")
cat("Other regions (>999):", sum(iso_summary$ISOcode > 999), "\n")