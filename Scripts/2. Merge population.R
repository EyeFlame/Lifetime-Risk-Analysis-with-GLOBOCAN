# =====================================
# Script: UN Population and Mortality Data Merger
# Author: Ke Pang (Enhanced by Claude)
# Date: 2025-08-12
# Purpose: Simple merge of age-stratified population and mortality data from UN World Population Prospects
# Data: Creates combined dataset with Ni (population) and Mi (mortality)
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
BASE_DIR <- "Data/population data from UN"

# Input files
POPULATION_FILE <- "unpopulation_regions_all.csv"
MORTALITY_FILE <- "undeath_regions_all.csv"

# Output file
OUTPUT_FILE <- "population_mortality_combined_2024.csv"

#2. Automatic setup - No need to modify ----

# Set up file paths
population_path <- file.path(BASE_DIR, POPULATION_FILE)
mortality_path <- file.path(BASE_DIR, MORTALITY_FILE)
output_path <- file.path(BASE_DIR, OUTPUT_FILE)

# Print configuration for verification
cat("=== DATA MERGING CONFIGURATION ===\n")
cat("Base Directory:", BASE_DIR, "\n")
cat("Population File:", POPULATION_FILE, "\n")
cat("Mortality File:", MORTALITY_FILE, "\n")
cat("Output File:", OUTPUT_FILE, "\n")
cat("==================================\n\n")

#3. File validation ----
cat("Checking file availability...\n")

# Check if input files exist
missing_files <- c()
if (!file.exists(population_path)) {
  missing_files <- c(missing_files, POPULATION_FILE)
}
if (!file.exists(mortality_path)) {
  missing_files <- c(missing_files, MORTALITY_FILE)
}

if (length(missing_files) > 0) {
  cat("ERROR: Missing files:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
  stop("Please ensure all required files are present before running the script.")
}
cat("All files found! ✓\n\n")

#4. Data loading ----
cat("Loading UN population data...\n")
population_df <- read.csv(population_path, stringsAsFactors = FALSE)
cat("  Population data loaded:", nrow(population_df), "records\n")

cat("Loading UN mortality data...\n")
mortality_df <- read.csv(mortality_path, stringsAsFactors = FALSE)
cat("  Mortality data loaded:", nrow(mortality_df), "records\n")

#5. Data cleaning ----
cat("Cleaning and standardizing data...\n")

# Clean population data - only select and rename columns
population_clean <- population_df %>%
  select(LocationId, AgeStart, AgeEnd, Value) %>%
  rename(
    ISOcode = LocationId,
    Ni = Value
  )

cat("  Population data cleaned:", nrow(population_clean), "records\n")

# Clean mortality data - only select and rename columns
mortality_clean <- mortality_df %>%
  select(LocationId, AgeStart, AgeEnd, Value) %>%
  rename(
    ISOcode = LocationId,
    Mi = Value
  )

cat("  Mortality data cleaned:", nrow(mortality_clean), "records\n")

#6. Data merging ----
cat("Merging population and mortality data...\n")

# Simple inner join
merged_data <- merge(
  population_clean, 
  mortality_clean, 
  by = c("ISOcode", "AgeStart", "AgeEnd"), 
  all = FALSE  # Inner join - keep only records present in both datasets
)

cat("  Merged dataset created:", nrow(merged_data), "records\n")

#7. Save results ----
cat("Saving merged dataset...\n")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}

# Sort and save merged data
final_data <- merged_data %>%
  arrange(ISOcode, AgeStart, AgeEnd)

write.csv(final_data, output_path, row.names = FALSE)

#8. Summary ----
cat("\n=== MERGING COMPLETE ===\n")
cat("Total records:", nrow(final_data), "\n")
cat("Regions/Countries:", length(unique(final_data$ISOcode)), "\n")
cat("Age groups:", length(unique(paste(final_data$AgeStart, final_data$AgeEnd, sep="-"))), "\n")
cat("Age range:", min(final_data$AgeStart), "-", max(final_data$AgeEnd), "\n")
cat("Output saved to:", output_path, "\n")
cat("========================\n")

# Show sample of results
cat("\nSample of merged data:\n")
print(head(final_data, 10))