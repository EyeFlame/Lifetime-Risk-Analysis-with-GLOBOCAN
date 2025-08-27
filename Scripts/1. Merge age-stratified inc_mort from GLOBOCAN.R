# =====================================
# Script: Flexible GLOBOCAN Data Processor with Regional Levels
# Author: Ke Pang (Enhanced by Claude)
# Date: 2025-08-26
# Purpose: Flexible script to process GLOBOCAN age-stratified data for different cancers, data types, and regional levels
# Data: Handles different cancer types with countries/subregions/HDI folder structure
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
CANCER_TYPE <- "colorectum"  # Options: "colorectum", "rectum", "lip-oral-cavity", etc.
DATA_TYPE <- "mort"               # Options: "inc" (incidence) or "mort" (mortality)
REGIONAL_LEVEL <- "HDI"    # Options: "countries", "subregions", "HDI"
YEAR <- "2022"                   # Data year

#2. Automatic setup - No need to modify ----

# Set up paths and filenames based on parameters
data_dir <- file.path("Data", CANCER_TYPE, REGIONAL_LEVEL)
output_filename <- paste0("age_specific_", 
                          ifelse(DATA_TYPE == "inc", "incidence", "mortality"), 
                          "_", REGIONAL_LEVEL, ".csv")
output_path <- file.path("Data", CANCER_TYPE, output_filename)

# Age group definitions
age_ends <- c(14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 999)  # 999 for 85+

# Generate filenames dynamically
generate_filename <- function(age_end, cancer_type, data_type, year) {
  if (age_end == 999) {
    # Special case for 85+ (no age limit in filename)
    return(paste0("dataset-", data_type, "-both-sexes-in-", year, "-", cancer_type, ".csv"))
  } else {
    return(paste0("dataset-", data_type, "-both-sexes-age-0-", age_end, "-in-", year, "-", cancer_type, ".csv"))
  }
}

files <- sapply(age_ends, function(age_end) {
  generate_filename(age_end, CANCER_TYPE, DATA_TYPE, YEAR)
})

# Print configuration for verification
cat("=== PROCESSING CONFIGURATION ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Data Type:", ifelse(DATA_TYPE == "inc", "Incidence", "Mortality"), "\n")
cat("Regional Level:", REGIONAL_LEVEL, "\n")
cat("Year:", YEAR, "\n")
cat("Data Directory:", data_dir, "\n")
cat("Output File:", output_path, "\n")
cat("================================\n\n")

#3. File validation ----
cat("Checking file availability...\n")
missing_files <- c()
for (i in seq_along(files)) {
  file_path <- file.path(data_dir, files[i])
  if (!file.exists(file_path)) {
    missing_files <- c(missing_files, files[i])
  }
}

if (length(missing_files) > 0) {
  cat("ERROR: Missing files:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
  stop("Please ensure all required files are present before running the script.")
}
cat("All files found! ✓\n\n")

#4. Data processing ----
cat("Reading and processing data...\n")

# Read all data files
df_list <- lapply(files, function(fname) {
  file_path <- file.path(data_dir, fname)  
  cat("  Reading:", fname, "\n")
  df <- read.csv(file_path)
  
  # Standardize column names and select relevant columns
  df <- df[, c("Population.code..ISO.UN.", "Number")]
  df <- df %>%
    rename(ISOcode = Population.code..ISO.UN.) %>%
    # Replace NA ISOcode with 900 (world data)
    mutate(ISOcode = ifelse(is.na(ISOcode), 900, ISOcode))
  
  return(df)
})

# Process age groups (calculate differences between cumulative data)
cat("Calculating age-specific values...\n")
results <- list()

for (i in 1:length(df_list)) {
  if (i == 1) {
    # First group: 0-14 years (direct from first df)
    age_start <- 0
    age_end <- age_ends[i]
    Ri <- df_list[[i]]
  } else {
    # Subsequent groups: current cumulative - previous cumulative
    age_start <- age_ends[i - 1] + 1
    age_end <- age_ends[i]
    
    # Merge and calculate difference
    Ri <- merge(df_list[[i]], df_list[[i - 1]], by = "ISOcode", suffixes = c("_current", "_prev"))
    Ri$Number <- Ri$Number_current - Ri$Number_prev
  }
  
  # Create result dataframe for this age group
  # Use Ri for incidence, Di for mortality
  value_col_name <- ifelse(DATA_TYPE == "inc", "Ri", "Di")
  
  result_df <- data.frame(
    ISOcode = Ri$ISOcode,
    AgeStart = rep(age_start, nrow(Ri)),
    AgeEnd = rep(age_end, nrow(Ri)),
    stringsAsFactors = FALSE
  )
  
  # Add the value column with appropriate name
  result_df[[value_col_name]] <- Ri$Number
  
  results[[i]] <- result_df
}

# Combine all age groups
final_df <- do.call(rbind, results)

#5. Split 0-14 age group ----
cat("Splitting 0-14 age group into 0-4, 5-9, 10-14...\n")

# Split 0-14 age group into three sub-groups
final_df_split <- final_df %>%
  filter(AgeStart == 0 & AgeEnd == 14) %>%
  group_by(ISOcode) %>%
  do({
    # Get the value column name (Ri or Di)
    value_col <- ifelse(DATA_TYPE == "inc", "Ri", "Di")
    original_value <- .[[value_col]]
    
    result <- data.frame(
      ISOcode = .$ISOcode,
      AgeStart = c(0, 5, 10),
      AgeEnd = c(4, 9, 14),
      stringsAsFactors = FALSE
    )
    
    # Add the appropriate column with values (0, 0, original_value)
    result[[value_col]] <- c(0, 0, original_value)
    
    result
  }) %>%
  ungroup()

# Replace original 0-14 group with split groups
final_df_new <- final_df %>%
  filter(!(AgeStart == 0 & AgeEnd == 14)) %>%  # Remove original 0-14
  bind_rows(final_df_split) %>%                # Add split groups
  arrange(ISOcode, AgeStart)                   # Sort by country and age

#6. Save results ----
cat("Saving results...\n")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}

# Save processed data
write.csv(final_df_new, output_path, row.names = FALSE)

#7. Summary ----
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Processed", nrow(final_df_new), "records\n")
cat("Countries/Regions:", length(unique(final_df_new$ISOcode)), "\n")
cat("Age Groups:", length(unique(paste(final_df_new$AgeStart, final_df_new$AgeEnd, sep="-"))), "\n")
cat("Output saved to:", output_path, "\n")
cat("===========================\n")

# Show sample of results
cat("\nSample of processed data:\n")
print(head(final_df_new, 10))