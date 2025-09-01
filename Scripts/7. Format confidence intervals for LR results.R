# =====================================
# Script: Format Confidence Intervals for Lifetime Risk Results
# Author: Ke Pang (Modified)
# Date: 2025-08-28
# Purpose: Create new CSV files with formatted confidence interval columns
# Format: X.XX(X.XX-X.XX) for publication-ready results
# =====================================

library(dplyr)
library(tools)

#1. Configuration ----
# Set the directory containing your CSV files
DATA_DIRECTORY <- "Data/LRI and LRM"  # Change this to your data directory

# File pattern to match (modify as needed)
FILE_PATTERN <- "lifetime_risk_.*\\.csv$"

# Output file suffix (will be added before .csv)
OUTPUT_SUFFIX <- "_formatted"

# Number of decimal places for formatting
DECIMAL_PLACES <- 2

#2. Function to format confidence intervals ----
format_confidence_interval <- function(estimate, ci_lower, ci_upper, decimal_places = 2) {
  # Handle missing values
  if (is.na(estimate) | is.na(ci_lower) | is.na(ci_upper)) {
    return(NA)
  }
  
  # Format numbers to specified decimal places
  est_formatted <- sprintf(paste0("%.", decimal_places, "f"), estimate)
  lower_formatted <- sprintf(paste0("%.", decimal_places, "f"), ci_lower)
  upper_formatted <- sprintf(paste0("%.", decimal_places, "f"), ci_upper)
  
  # Create formatted string: X.XX(X.XX-X.XX)
  return(paste0(est_formatted, "(", lower_formatted, "-", upper_formatted, ")"))
}

#3. Function to process a single CSV file ----
process_csv_file <- function(file_path) {
  cat("Processing file:", basename(file_path), "\n")
  
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Check what columns are available and add formatted columns accordingly
  columns_added <- 0
  
  # Process LRI columns if they exist
  lri_cols <- c("LRI_lifetime_risk_percent", "LRI_ci_lower_percent", "LRI_ci_upper_percent")
  if (all(lri_cols %in% colnames(data))) {
    data$LRI_formatted <- mapply(format_confidence_interval,
                                 data$LRI_lifetime_risk_percent,
                                 data$LRI_ci_lower_percent, 
                                 data$LRI_ci_upper_percent,
                                 MoreArgs = list(decimal_places = DECIMAL_PLACES),
                                 SIMPLIFY = TRUE)
    columns_added <- columns_added + 1
    cat("  ✓ Added LRI_formatted column\n")
  }
  
  # Process LRM columns if they exist
  lrm_cols <- c("LRM_lifetime_risk_percent", "LRM_ci_lower_percent", "LRM_ci_upper_percent")
  if (all(lrm_cols %in% colnames(data))) {
    data$LRM_formatted <- mapply(format_confidence_interval,
                                 data$LRM_lifetime_risk_percent,
                                 data$LRM_ci_lower_percent,
                                 data$LRM_ci_upper_percent,
                                 MoreArgs = list(decimal_places = DECIMAL_PLACES),
                                 SIMPLIFY = TRUE)
    columns_added <- columns_added + 1
    cat("  ✓ Added LRM_formatted column\n")
  }
  
  # If no relevant columns found
  if (columns_added == 0) {
    cat("  ⚠ No lifetime risk columns found, skipping file\n")
    return(FALSE)
  }
  
  # Reorder columns to put formatted columns right after the corresponding CI columns
  col_names <- colnames(data)
  new_order <- c()
  
  for (col in col_names) {
    new_order <- c(new_order, col)
    
    # Add formatted column right after the upper CI column
    if (col == "LRI_ci_upper_percent" && "LRI_formatted" %in% col_names) {
      new_order <- c(new_order, "LRI_formatted")
    }
    if (col == "LRM_ci_upper_percent" && "LRM_formatted" %in% col_names) {
      new_order <- c(new_order, "LRM_formatted")
    }
  }
  
  # Remove duplicates and reorder
  new_order <- unique(new_order)
  data <- data[, new_order]
  
  # Create output file name with suffix
  file_dir <- dirname(file_path)
  file_name <- tools::file_path_sans_ext(basename(file_path))
  file_ext <- tools::file_ext(file_path)
  output_path <- file.path(file_dir, paste0(file_name, OUTPUT_SUFFIX, ".", file_ext))
  
  # Write new formatted file
  write.csv(data, output_path, row.names = FALSE)
  cat("  ✓ Created new formatted file:", basename(output_path), "\n")
  cat("  ✓ Original file unchanged:", basename(file_path), "\n")
  
  return(TRUE)
}

#4. Main processing ----
cat("=== CONFIDENCE INTERVAL FORMATTER ===\n")
cat("Data Directory:", DATA_DIRECTORY, "\n")
cat("File Pattern:", FILE_PATTERN, "\n")
cat("Output Suffix:", OUTPUT_SUFFIX, "\n")
cat("Decimal Places:", DECIMAL_PLACES, "\n")
cat("Output Format: X.XX(X.XX-X.XX)\n")
cat("=====================================\n\n")

# Check if directory exists
if (!dir.exists(DATA_DIRECTORY)) {
  stop("Data directory not found: ", DATA_DIRECTORY)
}

# Find CSV files matching the pattern
csv_files <- list.files(
  path = DATA_DIRECTORY, 
  pattern = FILE_PATTERN, 
  full.names = TRUE, 
  recursive = TRUE
)

if (length(csv_files) == 0) {
  cat("No CSV files found matching pattern:", FILE_PATTERN, "\n")
  cat("Files in directory:\n")
  print(list.files(DATA_DIRECTORY, recursive = TRUE))
  stop("No files to process")
}

cat("Found", length(csv_files), "CSV files to process:\n")
for (file in csv_files) {
  cat(" -", basename(file), "\n")
}
cat("\n")

# Process each file
successful_files <- 0
failed_files <- 0

for (file_path in csv_files) {
  tryCatch({
    if (process_csv_file(file_path)) {
      successful_files <- successful_files + 1
    } else {
      failed_files <- failed_files + 1
    }
    cat("\n")
  }, error = function(e) {
    cat("  ✗ Error processing", basename(file_path), ":", e$message, "\n\n")
    failed_files <<- failed_files + 1
  })
}

#5. Summary ----
cat("=== PROCESSING SUMMARY ===\n")
cat("Total files found:", length(csv_files), "\n")
cat("Successfully processed:", successful_files, "\n")
cat("Failed/Skipped:", failed_files, "\n")

if (successful_files > 0) {
  cat("\n✓ Formatting completed successfully!\n")
  cat("✓ Original files remain unchanged\n")
  cat("✓ New formatted files created with '", OUTPUT_SUFFIX, "' suffix\n")
  cat("✓ New formatted columns added:\n")
  cat("  - LRI_formatted: LRI percentage with CI in format X.XX(X.XX-X.XX)\n")
  cat("  - LRM_formatted: LRM percentage with CI in format X.XX(X.XX-X.XX)\n")
  
  cat("\nExample of file naming:\n")
  cat("Original: lifetime_risk_colon.csv\n")
  cat("New:      lifetime_risk_colon_formatted.csv\n")
  
  cat("\nExample of formatted output:\n")
  cat("Before: LRI_lifetime_risk_percent = 9.35766, CI = (9.029191, 9.686129)\n")
  cat("After:  LRI_formatted = 9.36(9.03-9.69)\n")
} else {
  cat("\n⚠ No files were successfully processed\n")
}

cat("===========================\n")

# Optional: Show preview of processed files
if (successful_files > 0 && length(csv_files) > 0) {
  cat("\nPreview of first processed file:\n")
  
  # Get the first processed file's output version
  sample_file <- csv_files[1]
  file_dir <- dirname(sample_file)
  file_name <- tools::file_path_sans_ext(basename(sample_file))
  file_ext <- tools::file_ext(sample_file)
  sample_output_file <- file.path(file_dir, paste0(file_name, OUTPUT_SUFFIX, ".", file_ext))
  
  if (file.exists(sample_output_file)) {
    sample_data <- read.csv(sample_output_file, stringsAsFactors = FALSE)
    
    # Show formatted columns if they exist
    preview_cols <- c("ISOcode")
    
    if ("LRI_formatted" %in% colnames(sample_data)) {
      preview_cols <- c(preview_cols, "LRI_lifetime_risk_percent", "LRI_formatted")
    }
    
    if ("LRM_formatted" %in% colnames(sample_data)) {
      preview_cols <- c(preview_cols, "LRM_lifetime_risk_percent", "LRM_formatted")
    }
    
    if (length(preview_cols) > 1) {
      preview_data <- sample_data %>%
        select(all_of(preview_cols)) %>%
        head(5)
      
      print(preview_data)
      cat("\nFile:", basename(sample_output_file), "\n")
    }
  }
}