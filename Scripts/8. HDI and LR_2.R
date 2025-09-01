# Load required libraries
library(ggplot2)
library(dplyr)
library(countrycode)
library(gridExtra)
library(RColorBrewer)
library(cowplot)  # For extracting legends

df <- read.csv("Data/LRI and LRM/LRI_and_LRM_ratio.csv")

HDI <- rio::import("Data/HDI index/HDI_2022.xlsx")

HDI <- HDI |>
  mutate(ISOcode = countrycode(Region,origin = "country.name.en",destination = "iso3n"))

df_HDI <- merge(df,HDI,by = "ISOcode")

#1. Add UN subregions----
df_HDI$UN_subregion <- countrycode(
  sourcevar = df_HDI$ISOcode,
  origin = "iso3n",
  destination = "un.regionsub.name"
)

#2. Define color palette for UN subregions----
# Get unique subregions and assign colors
unique_subregions <- sort(unique(df_HDI$UN_subregion))
n_subregions <- length(unique_subregions)

# Use a combination of color palettes to get enough distinct colors
colors <- c(
  RColorBrewer::brewer.pal(min(11, n_subregions), "Spectral"),
  RColorBrewer::brewer.pal(min(9, max(0, n_subregions-11)), "Set1"),
  RColorBrewer::brewer.pal(min(8, max(0, n_subregions-20)), "Set2")
)[1:n_subregions]

names(colors) <- unique_subregions

#3. Function to create scatter plot with correlation----
create_cancer_plot <- function(data, x_var, y_var, y_label) {
  # Remove any rows with missing values for the variables of interest
  clean_data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
  
  # Calculate correlation with exact p-value method when possible
  cor_result <- cor.test(clean_data[[x_var]], clean_data[[y_var]], 
                         method = "spearman", exact = FALSE)
  r_value <- round(cor_result$estimate, 3)
  p_value <- cor_result$p.value
  
  # Format p-value
  p_text <- if(p_value < 0.001) "P < 0.001" else paste("P =", round(p_value, 3))
  
  # Create the plot using modern aes() syntax
  p <- ggplot(clean_data, aes(x = .data[[x_var]], y = .data[[y_var]], color = UN_subregion)) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_manual(values = colors) +
    labs(
      x = "Human development index",
      y = y_label,
      color = "UN Subregion"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank()
    ) +
    # Add correlation text
    annotate("text", x = min(clean_data[[x_var]], na.rm = TRUE) + 0.05, 
             y = max(clean_data[[y_var]], na.rm = TRUE) * 0.95,
             label = paste("r_s =", r_value, "\n", p_text),
             hjust = 0, vjust = 1, size = 3.5, fontface = "bold")
  
  return(list(plot = p, correlation = cor_result))
}

#4. Create a separate legend figure----
create_legend_plot <- function(data) {
  # Remove any rows with missing values to avoid warnings
  clean_data <- data[!is.na(data$HDI_index) & !is.na(data$LRI_CC) & !is.na(data$UN_subregion), ]
  
  # Create a simple plot just for displaying the legend
  legend_plot <- ggplot(clean_data, aes(x = HDI_index, y = LRI_CC, color = UN_subregion)) +
    geom_point(size = 4, alpha = 0.8) +
    scale_color_manual(values = colors) +
    theme_void() +  # Remove all plot elements except legend
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 11),
      legend.box = "horizontal",
      plot.margin = margin(20, 20, 20, 20)
    ) +
    guides(color = guide_legend(
      title = "UN Subregions",
      override.aes = list(size = 5, alpha = 1),
      ncol = 4,  # Adjust number of columns as needed
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5
    )) +
    labs(color = "UN Subregions")
  
  return(legend_plot)
}

# Create the legend plot
legend_plot <- create_legend_plot(df_HDI)

# Alternative: Create a legend-only plot using cowplot (simpler method)
create_legend_only <- function(data) {
  # Remove any rows with missing values
  clean_data <- data[!is.na(data$HDI_index) & !is.na(data$LRI_CC) & !is.na(data$UN_subregion), ]
  
  # Create a temporary plot with legend
  temp_plot <- ggplot(clean_data, aes(x = HDI_index, y = LRI_CC, color = UN_subregion)) +
    geom_point(size = 4, alpha = 0.8) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 11)
    ) +
    guides(color = guide_legend(
      title = "UN Subregions",
      override.aes = list(size = 5, alpha = 1),
      ncol = 4,
      byrow = TRUE
    ))
  
  # Extract just the legend using cowplot
  legend_only <- get_legend(temp_plot)
  
  return(legend_only)
}

# Create the legend using the simpler method
legend_only <- create_legend_only(df_HDI)

#5. Create plots for all cancer types (now without individual legends)----

# A. Colon Cancer - Incidence vs Mortality
plot_cc_inc <- create_cancer_plot(
  df_HDI, "HDI_index", "LRI_CC","Lifetime risk (%) of developing colon cancer"
)

plot_cc_mort <- create_cancer_plot(
  df_HDI, "HDI_index", "LRM_CC", "Lifetime risk (%) of dying from colon cancer"
)

# B. Rectum Cancer - Incidence vs Mortality
plot_rc_inc <- create_cancer_plot(
  df_HDI, "HDI_index", "LRI_RC",
  "Lifetime risk (%) of developing rectum cancer"
)

plot_rc_mort <- create_cancer_plot(
  df_HDI, "HDI_index", "LRM_RC",
  "Lifetime risk (%) of dying from rectum cancer"
)

# C. Colorectal Cancer - Incidence vs Mortality
plot_crc_inc <- create_cancer_plot(
  df_HDI, "HDI_index", "LRI_CRC",
  "Lifetime risk (%) of developing colorectal cancer"
)

plot_crc_mort <- create_cancer_plot(
  df_HDI, "HDI_index", "LRM_CRC",
  "Lifetime risk (%) of dying from colorectal cancer"
)

# D. RC/CRC ratio - Incidence vs Mortality
plot_ratio_inc <- create_cancer_plot(
  df_HDI, "HDI_index", "LRI_RC_to_CRC",
  "LRI Ratio: Rectal Cancer to Colorectal Cancer"
)
plot_ratio_mort <- create_cancer_plot(
  df_HDI, "HDI_index", "LRM_RC_to_CRC",
  "LRM Ratio: Rectal Cancer to Colorectal Cancer"
)

#6. Display the separate legend----
print("=== LEGEND FOR ALL PLOTS ===")
# Display the legend using cowplot's plot_grid
plot_grid(legend_only)

#7. Display plots without legends----
# Individual plots
print(plot_cc_inc$plot)
print(plot_cc_mort$plot)
print(plot_rc_inc$plot)
print(plot_rc_mort$plot)
print(plot_crc_inc$plot)
print(plot_crc_mort$plot)
print(plot_ratio_inc$plot)
print(plot_ratio_mort$plot)

#8. Print correlation results----
cat("Correlation Results:\n")
cat("==================\n")
cat("Colon Cancer Incidence: r_s =", round(plot_cc_inc$correlation$estimate, 3), 
    ", P =", format.pval(plot_cc_inc$correlation$p.value, digits = 3), "\n")
cat("Colon Cancer Mortality: r_s =", round(plot_cc_mort$correlation$estimate, 3), 
    ", P =", format.pval(plot_cc_mort$correlation$p.value, digits = 3), "\n")
cat("Rectum Cancer Incidence: r_s =", round(plot_rc_inc$correlation$estimate, 3), 
    ", P =", format.pval(plot_rc_inc$correlation$p.value, digits = 3), "\n")
cat("Rectum Cancer Mortality: r_s =", round(plot_rc_mort$correlation$estimate, 3), 
    ", P =", format.pval(plot_rc_mort$correlation$p.value, digits = 3), "\n")
cat("Colorectal Cancer Incidence: r_s =", round(plot_crc_inc$correlation$estimate, 3), 
    ", P =", format.pval(plot_crc_inc$correlation$p.value, digits = 3), "\n")
cat("Colorectal Cancer Mortality: r_s =", round(plot_crc_mort$correlation$estimate, 3), 
    ", P =", format.pval(plot_crc_mort$correlation$p.value, digits = 3), "\n")

#9. Data summary by subregion (optional analysis)----
subregion_summary <- df_HDI %>%
  group_by(UN_subregion) %>%
  summarise(
    n_countries = n(),
    mean_HDI = round(mean(HDI_index, na.rm = TRUE), 3),
    mean_CC_inc = round(mean(LRI_CC, na.rm = TRUE), 3),
    mean_CC_mort = round(mean(LRM_CC, na.rm = TRUE), 3),
    mean_RC_inc = round(mean(LRI_RC, na.rm = TRUE), 3),
    mean_RC_mort = round(mean(LRM_RC, na.rm = TRUE), 3),
    mean_CRC_inc = round(mean(LRI_CRC, na.rm = TRUE), 3),
    mean_CRC_mort = round(mean(LRM_CRC, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_HDI))

print("Summary by UN Subregion:")
print(subregion_summary)