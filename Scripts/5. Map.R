# Load required libraries
library(dplyr)
library(ggplot2)
library(maps)
library(countrycode)
library(viridis)
library(RColorBrewer)
library(scales)

#1. Input LR data and calculate RC/CRC
CRC_LR <-read.csv("Data/lifetime_risk_2024_2022_colorectum.csv")
RC_LR <- read.csv("Data/lifetime_risk_2024_2022_rectum.csv")

CRC_LR <- CRC_LR %>%
  select(ISOcode,lifetime_risk_percent,ci_lower_percent,ci_upper_percent)
RC_LR <- RC_LR %>%
  select(ISOcode,lifetime_risk_percent,ci_lower_percent,ci_upper_percent)

LR <- left_join(CRC_LR,RC_LR,by = "ISOcode",suffix = c("_colorectum","_rectum"))
LR <- LR %>%
  mutate(risk_ratio = lifetime_risk_percent_rectum / lifetime_risk_percent_colorectum) %>%
  mutate(Region = countrycode(ISOcode,origin = "iso3n",destination = "country.name.en")) %>%
  select(Region,everything())
write.csv(LR,"Data/LR_of_CRC_and_RC_with_RC_CRC_ratio.csv",row.names = FALSE)

# Get world map data
world_map <- map_data("world")

# Prepare the data for mapping by matching country names
LR_map <- LR %>%
  filter(Region != "World") %>%  # Remove the "World" observation
  mutate(
    # Convert ISO codes to country names that match map data
    region = countrycode(ISOcode, origin = "iso3n", destination = "country.name.en"),
    # Handle some common name mismatches
    region = case_when(
      region == "United States" ~ "USA",
      region == "United Kingdom" ~ "UK",
      region == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      region == "Congo - Brazzaville" ~ "Republic of Congo",
      TRUE ~ region
    )
  ) %>%
  filter(!is.na(region))

# Update risk ratio to rectum/colorectum
LR_map <- LR_map %>%
  mutate(risk_ratio_new = lifetime_risk_percent_rectum / lifetime_risk_percent_colorectum)

# Merge with world map data
world_data <- world_map %>%
  left_join(LR_map, by = "region")

# Function to get country centroids for labeling
get_country_centroids <- function(data, countries) {
  data %>%
    filter(region %in% countries) %>%
    group_by(region) %>%
    summarise(
      long = mean(long, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      .groups = 'drop'
    )
}

# Function to create ranking labels
create_ranking_labels <- function(data, metric_col, top_n = 10) {
  # Remove rows with missing data
  clean_data <- data[!is.na(data[[metric_col]]), ]
  
  # Create rankings
  clean_data$rank <- rank(-clean_data[[metric_col]], ties.method = "first")
  total_countries <- nrow(clean_data)
  
  # Select top 10 and bottom 10
  top_countries <- clean_data[clean_data$rank <= top_n, ]
  bottom_countries <- clean_data[clean_data$rank > (total_countries - top_n), ]
  
  # Combine and add rank labels
  ranking_data <- rbind(
    top_countries %>% mutate(rank_label = paste0("#", rank)),
    bottom_countries %>% mutate(rank_label = paste0("#", rank))
  )
  
  return(ranking_data)
}

# Function to create elegant maps with consistent styling
create_elegant_map <- function(data, fill_var, title, legend_title, color_palette = "viridis", 
                               ranking_data = NULL, key_countries = c("China", "USA", "Japan")) {
  
  # Choose color palette
  if (color_palette == "viridis") {
    colors <- scale_fill_viridis_c(name = legend_title, na.value = "grey90", 
                                   option = "plasma", direction = 1)
  } else if (color_palette == "blue") {
    colors <- scale_fill_gradient(name = legend_title, low = "#f0f9ff", high = "#0c4a6e", 
                                  na.value = "grey90")
  } else if (color_palette == "red") {
    colors <- scale_fill_gradient(name = legend_title, low = "#fef2f2", high = "#7f1d1d", 
                                  na.value = "grey90")
  } else if (color_palette == "heatmap") {
    colors <- scale_fill_gradient2(name = legend_title, 
                                   low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                                   midpoint = median(data[[fill_var]], na.rm = TRUE),
                                   na.value = "grey90")
  }
  
  # Base map
  p <- ggplot(data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes_string(fill = fill_var), color = "white", size = 0.1) +
    colors +
    coord_fixed(1.3) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = title,
      subtitle = "Data shows lifetime risk percentages by country",
      caption = "Source: GLOBOCAN 2022"
    )
  
  # Add ranking labels if provided
  if (!is.null(ranking_data)) {
    ranking_centroids <- get_country_centroids(data, ranking_data$region)
    ranking_labels <- ranking_centroids %>%
      left_join(ranking_data %>% select(region, rank_label), by = "region")
    
    p <- p + geom_text(data = ranking_labels, 
                       aes(x = long, y = lat, label = rank_label, group = NULL),
                       size = 3, fontface = "bold", color = "black",
                       bg.colour = "white", bg.r = 0.1)
  }
  
  # Add key country labels
  key_centroids <- get_country_centroids(data, key_countries)
  key_labels <- key_centroids %>%
    left_join(data %>% 
                distinct(region, .keep_all = TRUE) %>%
                select_at(c("region", fill_var)), by = "region") %>%
    filter(!is.na(.data[[fill_var]])) %>%
    mutate(label_text = paste0(region, "\n", round(.data[[fill_var]], 2)))
  
  p <- p + geom_label(data = key_labels,
                      aes(x = long, y = lat, label = label_text, group = NULL),
                      size = 3, fontface = "bold", 
                      fill = "yellow", alpha = 0.8, color = "black")
  
  return(p)
}

# Create ranking data for each metric
colorectal_rankings <- create_ranking_labels(LR_map, "lifetime_risk_percent_colorectum")
rectal_rankings <- create_ranking_labels(LR_map, "lifetime_risk_percent_rectum")
ratio_rankings <- create_ranking_labels(LR_map, "risk_ratio_new")

# Map 1: Colorectal Cancer Lifetime Risk
map1 <- create_elegant_map(
  world_data, 
  "lifetime_risk_percent_colorectum",
  "Colorectal Cancer Lifetime Risk by Country",
  "Lifetime Risk (%)",
  "red",
  colorectal_rankings
)

# Map 2: Rectal Cancer Lifetime Risk
map2 <- create_elegant_map(
  world_data, 
  "lifetime_risk_percent_rectum",
  "Rectal Cancer Lifetime Risk by Country", 
  "Lifetime Risk (%)",
  "blue",
  rectal_rankings
)

# Map 3: Risk Ratio (Rectum/Colorectal) with heatmap coloring
map3 <- create_elegant_map(
  world_data, 
  "risk_ratio_new",
  "Rectal to Colorectal Cancer Risk Ratio by Country",
  "Risk Ratio (Rectum/Colorectal)",
  "heatmap",
  ratio_rankings
)

# Display the maps
print(map1)
print(map2)
print(map3)

# Optional: Save the maps as high-quality images
ggsave("colorectal_cancer_lifetime_risk_map.png", map1, 
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave("rectal_cancer_lifetime_risk_map.png", map2, 
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave("cancer_risk_ratio_map.png", map3, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Optional: Create a combined plot using patchwork (if you have the package)
if(require(patchwork, quietly = TRUE)) {
  combined_plot <- (map1 / map2 / map3) + 
    plot_annotation(
      title = "Global Colorectal and Rectal Cancer Lifetime Risk Analysis",
      theme = theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"))
    )
  
  print(combined_plot)
  
  ggsave("combined_cancer_risk_maps.png", combined_plot, 
         width = 12, height = 16, dpi = 300, bg = "white")
}

# Summary statistics for context
cat("\n=== DATA SUMMARY ===\n")
cat("Colorectal Cancer Lifetime Risk:\n")
cat("Range:", round(range(LR_map$lifetime_risk_percent_colorectum, na.rm = TRUE), 2), "\n")
cat("Mean:", round(mean(LR_map$lifetime_risk_percent_colorectum, na.rm = TRUE), 2), "%\n\n")

cat("Rectal Cancer Lifetime Risk:\n")
cat("Range:", round(range(LR_map$lifetime_risk_percent_rectum, na.rm = TRUE), 2), "\n")
cat("Mean:", round(mean(LR_map$lifetime_risk_percent_rectum, na.rm = TRUE), 2), "%\n\n")

cat("Risk Ratio (Rectum/Colorectal):\n")
cat("Range:", round(range(LR_map$risk_ratio_new, na.rm = TRUE), 2), "\n")
cat("Mean:", round(mean(LR_map$risk_ratio_new, na.rm = TRUE), 2), "\n")

# Identify top 5 countries for each metric
cat("\n=== TOP 5 COUNTRIES ===\n")
cat("Highest Colorectal Cancer Risk:\n")
print(LR_map %>% 
        arrange(desc(lifetime_risk_percent_colorectum)) %>% 
        select(region, lifetime_risk_percent_colorectum) %>% 
        head(5))

cat("\nHighest Rectal Cancer Risk:\n")
print(LR_map %>% 
        arrange(desc(lifetime_risk_percent_rectum)) %>% 
        select(region, lifetime_risk_percent_rectum) %>% 
        head(5))

cat("\nHighest Risk Ratio (Rectum/Colorectal):\n")
print(LR_map %>% 
        arrange(desc(risk_ratio_new)) %>% 
        select(region, risk_ratio_new) %>% 
        head(5))

# Show values for key countries
cat("\n=== KEY COUNTRIES VALUES ===\n")
key_countries_data <- LR_map %>% 
  filter(region %in% c("China", "USA", "Japan")) %>%
  select(region, lifetime_risk_percent_colorectum, lifetime_risk_percent_rectum, risk_ratio_new)

print(key_countries_data)