library(dplyr)
library(ggplot2)
library(maps)
library(countrycode)
library(viridis)

# Read and process data
df <- read.csv("Data/LRI and LRM/LRI_and_LRM.csv")

df <- df |>
  mutate(LRI_RC_to_CC = LRI_RC / LRI_CC,
         LRM_RC_to_CC = LRM_RC / LRM_CC,
         LRI_RC_to_CRC = LRI_RC / LRI_CRC,
         LRM_RC_to_CRC = LRM_RC / LRM_CRC)

write.csv(df, "Data/LRI and LRM/LRI_and_LRM_ratio.csv")

df <- df |>
  mutate(Region = countrycode(ISOcode, origin = "iso3n", destination = "country.name.en")) |>
  select(Region, everything())

# Get world map data
world_map <- map_data("world")

df_map <- df %>%
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

# Merge with world map data
world_data <- world_map %>%
  left_join(df_map, by = "region")

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
      subtitle = "Data shows lifetime risk by country",
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
  
  # Add key country labels (China, Japan, USA)
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

# Create maps for each metric

# 1. LRI Colorectal Cancer Map
lri_colorectal_rankings <- create_ranking_labels(df_map, "LRI_CC")
map_lri_colorectal <- create_elegant_map(
  world_data, 
  "LRI_CC", 
  "Lifetime Risk Index (LRI) - Colorectal Cancer", 
  "LRI Colorectal",
  color_palette = "blue",
  ranking_data = lri_colorectal_rankings
)

# 2. LRM Colorectal Cancer Map
lrm_colorectal_rankings <- create_ranking_labels(df_map, "LRM_CC")
map_lrm_colorectal <- create_elegant_map(
  world_data, 
  "LRM_CC", 
  "Lifetime Risk of Mortality (LRM) - Colorectal Cancer", 
  "LRM Colorectal",
  color_palette = "red",
  ranking_data = lrm_colorectal_rankings
)

# 3. LRI Rectal Cancer Map
lri_rectal_rankings <- create_ranking_labels(df_map, "LRI_RC")
map_lri_rectal <- create_elegant_map(
  world_data, 
  "LRI_RC", 
  "Lifetime Risk Index (LRI) - Rectal Cancer", 
  "LRI Rectal",
  color_palette = "blue",
  ranking_data = lri_rectal_rankings
)

# 4. LRM Rectal Cancer Map
lrm_rectal_rankings <- create_ranking_labels(df_map, "LRM_RC")
map_lrm_rectal <- create_elegant_map(
  world_data, 
  "LRM_RC", 
  "Lifetime Risk of Mortality (LRM) - Rectal Cancer", 
  "LRM Rectal",
  color_palette = "red",
  ranking_data = lrm_rectal_rankings
)

# 5. LRI Ratio Map (Rectal to Colorectal)
lri_ratio_rankings <- create_ranking_labels(df_map, "LRI_RC_to_CC")
map_lri_ratio <- create_elegant_map(
  world_data, 
  "LRI_RC_to_CC", 
  "LRI Ratio: Rectal Cancer to Colorectal Cancer", 
  "LRI Ratio (RC/CC)",
  color_palette = "heatmap",
  ranking_data = lri_ratio_rankings
)

# 6. LRM Ratio Map (Rectal to Colorectal)
lrm_ratio_rankings <- create_ranking_labels(df_map, "LRM_RC_to_CC")
map_lrm_ratio <- create_elegant_map(
  world_data, 
  "LRM_RC_to_CC", 
  "LRM Ratio: Rectal Cancer to Colorectal Cancer", 
  "LRM Ratio (RC/CC)",
  color_palette = "heatmap",
  ranking_data = lrm_ratio_rankings
)

# Display all maps
print(map_lri_colorectal)
print(map_lrm_colorectal)
print(map_lri_rectal)
print(map_lrm_rectal)
print(map_lri_ratio)
print(map_lrm_ratio)

# Optional: Save maps as high-quality images
ggsave("LRI_Colorectal_Map.png", map_lri_colorectal, width = 12, height = 8, dpi = 300)
ggsave("LRM_Colorectal_Map.png", map_lrm_colorectal, width = 12, height = 8, dpi = 300)
ggsave("LRI_Rectal_Map.png", map_lri_rectal, width = 12, height = 8, dpi = 300)
ggsave("LRM_Rectal_Map.png", map_lrm_rectal, width = 12, height = 8, dpi = 300)
ggsave("LRI_Ratio_Map.png", map_lri_ratio, width = 12, height = 8, dpi = 300)
ggsave("LRM_Ratio_Map.png", map_lrm_ratio, width = 12, height = 8, dpi = 300)

# Create summary table for key countries
key_countries_summary <- df_map %>%
  filter(region %in% c("China", "USA", "Japan")) %>%
  select(region, LRI_CC, LRM_CC, LRI_RC, LRM_RC, LRI_RC_to_CC, LRM_RC_to_CC) %>%
  arrange(region)

print("Summary for Key Countries:")
print(key_countries_summary)