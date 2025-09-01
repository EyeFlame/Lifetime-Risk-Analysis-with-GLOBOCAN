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

write.csv(df, "Data/LRI and LRM/LRI_and_LRM_ratio.csv",row.names = FALSE)
df <- read.csv("Data/LRI and LRM/LRI_and_LRM_ratio.csv")

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

# Function to create elegant maps with categorical color scheme
create_elegant_map <- function(data, fill_var, title, legend_title, metric_type = "LRI", 
                               key_countries = c("China", "USA", "Japan")) {
  
  # Create categorical breaks based on data distribution
  values <- data[[fill_var]][!is.na(data[[fill_var]])]
  
  if (metric_type == "LRI") {
    # LRI color scheme (blues to reds like left map)
    breaks <- quantile(values, probs = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1), na.rm = TRUE)
    colors <- c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027")
    labels <- paste0(round(breaks[-length(breaks)], 2), "-", round(breaks[-1], 2))
  } else if (metric_type == "LRM") {
    # LRM color scheme (greens to purples like right map, no white)
    breaks <- quantile(values, probs = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1), na.rm = TRUE)
    colors <- c("#1a9850", "#66bd63", "#a6d96a", "#d9f0a3", "#FFCCFF", "#dda0dd", "#c994c7", "#df65b0")
    labels <- paste0(round(breaks[-length(breaks)], 2), "-", round(breaks[-1], 2))
  } else {
    # Ratio color scheme (diverging)
    breaks <- quantile(values, probs = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1), na.rm = TRUE)
    colors <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")
    labels <- paste0(round(breaks[-length(breaks)], 3), "-", round(breaks[-1], 3))
  }
  
  # Create categorical variable
  data$fill_cat <- cut(data[[fill_var]], breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Base map
  p <- ggplot(data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = fill_cat), color = "white", size = 0.1) +
    scale_fill_manual(name = legend_title, values = colors, na.value = "grey90") +
    coord_fixed(1.3) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = title,
      subtitle = "Data shows lifetime risk by country",
      caption = "Source: GLOBOCAN 2022"
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  # Add key country labels (China, Japan, USA only)
  key_centroids <- get_country_centroids(data, key_countries)
  key_labels <- key_centroids %>%
    left_join(data %>% 
                distinct(region, .keep_all = TRUE) %>%
                select_at(c("region", fill_var)), by = "region") %>%
    filter(!is.na(.data[[fill_var]])) %>%
    mutate(label_text = paste0(region, "\n", round(.data[[fill_var]], 3)))
  
  p <- p + geom_label(data = key_labels,
                      aes(x = long, y = lat, label = label_text, group = NULL),
                      size = 3.5, fontface = "bold", 
                      fill = "white", alpha = 0.9, color = "black",
                      label.padding = unit(0.3, "lines"))
  
  return(p)
}

# Create maps for each metric

# 1. LRI Colorectal Cancer Map
map_lri_colorectal <- create_elegant_map(
  world_data, 
  "LRI_CRC", 
  "Lifetime Risk Index (LRI) - Colorectal Cancer", 
  "Lifetime Risk (%)",
  metric_type = "LRI"
)

# 2. LRM Colorectal Cancer Map
map_lrm_colorectal <- create_elegant_map(
  world_data, 
  "LRM_CRC", 
  "Lifetime Risk of Mortality (LRM) - Colorectal Cancer", 
  "Lifetime Risk (%)",
  metric_type = "LRM"
)

# 3. LRI Rectal Cancer Map
map_lri_rectal <- create_elegant_map(
  world_data, 
  "LRI_RC", 
  "Lifetime Risk Index (LRI) - Rectal Cancer", 
  "Lifetime Risk (%)",
  metric_type = "LRI"
)

# 4. LRM Rectal Cancer Map
map_lrm_rectal <- create_elegant_map(
  world_data, 
  "LRM_RC", 
  "Lifetime Risk of Mortality (LRM) - Rectal Cancer", 
  "Lifetime Risk (%)",
  metric_type = "LRM"
)

# 5. LRI Ratio Map (Rectal to Colorectal)
map_lri_ratio <- create_elegant_map(
  world_data, 
  "LRI_RC_to_CRC", 
  "LRI Ratio: Rectal Cancer to Colorectal Cancer", 
  "Ratio (RC/CRC)",
  metric_type = "ratio"
)

# 6. LRM Ratio Map (Rectal to Colorectal)
map_lrm_ratio <- create_elegant_map(
  world_data, 
  "LRM_RC_to_CRC", 
  "LRM Ratio: Rectal Cancer to Colorectal Cancer", 
  "Ratio (RC/CRC)",
  metric_type = "ratio"
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