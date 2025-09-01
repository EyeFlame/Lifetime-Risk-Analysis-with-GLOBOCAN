library(dplyr)
library(ggplot2)
library(maps)
library(countrycode)
library(scatterpie)
library(ggnewscale)

# Read and process data
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

# Function to get country centroids for pie chart placement
get_country_centroids <- function(data, countries = NULL) {
  if (is.null(countries)) {
    # Get centroids for all countries with data
    data %>%
      group_by(region) %>%
      summarise(
        long = mean(long, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    # Get centroids for specific countries
    data %>%
      filter(region %in% countries) %>%
      group_by(region) %>%
      summarise(
        long = mean(long, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        .groups = 'drop'
      )
  }
}

# Function to create pie chart data for countries
create_pie_data <- function(world_data, df_map, ratio_var, key_countries = c("China", "USA", "Japan")) {
  # Get centroids for all countries
  country_centroids <- get_country_centroids(world_data)
  
  # Merge with ratio data
  pie_data <- country_centroids %>%
    left_join(df_map %>% select(region, all_of(ratio_var)), by = "region") %>%
    filter(!is.na(.data[[ratio_var]])) %>%
    mutate(
      # Create pie chart proportions
      rectal_prop = .data[[ratio_var]],
      other_prop = 1 - .data[[ratio_var]],
      # Determine pie chart size based on importance
      pie_size = ifelse(region %in% key_countries, 3, 1.5),
      # Create labels for key countries
      country_label = ifelse(region %in% key_countries, 
                             paste0(region, "\n", round(.data[[ratio_var]], 3)), 
                             "")
    )
  
  return(pie_data)
}

# Function to create map with both color coding and pie charts
create_color_pie_map <- function(world_data, df_map, ratio_var, title, key_countries = c("China", "USA", "Japan")) {
  
  # Create categorical breaks for color coding
  values <- df_map[[ratio_var]][!is.na(df_map[[ratio_var]])]
  breaks <- quantile(values, probs = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1), na.rm = TRUE)
  colors <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")
  labels <- paste0(round(breaks[-length(breaks)], 3), "-", round(breaks[-1], 3))
  
  # Add categorical variable to world_data
  world_data$fill_cat <- cut(world_data[[ratio_var]], breaks = breaks, labels = labels, include.lowest = TRUE)
  
  # Create pie chart data
  country_centroids <- get_country_centroids(world_data)
  pie_data <- country_centroids %>%
    left_join(df_map %>% select(region, all_of(ratio_var)), by = "region") %>%
    filter(!is.na(.data[[ratio_var]])) %>%
    mutate(
      # Create pie chart proportions
      rectal_prop = .data[[ratio_var]],
      other_prop = 1 - .data[[ratio_var]],
      # Determine pie chart size
      pie_size = case_when(
        region %in% key_countries ~ 4,  # Large for key countries
        .data[[ratio_var]] > quantile(values, 0.9) ~ 2.5,  # Medium for top 10%
        .data[[ratio_var]] < quantile(values, 0.1) ~ 2.5,  # Medium for bottom 10%
        TRUE ~ 1.5  # Small for others
      )
    )
  
  # Base map with color coding
  p <- ggplot() +
    geom_polygon(data = world_data, 
                 aes(x = long, y = lat, group = group, fill = fill_cat), 
                 color = "white", size = 0.1) +
    scale_fill_manual(name = "Ratio Range", values = colors, na.value = "grey90") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, order = 1)) +
    # Reset fill scale for pie charts
    new_scale_fill() +
    coord_fixed(1.3) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = title,
      subtitle = "Colors show ratio ranges; Pie charts show exact proportions (red=rectal, blue=other colorectal)",
      caption = "Source: GLOBOCAN 2022"
    )
  
  # Add pie charts with new fill scale
  p <- p + geom_scatterpie(data = pie_data,
                           aes(x = long, y = lat, r = pie_size),
                           cols = c("rectal_prop", "other_prop"),
                           color = "black", size = 0.3, alpha = 0.8) +
    scale_fill_manual(values = c("rectal_prop" = "#d73027", "other_prop" = "#4575b4"),
                      labels = c("Rectal Cancer", "Other Colorectal"),
                      name = "Pie Chart") +
    guides(fill = guide_legend(order = 2))
  
  # Add country labels for key countries
  key_labels <- pie_data %>% 
    filter(region %in% key_countries) %>%
    mutate(label_text = paste0(region, "\n", round(.data[[ratio_var]], 3)))
  
  p <- p + geom_text(data = key_labels,
                     aes(x = long, y = lat - 10, label = label_text),
                     size = 3.5, fontface = "bold", color = "black",
                     hjust = 0.5, vjust = 0.5,
                     bg.colour = "white", bg.r = 0.1)
  
  return(p)
}

# Alternative function using manual pie charts (if scatterpie doesn't work)
create_manual_pie_map <- function(world_data, df_map, ratio_var, title, key_countries = c("China", "USA", "Japan")) {
  
  # Get country centroids and data
  country_centroids <- get_country_centroids(world_data)
  
  pie_data <- country_centroids %>%
    left_join(df_map %>% select(region, all_of(ratio_var)), by = "region") %>%
    filter(!is.na(.data[[ratio_var]])) %>%
    mutate(
      ratio_value = .data[[ratio_var]],
      pie_size = ifelse(region %in% key_countries, 2.5, 1.2)
    )
  
  # Base world map
  p <- ggplot() +
    geom_polygon(data = world_data, 
                 aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "white", size = 0.1) +
    coord_fixed(1.3) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(20, 20, 20, 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = title,
      subtitle = "Pie charts show proportion of rectal cancer (red) vs other colorectal cancers (blue)",
      caption = "Source: GLOBOCAN 2022"
    )
  
  # Create pie charts manually for each country
  for (i in 1:nrow(pie_data)) {
    country_data <- pie_data[i, ]
    
    # Create pie chart data
    pie_df <- data.frame(
      category = c("Rectal", "Other"),
      value = c(country_data$ratio_value, 1 - country_data$ratio_value),
      angle_start = c(0, country_data$ratio_value * 2 * pi),
      angle_end = c(country_data$ratio_value * 2 * pi, 2 * pi)
    )
    
    # Create circle points for pie slices
    n_points <- 50
    
    for (j in 1:nrow(pie_df)) {
      if (pie_df$value[j] > 0) {
        angles <- seq(pie_df$angle_start[j], pie_df$angle_end[j], length.out = n_points)
        pie_slice <- data.frame(
          x = country_data$long + cos(angles) * country_data$pie_size,
          y = country_data$lat + sin(angles) * country_data$pie_size,
          category = pie_df$category[j]
        )
        
        # Add center point
        pie_slice <- rbind(
          data.frame(x = country_data$long, y = country_data$lat, category = pie_df$category[j]),
          pie_slice
        )
        
        color_fill <- ifelse(pie_df$category[j] == "Rectal", "#d73027", "#4575b4")
        
        p <- p + geom_polygon(data = pie_slice, 
                              aes(x = x, y = y), 
                              fill = color_fill, 
                              color = "black", 
                              size = 0.2)
      }
    }
  }
  
  # Add labels for key countries
  key_labels <- pie_data %>% 
    filter(region %in% key_countries) %>%
    mutate(label_text = paste0(region, "\n", round(ratio_value, 3)))
  
  p <- p + geom_text(data = key_labels,
                     aes(x = long, y = lat - 8, label = label_text),
                     size = 3.5, fontface = "bold", color = "black",
                     hjust = 0.5, vjust = 0.5)
  
  # Add manual legend
  p <- p + 
    annotate("rect", xmin = -180, xmax = -160, ymin = -60, ymax = -55, 
             fill = "#d73027", color = "black") +
    annotate("text", x = -155, y = -57.5, label = "Rectal Cancer", hjust = 0, size = 3) +
    annotate("rect", xmin = -180, xmax = -160, ymin = -70, ymax = -65, 
             fill = "#4575b4", color = "black") +
    annotate("text", x = -155, y = -67.5, label = "Other Colorectal", hjust = 0, size = 3)
  
  return(p)
}

# Merge with world map data
world_data <- world_map %>%
  left_join(df_map, by = "region")

# Create LRI ratio map with both colors and pie charts
map_lri_color_pie <- create_color_pie_map(world_data, df_map, "LRI_RC_to_CRC",
                                          "LRI Proportion: Rectal Cancer within Colorectal Cancer")

# Create LRM ratio map with both colors and pie charts  
map_lrm_color_pie <- create_color_pie_map(world_data, df_map, "LRM_RC_to_CRC",
                                          "LRM Proportion: Rectal Cancer within Colorectal Cancer")

# Display maps
print(map_lri_color_pie)
print(map_lrm_color_pie)

# Save maps
ggsave("LRI_Ratio_Color_Pie_Map.png", map_lri_color_pie, width = 14, height = 10, dpi = 300)
ggsave("LRM_Ratio_Color_Pie_Map.png", map_lrm_color_pie, width = 14, height = 10, dpi = 300)

# Print summary for key countries
key_countries_summary <- df_map %>%
  filter(region %in% c("China", "USA", "Japan")) %>%
  select(region, LRI_RC_to_CRC, LRM_RC_to_CRC) %>%
  arrange(region)

print("Proportion of Rectal Cancer within Colorectal Cancer for Key Countries:")
print(key_countries_summary)