library(dplyr)

colon <- read.csv("Data/LRI and LRM/lifetime_risk_colon_both_formatted.csv")
rectum <- read.csv("Data/LRI and LRM/lifetime_risk_rectum_both_formatted.csv")
colorectum <- read.csv("Data/LRI and LRM/lifetime_risk_colorectum_both_formatted.csv")

colon_print <- colon[,c("ISOcode","LRI_formatted","LRM_formatted")]
colnames(colon_print)[-1] <- paste0(colnames(colon_print)[-1], "_CC")

rectum_print <- rectum[,c("ISOcode","LRI_formatted","LRM_formatted")]
colnames(rectum_print)[-1] <- paste0(colnames(rectum_print)[-1], "_RC")

colorectum_print <- colorectum[,c("ISOcode","LRI_formatted","LRM_formatted")]
colnames(colorectum_print)[-1] <- paste0(colnames(colorectum_print)[-1], "_CRC")

df <- merge(colon_print, rectum_print, by = "ISOcode")
df <- merge(df, colorectum_print, by = "ISOcode")

# Create custom mapping for your special codes
custom_regions <- tibble(
  ISOcode = c(
    # ---- subregions ----
    900, 901, 902, 903, 904, 905, 906, 908, 909, 910,
    911, 912, 913, 914, 915, 916, 920, 922, 923, 924,
    925, 926, 927, 928, 931, 934, 935, 941, 947, 948,
    954, 957,
    # ---- HDI组 ----
    981, 982, 983, 984
  ),
  custom_region = c(
    "World",
    "Developed regions",
    "Developing regions",
    "Africa",
    "Latin America and the Caribbean",
    "Northern America",
    "Eastern Asia",
    "Europe",
    "Oceania",
    "Eastern Africa",
    "Middle Africa",
    "Northern Africa",
    "Southern Africa",
    "Western Africa",
    "Caribbean",
    "Central America",
    "South-Eastern Asia",
    "Western Asia",
    "Eastern Europe",
    "Northern Europe",
    "Southern Europe",
    "Western Europe",
    "Australia/New Zealand",
    "Melanesia",
    "South America",
    "Less developed regions, excluding least developed countries",
    "Asia",
    "Least developed countries",
    "Sub-Saharan Africa",
    "Developing regions, excluding China",
    "Micronesia",
    "Polynesia",
    "Very High HDI countries",
    "High HDI countries",
    "Medium HDI countries",
    "Low HDI countries"
  )
)

df <- df |>
  left_join(custom_regions, by = "ISOcode") |>
  mutate(
    Region = case_when(
      !is.na(custom_region) ~ custom_region,
      ISOcode < 900 ~ countrycode(ISOcode, origin = "iso3n", destination = "country.name.en"),
      ISOcode >= 900 & ISOcode < 980 ~ countrycode(ISOcode, origin = "iso3n", destination = "un.region.name"),
      TRUE ~ paste("Unknown code", ISOcode)
    )
  ) |>
  select(-custom_region) |>
  select(Region, LRI_formatted_CC, LRI_formatted_RC,LRI_formatted_CRC,LRM_formatted_CC, LRM_formatted_RC,LRM_formatted_CRC ,everything())

write.csv(df,"Figure/SP Table 2.csv",row.names = FALSE)
