library(countrycode)
library(dplyr)

CC_LR <- read.csv("Data/lifetime_risk_colon.csv")
RC_LR <- read.csv("Data/lifetime_risk_rectum.csv")
CRC_LR <- read.csv("Data/lifetime_risk_colorectum.csv")

CC_LR_print <- CC_LR[,c("ISOcode","lifetime_risk_percent","ci_lower_percent","ci_upper_percent")]
colnames(CC_LR_print)[-1] <- paste0(colnames(CC_LR_print)[-1], "_CC")

RC_LR_print <- RC_LR[,c("ISOcode","lifetime_risk_percent","ci_lower_percent","ci_upper_percent")]
colnames(RC_LR_print)[-1] <- paste0(colnames(RC_LR_print)[-1], "_RC")

CRC_LR_print <- CRC_LR[,c("ISOcode","lifetime_risk_percent","ci_lower_percent","ci_upper_percent")]
colnames(CRC_LR_print)[-1] <- paste0(colnames(CRC_LR_print)[-1], "_CRC")


CC_RC_LR <- merge(CC_LR_print, RC_LR_print, by = "ISOcode")
CC_RC_CRC_LR <- merge(CC_RC_LR, CRC_LR_print, by = "ISOcode")



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

CC_RC_CRC_LR <- CC_RC_CRC_LR |>
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
  select(Region, lifetime_risk_percent_CC, lifetime_risk_percent_RC, lifetime_risk_percent_CRC,everything())


write.csv(CC_RC_CRC_LR,"Data/Lifetime_risk_of_developing.csv",row.names = FALSE)
