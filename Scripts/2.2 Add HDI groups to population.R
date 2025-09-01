# =====================================
# Script: Add HDI groups to population data
# Author: Ke Pang
# Date: 2025-08-26
# Purpose: Add HDI groups as population with ISOcode 981,982,983,984 to population data
# Data: population_mortality_combined_85.csv,HDI_2022
# =====================================

library(dplyr)
library(countrycode)
library(rio)

#1. Input combined population data and HDI_2022----

UN_population <- read.csv("Data/population data from UN/popu_mort_2022_female.csv")
HDI <- import("Data/HDI index/HDI_2022.xlsx")


#2. Add ISO codes to HDI data----
HDI <- HDI |>
  mutate(ISOcode = countrycode(Region, origin = "country.name.en", destination = "iso3n"))

#3. Merge UN_population with HDI data to get HDI levels----
population_with_HDI <- UN_population |>
  left_join(HDI[c("ISOcode", "HDI_level")], by = "ISOcode") |>
  filter(!is.na(HDI_level))  # Keep only countries with HDI level data

#4. Create HDI group aggregations----
HDI_groups <- population_with_HDI |>
  group_by(HDI_level, AgeStart, AgeEnd) |>
  summarise(
    Ni = sum(Ni, na.rm = TRUE),
    Mi = sum(Mi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ISOcode = case_when(
      HDI_level == 1 ~ 981,  # Very High HDI
      HDI_level == 2 ~ 982,  # High HDI
      HDI_level == 3 ~ 983,  # Medium HDI
      HDI_level == 4 ~ 984   # Low HDI
    )
  ) |>
  select(ISOcode, AgeStart, AgeEnd, Ni, Mi)

#5. Combine original data with HDI groups----
UN_population_extended <- bind_rows(UN_population, HDI_groups) |>
  arrange(ISOcode, AgeStart)

#6. View results----

print("HDI group entries (ISOcode 981-984):")
print(UN_population_extended |> filter(ISOcode %in% 981:984))

write.csv(UN_population_extended,"Data/population data from UN/popu_mort_2022_HDI_female.csv",row.names = FALSE)

