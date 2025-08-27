# =====================================
# Script: UN Population and Mortality Data Merger
# Author: Ke Pang (Enhanced by Claude)
# Date: 2025-08-12
# Purpose: Merge age-stratified population and mortality data from UN World Population Prospects
# Data: Creates combined dataset with Ni (population) and Mi (mortality) for lifetime risk calculations
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
BASE_DIR <- "Data/population data from UN"

# Input files
popmort <- read.csv(file.path(BASE_DIR,"population_mortality_combined_2024.csv"))

#2. Collapse 85+ groups
popmort <- popmort %>%
  mutate(
    AgeStart = as.numeric(AgeStart),
    AgeEnd   = as.numeric(AgeEnd)
  )

# Part 1: rows before 85
under85 <- popmort %>%
  filter(AgeStart < 85)

# Part 2: collapsed 85+
over85 <- popmort %>%
  filter(AgeStart >= 85) %>%
  group_by(ISOcode) %>%
  summarise(
    AgeStart = 85,
    AgeEnd = 999,
    Ni = sum(Ni, na.rm = TRUE),
    Mi = sum(Mi, na.rm = TRUE),
    .groups = "drop"
  )

# Combine and sort
popmort_collapsed <- bind_rows(under85, over85) %>%
  arrange(ISOcode,AgeStart)

head(popmort_collapsed)

write.csv(popmort_collapsed,file.path(BASE_DIR,"population_mortality_combined_85.csv"),row.names = FALSE)
