library(dplyr)
library(rio)
library(countrycode)

colon <- read.csv("Data/LRI and LRM/lifetime_risk_colon_both.csv")
rectum <- read.csv("Data/LRI and LRM/lifetime_risk_rectum_both.csv")
colorectum <- read.csv("Data/LRI and LRM/lifetime_risk_colorectum_both.csv")

colon <- colon[,c("ISOcode","LRI_lifetime_risk_percent","LRM_lifetime_risk_percent")]
colnames(colon)[-1] <- c("LRI_CC","LRM_CC")

rectum <- rectum[,c("ISOcode","LRI_lifetime_risk_percent","LRM_lifetime_risk_percent")]
colnames(rectum)[-1] <- c("LRI_RC","LRM_RC")

colorectum <- colorectum[,c("ISOcode","LRI_lifetime_risk_percent","LRM_lifetime_risk_percent")]
colnames(colorectum)[-1] <- c("LRI_CRC","LRM_CRC")

df <- merge(colon, rectum, by = "ISOcode")
df <- merge(df, colorectum, by = "ISOcode")

write.csv(df,"Data/LRI and LRM/LRI_and_LRM.csv",row.names = FALSE)
df <- read.csv("Data/LRI and LRM/LRI_and_LRM.csv")

HDI <- import("Data/HDI index/HDI_2022.xlsx")

HDI <- HDI |>
  mutate(ISOcode = countrycode(Region,origin = "country.name.en",destination = "iso3n"))

df_HDI <- merge(df,HDI,by = "ISOcode")

library(dplyr)

# df 里有，但 HDI 没有的
not_in_HDI <- anti_join(df, HDI, by = "ISOcode")
not_in_HDI <- not_in_HDI |>
  mutate(Region = countrycode(ISOcode,origin = "iso3n",destination = "country.name.en"))

# HDI 里有，但 df 没有的
not_in_df <- anti_join(HDI, df, by = "ISOcode")



