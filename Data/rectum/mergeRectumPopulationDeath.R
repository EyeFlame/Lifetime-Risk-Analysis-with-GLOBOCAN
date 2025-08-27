library(dplyr)

population <- read.csv("unpopulation_countries.csv")
death <- read.csv("undeath_countries.csv")
death <- death %>% distinct()
colorectum <-read.csv("age_specific_colorectum_country.csv")
NM <- read.csv("NiMi.csv")

# population <- population[,c("LocationId","Location", "AgeStart","AgeEnd","Value")]
# death <- death[,c("LocationId","Location", "AgeStart","AgeEnd","Value")]
colorectum <- colorectum[,c("Region","AgeStart","AgeEnd","p_code","Ri","Di")]

colnames(colorectum)[4] <- "LocationId"
colnames(colorectum)[1] <- "Location"
# colnames(population)[5] <- "Ni"
# colnames(death)[5] <- "Mi"

# 提取各自的唯一值
region_colorectum <- unique(colorectum$LocationId)
region_death <- unique(death$LocationId)

# 比较两个列表中不一致的值
# 出现在 region_risk 中但不在 peridontal 中
not_in_perio <- setdiff(region_colorectum, region_population)

# 出现在 peridontal 中但不在 region_risk 中
not_in_risk <- setdiff(region_population, region_colorectum)

# 打印结果
cat("在终生风险数据中但不在人口数据中的地区：\n")
print(not_in_perio)

cat("在人口数据中但不在终生风险数据中的地区：\n")
print(not_in_risk)

filtered_population <- population %>% filter(!LocationId %in% not_in_risk)
filtered_death <- death %>% filter(!LocationId %in% not_in_risk)

NM <- left_join(filtered_death,filtered_population,by = c("LocationId","Location", "AgeStart","AgeEnd"))

write.csv(NM,"NiMi.csv",row.names = FALSE)

NM_Id <- NM[, c("LocationId", "AgeStart","AgeEnd","Ni","Mi")]
colorectum_Id <- colorectum[, c("LocationId", "AgeStart","AgeEnd","Ri","Di")]

NM_Id <- NM_Id %>%
  mutate(AgeStart = as.numeric(AgeStart),
         AgeEnd = as.numeric(AgeEnd))

# === Step 1: colorectum_Id - 拆分0-14 ===

# 找出原始 0-14 行
row_0_14 <- colorectum_Id %>% filter(AgeStart == 0 & AgeEnd == 14)

# 拆分为三段
colorectum_0_4 <- row_0_14 %>% mutate(AgeStart = 0, AgeEnd = 4, Ri = 0, Di = 0)
colorectum_5_9 <- row_0_14 %>% mutate(AgeStart = 5, AgeEnd = 9, Ri = 0, Di = 0)
colorectum_10_14 <- row_0_14 %>% mutate(AgeStart = 10, AgeEnd = 14)

# 剔除原始的 0-14 行
colorectum_Id <- colorectum_Id %>% filter(!(AgeStart == 0 & AgeEnd == 14))

# 添加拆分后的三行
colorectum_Id <- bind_rows(colorectum_Id, colorectum_0_4, colorectum_5_9, colorectum_10_14) %>%
  arrange(LocationId, AgeStart)

# === Step 2: NM_Id - 合并85岁以上 ===

# 过滤出85岁以下
NM_under_85 <- NM_Id %>% filter(AgeStart < 85)

# 合并85岁以上为一行
NM_over_85 <- NM_Id %>%
  filter(AgeStart >= 85) %>%
  group_by(LocationId) %>%
  summarise(AgeStart = 85, AgeEnd = 999,
            Ni = sum(Ni, na.rm = TRUE),
            Mi = sum(Mi, na.rm = TRUE), .groups = "drop") #这一步报错

# 合并上下两部分
NM_Id <- bind_rows(NM_under_85, NM_over_85) %>%
  arrange(LocationId, AgeStart)

# === Step 3: 合并两个数据框 ===


# 做左连接并标记哪些没有匹配上（NM_Id中的值会是NA）
merged_df <- left_join(colorectum_Id, NM_Id, by = c("LocationId", "AgeStart", "AgeEnd"))

# 过滤出未匹配的行（即右边的数据是NA的）
unmatched <- merged_df %>% filter(is.na(Ni))

# 找出colorectum_Id中有但NM_Id中没有的组合
anti_join(colorectum_Id, NM_Id, by = c("LocationId", "AgeStart", "AgeEnd"))

table(is.na(merged_df$some_column_from_NM_Id))

write.csv(merged_df,"merged_country_colorectum.csv",row.names = FALSE)



