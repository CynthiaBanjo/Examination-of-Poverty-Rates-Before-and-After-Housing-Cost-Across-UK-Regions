setwd("C:/Users/t/OneDrive/Documents/University of Manchaster/Assignment 1")
library(tidyverse)
library(readxl)
library(janitor)  # helps clean column names
library(haven)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Household below average income datasets
hbai1821 <- read_sav("i1821e_2324prices.sav") %>% clean_names()
hbai2124 <- read_sav("i2124e_2324prices.sav") %>% clean_names()
hbai_0912 <- read_sav("i0912e_2324prices.sav") %>% clean_names()

# Check column names
names(hbai_0912)

# Check if column names match
colnames(hbai1821)
colnames(hbai2124)

# Bind Rows Together Once columns align:
hbai_all <- bind_rows(hbai1821, hbai2124)

# Explore the data
glimpse(hbai_all)
summary(hbai_all)
colnames(hbai_all)
colnames(hbai_0912)

#Check if certain characters exits in data
grep("poor", names(hbai_all), value = TRUE)
grep("low60abhc", names(hbai_all), value = TRUE)
grep("income", names(hbai_0912), value = TRUE)

#Select relevant columns
hbai_clean <- hbai_all %>%
  select(year,country,
         low50ahc, low60ahc, low70ahc,entinchh,
         eqoahchh, eqobhchh,
         low50bhc, low60bhc, low70bhc,
         mdch, mdpn, mdwa) %>%
  mutate(year = as.integer(year))


hbai_0912_selected <- hbai_0912 %>%
  select(
    ahcyrdef, bhcyrdef,year,             # Year definitions
    entinchh,             # Income AHC and BHC
    low50ahc, low60ahc, low70ahc,   # Poverty AHC
    low50bhc, low60bhc, low70bhc,   # Poverty BHC
    gvtregn
  )

grep("obhc", names(hbai_0912), value = TRUE, ignore.case = TRUE)
grep("inc", names(hbai_0912), value = TRUE, ignore.case = TRUE)
summary(hbai_0912_selected$entinchh)

baseline_2010 <- hbai_0912_selected %>%
  filter(year == 17) %>%
  summarise(median_2010 = median(entinchh, na.rm = TRUE)) %>%
  pull(median_2010)

absolute_threshold_2010 <- 0.6 * baseline_2010
absolute_threshold_2010

absolute_poverty <- hbai_0912_selected %>%
  group_by(year) %>%
  summarise(
    median_income = median(entinchh, na.rm = TRUE),
    abs_poverty_rate = mean(entinchh < absolute_threshold_2010, na.rm = TRUE)
  )
absolute_poverty

hbai_0912_selected %>%
  filter(year >= 25 & year <= 30) %>%
  group_by(year) %>%
  summarise(
    total = n(),
    missing = sum(is.na(entinchh)),
    missing_pct = 100 * mean(is.na(entinchh))
  )


#Apply threshold to 2018–2024 dataset
absolute_poverty_1824 <- hbai_clean %>%
  filter(year >= 25 & year <= 30) %>%
  group_by(year) %>%
  summarise(
    median_income = median(entinchh, na.rm = TRUE),
    abs_poverty_rate = mean(entinchh < absolute_threshold_2010, na.rm = TRUE),
    min_income = min(entinchh, na.rm = TRUE),
    max_income = max(entinchh, na.rm = TRUE)
  )
absolute_poverty_1824



glimpse(hbai_0912_selected)
summary(hbai_0912_selected)
colSums(is.na(hbai_0912_selected))

# Handle missing values
# Count missing values per column
colSums(is.na(hbai_clean))



#Visualization 1: Poverty Rate Trend

# A.Poverty Rate Trend Before housing cost in the uk
# Compute 2010/11 median BHC income (used as absolute poverty threshold)

absolute_threshold_bhc <- hbai_0912_selected %>%
  filter(year == 17) %>%  # 17 corresponds to 2010/11
  summarise(median_income_2010 = median(entinchh, na.rm = TRUE)) %>%
  pull(median_income_2010)

absolute_threshold_bhc

poverty_bhc_uk <- hbai_clean %>%
  group_by(year) %>%
  summarise(
    rel_poverty_60 = mean(low60bhc, na.rm = TRUE),
    deep_poverty_50 = mean(low50bhc, na.rm = TRUE),
    abs_poverty_2010 = mean(entinchh < absolute_threshold_bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(rel_poverty_60, deep_poverty_50, abs_poverty_2010),
    names_to = "poverty_type",
    values_to = "rate"
  ) %>%
  mutate(financial_year = case_when(
    year == 25 ~ "2018/19",
    year == 26 ~ "2019/20",
    year == 27 ~ "2020/21",
    year == 28 ~ "2021/22",
    year == 29 ~ "2022/23",
    year == 30 ~ "2023/24"
  ))

ggplot(poverty_bhc_uk, aes(x = financial_year, y = rate, color = poverty_type, group = poverty_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "deep_poverty_50" = "#e41a1c",
      "rel_poverty_60" = "#377eb8",
      "abs_poverty_2010" = "#4daf4a"
    ),
    labels = c(
      "deep_poverty_50" = "Deep Poverty (50% BHC)",
      "rel_poverty_60" = "Relative Poverty (60% BHC)",
      "abs_poverty_2010" = "Absolute Poverty (2010/11 Threshold)"
    )
  ) +
  labs(
    title = "UK Poverty Trend Before Housing Costs (2018/19–2023/24)",
    subtitle = "Proportion below 50%, 60% of median income, and 2010/11 absolute threshold (BHC)",
    x = "Financial Year",
    y = "Poverty Rate (%)",
    color = "Poverty Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#B.Poverty Rate Trend Before housing cost in each country in the uk
# Map country names to the country codes
hbai_clean <- hbai_clean %>%
  mutate(country_name = case_when(
    country == 1 ~ "England",
    country == 2 ~ "Wales",
    country == 3 ~ "Scotland",
    country == 4 ~ "Northern Ireland",
    TRUE ~ "Unknown"
  ))

absolute_threshold_bhc <- hbai_0912_selected %>%
  filter(year == 17) %>%  # 2010/11
  summarise(median_income_2010 = median(entinchh, na.rm = TRUE)) %>%
  pull(median_income_2010)
absolute_threshold_bhc

# Compute mean poverty rate by country and year (with absolute poverty)
poverty_bhc <- hbai_clean %>%
  group_by(year, country_name) %>%
  summarise(
    low50bhc_mean = mean(low50bhc, na.rm = TRUE),
    low60bhc_mean = mean(low60bhc, na.rm = TRUE),
    abs_poverty_2010 = mean(entinchh < absolute_threshold_bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(low50bhc_mean, low60bhc_mean, abs_poverty_2010),
    names_to = "poverty_type",
    values_to = "rate"
  ) %>%
  mutate(financial_year = case_when(
    year == 25 ~ "2018/19",
    year == 26 ~ "2019/20",
    year == 27 ~ "2020/21",
    year == 28 ~ "2021/22",
    year == 29 ~ "2022/23",
    year == 30 ~ "2023/24"
  ))
#Visual

ggplot(poverty_bhc, aes(x = financial_year, y = rate, color = poverty_type, group = poverty_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ country_name) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "low50bhc_mean" = "#e41a1c",
      "low60bhc_mean" = "#377eb8",
      "abs_poverty_2010" = "#4daf4a"
    ),
    labels = c(
      "low50bhc_mean" = "Deep Poverty (50% BHC)",
      "low60bhc_mean" = "Relative Poverty (60% BHC)",
      "abs_poverty_2010" = "Absolute Poverty (2010/11 Threshold)"
    )
  ) +
  labs(
    title = "Poverty Trends Before Housing Costs (2018/19–2023/24)",
    subtitle = "By Country and Poverty Threshold",
    x = "Financial Year",
    y = "Poverty Rate (%)",
    color = "Poverty Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Poverty After housing cost in the uk
# Compute UK average AHC poverty rates by year

# Compute absolute poverty threshold (from 2010/11)
absolute_threshold_bhc <- hbai_0912_selected %>%
  filter(year == 17) %>%  # 17 = 2010/11
  summarise(median_2010 = median(entinchh, na.rm = TRUE)) %>%
  pull(median_2010) * 0.6

message("Absolute poverty threshold (60% of 2010/11 median): ", round(absolute_threshold_bhc, 2))

absolute_threshold_2010 <- hbai_0912_selected %>%
  filter(year == 17) %>%
  summarise(
    median_income_2010 = median(entinchh, na.rm = TRUE)
  ) %>%
  mutate(threshold = 0.6 * median_income_2010) %>%
  pull(threshold)

absolute_threshold_2010

# Compute poverty metrics after housing costs
poverty_ahc_1824 <- hbai_clean %>%
  group_by(year) %>%
  summarise(
    median_income_ahc = median(entinchh, na.rm = TRUE),
    relative_poverty_60 = mean(entinchh < 0.6 * median(entinchh, na.rm = TRUE), na.rm = TRUE),
    deep_poverty_50 = mean(entinchh < 0.5 * median(entinchh, na.rm = TRUE), na.rm = TRUE),
    absolute_poverty_2010 = mean(entinchh < absolute_threshold_2010, na.rm = TRUE),
    .groups = "drop"
  )
poverty_ahc_1824
poverty_ahc_1824 <- poverty_ahc_1824 %>%
  mutate(financial_year = case_when(
    year == 25 ~ "2018/19",
    year == 26 ~ "2019/20",
    year == 27 ~ "2020/21",
    year == 28 ~ "2021/22",
    year == 29 ~ "2022/23",
    year == 30 ~ "2023/24"
  ))

poverty_ahc_long <- poverty_ahc_1824 %>%
  pivot_longer(cols = c(relative_poverty_60, deep_poverty_50, absolute_poverty_2010),
               names_to = "poverty_type",
               values_to = "rate")

ggplot(poverty_ahc_long, aes(x = financial_year, y = rate, color = poverty_type, group = poverty_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "relative_poverty_60" = "#377eb8",
      "deep_poverty_50" = "#e41a1c",
      "absolute_poverty_2010" = "#4daf4a"
    ),
    labels = c(
      "relative_poverty_60" = "Relative Poverty (60% AHC)",
      "deep_poverty_50" = "Deep Poverty (50% AHC)",
      "absolute_poverty_2010" = "Absolute Poverty (2010 Threshold)"
    )
  ) +
  labs(
    title = "UK Poverty Trend After Housing Costs (2018/19–2023/24)",
    subtitle = "Proportion of population below 50%, 60% and 2010 absolute threshold (After Housing Costs)",
    x = "Financial Year",
    y = "Poverty Rate (%)",
    color = "Poverty Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#After housing cost for Each country in the uk
hbai_clean <- hbai_clean %>%
  mutate(country_name = case_when(
    country == 1 ~ "England",
    country == 2 ~ "Wales",
    country == 3 ~ "Scotland",
    country == 4 ~ "Northern Ireland",
    TRUE ~ "Unknown"
  ))
poverty_ahc_country <- hbai_clean %>%
  group_by(year, country_name) %>%
  summarise(
    median_income_ahc = median(entinchh, na.rm = TRUE),
    relative_poverty_60 = mean(entinchh < 0.6 * median(entinchh, na.rm = TRUE), na.rm = TRUE),
    deep_poverty_50 = mean(entinchh < 0.5 * median(entinchh, na.rm = TRUE), na.rm = TRUE),
    absolute_poverty_2010 = mean(entinchh < absolute_threshold_2010, na.rm = TRUE),
    .groups = "drop"
  )
poverty_ahc_country <- poverty_ahc_country %>%
  mutate(financial_year = case_when(
    year == 25 ~ "2018/19",
    year == 26 ~ "2019/20",
    year == 27 ~ "2020/21",
    year == 28 ~ "2021/22",
    year == 29 ~ "2022/23",
    year == 30 ~ "2023/24"
  ))
poverty_ahc_country_long <- poverty_ahc_country %>%
  pivot_longer(
    cols = c(relative_poverty_60, deep_poverty_50, absolute_poverty_2010),
    names_to = "poverty_type",
    values_to = "rate"
  )

ggplot(poverty_ahc_country_long,
       aes(x = financial_year, y = rate, color = poverty_type, group = poverty_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ country_name) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "relative_poverty_60" = "#377eb8",
      "deep_poverty_50" = "#e41a1c",
      "absolute_poverty_2010" = "#4daf4a"
    ),
    labels = c(
      "relative_poverty_60" = "Relative Poverty (60% AHC)",
      "deep_poverty_50" = "Deep Poverty (50% AHC)",
      "absolute_poverty_2010" = "Absolute Poverty (2010 Threshold)"
    )
  ) +
  labs(
    title = "Poverty Trends After Housing Costs (2018/19–2023/24)",
    subtitle = "By Country and Poverty Type (After Housing Costs)",
    x = "Financial Year",
    y = "Poverty Rate (%)",
    color = "Poverty Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

# Visualization 2: Comparison of Poverty Rates — AHC vs BHC
# Summarize BHC and AHC poverty rates by year
poverty_compare <- hbai_clean %>%
  group_by(year) %>%
  summarise(
    low60ahc_mean = mean(low60ahc, na.rm = TRUE),
    low60bhc_mean = mean(low60bhc, na.rm = TRUE)
  ) %>%
  mutate(financial_year = case_when(
    year == 25 ~ "2018/19",
    year == 26 ~ "2019/20",
    year == 27 ~ "2020/21",
    year == 28 ~ "2021/22",
    year == 29 ~ "2022/23",
    year == 30 ~ "2023/24",
    TRUE ~ as.character(year)
  ))

# Convert to long format for plotting
poverty_compare_long <- poverty_compare %>%
  tidyr::pivot_longer(
    cols = c(low60ahc_mean, low60bhc_mean),
    names_to = "measure",
    values_to = "poverty_rate")

# Visualize
ggplot(poverty_compare_long,
       aes(x = financial_year, y = poverty_rate, color = measure, group = measure)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Comparison of Poverty Rates (AHC vs BHC)",
    subtitle = "UK Poverty Trends (2018/19–2023/24)",
    x = "Financial Year",
    y = "Poverty Rate (%)",
    color = "Measure"
  ) +
  scale_color_manual(
    values = c("low60ahc_mean" = "#1f77b4", "low60bhc_mean" = "#2ca02c"),
    labels = c("After Housing Costs (AHC)", "Before Housing Costs (BHC)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#Comparison BHC and AHC for 2018/2019
poverty_compare_2018 <- hbai_clean %>%
  filter(year == 25) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2018, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2018/19)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


#Comparison BHC and AHC for 2019/2020
poverty_compare_2019 <- hbai_clean %>%
  filter(year == 26) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2019, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2019/20)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#Comparison BHC and AHC for 2020/2021
poverty_compare_2020 <- hbai_clean %>%
  filter(year == 27) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2020, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2020/21)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#Comparison BHC and AHC for 2021/2022
poverty_compare_2021 <- hbai_clean %>%
  filter(year == 28) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2021, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2021/22)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#Comparison BHC and AHC for 2022/2023
poverty_compare_2022 <- hbai_clean %>%
  filter(year == 29) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2022, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2022/23)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
#Comparison BHC and AHC for 2023/2024
poverty_compare_2023 <- hbai_clean %>%
  filter(year == 30) %>%   # 2018/19
  group_by(country_name) %>%
  summarise(
    AHC = mean(low60ahc, na.rm = TRUE),
    BHC = mean(low60bhc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(AHC, BHC),
               names_to = "Type",
               values_to = "Rate")

# --- Visualize ---
ggplot(poverty_compare_2023, aes(x = country_name, y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(Rate, accuracy = 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4) +
  scale_fill_manual(
    values = c(
      "AHC" = "#e41a1c",
      "BHC" = "#377eb8"
    ),
    labels = c(
      "AHC" = "After Housing Costs",
      "BHC" = "Before Housing Costs"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title = "Comparison of Poverty Rates — AHC vs BHC vs Absolute (2023/24)",
    subtitle = "Proportion below 60% of median (AHC/BHC), by country",
    x = "Country",
    y = "Poverty Rate (%)",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )
