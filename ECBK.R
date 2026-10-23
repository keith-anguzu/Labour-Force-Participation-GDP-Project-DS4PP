# ============================================================
# Gender Gaps and Economic Growth in Africa
# Catherine, Anguzu, Eva, Ben
#
# Does narrowing the male-female labor force participation gap
# go with faster subsequent GDP per capita growth?
# Data. World Bank World Development Indicators, 54 African countries
# Data file is in Desktop/DSPP4/Midterm Project
# ============================================================

# ------------------------------------------------------------
# Step 0. Load packages
# ------------------------------------------------------------
library(tidyverse)

# ------------------------------------------------------------
# Step 1. Analysis settings (change years here)
# ------------------------------------------------------------
# Note the space at the end of "Midterm Project " in the folder name
data_folder <- "/Users/keithanguzu/Desktop/DSPP4/Midterm Project "
data_file   <- file.path(data_folder, "afdb88c0-4ab9-4c0e-9618-81b1b8c95026_Data.csv")
if (!file.exists(data_file)) stop("Data file not found. Check the path in data_folder.")

gap_start    <- 2010   # start of participation change window
gap_end      <- 2013   # end of participation change window
growth_start <- 2014   # first year of subsequent growth
growth_end   <- 2019   # last year of subsequent growth
outlier_cut  <- 8      # sensitivity check drops countries with |average growth| above this

growth_n_years <- growth_end - growth_start + 1

# ------------------------------------------------------------
# Step 2. Import the World Bank data
# ------------------------------------------------------------
# Import the data (".." marks missing values in World Bank exports)
data_raw <- read.csv(
  data_file,
  na.strings = c("..", ""),
  check.names = FALSE
)

dim(data_raw)
str(data_raw[, 1:6])
tail(data_raw)   # the last rows are World Bank footer notes, removed below

# ------------------------------------------------------------
# Step 3. Keep the three indicators and check duplicates
# ------------------------------------------------------------
indicator_codes <- c(
  "SL.TLF.CACT.FE.ZS",  # Female labor force participation
  "SL.TLF.CACT.MA.ZS",  # Male labor force participation
  "NY.GDP.PCAP.KD.ZG"   # GDP per capita growth
)

# Keep only the three indicators (this also drops the footer rows)
data_clean <- data_raw[
  data_raw[["Series Code"]] %in% indicator_codes,
]

dim(data_clean)
length(unique(data_clean[["Country Code"]]))   # number of countries

# Each country should appear once per indicator
sum(duplicated(data_clean[c("Country Code", "Series Code")]))

# ------------------------------------------------------------
# Step 4. Missing data audit
# ------------------------------------------------------------
yr <- function(y) paste0(y, " [YR", y, "]")

participation_rows <- data_clean[["Series Code"]] %in% indicator_codes[1:2]
growth_rows        <- data_clean[["Series Code"]] == "NY.GDP.PCAP.KD.ZG"

# Participation missing at the start or end of the gap window
missing_participation <- participation_rows &
  rowSums(is.na(data_clean[yr(c(gap_start, gap_end))])) > 0

data_clean[
  missing_participation,
  c("Country Name", "Series Name", yr(c(gap_start, gap_end)))
]

# Growth missing in at least one year of the growth window
growth_cols <- yr(growth_start:growth_end)
missing_growth <- growth_rows &
  rowSums(is.na(data_clean[growth_cols])) > 0

data_clean[
  missing_growth,
  c("Country Name", growth_cols)
]

# ------------------------------------------------------------
# Step 5. Reshape to a country-year panel
# ------------------------------------------------------------
panel <- data_clean %>%
  select(
    country_code = `Country Code`,
    country      = `Country Name`,
    series       = `Series Code`,
    matches("^\\d{4} \\[YR")
  ) %>%
  pivot_longer(
    cols = matches("^\\d{4} \\[YR"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.integer(substr(year, 1, 4)),
    value = as.numeric(value),
    series = recode(
      series,
      "SL.TLF.CACT.FE.ZS" = "female_lfp",
      "SL.TLF.CACT.MA.ZS" = "male_lfp",
      "NY.GDP.PCAP.KD.ZG" = "gdp_growth"
    )
  ) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  mutate(participation_gap = male_lfp - female_lfp)

glimpse(panel)

# Check for duplicate country-year rows
panel %>% count(country_code, year) %>% filter(n > 1)

# ------------------------------------------------------------
# Step 6. Long-run changes in the gap, 2000 to 2024
# ------------------------------------------------------------
long_run <- panel %>%
  filter(year %in% c(2000, 2024)) %>%
  select(country, year, female_lfp, male_lfp, participation_gap) %>%
  pivot_wider(
    names_from = year,
    values_from = c(female_lfp, male_lfp, participation_gap)
  ) %>%
  drop_na() %>%
  mutate(
    female_change = female_lfp_2024 - female_lfp_2000,
    male_change   = male_lfp_2024 - male_lfp_2000,
    gap_change    = participation_gap_2024 - participation_gap_2000,
    driver = case_when(
      female_change > 0 & male_change < 0 ~ "Women up, men down",
      female_change > 0 & male_change >= 0 ~ "Both up, women faster",
      female_change <= 0 & male_change < 0 ~ "Both down, men faster",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(gap_change)

long_run %>%
  select(country, participation_gap_2000, participation_gap_2024,
         female_change, male_change, gap_change, driver) %>%
  slice_head(n = 10)

# ------------------------------------------------------------
# Step 7. Plot the 15 largest gap reductions
# ------------------------------------------------------------
p_long_run <- long_run %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(country, gap_change), y = gap_change)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Changes in Gender Labor Force Participation Gaps",
    subtitle = "15 largest decreases, 2000–2024",
    x = NULL,
    y = "Change in male–female gap (percentage points)",
    caption = "Source: World Bank WDI (modeled ILO estimates)"
  ) +
  theme_minimal()

print(p_long_run)

# ------------------------------------------------------------
# Step 8. Build the analysis sample
# ------------------------------------------------------------
# Participation change over the gap window
gap_window <- panel %>%
  filter(year %in% c(gap_start, gap_end)) %>%
  mutate(period = if_else(year == gap_start, "start", "end")) %>%
  select(country, period, participation_gap, female_lfp, male_lfp) %>%
  pivot_wider(
    names_from = period,
    values_from = c(participation_gap, female_lfp, male_lfp)
  ) %>%
  mutate(
    gap_change    = participation_gap_end - participation_gap_start,
    gap_narrowing = -gap_change,   # positive = gap narrowed
    female_change = female_lfp_end - female_lfp_start,
    male_change   = male_lfp_end - male_lfp_start
  )

# Average growth over the growth window
future_growth <- panel %>%
  filter(year >= growth_start, year <= growth_end) %>%
  group_by(country) %>%
  summarise(
    growth_years = sum(!is.na(gdp_growth)),
    avg_future_growth = if (growth_years == 0) NA_real_
    else mean(gdp_growth, na.rm = TRUE),
    .groups = "drop"
  )

# Merge and keep countries with complete data
growth_analysis <- gap_window %>%
  left_join(future_growth, by = "country") %>%
  filter(
    !is.na(gap_change),
    !is.na(avg_future_growth),
    growth_years == growth_n_years
  )

nrow(growth_analysis)

# Countries dropped for missing data
setdiff(unique(panel$country), growth_analysis$country)

# ------------------------------------------------------------
# Step 9. Descriptive statistics
# ------------------------------------------------------------
growth_analysis %>%
  summarise(
    across(
      c(gap_narrowing, female_change, male_change, avg_future_growth),
      list(mean = mean, sd = sd, min = min, max = max)
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_pattern = "(.*)_(mean|sd|min|max)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ------------------------------------------------------------
# Step 10. Scatterplot of gap narrowing and growth
# ------------------------------------------------------------
p_scatter <- ggplot(growth_analysis, aes(x = gap_narrowing, y = avg_future_growth)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Gender Participation Gap Narrowing and Subsequent Growth",
    subtitle = paste0("African countries, ", gap_start, "–", gap_end,
                      " change and ", growth_start, "–", growth_end, " growth"),
    x = "Reduction in male–female participation gap (pp)",
    y = "Average annual real GDP per capita growth (%)",
    caption = "Source: World Bank WDI"
  ) +
  theme_minimal()

print(p_scatter)

# ------------------------------------------------------------
# Step 11. Pearson and Spearman correlations
# ------------------------------------------------------------
cor_table <- tibble(
  relationship = c("Gap narrowing and growth", "Female change and growth"),
  pearson = c(
    cor(growth_analysis$gap_narrowing, growth_analysis$avg_future_growth),
    cor(growth_analysis$female_change, growth_analysis$avg_future_growth)
  ),
  spearman = c(
    cor(growth_analysis$gap_narrowing, growth_analysis$avg_future_growth,
        method = "spearman"),
    cor(growth_analysis$female_change, growth_analysis$avg_future_growth,
        method = "spearman")
  )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cor_table

# ------------------------------------------------------------
# Step 12. Outlier sensitivity check
# ------------------------------------------------------------
outliers <- growth_analysis %>%
  filter(abs(avg_future_growth) > outlier_cut) %>%
  select(country, gap_change, female_change, male_change, avg_future_growth)

outliers

sensitivity_df <- growth_analysis %>%
  filter(abs(avg_future_growth) <= outlier_cut)

tibble(
  sample = c("Full sample", paste0("Growth within ±", outlier_cut, "%")),
  countries = c(nrow(growth_analysis), nrow(sensitivity_df)),
  correlation = c(
    cor(growth_analysis$gap_narrowing, growth_analysis$avg_future_growth),
    cor(sensitivity_df$gap_narrowing, sensitivity_df$avg_future_growth)
  )
)

# ------------------------------------------------------------
# Step 13. Regression models
# ------------------------------------------------------------
model1 <- lm(avg_future_growth ~ gap_narrowing, data = growth_analysis)
model2 <- lm(avg_future_growth ~ gap_narrowing, data = sensitivity_df)
model3 <- lm(avg_future_growth ~ female_change, data = growth_analysis)
model4 <- lm(avg_future_growth ~ female_change + male_change, data = growth_analysis)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# ------------------------------------------------------------
# Step 14. Summary table of all models
# ------------------------------------------------------------
model_row <- function(fit, name) {
  s <- summary(fit)
  tibble(
    model = name,
    term = rownames(s$coefficients)[-1],
    estimate = s$coefficients[-1, 1],
    std_error = s$coefficients[-1, 2],
    p_value = s$coefficients[-1, 4],
    r_squared = s$r.squared,
    n = nobs(fit)
  )
}

bind_rows(
  model_row(model1, "1. Full sample"),
  model_row(model2, "2. Excluding outliers"),
  model_row(model3, "3. Female change"),
  model_row(model4, "4. Female and male change")
) %>%
  mutate(across(where(is.numeric) & !n, ~ round(.x, 3)))