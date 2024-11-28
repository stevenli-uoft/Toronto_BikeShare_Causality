#### Preamble ####
# Purpose: Run Difference-in-Difference model and validation tests
# Author: Steven Li
# Date: 28 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - '02-download_data.R' and '03-clean_data.R' must have been run first
# - The required libraries listed below must be installed:
#     - readr, tidyverse, fs, lubridate


#### Workspace setup ####
library(tidyverse)
library(fixest)
library(modelsummary)
library(ggplot2)

# Read the data
analysis_data <- read_csv("data/02-analysis_data/final_df.csv")


# 1. Basic DiD Model
# Use seasonally adjusted rides as dependent variable
basic_did <- feols(
  bikeway_monthly_rides_adjusted ~ 
    i(period, treatment, ref = "pre") | # DiD interaction terms
    bikeway_id + year_month,            # Fixed effects
  data = analysis_data
)

# 2. Event Study Model to test parallel trends
# Create relative month factor, setting -1 as reference
analysis_data <- analysis_data %>%
  mutate(relative_month_factor = factor(relative_month))

event_study <- feols(
  bikeway_monthly_rides_adjusted ~
    i(relative_month_factor, treatment, ref = "-1") | # Event study interaction
    bikeway_id + year_month,                          # Fixed effects
  data = analysis_data
)

# 3. Create visualization of treatment effects over time
event_study_plot <- ggplot(
  data = broom::tidy(event_study) %>%
    filter(str_detect(term, "treatment::")) %>%
    mutate(
      time = as.numeric(str_extract(term, "-?\\d+")),
      ci_lower = estimate - 1.96 * std.error,
      ci_upper = estimate + 1.96 * std.error
    ),
  aes(x = time, y = estimate)
) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Months Relative to Treatment",
    y = "Treatment Effect on Monthly Rides",
    title = "Event Study: Effect of Bikeway Upgrades on Ridership",
    subtitle = "Controlling for bikeway and time fixed effects"
  ) +
  theme_minimal()

# 4. Calculate average treatment effects by period
treatment_effects <- analysis_data %>%
  group_by(period, treatment) %>%
  summarise(
    mean_rides = mean(bikeway_monthly_rides_adjusted, na.rm = TRUE),
    sd_rides = sd(bikeway_monthly_rides_adjusted, na.rm = TRUE),
    n_obs = n()
  ) %>%
  ungroup()

# 5. Test for pre-trends
pre_trends_test <- analysis_data %>%
  filter(period == "pre") %>%
  feols(
    bikeway_monthly_rides_adjusted ~ 
      relative_month * treatment |
      bikeway_id + year_month,
    data = .
  )

# Print summary of models
summary(basic_did)
summary(event_study)
summary(pre_trends_test)

# Create regression table
models <- list(
  "Basic DiD" = basic_did,
  "Pre-trends" = pre_trends_test
)

modelsummary(
  models,
  stars = FALSE,
  gof_map = c("nobs", "r.squared")
)