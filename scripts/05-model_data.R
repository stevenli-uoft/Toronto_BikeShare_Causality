#### Preamble ####
# Purpose: Run Difference-in-Difference model and robustness tests
# Author: Steven Li
# Date: 1 December 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - '02-download_data.R', '03-clean_data.R' and '04-test_analysis_data.R' must have been run first
# - The required libraries listed below must be installed:
#     - tidyverse, fixest, modelsummary, and arrow


#### Workspace setup ####
library(tidyverse)
library(fixest)
library(modelsummary)
library(arrow)


# Read the data
analysis_data <- read_parquet("data/02-analysis_data/final_df.parquet")

################################ Basic DiD Model ############################
# Create main DiD model, with pre-treatment period as reference
main_did <- feols(
  monthly_rides_adj ~ 
    i(period, treatment, ref = "pre") | 
    bikeway_id + relative_month,            
  data = analysis_data
)

# Save model
saveRDS(main_did, file="models/main_did.rds")

# # Outut model stats
# modelsummary(
#   list("Main DiD" = main_did),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )

################################# Placebo Tests ##############################
# Randomly assign treatment
set.seed(778)

placebo_random <- analysis_data %>%
  # Get a list of unique bikeway IDs
  group_by(bikeway_id) %>%
  # Take the first row of each group to get treatment status
  slice(1) %>%
  # Randomly shuffle the treatment status across bikeways
  ungroup() %>%
  mutate(placebo_treatment = sample(treatment, n(), replace = FALSE)) %>%
  # Join back to the original data
  select(bikeway_id, placebo_treatment) %>%
  right_join(analysis_data, by = "bikeway_id")

# Run the placebo DiD
placebo_random_did <- feols(
  monthly_rides_adj ~ 
    i(period, placebo_treatment, ref = "pre") | 
    bikeway_id + relative_month,
  data = placebo_random
)

# Save model
saveRDS(placebo_random_did, file="models/placebo_random_did.rds")

# # Outut model stats
# modelsummary(
#   list("Random Treatment Placebo" = placebo_random_did),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )

############################# Parallel Trends Test ############################
# Filter the data to only include the pre-treatment period
pre_treatment_data <- analysis_data %>%
  filter(period == "pre" & relative_month < -5)

# Create interaction terms for relative months and treatment status
pre_treatment_data <- pre_treatment_data %>%
  mutate(relative_month_treatment_interaction = interaction(
    relative_month, treatment, drop = TRUE)
    )

# Run parallel trends regression
parallel_trends_test <- feols(
  monthly_rides_adj ~ 
    i(relative_month, treatment, ref = -6) | 
    bikeway_id + relative_month,
  data = pre_treatment_data
)

# Save model
saveRDS(parallel_trends_test, file="models/parallel_trends_test.rds")

# # Outut model stats
# modelsummary(
#   list("Parallel Trend Test" = parallel_trends_test),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )

######################## Heterogeneous Treatment Effects ######################
# Prepare data by creating a combined treatment-infrastructure type variable
analysis_data <- analysis_data %>%
  mutate(
    # Create a new variable that combines treatment status and infrastructure type
    treatment_infra = case_when(
      treatment == FALSE ~ "Control",
      treatment == TRUE & bikeway_type == "Protected Lanes" ~ "Treated-Protected",
      treatment == TRUE & bikeway_type == "On-Road Lanes" ~ "Treated-OnRoad",
      treatment == TRUE & bikeway_type == "Shared Roadways" ~ "Treated-Shared"
    ),
    # Make factors and set reference levels
    treatment_infra = relevel(factor(treatment_infra), ref = "Control"),
    sub_treatment_type = relevel(factor(sub_treatment_type), ref = "Upgraded"),
    period = factor(period, levels = c("pre", "treatment", "post"))
  )


#### Comparing treated bikeway type against control group, post-treatment period
did_hetero_lane_type <- feols(
  monthly_rides_adj ~ 
    i(period, treatment_infra, ref = "pre") - 1 |
    bikeway_id + relative_month,
  cluster = "bikeway_id",
  data = analysis_data
)

# Save model
saveRDS(did_hetero_lane_type, file="models/did_hetero_lane_type.rds")

# # Outut model stats
# modelsummary(
#   list("Infrastructure Type Comparison" = did_hetero_lane_type),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )


### Comparing upgraded, and newly-installed against post-treatment control group
did_hetero_treat_type <- feols(
  monthly_rides_adj ~ 
    i(period, sub_treatment_type, ref = "pre") - 1 |
    bikeway_id + relative_month,
  cluster = "bikeway_id",
  data = analysis_data
)

# Save model
saveRDS(did_hetero_treat_type, file="models/did_hetero_treat_type.rds")

# # Outut model stats
# modelsummary(
#   list("Installation Type Comparison" = did_hetero_treat_type),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )
