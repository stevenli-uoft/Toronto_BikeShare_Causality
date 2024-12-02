#### Preamble ####
# Purpose: Testing cleaned bike share and bike lane data
# Author: Steven Li
# Date: 1 December 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - 03-clean_data.R must have been run first
# - The required packages must be installed:
#     - tidyverse, testthat, arrow

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

# Read in the cleaned data
bike_lanes <- read_parquet("data/02-analysis_data/bikeway_data.parquet")
final_df <- read_parquet("data/02-analysis_data/final_df.parquet")

######################## Test Bike Lanes Data Structure ######################
test_that("Bike lanes data has correct structure", {
  # Test presence of required columns
  expected_columns <- c("OBJECTID", "INSTALLED", "UPGRADED", "INFRA_HIGHORDER")
  expect_true(all(expected_columns %in% colnames(bike_lanes)))
  
  # Test data types
  expect_true(is.numeric(bike_lanes$OBJECTID))
  expect_true(is.numeric(bike_lanes$INSTALLED))
  expect_true(is.numeric(bike_lanes$UPGRADED))
  expect_true(is.character(bike_lanes$INFRA_HIGHORDER))
})

test_that("Bike lane types are valid", {
  valid_types <- c("Protected Lanes", "On-Road Lanes", "Shared Roadways")
  
  # Test that all types are valid
  expect_true(all(bike_lanes$INFRA_HIGHORDER %in% valid_types))
  
  # Test that all types are represented
  expect_equal(length(unique(bike_lanes$INFRA_HIGHORDER)), length(valid_types))
})

test_that("Bike Way data has valid values", {
  # Test that all values are valid, no NA's and years are within boundaries
  expect_true(all(!is.na(bike_lanes$OBJECTID)))
  expect_true(all(!is.na(bike_lanes$INSTALLED)))
  expect_true(all(bike_lanes$INSTALLED >= 2001 & 
                    bike_lanes$INSTALLED <= 2023))
})

test_that("Bike Way Route IDs are unique", {
  # Test that there are unique bikeways
  expect_equal(length(unique(bike_lanes$OBJECTID)), nrow(bike_lanes))
})


######################## Test Final Dataset Structure #######################
test_that("Final dataset has correct structure", {
  # Test presence of required columns
  expected_columns <- c("bikeway_id", "construct_year", "period", "treatment",
                        "year_month", "calendar_year", "relative_month", 
                        "bikeway_type", "sub_treatment_type",
                        "monthly_rides", "monthly_rides_adj")
  expect_true(all(expected_columns %in% colnames(final_df)))
  
  # Test data types
  expect_true(is.numeric(final_df$bikeway_id))
  expect_true(is.numeric(final_df$construct_year))
  expect_true(is.character(final_df$period))
  expect_true(is.logical(final_df$treatment))
  expect_true(inherits(final_df$year_month, "POSIXct"))
  expect_true(is.numeric(final_df$calendar_year))
  expect_true(is.numeric(final_df$relative_month))
  expect_true(is.character(final_df$bikeway_type))
  expect_true(is.character(final_df$sub_treatment_type))
  expect_true(is.numeric(final_df$monthly_rides))
  expect_true(is.numeric(final_df$monthly_rides_adj))
})

######################## Test Final Dataset Categories #####################
test_that("Final dataset categories are valid", {
  # Test period categories
  valid_periods <- c("pre", "treatment", "post")
  expect_true(all(final_df$period %in% valid_periods))
  
  # Test bikeway types
  valid_types <- c("Protected Lanes", "On-Road Lanes", "Shared Roadways")
  expect_true(all(final_df$bikeway_type %in% valid_types))
  
  # Test treatment types
  valid_treatments <- c("Upgraded", "Newly-Installed", "No Treatment")
  expect_true(all(final_df$sub_treatment_type %in% valid_treatments))
})

######################## Test Final Dataset Temporal Logic #################
test_that("Final dataset temporal logic is correct", {
  # Test relative month ranges
  # Pre-period: -24 to -1
  # Treatment period: 0
  # Post-period: 1 to 24
  pre_period <- final_df %>% filter(period == "pre")
  expect_true(all(pre_period$relative_month >= -24 & pre_period$relative_month <= -1))
  
  treatment_period <- final_df %>% filter(period == "treatment")
  expect_true(all(treatment_period$relative_month == 0))
  
  post_period <- final_df %>% filter(period == "post")
  expect_true(all(post_period$relative_month >= 1 & post_period$relative_month <= 24))
  
  # Test calendar years are within expected range
  expect_true(all(final_df$calendar_year >= 2017))
  expect_true(all(final_df$calendar_year <= 2023))
})

######################## Test Treatment Assignment Logic ####################
test_that("Treatment assignment logic is correct", {
  # Treatment bikeways should have construct_year between 2019-2021
  treatment_bikeways <- final_df %>% 
    filter(treatment == TRUE) %>%
    distinct(bikeway_id, construct_year)
  expect_true(all(treatment_bikeways$construct_year >= 2019 & 
                    treatment_bikeways$construct_year <= 2021))
  
  # Each bikeway should have exactly 60 months of data
  months_per_bikeway <- final_df %>%
    group_by(bikeway_id) %>%
    summarise(n_months = n()) %>%
    pull(n_months)
  expect_true(all(months_per_bikeway == 60))
})

######################## Test Data Quality #################################
test_that("Data quality meets requirements", {
  # Test for missing values in bike lanes data
  required_cols <- c("OBJECTID", "INSTALLED", "INFRA_HIGHORDER")
  expect_false(any(is.na(bike_lanes[required_cols])))
  
  # Test for non-negative ride counts
  expect_true(all(final_df$monthly_rides >= 0))
  expect_true(all(final_df$monthly_rides_adj >= 0))
})

######################## Test Relationships Between Datasets ###############
test_that("Relationships between datasets are valid", {
  # All bikeways in final_df should exist in bike_lanes
  bikeways_in_final <- unique(final_df$bikeway_id)
  bikeways_in_lanes <- unique(bike_lanes$OBJECTID)
  expect_true(all(bikeways_in_final %in% bikeways_in_lanes))
  
  # Treatment types should match between datasets
  compare_types <- final_df %>%
    distinct(bikeway_id, bikeway_type) %>%
    left_join(
      bike_lanes %>% 
        select(OBJECTID, INFRA_HIGHORDER), 
      by = c("bikeway_id" = "OBJECTID")
    )
  expect_equal(compare_types$bikeway_type, compare_types$INFRA_HIGHORDER)
})