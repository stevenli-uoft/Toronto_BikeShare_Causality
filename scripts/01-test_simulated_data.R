#### Preamble ####
# Purpose: Testing simulated bike share and bike lane data for spatial analysis
# Author: Steven Li
# Date: 17 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - simulate_data.R must have been run first
# - The tidyverse, testthat, and lubridate packages must be installed

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)

# Read in the simulated data
bike_share_data <- read_csv("data/00-simulated_data/simulated_bike_share_data.csv")
bike_lane_data <- read_csv("data/00-simulated_data/simulated_bike_lane_data.csv")

######################## Test Bike Share Data Structure ######################
test_that("Bike Share data has correct structure", {
  # Test presence of required columns
  expected_columns <- c("TripID", "Date", "start_station", "end_station", 
                        "start_lon", "start_lat", "end_lon", "end_lat")
  expect_true(all(expected_columns %in% colnames(bike_share_data)))
  
  # Test number of trips
  expect_equal(nrow(bike_share_data), 10000)
  
  # Test for unique trip IDs
  expect_equal(length(unique(bike_share_data$TripID)), 10000)
  expect_false(any(duplicated(bike_share_data$TripID)))
  
  # Test data types
  expect_true(is.numeric(bike_share_data$TripID))
  expect_true(inherits(bike_share_data$Date, "Date"))
  expect_true(is.numeric(bike_share_data$start_station))
  expect_true(is.numeric(bike_share_data$end_station))
})

######################## Test Bike Share Spatial Bounds #######################
test_that("Bike Share coordinates are within Toronto bounds", {
  # Define Toronto boundaries
  toronto_bounds <- list(
    lat = c(43.58, 43.85),  # South-North
    lon = c(-79.62, -79.12) # West-East
  )
  
  # Test start coordinates
  expect_true(all(bike_share_data$start_lat >= toronto_bounds$lat[1]))
  expect_true(all(bike_share_data$start_lat <= toronto_bounds$lat[2]))
  expect_true(all(bike_share_data$start_lon >= toronto_bounds$lon[1]))
  expect_true(all(bike_share_data$start_lon <= toronto_bounds$lon[2]))
  
  # Test end coordinates
  expect_true(all(bike_share_data$end_lat >= toronto_bounds$lat[1]))
  expect_true(all(bike_share_data$end_lat <= toronto_bounds$lat[2]))
  expect_true(all(bike_share_data$end_lon >= toronto_bounds$lon[1]))
  expect_true(all(bike_share_data$end_lon <= toronto_bounds$lon[2]))
})

######################## Test Bike Share Temporal Range #######################
test_that("Bike Share dates are within expected range", {
  # Test date range
  expect_true(all(bike_share_data$Date >= as.Date("2015-01-01")))
  expect_true(all(bike_share_data$Date <= as.Date("2023-12-31")))
  
  # Test for missing dates
  expect_false(any(is.na(bike_share_data$Date)))
})

######################## Test Bike Lane Data Structure ########################
test_that("Bike Lane data has correct structure", {
  # Test presence of required columns
  expected_columns <- c("BikeLaneID", "Constructed", "lat", "lon", 
                        "Type", "Upgraded")
  expect_true(all(expected_columns %in% colnames(bike_lane_data)))
  
  # Test number of bike lanes
  expect_equal(nrow(bike_lane_data), 500)
  
  # Test for unique bike lane IDs
  expect_equal(length(unique(bike_lane_data$BikeLaneID)), 500)
  expect_false(any(duplicated(bike_lane_data$BikeLaneID)))
})

######################## Test Bike Lane Categories ############################
test_that("Bike Lane types are valid", {
  valid_types <- c("Protected Lanes", "On-Road Lanes", "Shared Roadways")
  
  # Test that all types are valid
  expect_true(all(bike_lane_data$Type %in% valid_types))
  
  # Test that all types are represented
  expect_equal(length(unique(bike_lane_data$Type)), length(valid_types))
})

######################## Test Bike Lane Temporal Logic #######################
test_that("Bike Lane dates follow logical rules", {
  # Test construction years are within range
  expect_true(all(bike_lane_data$Constructed >= 2015))
  expect_true(all(bike_lane_data$Constructed <= 2023))
  
  # Test upgrade years logic
  upgraded_lanes <- bike_lane_data %>% filter(!is.na(Upgraded))
  
  # Upgrade year should be after construction year
  expect_true(all(upgraded_lanes$Upgraded > upgraded_lanes$Constructed))
  
  # Upgrade year should be within range
  expect_true(all(upgraded_lanes$Upgraded >= 2015))
})

######################## Test Bike Lane Spatial Distribution #################
test_that("Bike Lane coordinates are within Toronto bounds", {
  toronto_bounds <- list(
    lat = c(43.58, 43.85),  # South-North
    lon = c(-79.62, -79.12) # West-East
  )
  
  # Test coordinates
  expect_true(all(bike_lane_data$lat >= toronto_bounds$lat[1]))
  expect_true(all(bike_lane_data$lat <= toronto_bounds$lat[2]))
  expect_true(all(bike_lane_data$lon >= toronto_bounds$lon[1]))
  expect_true(all(bike_lane_data$lon <= toronto_bounds$lon[2]))
})

######################## Test Data Quality ##################################
test_that("Data quality meets requirements", {
  # Test for missing values in bike share data
  expect_false(any(is.na(bike_share_data$TripID)))
  expect_false(any(is.na(bike_share_data$Date)))
  expect_false(any(is.na(bike_share_data$start_station)))
  expect_false(any(is.na(bike_share_data$end_station)))
  
  # Test for missing values in bike lane data (except Upgraded which can be NA)
  expect_false(any(is.na(bike_lane_data$BikeLaneID)))
  expect_false(any(is.na(bike_lane_data$Constructed)))
  expect_false(any(is.na(bike_lane_data$Type)))
  expect_false(any(is.na(bike_lane_data$lat)))
  expect_false(any(is.na(bike_lane_data$lon)))
})

######################## Test Station Distribution ##########################
test_that("Station distribution is reasonable", {
  # Test number of unique stations
  n_stations <- length(unique(c(bike_share_data$start_station, 
                                bike_share_data$end_station)))
  expect_equal(n_stations, 300)
  
  # Test station IDs are within expected range
  expect_true(all(bike_share_data$start_station >= 1))
  expect_true(all(bike_share_data$start_station <= 300))
  expect_true(all(bike_share_data$end_station >= 1))
  expect_true(all(bike_share_data$end_station <= 300))
})