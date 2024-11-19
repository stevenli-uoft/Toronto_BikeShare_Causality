#### Preamble ####
# Purpose: Cleans the downloaded data from Open Data Toronto
# Author: Steven Li
# Date: 18 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - 02-download_data.R must have been run first
# - The required libraries listed below must be installed:
#     - readr, tidyverse, fs, lubridate

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(fs)

############################# Manage Directories #############################
# Create directory to store cleaned data
analysis_data_dir <- "data/02-analysis_data"

# Check if the directory exists, if it does, delete it
if (dir_exists(analysis_data_dir)) {
  dir_delete(analysis_data_dir)  # Delete the folder and its contents
}

dir_create(analysis_data_dir)


########################### Cleaning Bike Way Data ############################
# Read in the raw data
raw_data <- read_csv("data/01-raw_data/03-raw_bikeway_data/bikeway_data.csv")

# Clean the data
cleaned_data <- raw_data %>%
  select(OBJECTID, INSTALLED, UPGRADED, INFRA_HIGHORDER, geometry) %>%
  drop_na(OBJECTID, INSTALLED, geometry) %>%
  filter(INSTALLED >= 2001 & INSTALLED <= 2023) %>%
  mutate(
    INFRA_HIGHORDER = case_when(
      str_detect(INFRA_HIGHORDER,
                 "Bi-Direction Cycle Track|Cycle Track|Multi-Use Trail") ~
        "Protected Lanes",
      str_detect(INFRA_HIGHORDER, "Bike Lane") ~ "On-Road Lanes",
      str_detect(INFRA_HIGHORDER, "Sharrows|Signed Route|Park Road") ~
        "Shared Roadways",
      TRUE ~ NA_character_ # Filter out other values
    )
  ) %>%
  drop_na(INFRA_HIGHORDER)  # Remove rows where INFRA_HIGHORDER doesn't match

#### Save cleaned data ####
write_csv(cleaned_data, "data/02-analysis_data/bikeway_data.csv")


# ################# Add Station ID info to 2017 Q3-Q4 rides  ####################
# # 2017 Q3 and Q4 are missing station ID data. Will join with bike station info to get ID's
# bike_station <- read_csv(
#   "data/01-raw_data/02-raw_bikestation_data/bike_station_data.csv")
# 
# df2017 <- read_csv(
#   "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q3).csv")
# 
# # Left join twice by station name for starting and ending station ID
# new_temp <- df2017 %>%
#   left_join(bike_station, by=c("from_station_name" = "name")) %>%
#   rename(from_station_id = station_id) %>%
#   left_join(bike_station, by=c("to_station_name" = "name")) %>%
#   rename(to_station_id = station_id) %>%
#   select(trip_id, trip_start_time, from_station_id, from_station_name,
#          to_station_id, to_station_name) %>%
#   drop_na() # filter rows with non-matching station IDs
# 
# write_csv(new_temp, "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q3).csv")
# 
# ### Do the same steps again fr Q4 2017
# df2017 <- read_csv(
#   "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q4).csv")
# 
# # Left join twice by station name for starting and ending station ID
# new_temp <- df2017 %>%
#   left_join(bike_station, by=c("from_station_name" = "name")) %>%
#   rename(from_station_id = station_id) %>%
#   left_join(bike_station, by=c("to_station_name" = "name")) %>%
#   rename(to_station_id = station_id) %>%
#   select(trip_id, trip_start_time, from_station_id, from_station_name,
#          to_station_id, to_station_name) %>%
#   drop_na() # filter rows with non-matching station IDs
# 
# write_csv(new_temp, "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q4).csv")


############################ Cleaning Bike Share Data #########################
csv_files <- list.files(path = "data/01-raw_data/01-raw_bikeshare_data",
                        pattern = "\\.csv$", full.names = TRUE)

# Function to standardize column names
standardize_column_names <- function(df) {
  df %>%
    rename_with(~ str_to_lower(.), everything()) %>%
    rename(
      trip_id = matches("trip.id|trip_id|ï..trip.id"),
      start_date = matches("start.time|trip_start_time"),
      start_station_id = matches("start.station.id|from_station_id"),
      end_station_id = matches("end.station.id|to_station_id")
    )
}

# Function to clean and convert data types
process_file <- function(file) {
  data <- read_csv(file, show_col_types = FALSE)

  # Ensure trip_id is numeric, then clean the data
  data %>%
    standardize_column_names() %>%
    select(trip_id, start_date, start_station_id, end_station_id) %>%
    mutate(
      # Convert trip_id, start_station_id, end_station_id to integer
      trip_id = as.integer(as.numeric(trip_id)),
      start_station_id = as.integer(as.numeric(start_station_id)),
      end_station_id = as.integer(as.numeric(end_station_id)),
      
      # Convert start_date from chr to Date, trying different formats
      start_date = parse_date_time(start_date,
                                   orders = c("mdy HMS", "mdy HM", "mdy",
                                              "m/d/y H:M:S", "m/d/y H:M",
                                              "m/d/y"))
    ) %>%
    mutate(start_date = floor_date(start_date, unit = "month")) %>%
    drop_na()
}

combined_data <- csv_files %>% map_dfr(process_file)

###################### Join Bike Share and Station Data #######################
# Left join lon and lat data of station info to combined_data
bike_station <- read_csv(
  "data/01-raw_data/02-raw_bikestation_data/bike_station_data.csv")

rides_with_loc <- combined_data %>%
    left_join(bike_station, by=c("start_station_id" = "station_id")) %>%
    rename(start_lat = lat, start_lon = lon) %>%
    left_join(bike_station, by=c("end_station_id" = "station_id")) %>%
    rename(end_lat = lat, end_lon = lon) %>%
    select(trip_id, start_date, start_lat, start_lon, end_lat, end_lon) %>%
    drop_na()

############################ Filter bike rides #############################
# Filter bike rides to include only those start and end station are within 100m
# of a designated bike lane
