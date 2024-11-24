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
library(sf)
library(units)

############################# Manage Directories #############################
# # Create directory to store cleaned data
# analysis_data_dir <- "data/02-analysis_data"
# 
# # Check if the directory exists, if it does, delete it
# if (dir_exists(analysis_data_dir)) {
#   dir_delete(analysis_data_dir)  # Delete the folder and its contents
# }
# 
# dir_create(analysis_data_dir)


########################### Cleaning Bike Way Data ############################
# Read in the raw data
bike_lanes <- st_read(
  "data/01-raw_data/03-raw_bikeway_data/bikeway_data.geojson")

# Clean the data
bike_lanes <- bike_lanes %>%
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

### Save cleaned data ####
write_csv(bike_lanes, "data/02-analysis_data/bikeway_data.csv")


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
    filter(year(start_date) <= 2023) %>%
    drop_na()
}

combined_rides <- csv_files %>% map_dfr(process_file)

############################ Filter bike rides #############################
# Filter bike rides to include only those where the start and end station are
# within 100m of a designated bike lane.

# First, filter bike stations to keep only those within 100m to a bike lane
bike_station <- read_csv(
  "data/01-raw_data/02-raw_bikestation_data/bike_station_data.csv")

# Convert bike stations to sf object
bike_stations_sf <- bike_station %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # WGS84 coordinate system

# Ensure bike lanes is in the same CRS
bike_lanes <- st_transform(bike_lanes, 4326)

# Convert to a projected CRS for accurate distance measurements
bike_stations_sf <- st_transform(bike_stations_sf, 32617)
bike_lanes <- st_transform(bike_lanes, 32617)

# Function to find nearest feature and return its ID
find_nearest_bikeway <- function(point_geometry, lines_sf) {
  distances <- st_distance(point_geometry, lines_sf)
  nearest_index <- which.min(distances)
  return(list(
    distance = min(distances),
    bikeway_id = lines_sf$OBJECTID[nearest_index]  # Adjust field name if needed
  ))
}

# Calculate distance and nearest bikeway for each station
stations_with_distance <- bike_stations_sf %>%
  rowwise() %>%
  mutate(
    nearest_info = list(find_nearest_bikeway(geometry, bike_lanes)),
    dist_meters = as.numeric(nearest_info$distance),
    nearest_bikeway_id = nearest_info$bikeway_id
  ) %>%
  ungroup()

# Filter stations within 100 meters of a bike lane
stations_near_lanes <- stations_with_distance %>%
  filter(dist_meters <= 100) %>%
  st_transform(4324) # Convert back to original CRS

# Convert back to regular dataframe
final_stations <- stations_near_lanes %>%
  st_drop_geometry() %>%
  select(station_id, name, dist_meters, nearest_bikeway_id)

# Create vector of valid station IDs for filtering
valid_stations <- final_stations$station_id

# Filter rides and add station information
filtered_rides <- combined_rides %>%
  filter(
    start_station_id %in% valid_stations
    # , end_station_id %in% valid_stations # If filtering for both stations near bikeway
  ) %>%
  # Join start station details
  left_join(
    final_stations %>%
      select(station_id,
             start_station_dist = dist_meters,
             start_nearest_bikeway = nearest_bikeway_id),
    by = c("start_station_id" = "station_id")
  ) %>%
  # Join end station details
  left_join(
    final_stations %>%
      select(station_id,
             end_station_dist = dist_meters,
             end_nearest_bikeway = nearest_bikeway_id),
    by = c("end_station_id" = "station_id")
  )
  # filter(start_nearest_bikeway == end_nearest_bikeway) # If filtering for both stations near same bikeway

## Save filtered rides as CSV
# write_csv(filtered_rides,
#           "data/02-analysis_data/filtered_ridership.csv")

################ Create Final Dataset for Diff-in-Diff model ##################
# Step 1: Identify treatment and control bikeways
bikeway_status <- bike_lanes %>%
  select(-geometry) %>%
  mutate(
    # Determine treatment year (either UPGRADED or INSTALLED if no upgrade)
    treatment_year = case_when(UPGRADED > 0 ~ UPGRADED,
                               TRUE ~ INSTALLED),
    # Flag if bikeway was treated in our window of interest (2019-2021)
    is_treated = treatment_year >= 2017 & treatment_year <= 2023,
    # Flag control bikeways (not constructed/upgraded during study period)
    is_control = (INSTALLED < 2017 & UPGRADED < 2017)
  ) %>%
  select(bikeway_id = OBJECTID, treatment_year, is_treated, is_control)

# Step 2: Get annual rides for each bikeway
bikeway_annual_rides <- filtered_rides %>%
  mutate(calendar_year = year(start_date)) %>%
  group_by(start_nearest_bikeway, calendar_year) %>%
  summarize(bikeway_annual_rides = n(), .groups = 'drop')

# Step 3: Create treatment dataset
treatment_data <- bikeway_status %>%
  filter(is_treated) %>%
  mutate(analysis_period = case_when(treatment_year == 2019 ~ "2017-2021",
                                     treatment_year == 2020 ~ "2018-2022",
                                     treatment_year == 2021 ~ "2019-2023")
  ) %>%
  crossing(relative_year = -2:2) %>%
  mutate(
    calendar_year = treatment_year + relative_year,
    treatment = TRUE
  ) %>%
  drop_na(analysis_period)

# Step 4: Create evenly distributed control dataset
control_data <- bikeway_status %>%
  filter(is_control) %>%
  # Randomly assign control bikeways to analysis periods
  mutate(
    analysis_period = sample(c("2017-2021", "2018-2022", "2019-2023"), 
                             n(), 
                             replace = TRUE,
                             prob = c(1/3, 1/3, 1/3)
    )
  ) %>%
  crossing(relative_year = -2:2) %>%
  mutate(
    # Calculate calendar_year based on analysis period
    calendar_year = case_when(
      analysis_period == "2017-2021" ~ 2019 + relative_year,
      analysis_period == "2018-2022" ~ 2020 + relative_year,
      analysis_period == "2019-2023" ~ 2021 + relative_year
    ),
    treatment = FALSE
  )

# Step 5: Combine treatment and control bikeways, with annual rides data
final_df <- bind_rows(treatment_data, control_data) %>%
  select(-geometry) %>%
  left_join(bikeway_annual_rides,
            by = c("bikeway_id" = "start_nearest_bikeway", "calendar_year")
  ) %>%
  mutate(bikeway_annual_rides = replace_na(bikeway_annual_rides, 0)) %>%
  arrange(analysis_period, treatment, bikeway_id, calendar_year)

### Save final_df as CSV
write_csv(final_df, "data/02-analysis_data/final_df.csv")

####### NOTES:
# 2017-2023 rides: 21.8m rides
# Rides from 2017-2023 where start_station is near bikelane: 13.8m rides
# After joining with control and treatment bikeways, there are ~8m rides
#
# This is because, we only gather 5 year periods of data for each bikeway, since
# our analysis focused on 5 relative years, +-2 around treatment year. Each control
# bikeway will have randomly selected pseudo-treatment years, with +- 2 years.
# So because we only focus on 5 years per bikeway, it won't contain all bike rides
# For example, we randomly selected bikewayID=4 as control bikeway, with 
# pseudo-treatment year as 2020, so bikewayID=4 will have data from 2018-2022. 
# So 2017 and 2023 rides for bikewayID=4 will be dropped from final_df.
#
#
#
# For filtered rides where start and end station is near bikelane: 1.01m rides
# After joining with control and treatment bikeways, there are ~672k rides
#