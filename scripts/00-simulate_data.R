#### Preamble ####
# Purpose: Simulates Bike Share Ridership and Bike Lane data for Toronto with spatial components
# Author: Steven Li
# Date: 17 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
#   - The following package must be installed: tidyverse, lubridate, sf

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(sf)

############################### Data expectations ############################
# Bike Share Ridership Data:
# - Columns: Trip ID, Date, start_station_location, end_station_location
# - Trip ID should be unique
# - Date should be between 2015-01-01 and 2023-12-31
# - Station locations should be within Toronto boundaries

# Bike Lane Data:
# - Columns: BikeLaneID, Constructed Year, Upgraded Year, Type, Location
# - BikeLaneID should be unique
# - Constructed Year should be between 2015 and 2023
# - Upgraded Year should be between Constructed Year and 2023, or NA
# - Type should be one of: "Protected Lanes", "On-Road Lanes", "Shared Roadways"

set.seed(778)

################################ Helper functions ############################
# Define Toronto boundaries (approximate)
toronto_bounds <- list(
  lat = c(43.58, 43.85),  # South-North
  lon = c(-79.62, -79.12) # West-East
)

# Generate coordinates within Toronto
generate_toronto_coords <- function(n) {
  tibble(
    lat = runif(n, toronto_bounds$lat[1], toronto_bounds$lat[2]),
    lon = runif(n, toronto_bounds$lon[1], toronto_bounds$lon[2])
  )
}

# Generate seasonal pattern for bike share ridership
generate_seasonal_pattern <- function(date) {
  base_pattern <- sin(2 * pi * yday(date) / 365)
  normalized_pattern <- (base_pattern + 1) / 2  # Scale to 0-1
  return(normalized_pattern)
}

#########################  Simulate Bike Lane Data ########################
num_lanes <- 500

# Simulate bike lane data
bike_lane_data <- tibble(
  BikeLaneID = seq(1, num_lanes),
  # Simulate constructed year, with more in recent years
  Constructed = sample(2015:2023, num_lanes, 
                              prob = c(rep(0.08, 5), rep(0.12, 4)),
                              replace = TRUE)
  ) %>%
  # Add bike lane locations within Toronto boundaries
  bind_cols(generate_toronto_coords(num_lanes)) %>%
  # Add Bike lane types, more 'Protected lanes' towards recent years
  mutate(
    Type = if_else(
      Constructed <= 2018,
      sample(c("Protected Lanes", "On-Road Lanes", "Shared Roadways"),
             n(),
             prob = c(0.2, 0.3, 0.5),
             replace = TRUE),
      sample(c("Protected Lanes", "On-Road Lanes", "Shared Roadways"),
             n(),
             prob = c(0.5, 0.3, 0.2),
             replace = TRUE)
    ),
    # Add Upgraded years to 20% of bike lanes
    Upgraded = case_when(
      runif(n()) < 0.2 ~ pmax(Constructed + 1,  
                              sample(2015:2023, n(), replace = TRUE)),
      TRUE ~ NA_real_
    )
  )

######################## Simulate Bike Share Station Locations ##############
num_stations <- 300

# Generate stations within Toronto boundaries
station_data <- generate_toronto_coords(num_stations) %>%
  mutate(station_id = row_number())

#################### Simulate Bike Share Ridership Data ####################
num_trips <- 10000

# Generate bike share ridership data
bike_share_data <- tibble(
  TripID = seq(1, num_trips),
  # Simulate trip dates, sampling more towards recent years
  Date = sample(seq(as.Date('2015-01-01'), 
                    as.Date('2023-12-31'), 
                    by = "day"), 
                num_trips,
                prob = seq(0.5, 1, length.out = 3287),
                replace = TRUE)
  ) %>%
  # Add a start and end stations of each ride
  mutate(
    start_station = sample(station_data$station_id, num_trips, replace = TRUE),
    end_station = sample(station_data$station_id, num_trips, replace = TRUE)
  ) %>%
  # Join with station locations to get coordinates
  left_join(station_data %>% 
              select(station_id, lon, lat) %>%
              rename(start_lon = lon, start_lat = lat), 
            by = c("start_station" = "station_id")) %>%
  left_join(station_data %>%
              select(station_id, lon, lat) %>%
              rename(end_lon = lon, end_lat = lat),
            by = c("end_station" = "station_id"))

################################ Visualizations ############################
# Plot ridership trends
bike_share_data %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  count(year_month) %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Monthly Bike Share Ridership",
       x = "Date",
       y = "Number of Rides") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Plot bike lane constructions
bike_lane_data %>%
  count(Constructed, Type) %>%
  ggplot(aes(x = Constructed, y = n, fill = Type)) +
  geom_col(position = "stack") +
  labs(title = "Bike Lane Constructions Over Time",
       x = "Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot bike lane upgrades
bike_lane_data %>%
  filter(!is.na(Upgraded)) %>%
  count(Upgraded, Type) %>%
  ggplot(aes(x = Upgraded, y = n, fill = Type)) +
  geom_col(position = "stack") +
  labs(title = "Bike Lane Upgrades Over Time",
       x = "Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save simulated data
write_csv(bike_share_data, "data/simulated_bike_share_data.csv")
write_csv(bike_lane_data, "data/simulated_bike_lane_data.csv")