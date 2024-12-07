#### Preamble ####
# Purpose: Cleans the downloaded data from Open Data Toronto
# Author: Steven Li
# Date: 18 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - 02-download_data.R must have been run first
# - The required libraries listed below must be installed:
#     - tidyverse, dplyr, readr, lubridate, fs, sf, units, arrow

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(fs)
library(sf)
library(units)
library(arrow)


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

### We need geometry data for further data processing. So geometry column will
### be dropped at the end, so bike_lanes can be saved as parquet file.

################# Add Station ID info to 2017 Q3-Q4 rides  ####################
# 2017 Q3 and Q4 are missing station ID data. Will join with bike station info to get ID's
bike_station <- read_csv(
  "data/01-raw_data/02-raw_bikestation_data/bike_station_data.csv")

df2017 <- read_csv(
  "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q3).csv")

# Left join twice by station name for starting and ending station ID
new_temp <- df2017 %>%
  left_join(bike_station, by=c("from_station_name" = "name")) %>%
  rename(from_station_id = station_id) %>%
  left_join(bike_station, by=c("to_station_name" = "name")) %>%
  rename(to_station_id = station_id) %>%
  select(trip_id, trip_start_time, from_station_id, from_station_name,
         to_station_id, to_station_name) %>%
  drop_na() # filter rows with non-matching station IDs

write_csv(new_temp, "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q3).csv")

### Do the same steps again fr Q4 2017
df2017 <- read_csv(
  "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q4).csv")

# Left join twice by station name for starting and ending station ID
new_temp <- df2017 %>%
  left_join(bike_station, by=c("from_station_name" = "name")) %>%
  rename(from_station_id = station_id) %>%
  left_join(bike_station, by=c("to_station_name" = "name")) %>%
  rename(to_station_id = station_id) %>%
  select(trip_id, trip_start_time, from_station_id, from_station_name,
         to_station_id, to_station_name) %>%
  drop_na() # filter rows with non-matching station IDs

write_csv(new_temp, "data/01-raw_data/01-raw_bikeshare_data/Bikeshare Ridership (2017 Q4).csv")


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

################ Create Final Dataset for Diff-in-Diff model ##################
###### Identify treatment and control bikeways
bikeway_status <- bike_lanes %>%
  mutate(
    # Determine treatment year (either UPGRADED or INSTALLED if no upgrade)
    construct_year = case_when(UPGRADED > 0 ~ UPGRADED
                                       , TRUE ~ INSTALLED),
    
    # Flag if bikeway was treated in our window of interest (2019-2021)
    is_treated = construct_year >= 2019 & construct_year <= 2021,
    
    # Flag control bikeways (not constructed/upgraded during study period)
    is_control = (INSTALLED < 2017 & (is.na(UPGRADED) | UPGRADED < 2017)),
    
    # Label treatment type as upgraded or newly-installed bike lanes
    sub_treatment_type = case_when(
      UPGRADED >= 2019 & UPGRADED <= 2021 ~ 'Upgraded'
      , INSTALLED >= 2019 & INSTALLED <= 2021 ~ 'Newly-Installed'
      , TRUE ~ 'No Treatment')
  ) %>%
  select(bikeway_id = OBJECTID, construct_year, is_treated, is_control,
         bikeway_type = INFRA_HIGHORDER, sub_treatment_type) %>%
  st_drop_geometry()

###### Get monthly rides for each bikeway and adjust for seasonality
# First get raw monthly rides
bikeway_monthly_rides <- filtered_rides %>%
  mutate(
    year_month = floor_date(start_date, "month"),
    month = month(start_date)
  ) %>%
  group_by(start_nearest_bikeway, year_month) %>%
  summarize(monthly_rides = n(), .groups = 'drop')

# Calculate seasonal indices using all data
seasonal_indices <- filtered_rides %>%
  mutate(
    year = year(start_date),
    month = month(start_date)
  ) %>%
  # Get total rides by year-month
  group_by(year, month) %>%
  summarize(
    monthly_total = n(),
    .groups = 'drop'
  ) %>%
  # Calculate average for each month across years
  group_by(month) %>%
  summarize(
    avg_monthly = mean(monthly_total),
    .groups = 'drop'
  ) %>%
  # Calculate seasonal indices
  mutate(
    overall_mean = mean(avg_monthly),
    seasonal_index = avg_monthly / overall_mean
  )

# Apply seasonal adjustment to bikeway_monthly_rides
bikeway_monthly_rides <- bikeway_monthly_rides %>%
  mutate(month = month(year_month)) %>%
  left_join(seasonal_indices %>% select(month, seasonal_index), by = "month") %>%
  mutate(
    # Adjust for seasonality by dividing by seasonal index
    monthly_rides_adj = monthly_rides / seasonal_index
  ) %>%
  select(-month, -seasonal_index) # Remove helper columns

###### Create treatment dataset with monthly periods
treatment_data <- bikeway_status %>%
  filter(is_treated) %>%
  mutate(
    analysis_period = case_when(
      construct_year == 2019 ~ "2017-2021",
      construct_year == 2020 ~ "2018-2022",
      construct_year == 2021 ~ "2019-2023"
    )
  ) %>%
  # Create template for all periods
  group_by(bikeway_id, construct_year, analysis_period) %>%
  do({
    # Pre-treatment: 24 months before treatment year
    pre_treatment <- tibble(
      year_month = seq(
        from = ymd(paste0(.$construct_year[1] - 2, "-01-01")),
        to = ymd(paste0(.$construct_year[1] - 1, "-12-01")),
        by = "months"
      ),
      period = "pre"
    )
    
    # Treatment year: all months in treatment year
    treatment_year <- tibble(
      year_month = seq(
        from = ymd(paste0(.$construct_year[1], "-01-01")),
        to = ymd(paste0(.$construct_year[1], "-12-01")),
        by = "months"
      ),
      period = "treatment"
    )
    
    # Post-treatment: 24 months after treatment year
    post_treatment <- tibble(
      year_month = seq(
        from = ymd(paste0(.$construct_year[1] + 1, "-01-01")),
        to = ymd(paste0(.$construct_year[1] + 2, "-12-01")),
        by = "months"
      ),
      period = "post"
    )
    
    bind_rows(pre_treatment, treatment_year, post_treatment) %>%
      mutate(analysis_period = .$analysis_period[1])
  }) %>%
  ungroup() %>%
  mutate(
    treatment = TRUE,
    calendar_year = year(year_month),
    relative_month = case_when(
      period == "pre" ~ as.numeric(interval(
        ymd(paste0(construct_year, "-01-01")), 
        year_month
      ) %/% months(1)),
      period == "treatment" ~ 0,
      period == "post" ~ as.numeric(interval(
        ymd(paste0(construct_year + 1, "-01-01")), 
        year_month
      ) %/% months(1)) + 1
    )
  )

###### Create control dataset with monthly periods
set.seed(778) # equally distributing control bikeways to 3 periods
control_data <- bikeway_status %>%
  filter(is_control) %>%
  # Randomly assign control bikeways to analysis periods
  mutate(
    analysis_period = sample(
      c("2017-2021", "2018-2022", "2019-2023"),
      n(),
      replace = TRUE,
      prob = c(1/3, 1/3, 1/3)
    ),
    # Assign pseudo treatment years based on analysis period
    pseudo_treatment_year = case_when(
      analysis_period == "2017-2021" ~ 2019,
      analysis_period == "2018-2022" ~ 2020,
      analysis_period == "2019-2023" ~ 2021
    )
  ) %>%
  # Use same approach as treatment_data to create monthly periods
  group_by(bikeway_id, pseudo_treatment_year, analysis_period) %>%
  do({
    # Pre-treatment: 24 months before treatment year
    pre_treatment <- tibble(
      year_month = seq(
        from = ymd(paste0(.$pseudo_treatment_year[1] - 2, "-01-01")),
        to = ymd(paste0(.$pseudo_treatment_year[1] - 1, "-12-01")),
        by = "months"
      ),
      period = "pre"
    )
    
    # Treatment year: all months in treatment year
    treatment_year <- tibble(
      year_month = seq(
        from = ymd(paste0(.$pseudo_treatment_year[1], "-01-01")),
        to = ymd(paste0(.$pseudo_treatment_year[1], "-12-01")),
        by = "months"
      ),
      period = "treatment"
    )
    
    # Post-treatment: 24 months after treatment year
    post_treatment <- tibble(
      year_month = seq(
        from = ymd(paste0(.$pseudo_treatment_year[1] + 1, "-01-01")),
        to = ymd(paste0(.$pseudo_treatment_year[1] + 2, "-12-01")),
        by = "months"
      ),
      period = "post"
    )
    
    bind_rows(pre_treatment, treatment_year, post_treatment)
  }) %>%
  ungroup() %>%
  mutate(
    treatment = FALSE,
    calendar_year = year(year_month),
    relative_month = case_when(
      period == "pre" ~ as.numeric(interval(
        ymd(paste0(pseudo_treatment_year, "-01-01")), 
        year_month
      ) %/% months(1)),
      period == "treatment" ~ 0,
      period == "post" ~ as.numeric(interval(
        ymd(paste0(pseudo_treatment_year + 1, "-01-01")), 
        year_month
      ) %/% months(1)) + 1
    )
  )

###### Combine treatment and control data, join with monthly rides
final_df <- bind_rows(
  treatment_data,
  control_data
) %>%
  left_join(
    bikeway_monthly_rides,
    by = c("bikeway_id" = "start_nearest_bikeway", "year_month")
  ) %>%
  # Add left join with original bikeway_status to ensure these columns are preserved
  left_join(
    bikeway_status %>% 
      select(bikeway_id, bikeway_type, sub_treatment_type),
    by = "bikeway_id"
  ) %>%
  mutate(
    monthly_rides = replace_na(monthly_rides, 0),
    monthly_rides_adj = replace_na(monthly_rides_adj, 0)
  ) %>%
  arrange(analysis_period, treatment, bikeway_id, year_month) %>%
  select(bikeway_id, construct_year, period, treatment, year_month, 
         calendar_year, relative_month, bikeway_type, sub_treatment_type,
         monthly_rides, monthly_rides_adj) 


########################### Save final dataset ##############################
write_parquet(final_df, "data/02-analysis_data/final_df.parquet")

############### Save bike lanes data, drop geometry column first ############
bike_lanes <- bike_lanes %>% st_drop_geometry()
write_parquet(bike_lanes, "data/02-analysis_data/bikeway_data.parquet")
