#### Preamble ####
# Purpose: Downloads and saves Bike Share Ridership, Bike Share Station Info, and Bikeway data from Open Data Toronto
# Author: Steven Li
# Date: 17 November 2024
# Contact: stevency.li@mail.utoronto.ca
# Pre-requisites: 
# - The required libraries listed below must be installed:
#     - opendatatoronto, tidyverse, fs, dplyr, readr, jsonlite, httr

# Load required libraries
library(opendatatoronto)
library(tidyverse)
library(fs)
library(dplyr)
library(readr)
library(jsonlite)
library(httr)


############################# !!! Please Read !!! #############################
############################# Data Download Issue #############################
# Unable to download November 2022 Bike Share Ridership data, due to it being
# a zip file embedded in another  zip file. Follow these steps after running 
# the script below:
# 1. Go to Open Data Toronto and search up "Bike Share Toronto Ridership Data"
# 2. Download the bikeshare-ridership-2022 zip file
# 3. Unzip the file, and unzip "Bike share ridership 2022-11.zip"
# 4. Import "Bike share ridership 2022-11.csv" into 01-raw_data/02-raw_bikeshare_data
###############################################################################


############################# Manage Directories #############################
raw_data_dir <- "data/01-raw_data"

# Check if the directory exists, if it does, delete it
if (dir_exists(raw_data_dir)) {
  dir_delete(raw_data_dir)  # Delete the folder and its contents
}

# Recreate the directories after deletion
bikeshare_file_path <- file.path(raw_data_dir, "01-raw_bikeshare_data")
dir_create(bikeshare_file_path)

bikestation_file_path <- file.path(raw_data_dir, "02-raw_bikestation_data")
dir_create(bikestation_file_path)

bikeway_file_path <- file.path(raw_data_dir, "03-raw_bikeway_data")
dir_create(bikeway_file_path)


########################## Bike Share Ridership data ##########################
# get package details
package <- show_package("7e876c24-177c-4605-9cef-e50dd74c617f")

# get all resources for this package
resources <- list_package_resources("7e876c24-177c-4605-9cef-e50dd74c617f")

# identify datastore resources (ZIP files)
datastore_resources <- filter(resources, format == 'ZIP')

#### Download and process each resource ####
for(i in 1:nrow(datastore_resources)) {
  # Get the resource ID and name
  resource_id <- datastore_resources$id[i]
  resource_name <- datastore_resources$name[i]
  
  # Download the resource using its ID
  resource_data <- get_resource(resource_id)
  
  # Iterate over the list of tibbles in the resource data
  for (name in names(resource_data)) {
    
    # Check if the object is a tibble (skip non-data objects)
    if (is_tibble(resource_data[[name]]) && nrow(resource_data[[name]]) > 1) {
      
      # Save the individual CSV file to the raw_data directory
      file_path <- file.path(bikeshare_file_path, name)
      write_csv(resource_data[[name]], file_path)
    }
  }
}

########################## Bike Share Stations data ##########################
# get package details
package <- show_package("2b44db0d-eea9-442d-b038-79335368ad5a")

# get all resources for this package
resources <- list_package_resources("2b44db0d-eea9-442d-b038-79335368ad5a")

# filter relevant file needed (bike-share-json)
datastore_resources <- filter(resources, name == 'bike-share-json')

# Get metadata (contains relevant URL to json)
gbfs_data <- get_resource(datastore_resources$id)

# Extract the station_information URL
station_url <- gbfs_data$data$en$feeds$url[3]  # Third URL is station_information

# Use httr to get the station data
response <- GET(station_url)
station_data <- fromJSON(rawToChar(response$content))

# Convert to a tibble
stations_df <- station_data$data$stations %>%
  as_tibble() %>%
  select(station_id, name, lat, lon)

#### Save data ####
file_path <- file.path(bikestation_file_path, "bike_station_data.csv")
write_csv(stations_df, file_path)

##################### Cycling Network (Bike ways) data ########################
# get package details
package <- show_package("cycling-network")

# get all resources for this package
resources <- list_package_resources("cycling-network")

# identify datastore resources (CSV files)
datastore_resources <- filter(resources, format == 'CSV')

# load datastore resource
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

#### Save data ####
file_path <- file.path(bikeway_file_path, "bikeway_data.csv")
write_csv(data, file_path)