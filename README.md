# The Causal Effect of Cycling Infrastructure on Bike Share Usage: A Spatial Analysis of Toronto's Bike Network

## Overview
This repository contains the code and data analysis for examining the causal impact of cycling infrastructure improvements on Bike Share Toronto ridership between 2017-2023. Using a difference-in-differences design with over 7.4 million rides across 1,191 bikeways, we analyze how different types of bikeways influence system usage. Our findings demonstrate that infrastructure improvements led to significant increases in ridership, with post-implementation effects reaching 133.1 additional monthly rides per bikeway.

## File Structure

The repo is structured as follows:

-   `data/00-simulated_data` contains the simulated data used for testing the model and validating analysis methods
-   `data/01-raw_data/01-raw_bikeshare_data` due to the raw Bake Share Ridership data being too large to upload, follow the "Reproducing Graphs and Tables" instruction below to obtain the data and reproduce the paper
-   `data/01-raw_data/02-raw_bikestation_data` contains the raw Bike Share stations data obtained from Open Data Toronto [Link]([https://open.toronto.ca/dataset/cycling-network/](https://open.toronto.ca/dataset/bike-share-toronto/)
-   `data/01-raw_data/03-raw_bikeway_data`  contains the raw bikeway data obtained from Open Data Toronto [Link](https://open.toronto.ca/dataset/cycling-network/)
-   `data/02-analysis_data` contains the cleaned and processed dataset to be used for running the DiD model, and further analysis.
-   `models` contains fitted models including the main difference-in-differences specifications and robustness checks
-   `other` contains details about LLM chat interactions, sketches of planned visualizations and analyses, and the datasheet documenting our dataset characteristics
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper
-   `scripts` contains the R scripts used to simulate, download, clean, and test the data, as well as create the DiD models:

## Reproducing the Paper
To reproduce the graphs and tables from the paper, follow these steps:
1. Clone the repository to your local machine.
2. Go to `scripts/` and run `02-download_data.R`.
  - Note: The November 2022 Bike Share Ridership data is not automatically downloaded by the script due to a file format limitation. Follow these steps manually after running the script:
    1. Go to Bike Share Toronto Ridership Data in Open Data Toronto Portal [Link](https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/)
    2. Download the bikeshare-ridership-2022.zip file.
    3. Unzip the file and then unzip Bike share ridership 2022-11.zip.
    4. Import the file Bike share ridership 2022-11.csv into the raw_data/raw_bikeshare_data/ folder.
4. Go to `scripts/` and run `03-data_cleaning.R`: This script processes the raw data into our panel dataset, designed for our DiD model.
5. Go to `scripts/` and run `04-test_analysis_data.R`: Run this script to ensure that the downloaded data meets all requirements and specifications for the analysis
6. Open the Quarto document in `paper/paper.qmd` and render the PDF file.

## Dependencies
This analysis requires R and the following R packages:
- opendatatoronto (for accessing Toronto Open Data)
- tidyverse (for data manipulation and visualization)
- fs (for file system operations)
- dplyr (for data manipulation)
- readr (for reading data)
- jsonlite (for JSON processing)
- httr (for HTTP requests)
- fixest (for difference-in-differences estimation)
- modelsummary (for regression table creation)
- arrow (for working with parquet files)
- lubridate (for date/time manipulation)
- sf (for spatial data processing)
- testthat (for unit testing)
- units (for unit conversion and handling)

## Statement on LLM usage
Large Language Models such as ___ were used to assist in parts of the data analysis and writing process. The chat logs are saved in the `other/llm_usage` folder for full transparency.

