# Toronto Bike Infrastructure Impact Analysis

## Overview
This repository contains the code and data analysis for examining the causal impact of cycling infrastructure improvements on Bike Share Toronto ridership between 2017-2023. Using a difference-in-differences design with over 7.4 million rides across 1,191 bikeways, we analyze how different types of bikeways influence system usage. Our findings demonstrate that infrastructure improvements led to significant increases in ridership, with post-implementation effects reaching 133.1 additional monthly rides per bikeway.

## File Structure

The repository is structured as follows:

- `data/00-simulated_data` contains the simulated data used for testing the model and validating analysis methods
- `data/01-raw_data/`
  - `01-raw_bikeshare_data` due to the raw Bake Share Ridership data being too large to upload, follow the "Reproducing the Paper" instruction below to obtain the data
  - `02-raw_bikestation_data` contains the raw Bike Share stations data obtained from Open Data Toronto [Source](https://open.toronto.ca/dataset/bike-share-toronto/)
  - `03-raw_bikeway_data`  contains the raw bikeway data obtained from Open Data Toronto [Source](https://open.toronto.ca/dataset/cycling-network/)
- `data/02-analysis_data` contains the cleaned and processed dataset to be used for running the DiD model, and further analysis.
- `models` contains fitted models including the main difference-in-differences specifications and robustness checks
- `other` contains details about LLM chat interactions, sketches of planned visualizations and analyses, and the datasheet documenting our dataset characteristics
- `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper
- `scripts` contains the R scripts used to simulate, download, clean, and test the data, as well as create the DiD models:

## Dependencies
This analysis requires R version 4.4.0 or later, and the following R packages: `opendatatoronto`, `tidyverse`, `fs`, `dplyr`, `readr`, `jsonlite`, `httr`, `fixest`, `modelsummary`, `arrow`, `lubridate`, `sf`, `testthat`, `units`

## Reproducing the Paper
To reproduce the paper, follow these steps:
1. Clone the repository to your local machine.
2. Ensure R and packages listed in 'Dependencies' have been installed.
3. Go to `scripts/` and run `02-download_data.R`.
  - Note: The November 2022 Bike Share Ridership data is not automatically downloaded by the script due to a file format limitation. Follow these steps manually after running the script:
    1. Go to Bike Share Toronto Ridership Data in Open Data Toronto Portal [Source](https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/)
    2. Download the bikeshare-ridership-2022.zip file.
    3. Unzip the file and then unzip Bike share ridership 2022-11.zip.
    4. Import the file Bike share ridership 2022-11.csv into the `data/01-raw_data/01-raw_bikeshare_data` folder.
4. Go to `scripts/` and run `03-data_cleaning.R`: This script processes the raw data into our panel dataset, designed for our DiD model.
5. Go to `scripts/` and run `04-test_analysis_data.R`: Run this script to ensure that the downloaded data meets all requirements and specifications for the analysis
6. Open the Quarto document in `paper/paper.qmd` and render the PDF file.

## Datasheet Documentation
This repository includes a detailed datasheet documenting:

- Dataset composition and collection methodology
- Processing steps and quality controls
- Known limitations and biases
- Usage guidelines and maintenance plans

Find the complete datasheet in `other/datasheet/datasheet.pdf`

## Statement on LLM usage
Large Language Models, specifically Claude 3.5 Sonnet, were used to assist with parts of the data analysis, debugging, and improving text clarity. The complete chat logs are saved in the `other/llm_usage` folder for full transparency.

