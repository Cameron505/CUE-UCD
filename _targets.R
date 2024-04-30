# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
source("Code/0-packages.R")
source("Code/a-processing_functions.R")
source("Code/b-analysis_functions.R")
#source("Code/fticr/b-functions_analysis.R")


# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # data files
  tar_target(sample_key_data, "Data/Data_sheet_1.csv", format = "file"),
  tar_target(sample_data_1, read.csv(sample_key_data)),
  
  # analysis - graphs
  tar_target(Soil_infiltration, plot_infiltration(sample_data_1)),
  
  #reports
  tar_render(report, path = "reports/CUE_Davis_Soil_report.Rmd")
)
