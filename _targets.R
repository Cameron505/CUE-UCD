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
source("Code/Plot_all_function2.R")
#source("Code/fticr/b-functions_analysis.R")


# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # data files
  tar_target(sample_key_data, "Data/Data_sheet_1.csv", format = "file"),
  tar_target(sample_data_1, read.csv(sample_key_data)),
  tar_target(sample_key_data2, "Data/WHC at collection.csv", format = "file"),
  tar_target(sample_data_2, read.csv(sample_key_data2)),
  tar_target(sample_key_data3, "Data/Water holding capacity.csv", format = "file"),
  tar_target(sample_data_3, read.csv(sample_key_data3)),
  tar_target(sample_key_data4, "Data/2024_Summary_Data.csv", format = "file"),
  tar_target(sample_data_4, read.csv(sample_key_data4)),
  tar_target(sample_key_data5, "Data/2024_RR_ko_counts_merged.tsv", format = "file"),
  tar_target(sample_data_5, read_delim(sample_key_data5, delim = '\t') %>%
               rename_with(~gsub("_gene-counts.tsv", "", .x))),
  tar_target(sample_key_data6, 'Data/2024_RR_Sample_meta.csv', format = "file"),
  tar_target(sample_data_6, read_delim(sample_key_data6, delim = ',')),
  tar_target(sample_key_data7, "Data/Metab_RR_2024.csv", format = "file"),
  tar_target(sample_data_7, read.csv(sample_key_data7)),
  tar_target(sample_key_data8, "Data/2024_Summary_meta.csv", format = "file"),
  tar_target(sample_data_8, read.csv(sample_key_data8)),
  
  tar_target(sample_key_data9, "Data/mag_plus_contig_ko.tsv", format = "file"),
  tar_target(sample_data_9, read_delim(sample_key_data9, delim = '\t')),
  tar_target(sample_key_data10, "Data/mag_tax_function_all.tsv"),
  tar_target(sample_data_10, read_delim(sample_key_data10, delim = '\t')),
  tar_target(sample_key_data11, "Data/soilProp.csv", format = "file"),
  tar_target(sample_data_11, read.csv(sample_key_data11)),
  #tar_target(sample_key_data11, "Data/mag_tax_function_all_samples.tsv"),
  #tar_target(sample_data_11, read_delim(sample_key_data11, delim = '\t')),
 
  
  

  
  # analysis - graphs
  tar_target(Soil_infiltration, plot_infiltration(sample_data_1)),
  tar_target(Soil_Bulk, plot_WHC(sample_data_2)),
  tar_target(Soil_Bulk2, plot_WHC2(sample_data_3)),
  tar_target(Soil_Aggregate_proportion, plot_aggregate(sample_data_1)),
  tar_target(gg_Aggregate, plot_respiration(sample_data_4)),
  tar_target(gg_Res, plot_respiration2(sample_data_4)),
  tar_target(gg_biomass, plot_biomass(sample_data_4)),
  tar_target(gg_FDA, plot_FDA(sample_data_4)),
  tar_target(gg_POXC, plot_POXC(sample_data_4)),
  tar_target(gg_CUE, plot_CUE(sample_data_4)),
  tar_target(gg_soil, plot_soil(sample_data_4, sample_data_11)),
  #tar_target(gg_DNA_Yield, plot_DNA_Yield(sample_data_4)),
  tar_target(gg_enzymes, plot_Enzyme(sample_data_4)),
  tar_target(gg_gene, plot_gene(sample_data_5,sample_data_4,sample_data_6)),
  tar_target(gg_metab, plot_metab(sample_data_7,sample_data_4,sample_data_6)),
  tar_target(gg_KEGG, plot_KEGG(gg_gene, gg_metab)),
  tar_target(soil_plots, plot_all(sample_data_4 = sample_data_4, sample_data_11 = sample_data_11)),
  tar_target(gg_MAOM, plot_MAOM(sample_data_4)),
  tar_target(gg_MAOM2, plot_MAOM_pairs_within_pool3(sample_data_4, facet_by = "metric_pool")),
  tar_target(gg_MAOM_prop, plot_MAOM_PROP(sample_data_4)),

  #reports
 tar_render(report, path = "reports/CUE_Davis_Soil_report.Rmd"),
  tar_render(report2, path= "reports/soil_plots.Rmd")
)
