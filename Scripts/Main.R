# Main script for processing the other scripts ----------------------------

# Packages and settings ---------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(pacman)
p_load(tidyverse,
       data.table,
       tsfeatures,
       future,
       RcppRoll,
       arrow,
       zoo,
       R.matlab,
       rhdf5,
       ctmm,
       signal,
       lmerTest
       )

# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

# clear the system
# all_csvs <- list.files(file.path(base_path, "AccelerometerData"), 
#                        pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# # Keep only the raw data or individual analysis
# raw_csvs <- grep("/raw/", all_csvs, value = TRUE)
# raw_csvs <- grep("/Individual_Analyses/", raw_csvs, value = TRUE)
# to_delete <- setdiff(all_csvs, raw_csvs)
# file.remove(to_delete)

# Variables #####
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))
source(file = file.path(base_path, "Scripts", "GeneralFunctions.R")) # general functions
species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

## EXPERIMENTATION #####
# Experimenting with Sampling Rate ----------------------------------------
source(file = file.path(base_path, "Scripts", "SamplingRateExperiment.R"))

## PREPARE ALL DATASETS #####
# Get all datasets into a consistent format -------------------------------
for (dataset in species_list){
  species <- basename(dataset)
  
  if (species == "Clemente_Impala"){
    next
  }
  print(species)
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))){
    print("already refomatted")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "FormattingRawData.csv"))
  }

  # Filtering ---------------------------------------------------------------
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))){
    print("already smoothed")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "SmoothingRawData.R"))
  }
}



# Filtering outliers and downsampling to standard rate --------------------
# standard_sampling_rate <- 10
# if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_resampled.csv")))){
#   print("already resampled")
# } else {
#   source(file = file.path(base_path, "Scripts", "DownsamplingFormattedData.R"))
# }

# Determining which ones were measured in Gs ------------------------------
# source(file = file.path(base_path, "Scripts", "DeterminingCalibration.R"))
# commented this out as I removed all the ones not in Gs

# Generating VBDA ---------------------------------------------------------
for (dataset in species_list){
  species <- basename(dataset)
  if (species == "Clemente_Impala"){
    next
  }
  print(species)
  # Calculating and thresholding between active and inactive for each species
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))){
    print("already summarised")
  } else {
     source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "GeneratingVDBA.R"))
  }
}

## ANALYSIS #####
# Scaling -----------------------------------------------------------------
# understanding these results
source(file = file.path(base_path, "Scripts", "ResultsMarkdown.Rmd"))

