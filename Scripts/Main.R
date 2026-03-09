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
       plotly,
       zoo,
       R.matlab,
       rhdf5,
       ctmm,
       signal,
       lmerTest,
       mgcv,
       glmmTMB
       )

# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

# clear the system
# all_csvs <- list.files(file.path(base_path, "AccelerometerData"),
#                        pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# Keep only the raw data or individual analysis
# summary_csvs <- grep("summary", all_csvs, value = TRUE)
# file.remove(summary_csvs)

# Variables #####
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))
source(file = file.path(base_path, "Scripts", "GeneralFunctions.R")) # general functions
species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

## EXPERIMENTATION #####
# Experimenting with Sampling Rate ----------------------------------------
# source(file = file.path(base_path, "Scripts", "SamplingRateExperiment.R"))

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
    source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "FormattingRawData.R"))
  }

  # Filtering ---------------------------------------------------------------
  # if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))){
  #   print("already cleaned")
  # } else {
  #   source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "CleanFormattedData.R"))
  # }
}

# Generating summary of the data ------------------------------------------
for (dataset in species_list){
  species <- basename(dataset)
  source(file = file.path(base_path, "Scripts", "DatasetCharacteristics.R"))
}

# Generating VBDA ---------------------------------------------------------
for (dataset in species_list){
  species <- basename(dataset)
  print(species)
  
  window_seconds <- 5 # TODO: Define the window length
  # if (as.numeric(dataset_variables[Name == species]$Frequency) == 1){
  #   window_seconds <- 3
  # }
  
  # Calculating and thresholding  between active and inactive for each species
  source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "GeneratingVDBA.R"))

}

## ANALYSIS #####
# Scaling -----------------------------------------------------------------
# understanding these results
source(file = file.path(base_path, "Scripts", "ResultsMarkdown.Rmd"))


## HUMAN SIMULATION #####
# Adding the data from the human simulations ------------------------------
source(file = file.path(base_path, "Scripts", "SimulationData.Rmd"))
