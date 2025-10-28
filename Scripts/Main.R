# Main script for processing the other scripts ----------------------------

# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

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
       glmmTMB
       )


# clear the system
# all_csvs <- list.files(file.path(base_path, "AccelerometerData"), 
#                        pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# # Keep only the raw data or individual analysis
# raw_csvs <- grep("/raw/", all_csvs, value = TRUE)
# raw_csvs <- grep("/Individual_Analyses/", raw_csvs, value = TRUE)
# to_delete <- setdiff(all_csvs, raw_csvs)
# file.remove(to_delete)


# Variables ---------------------------------------------------------------
source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))
source(file = file.path(base_path, "Scripts", "GeneralFunctions.R")) # general functions
species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)




# Experimenting with Sampling Rate ----------------------------------------
source(file = file.path(base_path, "Scripts", "SamplingRateExperiment.R"))

# Within Species and Dataset Analysis -------------------------------------
source(file = file.path(base_path, "Scripts", "IndividualAnalysis.R"))

# Preprocessing -----------------------------------------------------------
# getting all the data into the same format
source(file = file.path(base_path, "Scripts", "PreProcessing.R"))

# Generating VBDA ---------------------------------------------------------
for (dataset in species_list){
  species <- basename(dataset)
  print(species)
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo", "Friedlaender_Whale", "Kamminga_Horse", "Studd_Squirrel")){
    next
  }
  # Calculating and thresholding between active and inactive for each species
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))){
    print("already summarised")
  } else {
     source(file = file.path(base_path, "Scripts", "GeneratingVDBA.R"))
  }
}

# Scaling -----------------------------------------------------------------
# understanding these results
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))

