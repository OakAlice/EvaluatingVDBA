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
       arrow)

source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))

# Within Species and Dataset Analysis -------------------------------------
source(file = file.path(base_path, "Scripts", "WithinSpeciesWithinDatasetAnalysis.R"))

# Within Species Across Datsets Analysis ----------------------------------
source(file = file.path(base_path, "Scripts", "WithinSpeciesAcrossDatasetAnalysis.R"))

# Main Multi-Species Comparison -------------------------------------------
max_samples <- 24 # in hours # the maximum samples from any individual

species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

for (dataset in species_list){
  
  species <- basename(dataset)
  print(species)
  
  if(species %in% c("Annett_Kangaroo", "Clemente_Impala")){
    next
  }
  
  # Formatting all data sources into same structure
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))){
    print("already formatted")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))
  }
  
  # Generating VDBA
  selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))){
    print("already did this one")
  } else {
    source(file = file.path(base_path, "Scripts", "GenerateVDBA.R"))
  }

  # Finding the threshold between active and inactive for each species  
  source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))
  
  # Accounting for different brands and acceleration scales
  source(file = file.path(base_path, "Scripts", "CalibratingDevices.R"))
}

# Plotting these results
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))




