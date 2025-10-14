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
       future)
# library(rhdf5)

# Variables dictionaries --------------------------------------------------
source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))

# Formatting all data sources into same structure -------------------------
source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))

# Listing the files and chooseing the smallest of them --------------------
formatted_files <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "*_reformatted\\.csv$", full.names = TRUE)
file_info <- file.info(formatted_files)
files_with_size <- data.frame(
  file = basename(dirname(rownames(file_info))),
  size_mb = round(file_info$size / (1024^2), 2)
) %>%
  arrange(size_mb)

# Generating VDBA ---------------------------------------------------------
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")
for (species in c("Mauny_Goat", "Smit_Cat", "Ladds_Seal", "Studd_Squirrel")){
  
  # row in 1:nrow(files_with_size)){
  # print(files_with_size[row,])
  # species <- files_with_size[row,]$file
  
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))){
    print("already did this one")
  } else {
    source(file = file.path(base_path, "Scripts", "GenerateVDBA.R"))
  }
  
  source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))
}

# Finding the threshold between active and inactive for each species ------
source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))



# Plotting these results --------------------------------------------------
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))

