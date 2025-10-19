# Across Species, Across Datasets Analysis --------------------------------

# Variables dictionaries
source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))

# Formatting all data sources into same structure
source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))

# Generating VDBA
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")
if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))){
  print("already did this one")
} else {
  source(file = file.path(base_path, "Scripts", "GenerateVDBA.R"))
}

# Finding the threshold between active and inactive for each species  
source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))

# Plotting these results
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))

