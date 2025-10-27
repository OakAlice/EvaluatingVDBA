
# Prepping the data into a consistent format
max_samples <- 24 # in hours # the maximum samples from any individual
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")

for (dataset in species_list){
  
  species <- basename(dataset)
  print(species)
  
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo")){
    next
  }
  
  # Formatting all data sources into same structure
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))){
    print("already formatted")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))
  }
  
  # Downsampling to the same sampling rate
  standard_sampling_rate <- 10
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_resampled.csv")))){
    print("already resampled")
  } else {
    source(file = file.path(base_path, "Scripts", "DownsamplingFormattedData.R"))
  }
  
}

# putting them all on the same Gs acceleration scale
source(file = file.path(base_path, "Scripts", "DeterminingCalibration.R"))