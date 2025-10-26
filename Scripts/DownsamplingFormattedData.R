# Downsampling Formatted Data ---------------------------------------------

# load in the data and downsample it to a consistent sampling rate

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

sampling_style <- dataset_variables[Name == species]$SamplingStyle
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

if (standard_sampling_rate > freq){
  print("the sampling rate is smaller than the standard sampling rate... skipping")
} else {
  downsampling_factor <- round(freq / standard_sampling_rate,0)

  if (sampling_style == "Continuous"){
    resampled <- data[seq(1, .N, by = downsampling_factor)]
  } else {
    data <- detect_bursts(data, gap_threshold = 1)
    resampled <- data[, .SD[seq(1, .N, by = downsampling_factor)], by = burst_id]
  }
  
  fwrite(resampled, file.path(base_path, "AccelerometerData", species, paste0(species, "_resampled.csv")))

}


