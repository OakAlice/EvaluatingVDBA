# generate the vdba and summary -------------------------------------------

if(file.exists(file.path(base_path, "Data/AccelerometerData", species, paste0(species, "_", window_seconds, "_summary.csv")))){
  print("already summarised")
} else {
  cleaned_file <- file.path(base_path, "Data/AccelerometerData", species, paste0(species, "_cleaned_reformatted.csv"))
  if (file.exists(cleaned_file)){
     accel <- fread(cleaned_file)
  } else {
    accel <- fread(file.path(base_path, "Data/AccelerometerData", species, paste0(species, "_reformatted.csv")))
  }
  
  accel <- generate_vdba(accel, species, dataset_variables, window_seconds)
  accel <- smooth_vdba(accel, species, dataset_variables, window = 5)
  
  # making some diagnostic plots ---------------------------------------------
  # plot the smoothed data and then where the threshold is
  # currently commented out as the images can take a while to generate
  # samples <- min(
  #   as.numeric(dataset_variables[Name == species]$Frequency) * 60 * 60, nrow(accel)
  # )
  # accel_sample <- accel[1:samples, ]
  # p1 <- ggplot(accel_sample, aes(x = seq(1:nrow(accel_sample)), y = smooth_vdba, colour = threshold, group = 1)) + geom_line()
  # p2 <- ggplot(accel_sample, aes(x = smooth_vdba, fill = threshold)) +
  #   geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  #   labs(y = "Frequency", fill = "Threshold") +
  #   theme_minimal()
  
  # Generate the summary stats ----------------------------------------------
  freq <- as.numeric(dataset_variables[Name == species]$Frequency) 
  
  # TODO: Need to update the threshold in this... ####
  vedba_stats <- summarise_vdba(accel, freq, window_seconds) 
  
  # save
  fwrite(vedba_stats$summary, file.path(base_path, "Data/AccelerometerData", species, 
                                  paste0(species, "_", window_seconds, "_summary.csv")))
  fwrite(vedba_stats$accel, file.path(base_path, "Data/AccelerometerData", species, 
                                        paste0(species, "_", window_seconds, "_processed.csv")))
  
}
