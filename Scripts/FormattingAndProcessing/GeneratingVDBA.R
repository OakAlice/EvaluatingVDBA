# generate the vdba and summary -------------------------------------------
if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))){
  print("already summarised")
} else {
  cleaned_file <- file.path(base_path, "AccelerometerData", species, paste0(species, "_cleaned_reformatted.csv"))
  if (file.exists(cleaned_file)){
     accel <- fread(cleaned_file)
  } else {
    accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
  }
  
  accel <- generate_vdba(accel, species, dataset_variables)
  
  # have removed the smoothing because I'm already doing a mean so don't need the extra step???
  # sampling_style <- dataset_variables[Name == species]$SamplingStyle
  # if (sampling_style == "Continuous"){
  #   accel <- smooth_vdba(accel, species, dataset_variables, window = 1)
  # }
  
  # removed threshold because its now calculated more simply inside the summarisation
  # accel <- generate_threshold(accel, species, dataset_variables)
  
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
  # 
  # ggsave(file.path(base_path, "AccelerometerData", species, "assessing_the_threshold.png"), plot = p1,
  #        width = 10, height = 4, dpi = 300)
  # ggsave(file.path(base_path, "AccelerometerData", species, "frequency_of_vedba.png"),  plot = p2,
  #        width = 10, height = 4, dpi = 300)
  
  # Generate the summary stats ----------------------------------------------
  freq <- as.numeric(dataset_variables[Name == species]$Frequency) 
  burst <- as.character(dataset_variables[Name == species]$SamplingStyle)
  vedba_stats <- summarise_vdba(accel, freq) 
  
  fwrite(vedba_stats$summary, file.path(base_path, "AccelerometerData", species, 
                                  paste0(species, "_summary.csv")))
  
  fwrite(vedba_stats$accel, file.path(base_path, "AccelerometerData", species, 
                                        paste0(species, "_processed.csv")))
  
}



# Active minutes ----------------------------------------------------------
# calculating the active minutes per day
# Define cut points for active levels (this will be relative to the species)
accel <- fread(file.path(base_path, "AccelerometerData", species, 
                                    paste0(species, "_processed.csv")))
accel <- accel %>%
  mutate(
    activity_level = case_when(
      threshold == "inactive"                                       ~ "inactive",
      seconds_VDBA > 0.75 * max(accel$seconds_VDBA)     ~ "high",
      seconds_VDBA > 0.5 *  max(accel$seconds_VDBA)     ~ "medium",
      seconds_VDBA > 0.05 * max(accel$seconds_VDBA)     ~ "low",
      TRUE                    ~ "low"
    )
  )

# Frequency from metadata 
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

# Summaries
minutes_per_id <- accel %>%
  group_by(ID) %>%
  summarise(
    total_min = n() / freq / 60,
    high_min  = sum(activity_level == "high")    / freq / 60,
    med_min   = sum(activity_level == "medium")  / freq / 60,
    low_min   = sum(activity_level == "low")     / freq / 60,
    rest_min  = sum(activity_level == "inactive")/ freq / 60,
    prop_high = high_min / total_min,
    prop_med  = med_min  / total_min,
    prop_low  = low_min  / total_min,
    prop_rest = rest_min / total_min
  )

fwrite(minutes_per_id, file.path(base_path, "AccelerometerData", species, paste0(species, "_active_minutes.csv")))

