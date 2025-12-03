# generate the vdba and summary -------------------------------------------

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

# calculating the active minutes per day
freq <- as.numeric(dataset_variables[Name == species]$Frequency)
minutes_per_id <- vedba_stats$accel %>%
  group_by(ID) %>%
  summarise(
    active_min   = (sum(threshold == "active") / freq) / 60,
    total_min    = (n() / freq) / 60,
    prop_active  = active_min / total_min
  )
fwrite(minutes_per_id, file.path(base_path, "AccelerometerData", species, paste0(species, "_active_minutes.csv")))

