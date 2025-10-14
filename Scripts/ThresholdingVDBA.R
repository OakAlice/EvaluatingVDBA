# Finding the active / inactive threshold for each dataset ----------------

# load in the data
rescaled_data_path <- file.path(base_path, "AccelerometerData", species, paste0(species, "_processed_rescaled.csv"))
if (file.exists(rescaled_data_path)){
  data <- fread(rescaled_data_path)
} else {
  data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))
}

data <- data %>%
  select(ID, vedba, odba) %>%
  na.omit()

# make a plot of the frequency
ggplot(data, aes(x = vedba))+
  geom_freqpoly() +
  theme_minimal()

# find the vedba of the peak
dens <- density(data$vedba, na.rm = TRUE)
vedba_peak <- dens$x[which.max(dens$y)]

inactive_vedba <- vedba_peak * 2 # double it for the threshold

data <- data %>%
  mutate(threshold = ifelse(vedba > inactive_vedba, "active", "inactive"))

if ("Activity" %in% colnames(data)){
  # if Activity was recorded, look at whether the threshold was logical
  ggplot(data, aes(x = Activity, y = vedba, colour = threshold))+
    geom_point(position = "jitter") +
    theme_minimal()
}

# now we should take the means
summary <- data %>%
  group_by(ID, threshold) %>%
  summarise(meanVDBA = mean(vedba),
            minVDBA = min(vedba),
            maxVDBA = max(vedba),
            meanODBA = mean(odba),
            minODBA = min(odba),
            maxODBA = max(odba)
  )

overall_summary <- data %>%
  group_by(ID) %>%
  summarise(meanVDBA = mean(vedba),
            minVDBA = min(vedba),
            maxVDBA = max(vedba),
            meanODBA = mean(odba),
            minODBA = min(odba),
            maxODBA = max(odba)
  )
overall_summary$threshold <- "all"

vedba_stats <- rbind(summary, overall_summary)

# save the summary
fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))
