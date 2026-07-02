# Finding the active / inactive threshold for each dataset ----------------

# load in the data
data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_", window_seconds, "_processed.csv")))

# ggplot(data, aes(x = vedba))+
#    geom_freqpoly() +
#    theme_minimal()

#### METHOD 1: DENSITY PLOT
# on the edge case of the peak being 0 it spilled into negative - setting an impossible threshold
# dens <- density(data$vedba, na.rm = TRUE, from = 0)
# vedba_peak <- dens$x[which.max(dens$y)]
# inactive_vedba <- vedba_peak * 2 # double it for the threshold

##### METHOD 2: 90th PERCENTILE
threshold_pct <- quantile(data$calibration_vedba, probs = 0.90, na.rm = TRUE)
threshold_pct <- ifelse(threshold_pct > 0.5, 0.5, threshold_pct)

data <- data %>%
  mutate(threshold = ifelse(calibration_vedba > threshold_pct, "active", "inactive"))

# if ("Activity" %in% colnames(data)){
#   # if Activity was recorded, look at whether the threshold was logical
#   ggplot(data, aes(x = Activity, y = vedba, colour = threshold))+
#     geom_point(position = "jitter") +
#     theme_minimal()
# }

# now we should take the means
summary <- data %>%
  group_by(ID, threshold) %>%
  summarise(meanVDBA = mean(straight_vedba),
            meanCalVDBA = mean(calibration_vedba)
  )

overall_summary <- data %>%
  group_by(ID) %>%
  summarise(meanVDBA = mean(straight_vedba),
            meanCalVDBA = mean(calibration_vedba)
  )
overall_summary$threshold <- "all"

vedba_stats <- rbind(summary, overall_summary)

# save the summary
fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, paste0(species, "_", window_seconds, "_summary.csv")))
