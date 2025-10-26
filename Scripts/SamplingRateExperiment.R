# Sampling Rate Experiment ------------------------------------------------

# Pasha suggested that the differential sampling rates may be playing a role.
# Going to do a sensitivity analysis on sampling rate to see whetehr it works
# The echidna data was sampled continuously at 100Hz so makes a good example


# Function for generating the VDBA ----------------------------------------
generate_vdba <- function(accel, freq){
  
  win <- 10 * freq
  # calculate the static accelerations
  ax_static <- frollmean(accel$Accel.X, n = win, align = "center", fill = NA)
  ay_static <- frollmean(accel$Accel.Y, n = win, align = "center", fill = NA)
  az_static <- frollmean(accel$Accel.Z, n = win, align = "center", fill = NA)
  
  # get the dynamic component 
  ax_dynamic <- accel$Accel.X - ax_static
  ay_dynamic <- accel$Accel.Y - ay_static
  az_dynamic <- accel$Accel.Z - az_static
  
  vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
  
  accel$vedba <- vedba
  
  # find the max of the static periods
  accel$rolling_sd <- roll_sd(accel$vedba, n = win, fill = NA, align = "center") # get the sd of the rolling windows
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE)) # periods of flat
  static_accel <- accel[static_idx, ]
  threshold_pct <- max(static_accel$vedba, na.rm = TRUE) # max acceleration in the flat periods
  
  accel <- na.omit(accel)
  accel <- accel %>%
    mutate(threshold = ifelse(vedba >= threshold_pct, "active", "inactive"))
  
  summary <- accel %>%
    group_by(ID, threshold) %>%
    summarise(
      meanVDBA = mean(vedba),
      minVDBA = min(vedba),
      maxVDBA = max(vedba),
      .groups = "drop"
    )
  
  overall_summary <- accel %>%
    group_by(ID) %>%
    summarise(
      meanVDBA = mean(vedba, na.rm = TRUE),
      minVDBA = min(vedba, na.rm = TRUE),
      maxVDBA = max(vedba, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(threshold = "all")
  
  sum <- rbind(summary, overall_summary)
  
  return(list(accel = accel,
              summary = sum))
}

# Code --------------------------------------------------------------------

species <- c("Kamminga_Horse", "Chakravarty_Meerkat", "Clemente_Echidna")

summaries <- lapply(species, function(x){
  print(x)
  
  # generate the datasets and generate the stats
  data100 <- fread(file.path(base_path, "AccelerometerData", x, paste0(x, "_reformatted.csv")))
  
  freqs <- c(100, 50, 25, 10, 5, 1)
  freqs_dict <- list( # define how to downsample them
    `100` = 1,
    `50`  = 2,
    `25`  = 4,
    `10`  = 10,
    `5`   = 20,
    `1`   = 100
  )
  
  vedba_stats <- lapply(freqs, function(ii) {
    
    # subsample the data
    dat <- data100[seq(1, .N, by = freqs_dict[[as.character(ii)]])]
    summary <- generate_vdba(dat, ii)$summary
    summary$freq <- ii
    
    summary
  })
  
  # Combine
  vedba_stats <- rbindlist(vedba_stats, fill = TRUE)
  vedba_stats$dataset <- x
  
  vedba_stats
})

vedba_stats <- rbindlist(summaries)

# save that shite
fwrite(vedba_stats, file.path(base_path, "Output", "Subsampling_experiments.csv"))
# vedba_stats <- fread(file.path(base_path, "Output", "Subsampling_experiments.csv"))

# test whetehr the sampling rate has a significant effect
summary(aov(maxVDBA ~ as.factor(freq) + as.factor(dataset), data = vedba_stats))
model <- lmer(maxVDBA ~ as.factor(freq) + (1 | dataset), data = vedba_stats)
summary(model)


summary(aov(meanVDBA ~ as.factor(freq) + as.factor(dataset), data = vedba_stats))
model <- lmer(meanVDBA ~ as.factor(freq) + (1 | dataset), data = vedba_stats)
summary(model)

# plot
ggplot(vedba_stats, aes(x = as.factor(freq), y = maxVDBA, colour = as.factor(dataset))) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")
# plot
ggplot(vedba_stats, aes(x = as.factor(freq), y = meanVDBA, colour = as.factor(dataset))) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")




