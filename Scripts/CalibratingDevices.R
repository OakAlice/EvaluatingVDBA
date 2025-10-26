# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1









# plot the ones that are already in Gs -------------------------------------
for (species in Gs_species){
  
  accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
  sampling_style <- dataset_variables[Name == species]$SamplingStyle
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  
  if (sampling_style == "Continuous") {
    
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
    
  } else { # burst
    
    accel <- detect_bursts(accel, gap_threshold = 1)
    
    burst_means <- accel[, .(
      mean_X = mean(Accel.X, na.rm = TRUE),
      mean_Y = mean(Accel.Y, na.rm = TRUE),
      mean_Z = mean(Accel.Z, na.rm = TRUE)
    ), by = .(ID, burst_id)]
    
    accel <- merge(accel, burst_means, by = c("ID", "burst_id"), all.x = TRUE)
    
    accel[, ax_dynamic := Accel.X - mean_X]
    accel[, ay_dynamic := Accel.Y - mean_Y]
    accel[, az_dynamic := Accel.Z - mean_Z]
    
    vedba <- sqrt(accel$ax_dynamic^2 + accel$ay_dynamic^2 + accel$az_dynamic^2)
  }
  
  accel$vedba <- vedba
  
  # ggplot(dat, aes(x = vedba)) + geom_density()
  # ggplot(dat[1:10000, ], aes(x = seq(1:10000), y = Accel.X, colour = threshold)) + geom_point()
  
  # find the max of the static periods
  accel$rolling_sd <- roll_sd(accel$vedba, n = win, fill = NA, align = "center") # get the sd of the rolling windows
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE)) # periods of flat
  static_accel <- accel[static_idx, ]
  threshold_pct <- max(static_accel$vedba, na.rm = TRUE) # max acceleration in the flat periods
  
  accel <- accel %>%
    mutate(threshold = ifelse(vedba > threshold_pct, "active", "inactive"))
  
  # Save overall species summary
  fwrite(accel, file.path(base_path, "AccelerometerData", species, 
                       paste0(species, "_processed.csv")))
  
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
      meanVDBA = mean(vedba),
      minVDBA = min(vedba),
      maxVDBA = max(vedba),
      .groups = "drop"
    ) %>%
    mutate(threshold = "all")
  
  vedba_stats <- rbind(summary, overall_summary)
  
  fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, 
                                paste0(species, "_summary.csv")))
}

# plot the ones not in Gs -------------------------------------------------














































# Functions ---------------------------------------------------------------
# detect the bursts based on time gaps
detect_bursts <- function(data, gap_threshold = 1) {
  setDT(data)
  
  data <- data[order(ID, Time)]  # ensure sorted by ID and time
  id_change <- c(TRUE, diff(as.integer(factor(data$ID))) != 0)  # TRUE where ID changes
  time_gap <- c(FALSE, diff(data$Time) > gap_threshold)
  new_burst <- id_change | time_gap
  data[, burst_id := cumsum(new_burst)]
  
  return(data)
}

calculate_dyn_vedba <- function(accel, freq){
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

  return(vedba)
}

calculate_dyn_burst_vedba <- function(accel){
  # calculate the burst means as the static component
  burst_means <- accel[, .(
    mean_X = mean(Accel.X, na.rm = TRUE),
    mean_Y = mean(Accel.Y, na.rm = TRUE),
    mean_Z = mean(Accel.Z, na.rm = TRUE)
  ), by = .(ID, burst_id)]
  
  accel <- merge(accel, burst_means, by = c("ID", "burst_id"), all.x = TRUE)
  
  accel[, ax_dynamic := Accel.X - mean_X]
  accel[, ay_dynamic := Accel.Y - mean_Y]
  accel[, az_dynamic := Accel.Z - mean_Z]
  
  vedba <- sqrt(accel$ax_dynamic^2 + accel$ay_dynamic^2 + accel$az_dynamic^2)
  
  return(vedba)
}

calibrated_dyn_accel <- function(accel, freq){
  
  win <- 10 * freq  # 10-second rolling window
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  
  # faster version
  accel$rolling_sd <- roll_sd(mag, n = win, fill = NA, align = "center")

  # Identify static periods (lowest variance) and estimate the VDBA in those spots
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE))
  
  # Compute static mean and scale factor
  static_mean <- colMeans(accel[static_idx, .(Accel.X, Accel.Y, Accel.Z)], na.rm = TRUE)
  scale_factor <- sqrt(sum(static_mean^2))
  
  # method 1, no changes
  vedba1 <- calculate_dyn_vedba(accel, freq)
  
  # minusing the bias from each axis
  accel2 <- copy(accel)
  accel2[, `:=`(
    Accel.X = Accel.X - static_mean["Accel.X"],
    Accel.Y = Accel.Y - static_mean["Accel.Y"],
    Accel.Z = Accel.Z - static_mean["Accel.Z"]
  )]
  vedba2 <- calculate_dyn_vedba(accel2, freq)
  
  # dividing each axis by the scale factor
  accel3 <- copy(accel)
  accel3[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
  vedba3 <- calculate_dyn_vedba(accel3, freq)
  
  # dividing the original vedba by the scale factor
  vedba4 <- vedba1 / scale_factor
  
  # offset and scaling
  accel5 <- copy(accel2)
  accel5[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
  vedba5 <- calculate_dyn_vedba(accel5, freq)
    
  return(list(vedba1 = vedba1,
              vedba2 = vedba2,
              vedba3 = vedba3,
              vedba4 = vedba4,
              vedba5 = vedba5
              ))
}

calibrated_dyn_burst_accel <- function(accel){
  
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  accel$mag <- mag
  
  # Calculate per-burst static stats
  burst_static <- accel[, .(
    sd_mag = sd(mag, na.rm = TRUE)
  ), by = burst_id]
  
  # Use bursts with lowest variance (most static) to calculate the mean
  static_bursts <- burst_static[sd_mag < quantile(sd_mag, 0.25, na.rm = TRUE), burst_id]
  static_mean <- accel[burst_id %in% static_bursts, 
                       .(Accel.X, Accel.Y, Accel.Z)] |>
    colMeans(na.rm = TRUE)
  scale_factor <- sqrt(sum(static_mean^2))
  
  # calculayte the vedbas
  # method 1, no changes
  vedba1 <- calculate_dyn_burst_vedba(accel)
  
  # minusing the bias from each axis
  accel2 <- copy(accel)
  accel2[, `:=`(
    Accel.X = Accel.X - static_mean["Accel.X"],
    Accel.Y = Accel.Y - static_mean["Accel.Y"],
    Accel.Z = Accel.Z - static_mean["Accel.Z"]
  )]
  vedba2 <- calculate_dyn_burst_vedba(accel2)
  
  # dividing each axis by the scale factor
  accel3 <- copy(accel)
  accel3[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
  vedba3 <- calculate_dyn_burst_vedba(accel3)
  
  # dividing the original vedba by the scale factor
  vedba4 <- vedba1 / scale_factor
  
  # bias offset and the scale factor
  accel5 <- copy(accel2)
  accel5[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
  vedba5 <- calculate_dyn_burst_vedba(accel5)
  
  return(list(vedba1 = vedba1,
              vedba2 = vedba2,
              vedba3 = vedba3,
              vedba4 = vedba4,
              vedba5 = vedba5
  ))
}

# Code --------------------------------------------------------------------
# load in the data and the variables
accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

sampling_style <- dataset_variables[Name == species]$SamplingStyle
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

# Determine whether it needs to be rescaled or not ------------------------
if (sampling_style == "Continuous") {
  win <- 10 * freq  # 10-second rolling window
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  accel$mag <- mag
  
  # faster version
  accel$rolling_sd <- roll_sd(mag, n = win, fill = NA, align = "center")
  
  # ggplot(accel, aes(x = rolling_sd))+
  #   geom_density()
  
  # Identify static periods (lowest variance) and estimate the VDBA in those spots
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE))
  static_accel <- accel[static_idx, ]
  static_accel_mean <- mean(static_accel$mag, na.rm = TRUE)
  print(paste0("Average acceleration when the device is still: ", static_accel_mean))

} else { # for the burst data
  
  accel <- detect_bursts(accel, gap_threshold = 1)
  
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  accel$mag <- mag
  
  # Calculate per-burst static stats
  burst_static <- accel[, .(
    sd_mag = sd(mag, na.rm = TRUE)
  ), by = burst_id]
  
  # ggplot(burst_static, aes(x = sd_mag))+
  #   geom_density()
  
  # Use bursts with lowest variance (most static) to calculate the mean
  static_bursts <- burst_static[sd_mag < quantile(sd_mag, 0.25, na.rm = TRUE), burst_id]
  static_data <- accel[burst_id %in% static_bursts, ]
  static_accel_mean <- mean(static_data$mag, na.rm = TRUE)
  print(paste0("Average acceleration when the device is still: ", static_accel_mean))
}

# clean it back up
cols_to_keep <- intersect(names(accel), c("ID", "Time", "Accel.X", "Accel.Y", "Accel.Z", "burst_id"))
accel <- accel[, ..cols_to_keep]

# Calculate VDBA ----------------------------------------------------------
# if the data doesn't need to be rescaled, leave in Gs
if (static_accel_mean < 1.5 & static_accel_mean > 0.5){
  print("already in Gs, no need to recalibrate")
  
  if (sampling_style == "Continuous") {
    accel$vedba1 <- calculate_dyn_vedba(accel, freq)
  } else { # burst
    accel$vedba1 <- calculate_dyn_burst_vedba(accel)
  }
}
    
# if the data does need to be recalibrated, then try multiple methods
# this is just me experimenting until we figure out what we're going to use
if (static_accel_mean > 1.5 | static_accel_mean < 0.5){
  if (sampling_style == "Continuous") {
    
    vedba_variants <- calibrated_dyn_accel(accel, freq)
    accel$vedba1 <- vedba_variants$vedba1 # control, no reclibration
    accel$vedba2 <- vedba_variants$vedba2 # minusing out the bias
    accel$vedba3 <- vedba_variants$vedba3 # dividing by the scale factor
    accel$vedba4 <- vedba_variants$vedba4 # dividing the vedba by the scale factor
    accel$vedba5 <- vedba_variants$vedba5 # bias removal and scaling
    
  } else { # burst
    
    vedba_variants <- calibrated_dyn_burst_accel(accel)
    accel$vedba1 <- vedba_variants$vedba1 # control, no reclibration
    accel$vedba2 <- vedba_variants$vedba2 # minusing out the bias
    accel$vedba3 <- vedba_variants$vedba3 # dividing by the scale factor
    accel$vedba4 <- vedba_variants$vedba4 # dividing the vedba by the scale factor
    accel$vedba5 <- vedba_variants$vedba5 # bias removal and scaling
    
  }
}

fwrite(accel,
       file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))

