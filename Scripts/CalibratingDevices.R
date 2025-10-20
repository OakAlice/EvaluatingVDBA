# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1

accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

sampling_style <- dataset_variables[Name == species]$SamplingStyle
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

if (sampling_style == "Continuous") {
  win <- 10 * freq  # 1-second rolling window
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  
  accel$rolling_sd <- rollapply(mag, win, sd, fill = NA, align = "left")
  
  # Identify static periods (lowest 25% of variance)
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE))
  
  # Estimate offsets (this is the sensor bias) from the known static periods
  static_mean <- colMeans(accel[static_idx, .(Accel.X, Accel.Y, Accel.Z)], na.rm = TRUE)
  
  # Subtract bias and scale to unit gravity
  accel2 <- copy(accel)
  accel2[, `:=`(
    Accel.X = Accel.X - static_mean["Accel.X"],
    Accel.Y = Accel.Y - static_mean["Accel.Y"],
    Accel.Z = Accel.Z - static_mean["Accel.Z"]
  )]
  scale_factor <- mean(sqrt(accel2$Accel.X^2 + accel2$Accel.Y^2 + accel2$Accel.Z^2), na.rm = TRUE)
  accel2[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
  
} else {
  
  # is this burst or continuous data, and if busts, label the bursts
  accel <- detect_bursts(accel, gap_threshold = 1)
  # data$burst_id <- "1"
  
  # Find static periods for the whole dataset ifnoring the bursts for now
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  accel$mag <- mag
  
  # Calculate per-burst static stats
  burst_static <- accel[, .(
    mean_mag = mean(mag, na.rm = TRUE),
    sd_mag = sd(mag, na.rm = TRUE)
  ), by = burst_id]
  
  # Use bursts with lowest variance (most static)
  static_bursts <- burst_static[sd_mag < quantile(sd_mag, 0.25, na.rm = TRUE), burst_id]
  
  # Compute mean offsets across static bursts
  static_mean <- accel[burst_id %in% static_bursts,
                       lapply(.SD, mean, na.rm = TRUE),
                       .SDcols = c("Accel.X", "Accel.Y", "Accel.Z")]
  
  offsets <- unlist(static_mean)
  
  accel2 <- copy(accel)
  accel2[, `:=`(
    Accel.X = Accel.X - offsets["Accel.X"],
    Accel.Y = Accel.Y - offsets["Accel.Y"],
    Accel.Z = Accel.Z - offsets["Accel.Z"]
  )]
  
  scale_factor <- mean(sqrt(accel2$Accel.X^2 + accel2$Accel.Y^2 + accel2$Accel.Z^2), na.rm = TRUE)
  accel2[, `:=`(
    Accel.X = Accel.X / scale_factor,
    Accel.Y = Accel.Y / scale_factor,
    Accel.Z = Accel.Z / scale_factor
  )]
}

fwrite(accel2,
       file.path(base_path, "AccelerometerData", species, paste0(species, "_rescaled.csv")))
