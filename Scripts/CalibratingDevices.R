# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1













# All of this is wrong ----------------------------------------------------
### NEED TO REDO ALL OF THIS WHEN MY THINKING IS CLEAR #######







accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

sampling_style <- dataset_variables[Name == species]$SamplingStyle
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

## if its one or a little bit above 1 then its a G, and don't rescale it
# only rescale if above 2

if (sampling_style == "Continuous") {
  win <- 10 * freq  # 10-second rolling window
  mag <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  
  # faster version
  accel$rolling_sd <- roll_sd(mag, n = win, fill = NA, align = "left")
  
  # ggplot(accel, aes(x = rolling_sd))+
  #   geom_density()
  
  # Identify static periods (lowest variance) and estimate the VDBA in those spots
  static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE))
  static_accel <- accel[static_idx, ]
  static_vdba <- mean(sqrt(static_accel$Accel.X^2 + static_accel$Accel.Y^2 + static_accel$Accel.Z^2), na.rm = TRUE)
  
  # check whether these are alreay close to gravity
  if (static_vdba < 1.5 & static_vdba > 0.5){
    
    print("already in Gs, no need to calibrate")
    accel[, `:=`(
      ax_static = frollmean(Accel.X, n = win, align = "center", fill = NA),
      ay_static = frollmean(Accel.Y, n = win, align = "center", fill = NA),
      az_static = frollmean(Accel.Z, n = win, align = "center", fill = NA)
    )]
    accel[, `:=`(
      ax_dynamic = Accel.X - ax_static,
      ay_dynamic = Accel.Y - ay_static,
      az_dynamic = Accel.Z - az_static
    )]
    accel[, `:=`(
      vedba1 = sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
    )]
    
    accel <- accel[, c("ID", "Time", "Accel.X", "Accel.Y", "Accel.Z",
                       "vedba1")]
    
  } else {
  
    # Method 1: Raw VDBA (no calibration) 
    accel[, vedba1 := sqrt(Accel.X^2 + Accel.Y^2 + Accel.Z^2)]
    
    # Compute static mean and scale factor
    static_mean <- colMeans(accel[static_idx, .(Accel.X, Accel.Y, Accel.Z)], na.rm = TRUE)
    scale_factor <- sqrt(sum(static_mean^2))
    
    # Bias-corrected and scaled accelerations (vectorised)
    accel[, `:=`(
      X_bias = Accel.X - static_mean["Accel.X"],
      Y_bias = Accel.Y - static_mean["Accel.Y"],
      Z_bias = Accel.Z - static_mean["Accel.Z"],
      X_scale = Accel.X / scale_factor,
      Y_scale = Accel.Y / scale_factor,
      Z_scale = Accel.Z / scale_factor
    )]
    
    # Compute rolling static and dynamic components for all variants
    accel[, `:=`(
      ax_static = frollmean(Accel.X, n = win, align = "center", fill = NA),
      ay_static = frollmean(Accel.Y, n = win, align = "center", fill = NA),
      az_static = frollmean(Accel.Z, n = win, align = "center", fill = NA),
      bx_static = frollmean(X_bias,   n = win, align = "center", fill = NA),
      by_static = frollmean(Y_bias,   n = win, align = "center", fill = NA),
      bz_static = frollmean(Z_bias,   n = win, align = "center", fill = NA),
      sx_static = frollmean(X_scale,  n = win, align = "center", fill = NA),
      sy_static = frollmean(Y_scale,  n = win, align = "center", fill = NA),
      sz_static = frollmean(Z_scale,  n = win, align = "center", fill = NA)
    )]
    
    # Dynamic components
    accel[, `:=`(
      ax_dynamic = Accel.X - ax_static,
      ay_dynamic = Accel.Y - ay_static,
      az_dynamic = Accel.Z - az_static,
      bx_dynamic = X_bias   - bx_static,
      by_dynamic = Y_bias   - by_static,
      bz_dynamic = Z_bias   - bz_static,
      sx_dynamic = X_scale  - sx_static,
      sy_dynamic = Y_scale  - sy_static,
      sz_dynamic = Z_scale  - sz_static
    )]
    
    # Compute VDBA for all methods
    accel[, `:=`(
      vedba2 = sqrt(bx_dynamic^2 + by_dynamic^2 + bz_dynamic^2),                # bias-corrected
      vedba3 = sqrt(sx_dynamic^2 + sy_dynamic^2 + sz_dynamic^2),                # scaled input
      vedba4 = sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2) / scale_factor  # scaled output
    )]
    
    accel[, grep("_(bias|scale|static|dynamic)$", names(accel), value = TRUE) := NULL]
  }
  
} else { # for the burst data
  
  # is this burst or continuous data, and if busts, label the bursts
  accel <- detect_bursts(accel, gap_threshold = 1)
  # data$burst_id <- "1"
  
  # calculate whether its already in Gs or not
  # Find static periods for the whole dataset ifnoring the bursts for now
  vedba2 <- sqrt(accel$Accel.X^2 + accel$Accel.Y^2 + accel$Accel.Z^2)
  accel$vedba2 <- vedba2
  
  # Calculate per-burst static stats
  burst_static <- accel[, .(
    mean_vedba = mean(vedba2, na.rm = TRUE),
    sd_vedba = sd(vedba2, na.rm = TRUE)
  ), by = burst_id]
  
  # Use bursts with lowest variance (most static)
  static_bursts <- burst_static[sd_vedba < quantile(sd_vedba, 0.25, na.rm = TRUE), burst_id]
  
  # Compute mean offsets across static bursts
  static_mean <- accel[burst_id %in% static_bursts,
                       lapply(.SD, mean, na.rm = TRUE),
                       .SDcols = c("Accel.X", "Accel.Y", "Accel.Z")]
  # static data
  static_data <- accel[burst_id %in% static_bursts, ]
  static_vdba <- mean(sqrt(static_data$Accel.X^2 + static_data$Accel.Y^2 + static_data$Accel.Z^2), na.rm = TRUE)
  
  # calculate the burst means
  # his represents the static component
  burst_means <- accel[, .(
    mean_X = mean(Accel.X, na.rm = TRUE),
    mean_Y = mean(Accel.Y, na.rm = TRUE),
    mean_Z = mean(Accel.Z, na.rm = TRUE)
  ), by = .(ID, burst_id)]
  
  #  Merge back into accel
  accel <- merge(accel, burst_means, by = c("ID", "burst_id"), all.x = TRUE)
  
  if (static_vdba < 1.5 & static_vdba > 0.5){
    
    print("already in Gs, no need to calibrate")
    
    accel[, `:=`(
      X_dyn = Accel.X - mean_X,
      Y_dyn = Accel.Y - mean_Y,
      Z_dyn = Accel.Z - mean_Z
    )]
    
    accel[, `:=`(
      vedba1 = sqrt(X_dyn^2 + Y_dyn^2 + Z_dyn^2)
    )]
    
  } else {
    
    accel[, burst_scale := sqrt(mean_X^2 + mean_Y^2 + mean_Z^2)]
    
    accel[, `:=`(
      X_bias = Accel.X - mean_X,
      Y_bias = Accel.Y - mean_Y,
      Z_bias = Accel.Z - mean_Z,
      X_scale = Accel.X / burst_scale,
      Y_scale = Accel.Y / burst_scale,
      Z_scale = Accel.Z / burst_scale
    )]
    
    accel[, `:=`(
      # Raw (no calibration)
      dx_raw = Accel.X - mean_X,
      dy_raw = Accel.Y - mean_Y,
      dz_raw = Accel.Z - mean_Z,
      
      # Bias-corrected
      dx_bias = X_bias,
      dy_bias = Y_bias,
      dz_bias = Z_bias,
      
      # Scaled-input
      dx_scale = X_bias / burst_scale,
      dy_scale = Y_bias / burst_scale,
      dz_scale = Z_bias / burst_scale
    )]
    
    # calculate the variants 
    accel[, `:=`(
      vedba1 = sqrt(dx_raw^2 + dy_raw^2 + dz_raw^2),                       # uncalibrated
      vedba3 = sqrt(dx_scale^2 + dy_scale^2 + dz_scale^2),                 # scaled-input
      vedba4 = sqrt(dx_raw^2 + dy_raw^2 + dz_raw^2) / burst_scale          # scaled-output
    )]
    
    accel <- accel[, c("ID", "burst_id", "Time", "Accel.X", "Accel.Y", "Accel.Z",
              "vedba1", "vedba2", "vedba3", "vedba4")]
    
  }
}

fwrite(accel,
       file.path(base_path, "AccelerometerData", species, paste0(species, "_rescaled.csv")))
