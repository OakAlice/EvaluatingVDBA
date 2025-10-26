# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1


Gs_scaling <- fread(file.path(base_path, "Output", "G_scaling.csv"))
species_list <- na.omit(unique(unlist(Gs$species)))
Calib_species <- na.omit(unique(unlist(Gs$species[Gs$rescale == "Other"])))
Gs_species <- na.omit(unique(unlist(Gs$species[Gs$rescale == "G"])))

  if (species %in% Gs_species){
    accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
    next
  } else {
    accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_recalibrated.csv")))
  }
  
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
    
    # window will be the length of the burst
    win <- accel %>% group_by(burst_id) %>% count() %>% ungroup() %>% summarise(mean = mean(n))
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

