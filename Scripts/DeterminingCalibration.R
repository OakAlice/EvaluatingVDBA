# Determining Calibration -------------------------------------------------
# there are many different devices with many different settings and units
# how can we compare them?
# this is the script for figuring that out

# determine which datasets are alreay in Gs 
# Considered to be in Gs when the static acceleration of flat spots = 1



# Functions ---------------------------------------------------------------
detect_bursts <- function(data, gap_threshold = 1) {
  setDT(data)
  
  data <- data[order(ID, Time)]  # ensure sorted by ID and time
  id_change <- c(TRUE, diff(as.integer(factor(data$ID))) != 0)  # TRUE where ID changes
  time_gap <- c(FALSE, diff(data$Time) > gap_threshold)
  new_burst <- id_change | time_gap
  data[, burst_id := cumsum(new_burst)]
  
  return(data)
}

calculate_static_accel <- function(accel, sampling_style, freq){
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
  
  return(static_accel_mean)
}

# Code --------------------------------------------------------------------
species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

# identify the data that are already in Gs
Gs <- lapply(species_list, function(x){
  
  species <- basename(x)
    
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo", "Minasandra_Hyena", "Kamminga_Horse")){
    data.frame(species, NA)
  } else {
  
  accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
  
  sampling_style <- dataset_variables[Name == species]$SamplingStyle
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  
  static_accel_mean <- calculate_static_accel(accel, sampling_style, freq)
  
  data.frame(species, static_accel_mean)
  }
})

Gs <- rbindlist(Gs, fill = TRUE)
Gs$rescale <- ifelse(Gs$static_accel_mean < 1.5 & Gs$static_accel_mean > 0.5, "G", "Other")
Gs_species <- unique(unlist(Gs$species[Gs$rescale == "G"]))

# plot the ones that are already in Gs
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
  
}





