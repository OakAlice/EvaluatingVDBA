# Determining Calibration -------------------------------------------------
# there are many different devices with many different settings and units
# how can we compare them?
# this is the script for figuring that out

# determine which datasets are alreay in Gs 
# Considered to be in Gs when the static acceleration of flat spots = 1

# Functions ---------------------------------------------------------------
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


# Identify which ones are already in Gs -----------------------------------
species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

# identify the data that are already in Gs
Gs <- lapply(species_list, function(x){
  
  species <- basename(x)
    
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo", "Minasandra_Hyena", "Kamminga_Horse")){
    NULL
  } else {
  
  resampled_data <- file.path(base_path, "AccelerometerData", species, paste0(species, "_resampled.csv"))
  if (file.exists(resampled_data)){
    accel <- fread(resampled_data)
  } else {
    accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
    print("note that this data wasn't resampled... its samplign rate was too low")
  }
  
  maxX <- round(max(accel$Accel.X),3)
  minX <- round(min(accel$Accel.X),3)
  meanX <- round(mean(accel$Accel.X),3)
  medianX <- round(median(accel$Accel.X),3)
  
  sampling_style <- dataset_variables[Name == species]$SamplingStyle
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  
  static_accel_mean <- calculate_static_accel(accel, sampling_style, freq = 10) # because the frequency is 10Hz now
  
  table <- data.frame(species, static_accel_mean, maxX, minX, meanX, medianX)
  table
  }
})

Gs <- rbindlist(Gs, fill = TRUE)
Gs$rescale <- ifelse(Gs$static_accel_mean < 1.5 & Gs$static_accel_mean > 0.5, "G", "Other")
Gs$static_accel_mean <- round(Gs$static_accel_mean, 3)
fwrite(Gs, file.path(base_path, "Output", "G_scaling.csv"))
Gs_species <- unique(unlist(Gs$species[Gs$rescale == "G"]))
Gs_species <- na.omit(Gs_species)

# For the ones not in Gs, determine how to scale them ---------------------
# of the data I currently have, there appears to be a consistent scale to the non-G data

calib_species <- setdiff(basename(species_list), Gs_species)

lapply(calib_species, function(x){
  species <- x
  
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo", "Minasandra_Hyena", "Kamminga_Horse")){
    NULL
  } else {
    
    calib_factor <- Gs$static_accel_mean[Gs$species == species]
    
    accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
    
    accel[, Accel.X := Accel.X / calib_factor]
    accel[, Accel.Y := Accel.Y / calib_factor]
    accel[, Accel.Z := Accel.Z / calib_factor]
    
    fwrite(accel, file.path(base_path, "AccelerometerData", species, paste0(species, "_recalibrated.csv")))
  }
  
})
