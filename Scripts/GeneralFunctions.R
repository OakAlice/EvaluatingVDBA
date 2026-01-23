
# Detect bursts ------------------------------------------------------
detect_bursts <- function(data, gap_threshold = 1) {
  setDT(data)
  
  data <- data[order(ID, Time)]  # ensure sorted by ID and time
  id_change <- c(TRUE, diff(as.integer(factor(data$ID))) != 0)  # TRUE where ID changes
  time_gap <- c(FALSE, diff(data$Time) > gap_threshold)
  new_burst <- id_change | time_gap
  data[, burst_id := cumsum(new_burst)]
  
  return(data)
}

# Reformatting  --------------------------------------------------------
reformat_eobs_data <- function(data){
  long <- data[, .(
    acc = as.numeric(unlist(strsplit(`eobs:accelerations-raw`, " ")))
  ), by = .(Event.ID = `event-id`, 
            timestamp, 
            ID = `individual-local-identifier`,
            freq = `eobs:acceleration-sampling-frequency-per-axis`)]
  
  if (data$`eobs:acceleration-axes`[1] == "XYZ"){
    long[, idx := rep(1:floor(.N/3), each = 3)[1:.N], by = .(Event.ID, timestamp)]
    long[, axis := rep(c("X", "Y", "Z"), length.out = .N), by = .(Event.ID, timestamp)]
    
  } else if (data$`eobs:acceleration-axes`[1] == "XY"){
    long[, idx := rep(1:(.N/2), each = 2)[1:.N], by = .(Event.ID, timestamp)]
    long[, axis := rep(c("X", "Y"), times = .N/2), by = .(Event.ID, timestamp)]  
  } else {
    print("there is a different number of axes in this case")
  }
  
  wide <- dcast(long, Event.ID + timestamp + ID + freq + idx ~ axis, value.var = "acc")
  
  wide[, Time := timestamp + (idx - 1) * (1/freq)]
  
  if (data$`eobs:acceleration-axes`[1] == "XYZ"){
    data2 <- wide %>%
      rename(Accel.X = X,
             Accel.Y = Y,
             Accel.Z = Z) %>%
      select(Accel.X, Accel.Y, Accel.Z, Time, Event.ID, ID) %>%
      mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS"))
  } else if (data$`eobs:acceleration-axes`[1] == "XY"){
    data2 <- wide %>%
      rename(Accel.X = X,
             Accel.Y = Y) %>%
      select(Accel.X, Accel.Y, Time, Event.ID, ID) %>%
      mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS"))
  }
  
  return(data2)
}

reformat_clemente_data <- function(x){
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
  dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
}



# vdba generation ---------------------------------------------------------
generate_vdba <- function(accel, species, dataset_variables, window_seconds){
  
  sampling_style <- dataset_variables[Name == species]$SamplingStyle
  # freq <- as.numeric(dataset_variables[Name == species]$Frequency) # I've now set it to be 10 for all
  
  if (sampling_style == "Continuous") {
    
    freq <- as.numeric(dataset_variables[Name == species]$Frequency)
    if (is.na(freq)) stop("Frequency missing for species: ", species)
    win <- window_seconds * freq  # smoothing window
    
    # calculate the static accelerations
    ax_static <- frollmean(accel$Accel.X, n = win, align = "center", fill = NA)
    ay_static <- frollmean(accel$Accel.Y, n = win, align = "center", fill = NA)
    az_static <- frollmean(accel$Accel.Z, n = win, align = "center", fill = NA)
    
    # get the dynamic component 
    ax_dynamic <- accel$Accel.X - ax_static
    ay_dynamic <- accel$Accel.Y - ay_static
    az_dynamic <- accel$Accel.Z - az_static
    
    vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
    odba <- abs(ax_dynamic) + abs(ay_dynamic) + abs(az_dynamic)
    
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
    odba <- abs(accel$ax_dynamic) + abs(accel$ay_dynamic) + abs(accel$az_dynamic)
  }
  
  accel$vedba <- vedba
  accel$odba <- odba
  
  return(accel)
}

# smooth it out based on a rolling window
smooth_vdba <- function(accel, species, dataset_variables, window = 5) {
  
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  if (is.na(freq)) stop("Frequency missing for species: ", species)
  
  win <- window * freq
  
  # smooth VeDBA using rolling mean
  accel[, smooth_vdba := frollmean(vedba, n = win, align = "center", fill = NA)]
  accel[, smooth_odba := frollmean(odba, n = win, align = "center", fill = NA)]
  # accel<- accel %>% select(ID, Time, smooth_vdba) %>% na.omit()
  
  return(accel)
}

# generate_threshold <- function(accel, species, dataset_variables) {
#   
#   sampling_style <- dataset_variables[Name == species]$SamplingStyle
#   
#   if (sampling_style == "Continuous") {
#     
#     freq <- as.numeric(dataset_variables[Name == species]$Frequency)
#     if (is.na(freq)) stop("Frequency missing for species: ", species)
#     win <- 5 * freq
#     
#     accel$rolling_sd <- roll_sd(accel$smooth_vdba, n = win, fill = NA, align = "center")
#     
#     static_idx <- which(accel$rolling_sd < quantile(accel$rolling_sd, 0.25, na.rm = TRUE))
#     static_accel <- accel[static_idx, ]
#     threshold_pct <- mean(static_accel$smooth_vdba, na.rm = TRUE)
#     
#     accel <- accel %>%
#       na.omit() %>%
#       mutate(threshold = ifelse(smooth_vdba > threshold_pct, "active", "inactive"))
#     
#   } else {
#     
#     accel <- detect_bursts(accel, gap_threshold = 1)
#     
#     threshold_pct <- accel %>%
#       group_by(burst_id) %>%
#       summarise(sd = sd(vedba, na.rm = TRUE), .groups = "drop") %>%
#       arrange(sd) %>%
#       slice(floor(n() * 0.25)) %>%
#       pull(sd)
#     
#     accel_activities <- accel %>%
#       na.omit() %>%
#       group_by(ID, burst_id) %>%
#       summarise(sd = sd(vedba, na.rm = TRUE), .groups = "drop") %>%
#       mutate(threshold = ifelse(sd > threshold_pct, "active", "inactive"))
#     
#     accel <- merge(accel, accel_activities, by = c("ID", "burst_id"))
#       
#   }
#   
#   return(accel)
# }

summarise_vdba <- function(accel, freq, window_seconds) {

  # firstly take the mean of each second
  # convert the times into bins of the defined window length
  seconds <- accel %>%
    group_by(ID) %>%
    mutate(
      sample_i = row_number(),
      window = floor((sample_i - 1) / (window_seconds * freq))
    ) %>%
    group_by(ID, window) %>%
    summarise(
      seconds_VDBA = mean(vedba, na.rm = TRUE),
      seconds_ODBA = mean(odba, na.rm = TRUE),
      int_VDBA = { # take the trapezoidal integration
        v <- vedba[!is.na(vedba)]
        if (length(v) < 2) NA_real_
        else (sum(v) - 0.5 * (v[1] + v[length(v)])) / freq
      },
      .groups = "drop"
    )
  
  
  seconds <- seconds %>%
    na.omit() %>%
    mutate(threshold = ifelse(seconds_VDBA > 0.05, "active", "inactive"))
  
  # now take a mean across the seconds
  summary_state <- seconds %>%
    group_by(ID, threshold) %>%
    summarise(
      meanVDBA = mean(.data$seconds_VDBA, na.rm = TRUE),
      minVDBA  = min(.data$seconds_VDBA, na.rm = TRUE),
      maxVDBA  = max(.data$seconds_VDBA, na.rm = TRUE),
      meanInt = mean(.data$int_VDBA, na.rm = TRUE),
      minInt  = min(.data$int_VDBA, na.rm = TRUE),
      maxInt  = max(.data$int_VDBA, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_overall <- seconds %>%
    group_by(ID) %>%
    summarise(
      meanVDBA = mean(.data$seconds_VDBA, na.rm = TRUE),
      minVDBA  = min(.data$seconds_VDBA, na.rm = TRUE),
      maxVDBA  = max(.data$seconds_VDBA, na.rm = TRUE),
      meanInt = mean(.data$int_VDBA, na.rm = TRUE),
      minInt  = min(.data$int_VDBA, na.rm = TRUE),
      maxInt  = max(.data$int_VDBA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(threshold = "all")
  
  # Return combined
  return(list(summary = bind_rows(summary_state, summary_overall),
              accel = seconds))
}
