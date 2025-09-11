# Generate Features -------------------------------------------------------

# Functions ---------------------------------------------------------------

process_burst_VDBA <- function(data){
  # if the data is collected in bursts, then take the average VDBA for that burst as the static
  # look, it's not great, but its something
    
  # compute within each burst
  burst_means <- data[, .(
    ax_static = mean(Accel.X, na.rm = TRUE),
    ay_static = mean(Accel.Y, na.rm = TRUE),
    az_static = mean(Accel.Z, na.rm = TRUE)
  ), by = .(ID, burst_id)]
      
  # add that back in
  data <- merge(
    data, burst_means, by = c("ID", "burst_id"), all.x = TRUE
  )
      
  data[, `:=`(
    ax_dynamic = Accel.X - ax_static,
    ay_dynamic = Accel.Y - ay_static,
    az_dynamic = Accel.Z - az_static
  )]
      
  # calculate the dynamic VDBA for each point
  data[, vedba := sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)]
  data[, odba := abs(ax_dynamic) + abs(ay_dynamic) + abs(az_dynamic)]
  
  return(data)
}

# calculate when the data is continuous
process_cont_VDBA <- function(data, window_length){
  # in this case, we can calculate static acceleration in the more traditional way
  # rolling mean instead of a direct average

  # rolling means (static acceleration)
  data[, ax_static := frollmean(Accel.X, n = window_length, align = "center", fill = NA)]
  data[, ay_static := frollmean(Accel.Y, n = window_length, align = "center", fill = NA)]
  data[, az_static := frollmean(Accel.Z, n = window_length, align = "center", fill = NA)]
    
  # dynamic acceleration
  data[, ax_dynamic := Accel.X - ax_static]
  data[, ay_dynamic := Accel.Y - ay_static]
  data[, az_dynamic := Accel.Z - az_static]
    
  # VDBA
  data[, vedba := sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)]
  data[, odba := abs(ax_dynamic) + abs(ay_dynamic) + abs(az_dynamic)]

  return(data)
}

# detect the bursts based on time gaps
detect_bursts <- function(data, gap_threshold = 1) {
  setDT(data)
  
  data[, burst_id := cumsum(c(0, diff(Time)) > gap_threshold) + 1, by = ID]
  
  return(data)
}

# summarise into windows of VDBA
summarise_cont_VDBA <- function(data, window){
  # chunk the data into window number of samples
  # calculate the mean, min, and max for vedba and odba columns
  data[, window_id := ((seq_len(.N) - 1) %/% window) + 1]
  
  # calculate within each of these wuindows
  summary <- data[, .(
    Time = first(Time),    # first timestamp in the window
    ID = first(ID),
    vedba_mean = mean(vedba, na.rm = TRUE),
    vedba_min  = min(vedba, na.rm = TRUE),
    vedba_max  = max(vedba, na.rm = TRUE),
    odba_mean  = mean(odba, na.rm = TRUE),
    odba_min   = min(odba, na.rm = TRUE),
    odba_max   = max(odba, na.rm = TRUE)
  ), by = .(ID, window_id)]
  
  # clean up the NA head and tail
  summary <- na.omit(summary)
  
  return(summary)
}

# and for the bursts too
summarise_cont_VDBA <- function(data){
  # calculate the mean, min, and max for vedba and odba columns
  # calculate within each of the bursts
  summary <- data[, .(
    Time = first(Time),    # first timestamp in the window
    ID = first(ID),
    vedba_mean = mean(vedba, na.rm = TRUE),
    vedba_min  = min(vedba, na.rm = TRUE),
    vedba_max  = max(vedba, na.rm = TRUE),
    odba_mean  = mean(odba, na.rm = TRUE),
    odba_min   = min(odba, na.rm = TRUE),
    odba_max   = max(odba, na.rm = TRUE)
  ), by = .(ID, burst_id)]
  
  # clean up the NA head and tail
  summary <- na.omit(summary)
  
  return(summary)
}

# Code --------------------------------------------------------------------

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
available.axes <- intersect(selected.axes, colnames(data))

# convert the time
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d %H:%M:%S")

# is this burst or continuous data, and if busts, label the bursts
data <- detect_bursts(data, gap_threshold = 1)

if (length(unique(data$burst_id))>1){
  # process in bursts
  processed_data <- process_burst_VDBA(data)
  summarised_data <- summarise_burst_VDBA(data = processed_data)
} else {
  window <- ifelse(frequency_dictionary[[species]]>5, 2, 5)
  window_samples <- window * frequency_dictionary[[species]]
  
  processed_data <- process_cont_VDBA(data, window_length = window_samples)
  summarised_data <- summarise_cont_VDBA(data = processed_data, window_samples)
}

# save it
fwrite(processed_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))
fwrite(summarised_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_summarised.csv")))




# Old method --------------------------------------------------------------
# just saving this for later if I end up needing or using it
processVDBA <- function(data, available.axes, window_length, sample_rate, overlap_percent) {
  
  # Calculate window length and overlap
  samples_per_window <- window_length * sample_rate
  overlap_samples <- if (overlap_percent > 0) ((overlap_percent / 100) * samples_per_window) else 0
  num_windows <- ceiling((nrow(data) - overlap_samples) / (samples_per_window - overlap_samples))
  
  # Function to process each window for this specific ID
  process_window <- function(i) {
    start_index <- max(1, round((i - 1) * (samples_per_window - overlap_samples) + 1))
    end_index <- min(start_index + samples_per_window - 1, nrow(data))
    window_chunk <- data[start_index:end_index, ]
    
    # Initialise output features
    window_info <- tibble(Time = NA, ID = NA, Activity = NA)
    statistical_features <- tibble() 
    
    statistical_features <- data.table()
    
    window_chunk <- setDT(window_chunk)
    
    # calculate SMA, ODBA, and VDBA
    statistical_features[, SMA := sum(rowSums(abs(window_chunk[, available.axes, with = FALSE]))) / nrow(window_chunk)]
    ODBA <- rowSums(abs(window_chunk[, available.axes, with = FALSE]))
    statistical_features[, `:=`(
      minODBA = min(ODBA, na.rm = TRUE),
      maxODBA = max(ODBA, na.rm = TRUE)
    )]
    VDBA <- sqrt(rowSums(window_chunk[, available.axes, with = FALSE]^2))
    statistical_features[, `:=`(
      minVDBA = min(VDBA, na.rm = TRUE),
      maxVDBA = max(VDBA, na.rm = TRUE)
    )]
    
    if (nrow(window_chunk) > 0) {
      window_info <- window_chunk %>% 
        summarise(
          Time = first(Time),
          ID = if ("ID" %in% colnames(data)) {
            first(data$ID)} else {NA},
          Activity = if ("Activity" %in% names(.)) {
            as.character(names(sort(table(Activity), decreasing = TRUE))[1])
          } else {
            NA
          }
        ) %>% 
        ungroup()
    }
    
    # Ensure that blank inputs are handled by replacing them with placeholders
    window_info <- if (is.null(window_info) || nrow(window_info) == 0) data.frame(matrix(NA, nrow = 1, ncol = 0)) else window_info
    statistical_features <- if (is.null(statistical_features) || nrow(statistical_features) == 0) data.frame(matrix(NA, nrow = 1, ncol = 0)) else statistical_features
    
    # Combine the data frames
    combined_features <- cbind(window_info, statistical_features) %>%
      mutate(across(everything(), ~replace_na(., NA)))  # Ensure all columns are present
    
    return(combined_features)
  }
  
  # Use lapply to process each window for the current ID
  plan(multisession) # parallel processing
  window_features_list <- lapply(1:num_windows, process_window)
  plan(sequential)
  
  # Combine all the windows for this ID into a single data frame
  features <- bind_rows(window_features_list)
  
  return(features)
}

