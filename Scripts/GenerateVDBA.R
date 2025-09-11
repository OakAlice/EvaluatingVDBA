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
summarise_burst_VDBA <- function(data){
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

if (!"ID" %in% colnames(data)){
  data$ID <- rep("Unsure", nrow(data))
}

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
