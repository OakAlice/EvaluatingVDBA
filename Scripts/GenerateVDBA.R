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
  window <- ifelse(dataset_variables[Name == species]$Frequency>5, 2, 5)
  window_samples <- window * dataset_variables[Name == species]$Frequency
  
  processed_data <- process_cont_VDBA(data, window_length = window_samples)
  # summarised_data <- summarise_cont_VDBA(data = processed_data, window_samples)
}

fwrite(processed_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))

# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1

# find data rows where the vedba is at the minimum
flats <- processed_data[which(vedba == min(processed_data$vedba, na.rm = TRUE)),]
# if it's not exactly 0, then I need to find whether it's close to 0
# with "close to 0" differing depending on the scale
zero <- ifelse(min(processed_data$vedba, na.rm= TRUE)[1]/max(processed_data$vedba, na.rm = TRUE)[1] < 0.1 | min(processed_data$vedba, na.rm = TRUE) == 0, TRUE, FALSE)
ifelse(zero == FALSE, print("there isn't a flat spot to calibrate from"), print("calibrating"))

if (zero){
  # in these flat spots, the static accel should == 1
  # calculate the static accel
  flats[, static_vedba := sqrt(ax_static^2 + ay_static^2 + az_static^2)]
  
  # see whether it is close to one
  if(0.50 < mean(flats$static_vedba, na.rm = TRUE) & mean(flats$static_vedba, na.rm = TRUE) < 1.5){
    # if its within 50% tolerance of 1, then it's fine
    print("resting static already close to one - no need to adjust scale")
    
  } else {
    
    print(mean(flats$static_vedba, na.rm = TRUE))
    print("not close to 1: adjusting scale")
    
    # if the mean static accel when gravity = 1 is
    flat_static <- mean(flats$static_vedba, na.rm = TRUE)
    # then the "scalar" I need to apply is that value
    
    # apply that to each of my axes and recalculate the vedba
    processed_data$Accel.X <- processed_data$Accel.X / flat_static
    processed_data$Accel.Y <- processed_data$Accel.Y / flat_static
    processed_data$Accel.Z <- processed_data$Accel.Z / flat_static
    
    # reset the daat.table
    keep_cols <- intersect(
      c("ID", "Accel.X", "Accel.Y", "Accel.Z", "Time", "Event.ID", "burst_id"),
      names(processed_data))
    processed_data <- processed_data[, ..keep_cols]
    
    # is this burst or continuous data, and if busts, label the bursts
    dat <- detect_bursts(processed_data, gap_threshold = 1)
    
    if (length(unique(dat$burst_id))>1){
      # process in bursts
      processed_data <- process_burst_VDBA(dat)
    } else {
      window <- ifelse(dataset_variables[Name == species]$Frequency>5, 2, 5)
      window_samples <- window * dataset_variables[Name == species]$Frequency
      
      processed_data <- process_cont_VDBA(dat, window_length = window_samples)
    }
    
    # save the rescaled data
    fwrite(processed_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed_rescaled.csv")))
  }
  
} else {
  print("there isn't a flat point in this analysis... :O")
}
