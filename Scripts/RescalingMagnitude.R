# Normalise scales --------------------------------------------------------
# Because this data has been collected from a number of different sources with different devices
# some of the devices are on radically different scales to each other
# Need to find a place in each of the datasets where the animal is still
# the only acceleration in this section will be due to gravity
# therefore, in the still section, acceleration = 1
# adjust the measurements to = 1

processed_data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))

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
      window <- ifelse(as.numeric(dataset_variables[Name == species]$Frequency)>5, 2, 5)
      window_samples <- window * as.numeric(dataset_variables[Name == species]$Frequency)
      
      processed_data <- process_cont_VDBA(dat, window_length = window_samples)
    }
    
    # save the rescaled data
    fwrite(processed_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed_rescaled.csv")))
  }
  
} else {
  print("there isn't a flat point in this analysis... :O")
}
