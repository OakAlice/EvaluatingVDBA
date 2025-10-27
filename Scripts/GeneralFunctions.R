
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