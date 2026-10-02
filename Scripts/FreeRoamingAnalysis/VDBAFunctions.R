# vdba generation ---------------------------------------------------------
generate_vdba <- function(accel, freq, window_seconds){
  
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
  
  accel$vedba <- vedba
  
  return(accel)
}

# smooth it out based on a rolling window
smooth_vdba <- function(accel, freq, window = 5) {
  
  win <- window * freq
  
  # smooth VeDBA using rolling mean
  accel[, smooth_vdba := frollmean(vedba, n = win, align = "center", fill = NA)]
  # accel<- accel %>% select(ID, Time, smooth_vdba) %>% na.omit()
  
  return(accel)
}

summarise_vdba <- function(accel, freq, window_seconds) {

  # firstly find the natural log of each "record" # note base e is the default in R
  accel$nlog_vdba <- log(accel$smooth_vdba)
  
  # now take a mean per individual
  summary <- accel %>%
    group_by(ID) %>%
    summarise(
      meanVDBA = mean(nlog_vdba, na.rm = TRUE),
      sdVDBA  = sd(nlog_vdba, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary)
}
