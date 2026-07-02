

# Get vedba ---------------------------------------------------------------
get_vedba <- function(dat, freq){
  # VDBA window
  win <- 1 * freq 
  
  # calculate the static accelerations
  ax_static <- frollmean(dat$X, n = win, align = "center", fill = NA)
  ay_static <- frollmean(dat$Y, n = win, align = "center", fill = NA)
  az_static <- frollmean(dat$Z, n = win, align = "center", fill = NA)
  
  # get the dynamic component 
  ax_dynamic <- dat$X - ax_static
  ay_dynamic <- dat$Y - ay_static
  az_dynamic <- dat$Z - az_static
  
  # generate and return
  dat$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
  dat
}


# Mode --------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Get just the locomotion data --------------------------------------------
get_locomotion <- function(dat, freq, stride_window, list_locomotion_labels){
  win <- stride_window * freq 
  
  # make a per individual window id
  dat[, wind_id := (seq_len(.N) - 1) %/% win, by = ID]
  
  # get the most common behaviour per window 
  dat_win <- dat[, .(Behaviour = get_mode(Activity)), by = .(ID, wind_id)]
  
  # Keep only locomotion windows
  dat_win <- dat_win[Behaviour %in% list_locomotion_labels, ]
  
  # Filter original data
  dat <- dat[dat_win, on = .(ID, wind_id), nomatch = 0]
  
  dat
}
