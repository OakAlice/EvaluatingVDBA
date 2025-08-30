# Generate Features -------------------------------------------------------

# Functions ---------------------------------------------------------------
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


# Code --------------------------------------------------------------------

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
available.axes <- intersect(selected.axes, colnames(data))

if (frequency_dictionary[[species]] > 5){
  window_sec = 1 # if the sampling rate is high, do in 1 second windows
} else {
  window_sec = 5 # if the sampling rate is low, do in 5 second windows
}

processed_data <- processVDBA(data, available.axes, window_length = window_sec, sample_rate = frequency_dictionary[[species]], overlap_percent = 0)

fwrite(processed_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))
