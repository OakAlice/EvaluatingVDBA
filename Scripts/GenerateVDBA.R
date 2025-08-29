# Generate Features -------------------------------------------------------

# Functions ---------------------------------------------------------------
processDataPerID <- function(id_raw_data, window_length, sample_rate, overlap_percent) {
  
  # Calculate window length and overlap
  samples_per_window <- window_length * sample_rate
  overlap_samples <- if (overlap_percent > 0) ((overlap_percent / 100) * samples_per_window) else 0
  num_windows <- ceiling((nrow(id_raw_data) - overlap_samples) / (samples_per_window - overlap_samples))
  
  # Function to process each window for this specific ID
  process_window <- function(i) {
    print(i)
    start_index <- max(1, round((i - 1) * (samples_per_window - overlap_samples) + 1))
    end_index <- min(start_index + samples_per_window - 1, nrow(id_raw_data))
    window_chunk <- id_raw_data[start_index:end_index, ]
    
    # Initialise output features
    window_info <- tibble(Time = NA, ID = NA, Activity = NA)
    statistical_features <- tibble() 
    
    statistical_features <- data.table()
    
    window_chunk <- setDT(window_chunk)
    
    # calculate SMA, ODBA, and VDBA
    statistical_features[, SMA := sum(rowSums(abs(window_chunk[, available_axes, with = FALSE]))) / nrow(window_chunk)]
    ODBA <- rowSums(abs(window_chunk[, available_axes, with = FALSE]))
    statistical_features[, `:=`(
      minODBA = min(ODBA, na.rm = TRUE),
      maxODBA = max(ODBA, na.rm = TRUE)
    )]
    VDBA <- sqrt(rowSums(window_chunk[, available_axes, with = FALSE]^2))
    statistical_features[, `:=`(
      minVDBA = min(VDBA, na.rm = TRUE),
      maxVDBA = max(VDBA, na.rm = TRUE)
    )]
    
    if (nrow(window_chunk) > 0) {
      window_info <- window_chunk %>% 
        summarise(
          Time = first(Time),
          ID = first(ID),
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
available.axes <- # list if it has any of Accel.X, Acce.

data <- data[1:1000, ]
