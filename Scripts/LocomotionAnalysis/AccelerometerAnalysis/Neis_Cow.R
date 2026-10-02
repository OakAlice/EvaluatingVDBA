# Neis Cow ----------------------------------------------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Neis_Cow"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Neis_Cow"]
list_locomotion_labels <- c("Locomotion")

# Prepare data ------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Neis_Cow/raw"), full.names = TRUE, recursive = TRUE)
data <- lapply(files, function(x){
  df <- fread(x) %>%
    rename(Time = timestamp,
           X = accel_x_mps2,
           Y = accel_y_mps2,
           Z = accel_z_mps2) %>%
    select(Time, X, Y, Z) %>%
    mutate(ID = basename(x))

  # convery from mps2 to Gs
  df[, `:=`(
    X = X / 9.80665,
    Y = Y / 9.80665,
    Z = Z / 9.80665
  )]

  df
})
data <- rbindlist(data)

# vedba threshold for detecting walking ----------------------
data <- get_vedba(data, freq)
data$Activity <- ifelse(data$vedba > 1, "Locomotion", "Other")

# Get locomotion ----------------------------------------------------------
data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- data %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Neis_Cow"]
summary$LogMass <- animal_mass

# Final summarisation -----------------------------------------------------
summ_stats <- summary %>%
  group_by(ID, LogMass) %>%
  summarise(
    mean_vedba_raw = mean(mean_vedba, na.rm = TRUE),
    sd_vedba_raw = sd(mean_vedba, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_vedba_raw = sd_vedba_raw / sqrt(n),
    logmean = log10(mean_vedba_raw),
    log_upper = log10(mean_vedba_raw + se_vedba_raw),
    log_lower = log10(mean_vedba_raw - se_vedba_raw)
  ) %>%
  mutate(ID = as.character(ID))

fwrite(summ_stats, file.path(base_path, "Output/Neis_CowAccelerometer_summary_stats.csv"))





