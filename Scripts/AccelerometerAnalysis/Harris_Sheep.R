# Analysis of Harris Sheep dataset --------------------------------------

# labelled training data
freq <- dataset_variables$Frequency[dataset_variables$Name == "Harris_Sheep"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Harris_Sheep"]
list_locomotion_labels <- c("1")

# Prepare data ------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Harris_Sheep/raw"), full.names = TRUE)
data <- lapply(files, function(x){
  df <- fread(x)
  
  x_cols <- grep("^x_", names(df), value = TRUE)
  y_cols <- grep("^y_", names(df), value = TRUE)
  z_cols <- grep("^z_", names(df), value = TRUE)
  
  n_samples <- length(x_cols)
  
  df_long <- melt(
    df,
    id.vars = c("sheep_number", "time_stamp", "walking"),
    measure.vars = list(X = x_cols, Y = y_cols, Z = z_cols),
    variable.name = "sample_index",
    value.name = c("X", "Y", "Z")
  )
  
  # sample_index comes out as a factor (1, 2, 3...n_samples) — convert to integer
  df_long[, sample_index := as.integer(sample_index)]
  # Sort so it reads sensibly
  setorder(df_long, sheep_number, time_stamp, sample_index)
  
  # now format and rename
  df_long <- df_long %>% select(!sample_index)
  colnames(df_long) <- c("ID", "Time", "Activity", "X", "Y", "Z")
  df_long
})
data <- rbindlist(data)


# Get Vedba ---------------------------------------------------------------
data <- get_vedba(data, freq)

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(data, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Harris_Sheep", "Mass_of_Individuals.csv")) %>%
  pull(LogMass)
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

fwrite(summ_stats, file.path(base_path, "Output/Harris_SheepAccelerometer_summary_stats.csv"))

