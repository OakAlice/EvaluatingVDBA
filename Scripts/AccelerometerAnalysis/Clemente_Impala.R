# Clemente Impala dataset with labels -------------------------------------
# have only generated labels for some individuals so far
# predictions were based on the time matched artemis but raw data is from the axivity (not perfectly matched)
# need to decide how to deal with this

freq <- dataset_variables$Frequency[dataset_variables$Name == "Clemente_Impala"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Clemente_Impala"]
list_locomotion_labels <- c("Locomotion_Fast", "Locomotion_Walk")

# using just the labelled data for now
data <- fread(file.path(base_path, "Data/Accelerometer/Clemente_Impala/CleanedlLabelledData.csv")) %>%
  rename(X = RawAX.cl, Y = RawAY.cl, Z = RawAZ.cl,
         Time = utc_datetime)

# Get Vedba ---------------------------------------------------------------
data <- get_vedba(data, freq)

# Get locomotion ----------------------------------------------------------
data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- data %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Clemente_Impala", "Mass_of_Individuals.csv")) %>% 
  select("ID", "LogMass")
summary <- merge(summary, animal_mass, by = "ID")

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

fwrite(summ_stats, file.path(base_path, "Output/Clemente_ImpalaAccelerometer_summary_stats.csv"))

