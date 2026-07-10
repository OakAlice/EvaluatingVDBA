# Christensen Baboon data -------------------------------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Christensen_Baboon"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Christensen_Baboon"]
list_locomotion_labels <- c("TRA", "RUN")

# Set up ------------------------------------------------------------------
data <- fread(file.path(base_path, "Data/Accelerometer/Christensen_Baboon/raw/rsos221103_si_005.txt")) %>%
  rename(X = stX, Y = stY, Z = stZ,
         Activity = Category) %>%
  mutate(Time = row_number()) %>%
  select(ID, Time, X, Y, Z, Activity)

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
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Christensen_Baboon"]
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

fwrite(summ_stats, file.path(base_path, "Output/Christensen_BaboonAccelerometer_summary_stats.csv"))
