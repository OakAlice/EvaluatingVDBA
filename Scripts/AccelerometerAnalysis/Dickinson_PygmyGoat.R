# Analysis for Goat data from Dickinson et al. ----------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Dickinson_Goat"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Dickinson_Goat"]
list_locomotion_labels <- c("Walking", "Trotting", "Running")

# Prep data ---------------------------------------------------------------
dat <- fread(file.path(base_path, "Data/Accelerometer/Dickinson_Goat/raw/Goat10hz.csv")) %>%
  select(ID, Timestamp, Behaviour, Accx, Accy, Accz) %>%
  rename(X = Accx,
         Y = Accy,
         Z = Accz,
         Activity = Behaviour)

# Get Vedba ---------------------------------------------------------------
dat <- get_vedba(dat, freq)

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Dickinson_Goat", "Mass_of_Individuals.csv")) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/Dickinson_PygmyGoatAccelerometer_summary_stats.csv"))

