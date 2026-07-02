# Smit Cat ----------------------------------------------------------------
# labelled training data
freq <- dataset_variables$Frequency[dataset_variables$Name == "Smit_Cat"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Smit_Cat"]
list_locomotion_labels <- c("Locomotion")

# Prepare data ------------------------------------------------------------
# AAlready formatted in the DataReformatting github
dat <- fread(file.path(base_path, "Data/Accelerometer/Smit_Cat/Smit_Cat_formatted.csv"))[, c(1:5, 7)] %>%
  rename(Activity = FuncActivity)

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
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Smit_Cat", "Mass_of_Individuals.csv")) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/Smit_CatAccelerometer_summary_stats.csv"))

