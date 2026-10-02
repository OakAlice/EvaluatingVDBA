# Clemente Kudu data ------------------------------------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Clemente_Kudu"]
stride_window <-dataset_variables$StrideWindow[dataset_variables$Name == "Clemente_Kudu"]
list_locomotion_labels <- c("Locomotion")

# Prepare data ------------------------------------------------------------
file <- list.files(file.path(base_path, "Data/Accelerometer/Clemente_Kudu/raw"), full.names = TRUE)
raw <- fread(file)[, c(1:4)]
colnames(raw) <- c("Time", "X", "Y", "Z")
raw$ID <- tools::file_path_sans_ext(basename(file))
  
# crop the first 2 hours off
crop_off <- 50*60*60*2
raw <- raw[crop_off:nrow(raw), ]
  
# vedba threshold for detecting walking # used 1 because that seemed to woek in other species
raw <- get_vedba(raw, freq)
raw$Activity <- ifelse(raw$vedba > 1, "Locomotion", "Other")

# Get locomotion ----------------------------------------------------------
raw <- get_locomotion(raw, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- raw %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Clemente_Kudu"]
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

fwrite(summ_stats, file.path(base_path, "Output/Clemente_KuduAccelerometer_summary_stats.csv"))

