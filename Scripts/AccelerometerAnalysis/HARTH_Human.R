# Human -------------------------------------------------------------------
# labelled data downloaded from https://github.com/ntnu-ai-lab/harth-ml-experiments/tree/main/adult_walking_speed
# used the lower back accelerometer as closer to the COM

freq <- dataset_variables$Frequency[dataset_variables$Name == "HARTH_Human"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "HARTH_Human"]
list_locomotion_labels <- c("2", "101", "102", "103")

# Set up data -------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/HARTH_Human/raw"), full.names = TRUE)
dat <- lapply(files, function(x){
  dat <- fread(x)
  dat <- dat[,c(1:4,8)]
  colnames(dat) <- c("Time", "X", "Y", "Z", "Activity")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
})
dat <- rbindlist(dat)

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
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/HARTH_Human", "Mass_of_Individuals.csv")) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/HARTH_HumanAccelerometer_summary_stats.csv"))


