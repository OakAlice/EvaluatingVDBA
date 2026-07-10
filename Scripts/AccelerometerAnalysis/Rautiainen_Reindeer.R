# Analysis for the reindeer data ------------------------------------------
# comes with annotations

freq <- dataset_variables$Frequency[dataset_variables$Name == "Rautiainen_Reindeer"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Rautiainen_Reindeer"]
list_locomotion_labels <- c("Locomotion")

# Set up data -------------------------------------------------------------
data <- fread(file.path(base_path, "Data/Accelerometer/Rautiainen_Reindeer/raw/acceleration.csv"))
data <- data %>%
      mutate(Time = as.POSIXct(Timestamp, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")) %>%
      rename(ID = TagID) %>%
      select(ID, Time, X, Y, Z)

# figuring out how to get the annotations on the data was annoying
# I initially assumed that Observation.date was the start time and the Start were seconds from then
# However, in that case, the annotations start after the data ends.
# If the seconds is seconds since beginning... then it is 10 hours after whenever the original start was...
  # annotations <- fread(file.path(base_path, "Data/Accelerometer/Rautiainen_Reindeer/raw/annotations.csv")) %>%
  #   mutate(Start = as.POSIXct("2020-02-27 23:11:13", tz = "UTC") + Start..s.,
  #          Stop = Observation.date + Stop..s.) %>%
  #   rename(Activity = Behavioral.category) %>%
  #   select(Activity, Start, Stop)

# ultimately I concluded that these data wasn't necessarily even meant to match up
# annotations cover 9 hours whereas the data is only from 2 hours

# try using a threshold instead
data <- get_vedba(data, freq)
data$Activity <- ifelse(data$vedba > 0.5, "Locomotion", "Other")

# Get locomotion ----------------------------------------------------------
data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- data %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Rautiainen_Reindeer"]
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

fwrite(summ_stats, file.path(base_path, "Output/Rautiainen_ReindeerAccelerometer_summary_stats.csv"))
