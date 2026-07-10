# Agarwal_WildDog analysis ------------------------------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Agarwal_WildDog"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Agarwal_WildDog"]
list_locomotion_labels <- c("Running")


# Set up ------------------------------------------------------------------
data <- fread(file.path(base_path, "Data/Accelerometer/Agarwal_WildDog/raw/matched_acceleration_data_out.csv")) %>%
  rename(X = acc_x, Y = acc_y, Z = acc_z, Activity = behavior)
ID <- fread(file.path(base_path, "Data/Accelerometer/Agarwal_WildDog/raw/matched_acceleration_metadata_out.csv"))
data$ID <- ID$`individual ID`

parse_vec <- function(x) as.numeric(strsplit(gsub("\\[|\\]", "", x), ",\\s*")[[1]])
X_list <- lapply(data$X, parse_vec)
Y_list <- lapply(data$Y, parse_vec)
Z_list <- lapply(data$Z, parse_vec)
n <- lengths(X_list)
melted <- data.table(
  Activity       = rep(data$Activity, n),
  behavior_start = rep(data$behavior_start, n),
  behavior_end   = rep(data$behavior_end, n),
  duration       = rep(data$duration, n),
  Source         = rep(data$Source, n),
  ID             = rep(data$ID, n),
  X = unlist(X_list),
  Y = unlist(Y_list),
  Z = unlist(Z_list)
)
melted[, sample_idx := sequence(n) - 1L]
melted[, Time := behavior_start + sample_idx / 16]

melted <- melted %>% select(Activity, ID, Time, X, Y, Z)

# Get Vedba ---------------------------------------------------------------
melted <- get_vedba(melted, freq)

# Get locomotion ----------------------------------------------------------
melted <- get_locomotion(melted, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- melted %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Agarwal_WildDog"]
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

fwrite(summ_stats, file.path(base_path, "Output/Agarwal_WildDogAccelerometer_summary_stats.csv"))

