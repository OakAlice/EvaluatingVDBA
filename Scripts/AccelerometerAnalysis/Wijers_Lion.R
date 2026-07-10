# Extracting vdba information from the wijers lion ------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Wijers_Lion"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Wijers_Lion"]
list_locomotion_labels <- c("walk", "fast")

# Set up ------------------------------------------------------------------
data <- fread(file.path(base_path, "Data/Accelerometer/Wijers_Lion/raw/SHUMBA_RAW_ACC.csv")) %>%
  rename(X = ACC_rawX, Y = ACC_rawY, Z = ACC_rawZ,
         Time = ACC_UTC,
         ID = lion,
         Activity = behaviour) %>%
  select(ID, Time, X, Y, Z, Activity)
# rearrange so its a long dataframe 
parse_vec <- function(x) as.numeric(strsplit(gsub("\\[|\\]", "", x), ",\\s*")[[1]])
data <- data[, {
  x <- parse_vec(X)
  y <- parse_vec(Y)
  z <- parse_vec(Z)
  n <- length(x)
  .(Time = Time + seq_len(n) / 100,   # arbitraryu decomalisation so stays in thre right order
    X = x, Y = y, Z = z,
    Activity = Activity)
}, by = .(ID, Time)]
# now convert these numbers to Gs
data <- data[, c(1:2, 4:7)]
data <- data %>%
  mutate(X = X/9.8, Y = Y/9.8, Z = Z/9.8)

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
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Wijers_Lion"]
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

fwrite(summ_stats, file.path(base_path, "Output/Wijers_LionAccelerometer_summary_stats.csv"))

