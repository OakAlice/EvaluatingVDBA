# Data for MoralesVargas_Cow, labellewd -----------------------------------
# I onl;y selected the walking data for download from the github
# 10 cows

# datasheet for the accelerometer: https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bno055-ds000.pdf
# conversion to Gs = /9.8

freq <- dataset_variables$Frequency[dataset_variables$Name == "MoralesVargas_Cow"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "MoralesVargas_Cow"]

# Data set up ------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/MoralesVargas_Cow/raw"), full.names = TRUE)
data <- lapply(files, function(x){
  df <- fread(x) %>%
    select(Time, BNO055_AX, BNO055_AY, BNO055_AZ)  %>%
    rename(X = BNO055_AX,
           Y = BNO055_AY,
           Z = BNO055_AZ) %>%
    mutate(X = X/9.8,
           Y = Y/9.8,
           Z = Z/9.8)
  df$ID <- str_split(basename(x), "_", simplify = TRUE)[3]
  df
})
data <- rbindlist(data)

# Get Vedba ---------------------------------------------------------------
data <- get_vedba(data, freq)

# Get locomotion ----------------------------------------------------------
# all of this data was locomotion data
data$Activity <- "Locomotion"
data <- get_locomotion(data, freq, stride_window, "Locomotion")

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- data %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "MoralesVargas_Cow"]
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

fwrite(summ_stats, file.path(base_path, "Output/MoralesVargas_CowAccelerometer_summary_stats.csv"))
