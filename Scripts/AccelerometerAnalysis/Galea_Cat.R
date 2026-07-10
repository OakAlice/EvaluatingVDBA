# Analysis of cat data from predictions made Wilson et al., 2026 ----------
# Data too big to copy over, going to pull from where I have it saved

freq <- dataset_variables$Frequency[dataset_variables$Name == "Galea_Cat"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Galea_Cat"]
list_locomotion_labels <- c("Locomotion")

# Set up the data ---------------------------------------------------------
if (!file.exists(file.path(base_path, "Data/Accelerometer/Galea_Cat/Galea_Cat_processed.csv"))){
  predictions <- fread(file.path(base_path, "Data/Accelerometer/Galea_Cat/raw/Final_predictions.csv")) %>%
    select(ID, time, prediction) %>%
    rename(Time = time,
           Activity = prediction)
  
  raw_files <- list.files("C:/Users/PC/Documents/Catdata", full.names = TRUE, recursive = FALSE)[2:11]
  raw <- lapply(raw_files, function(x){
    cat <- str_split(basename(x), "_", simplify = T)[1]
    print(cat)
    
    dat <- fread(x)
    colnames(dat) <- c("Time", "X", "Y", "Z")
    dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
    
    # join them together
    setkey(dat, Time)
    preds <- predictions %>% dplyr::filter(ID == cat)
    setkey(preds, Time)
    
    dat <- preds[dat, on = "Time", roll = TRUE]
    
    dat
  })
  dat <- rbindlist(raw)
  
  fwrite(dat, file.path(base_path, "Data/Accelerometer/Galea_Cat/Galea_Cat_processed.csv"))
} else {
  dat <- fread(file.path(base_path, "Data/Accelerometer/Galea_Cat/Galea_Cat_processed.csv"))
}

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
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Galea_Cat"]
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

fwrite(summ_stats, file.path(base_path, "Output/Galea_CatAccelerometer_summary_stats.csv"))

