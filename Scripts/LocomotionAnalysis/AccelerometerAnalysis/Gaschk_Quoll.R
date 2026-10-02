# Analysis of the quoll ---------------------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Gaschk_Quoll"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Gaschk_Quoll"]
list_locomotion_labels <- c("Locomotion")

# Set up the data ---------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Gaschk_Quoll/raw"), full.names = TRUE)
dat <- lapply(files, function(x){
  
  print(x)
  
  data <- fread(x)
  colnames(data) <- c("Time", "X", "Y", "Z")
  data$Time <- as.POSIXct((data$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  
  # Crop off the first few hours of data # determined by playing with the plot
  hour <- 50*60*60*2
  data <- data[hour:nrow(data),]
  print(nrow(data))
  
  # calculate vedba # threshold based on plots again
  data <- get_vedba(data, freq)
  data$Activity <- ifelse(data$vedba > 1, "Locomotion", "Other")
  
  data$ID <- str_split(basename(x), "_", simplify = T)[1]
    
  data
})
dat <- rbindlist(dat)

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Gaschk_Quoll", "Mass_of_Individuals.csv"))
summary <- merge(summary, animal_mass %>% select("ID", "LogMass"), by = "ID") 

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

fwrite(summ_stats, file.path(base_path, "Output/Gaschk_QuollAccelerometer_summary_stats.csv"))
