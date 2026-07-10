# Clemente_Imapal ---------------------------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Clemente_Impala"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Clemente_Impala"]
list_locomotion_labels <- c("Locomotion")


# Prepare data ------------------------------------------------------------
if(!file.exists(file.path(base_path, "Data/AccelerometerData/Clemente_Impala/formatted.csv"))){

  bw_cutoff = 5
  bw_order = 4
  bf <- butter(bw_order, bw_cutoff/(freq/2), type = "low")
  
  deployment_dates <- fread(file.path(base_path, "Data/Accelerometer/Clemente_Impala/Metadata.csv")) %>%
    select(CollarNumber, DeploymentStart, DeploymentEnd) %>%
    mutate(Start = as.Date(as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "UTC")),
           End = as.Date(as.POSIXct(DeploymentEnd, format = "%d.%m.%y %H:%M:%S", tz = "UTC")))
  
  # loop through the individuals
  files <- list.files(file.path(base_path, "Data/Accelerometer/Clemente_Impala/raw"), full.names = TRUE)
  dat <- lapply(files, function(x){
    
    ID <- tools::file_path_sans_ext(basename(x))
    
    raw <- fread(x)[, c(1:4)]
    colnames(raw) <- c("Time", "X", "Y", "Z")
    raw$ID <- ID
    
    # crop off before deployment began
    # convert to normal time
    raw$Date <- as.Date(as.POSIXct((raw$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
    Start_day <- deployment_dates$Start[deployment_dates$CollarNumber == ID]
    End_day <- deployment_dates$End[deployment_dates$CollarNumber == ID]
    raw <- raw[Date > Start_day & Date < End_day, ]
    
    # clean with a butterworth
    raw$X <- filtfilt(bf, raw$X)
    raw$Y <- filtfilt(bf, raw$Y)
    raw$Z <- filtfilt(bf, raw$Z)
    
    # vedba threshold for detecting locomotion
    raw <- get_vedba(raw, freq)
    raw$Activity <- ifelse(raw$vedba > 1, "Locomotion", "Other")
    
    # plot <- raw[vedba > 0.5, ][1:1000,]
    # ggplot(plot, aes(x = seq(1:nrow(plot)))) +
    #     geom_path(aes(y = X, colour = "X")) +
    #     geom_path(aes(y = Y, colour = "Y")) +
    #     geom_path(aes(y = Z, colour = "Z")) +
    #     geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
    #     geom_hline(yintercept = 1, linetype="dashed", color = "red") +
    #     my_theme()
    
    # Get locomotion ----------------------------------------------------------
    raw <- get_locomotion(raw, freq, stride_window, list_locomotion_labels)
    
    raw
  })
  dat <- rbindlist(dat)
  
  # save this
  fwrite(dat, file.path(base_path, "Data/Accelerometer/Clemente_Impala/formatted.csv"))
} else {
  dat <- fread(file.path(base_path, "Data/Accelerometer/Clemente_Impala/formatted.csv"))
}
# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Clemente_Impala", "Mass_of_Individuals.csv"))
summary <- merge(summary, animal_mass %>% select(ID, LogMass))

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

fwrite(summ_stats, file.path(base_path, "Output/Clemente_ImpalaAccelerometer_summary_stats.csv"))


