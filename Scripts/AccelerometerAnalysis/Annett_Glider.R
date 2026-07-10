# Annett Glider labelled data ---------------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Annett_Glider"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Annett_Glider"]
list_locomotion_labels <- c(11, 12, 13)

# Reading in and relabeling data -----------------------------------------
raw_files <- list.files(
    path = file.path(base_path, "Data/Accelerometer/Annett_Glider/raw"),
    recursive = TRUE,
    pattern = "\\.txt$",
    full.names = TRUE)
  
alldata <- lapply(raw_files, function(x){
  data <- fread(x)
  ind <- ifelse(grepl("Flip", x), "Flip", "Gilberta") # Take ID names from files
  data <- data %>% 
    # Generating a generic time stamp for the start time
    mutate(
      time=as.POSIXct((V1 - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
  
  data <- data %>% 
    select(V2, V3, V4, V5, time) %>% 
    rename(X = V2,
           Y = V3,
           Z = V4,
           Activity = V5,
           Time = time) %>%
    mutate(ID = ind) 
  
  data <- dplyr::filter(data, Activity != 0) 
  data
})
data <- rbindlist(alldata)

# Get Vedba ---------------------------------------------------------------
dat <- get_vedba(data, freq)

# figure out which behaviour is locomotion
# samp_data <- dat %>%
#   dplyr::filter(Activity %in% c(3, 6, 7, 8, 11, 12, 13)) %>%
#   group_by(Activity) %>%
#   mutate(idx = dplyr::row_number()) %>%
#   slice(1:500) %>%
#   ungroup()
# 
# ggplot(samp_data, aes(x = idx)) +
#   geom_path(aes(y = X, colour = "X")) +
#   geom_path(aes(y = Y, colour = "Y")) +
#   geom_path(aes(y = Z, colour = "Z")) +
#   geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   my_theme() +
#   facet_wrap(~Activity, scales = "free_x")

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Annett_Glider"]
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

fwrite(summ_stats, file.path(base_path, "Output/Annett_GliderAccelerometer_summary_stats.csv"))






