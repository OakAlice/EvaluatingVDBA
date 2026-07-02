# Analysis of Jasmin Annett's Wallaby data --------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Annett_Wallaby"]
stride_window <- 2 # dataset_variables$StrideWindow[dataset_variables$Name == "Annett_Wallaby"]
list_locomotion_labels <- c("Locomotion")

# Prepare data ------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Annett_Wallaby/raw"), full.names = TRUE)
dat <- lapply(files, function(x){
  raw <- fread(x)[, c(1:4)]
  colnames(raw) <- c("Time", "X", "Y", "Z")
  raw$ID <- tools::file_path_sans_ext(basename(x))
  
  # crop the first hour off
  crop_off <- 50*60*60
  raw <- raw[crop_off:nrow(raw), ]
  
  # vedba threshold for detecting hopping # experimented with threshold at bottom
  raw <- get_vedba(raw, freq)
  raw$Activity <- ifelse(raw$vedba > 1, "Locomotion", "Other")
  
  raw
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
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Annett_Wallaby", "Mass_of_Individuals.csv")) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/Annett_WallabyAccelerometer_summary_stats.csv"))





# Finding threshold -------------------------------------------------------
# 
# plot <- dat[1:100000,]
# ggplot(plot, aes(x = Time)) +
#     geom_path(aes(y = X, colour = "X")) +
#     geom_path(aes(y = Y, colour = "Y")) +
#     geom_path(aes(y = Z, colour = "Z")) +
#     geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
#     geom_hline(yintercept = 0.5, linetype="dashed", color = "red") +
#     my_theme()
