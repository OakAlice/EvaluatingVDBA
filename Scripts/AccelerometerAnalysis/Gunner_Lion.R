# Analysis of the Gunner_Lion dataset -------------------------------------

freq <- dataset_variables$Frequency[dataset_variables$Name == "Gunner_Lion"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Gunner_Lion"]
list_locomotion_labels <- c("Displacement")

# Set up ------------------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Gunner_Lion/raw"), full.names = TRUE)
data <- lapply(files, function(x){
  fread(x) %>%
    rename(X = Acc_x, Y = Acc_y, Z = Acc_z) %>%
    mutate(Time = paste(Date, `Time hh:mm:ss.ddd`),
           ID = "L1") %>%
    select(ID, Time, X, Y, Z)
})
data <- rbindlist(data)

# Get Vedba ---------------------------------------------------------------
data <- get_vedba(data, freq)

# Get locomotion ----------------------------------------------------------
# had to threshold this

# plot <- data[14000:14500,]
# ggplot(plot, aes(x = seq(1:nrow(plot)))) +
#     geom_path(aes(y = X, colour = "X")) +
#     geom_path(aes(y = Y, colour = "Y")) +
#     geom_path(aes(y = Z, colour = "Z")) +
#     geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
#     geom_hline(yintercept = 1, linetype="dashed", color = "red") +
#     my_theme()

data$Activity <- ifelse(data$vedba > 1, "Locomotion", "Other")
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

fwrite(summ_stats, file.path(base_path, "Output/Gunner_LionAccelerometer_summary_stats.csv"))
