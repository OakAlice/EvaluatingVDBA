# Analysis of the Buchmann Datasets ---------------------------------------
# many of them, all same format
# these datasets were collected exclusively during lcoomotion periods

species <- list.dirs(file.path(base_path, "Data/Accelerometer/Buchmann_Ungulates/"), recursive = FALSE)

# loop to do everything
for (sp in species){
  # was formatted previously
  data <- fread(file.path(sp, paste0(basename(sp), "_reformatted.csv")))
  colnames(data) <- c("ID", "Time", "X", "Y", "Z")
  
  # get the variables
  freq <- dataset_variables$Frequency[dataset_variables$Name == basename(sp)]
  stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == basename(sp)]
  list_locomotion_labels <- c("Locomotion")
  
  data <- get_vedba(data, freq)
  
  # check what is locomotion data
  # ggplot(data, aes(x = seq(1:nrow(data)))) +
  #       geom_path(aes(y = X, colour = "X")) +
  #       geom_path(aes(y = Y, colour = "Y")) +
  #       geom_path(aes(y = Z, colour = "Z")) +
  #       geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
  #       geom_hline(yintercept = 1, linetype="dashed", color = "red")
  
  data$Activity <- ifelse(data$vedba > 1, "Locomotion", "Other")
  data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)
  
  # Summarise ---------------------------------------------------------------
  # now take a mean across each window
  summary <- data %>%
    group_by(ID, wind_id) %>%
    summarise(mean_vedba = mean(vedba, na.rm = TRUE),
              max_vedba = max(vedba, na.rm = TRUE))
  
  # Add animal mass ---------------------------------------------------------
  animal_mass <- dataset_variables$LogMass[dataset_variables$Name == basename(sp)]
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
  
  fwrite(summ_stats, file.path(base_path, paste0("Output/", basename(sp), "Accelerometer_summary_stats.csv")))
  
}

