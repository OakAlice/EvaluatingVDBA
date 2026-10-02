# Analysis for Minasandra Hyena -------------------------------------------
# Data has been labelled in the publication
# Firstly there is the challenge of constructing the data to the format I need it in
# I was unable to do this to my satisfaction and therefore reverted to a threshold based analysis

freq <- dataset_variables$Frequency[dataset_variables$Name == "Minasandra_Hyena"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Minasandra_Hyena"]
list_locomotion_labels <- c("Locomotion")



# Prepare the data --------------------------------------------------------
if(!file.exists(file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/Minasandra_Hyena_formatted_locomotion.csv"))){
  
  bw_cutoff = 5
  bw_order = 4
  bf <- butter(bw_order, bw_cutoff/(freq/2), type = "low")

  # these are h5 files so require being unpacked before read
  raw_files <- list.files(file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/raw"), full.names = TRUE, pattern = "\\.h5$")
  label_files <- list.files(file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/raw"), full.names = TRUE, pattern = "txt")
  dfs <- lapply(raw_files, function(x){
    # List top-level groups/datasets
    # h5ls(x)
  
    # get the accel data out ------------------------------------------
    data <- h5read(x, "A")
    data <- as.data.table(data)
    colnames(data) <- c("X", "Y", "Z")
  
    # dont need the timestamps, just make a row number
    data$Time <- 1:nrow(data)
    
    # get the name of the individual
    ID_name <- gsub("_A_25Hz", "", tools::file_path_sans_ext(basename(x)))
    ID <- rep(ID_name, nrow(data))
  
    # stitch it all together and remove the NA rows
    data[, `:=`(Time = Time, ID = ID)]
    data <- na.omit(data)
  
    H5close() # close the file
    
    # get the labels --------------------------------------------------
    # v1 is timestamp (rows since the beginning), V2 is duration, V3 is the behaviour
    # label_file <- grep(ID_name, label_files, value = T)
    # labels <- fread(label_file)
    # colnames(labels) <- c("StartTime", "Duration", "Activity")
    # labels$rows <- labels$Duration * freq
    # labels$EndTime <- labels$StartTime + labels$rows - 1
    # labels <- labels %>% dplyr::filter(Activity %in% list_locomotion_labels)
    # 
    # # Join together -----------------------------------------------------------
    # setDT(data)
    # setDT(labels)
    # 
    # data_labelled <- labels[
    #   data,
    #   on = .(StartTime <= Time, EndTime >= Time),
    #   .(
    #     Time,
    #     X, Y, Z,
    #     ID,
    #     Activity
    #   )
    # ]
    # 
    # # remove the empty ones
    # data_labelled <- na.omit(data_labelled)
    # 
    # data_labelled
    
    # label getting didnt work... just use thresholding instead
    # clean with a butterworth
    # TODO: Improve this cleaning later
    data$X <- filtfilt(bf, data$X)
    data$Y <- filtfilt(bf, data$Y)
    data$Z <- filtfilt(bf, data$Z)
    
    # calculate the VDBA
    data <- get_vedba(data, freq)
    
    # define locomotion as above a certain threshold
    # plot <- data[88461229:88497229,]
    # ggplot(plot, aes(x = seq(1:nrow(plot)))) +
    #   geom_path(aes(y = X)) +
    #   geom_path(aes(y = Y)) +
    #   geom_path(aes(y = Z)) +
    #   geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
    #   geom_hline(yintercept = 1, linetype="dashed", color = "red")
    
    data$Activity <- ifelse(data$vedba > 1, "Locomotion", "other")
    
    # and now do the normal stuff (file too big to save all output)
    data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)
    
    # and remove the non locomotion ones
    data <- data[Activity == list_locomotion_labels, ]
    
    data
  })
  data <- rbindlist(dfs)
  # save this
  fwrite(data, file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/Minasandra_Hyena_formatted_locomotion.csv"))

} else {
  data <- fread(file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/Minasandra_Hyena_formatted_locomotion.csv"))
}

# Continue
# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- data %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Minasandra_Hyena"]
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

fwrite(summ_stats, file.path(base_path, "Output/Minasandra_HyenaAccelerometer_summary_stats.csv"))
