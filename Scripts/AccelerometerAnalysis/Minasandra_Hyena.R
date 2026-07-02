# Analysis for Minasandra Hyena -------------------------------------------
# Data has been labelled in the publication
# Firstly there is the challenge of constructing the data to the format I need it in

freq <- dataset_variables$Frequency[dataset_variables$Name == "Minasandra_Hyena"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Minasandra_Hyena"]
list_locomotion_labels <- c("WALK", "LOPE")

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
  label_file <- grep(ID_name, label_files, value = T)
  labels <- fread(label_file)
  colnames(labels) <- c("StartTime", "Duration", "Activity")
  labels$rows <- labels$Duration * freq
  labels$EndTime <- labels$StartTime + labels$rows - 1
  
  # Join together -----------------------------------------------------------
  setDT(data)
  setDT(labels)
  
  data_labelled <- labels[
    data,
    on = .(StartTime <= Time, EndTime >= Time),
    .(
      Time,
      X, Y, Z,
      ID,
      Activity
    )
  ]
  
  # Note. I think there might be some duplication such that multiple behaviours 
  # can be coded simultaneoulsy
  # That would be necessary to deal with normally but fine for this specific analsis
  # so I didn't deal with it
  
  # remove the empty ones
  data_labelled <- na.omit(data_labelled)
  
  data_labelled
})
data <- rbindlist(dfs)
# save this
fwrite(data, file.path(base_path, "Data/Accelerometer/Minasandra_Hyena/Minasandra_Hyena_formatted.csv"))


ggplot(data, aes(x = seq(1:nrow(data)), y = X)) + 
  geom_path()



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

fwrite(summ_stats, file.path(base_path, "Output/Smit_CatAccelerometer_summary_stats.csv"))


