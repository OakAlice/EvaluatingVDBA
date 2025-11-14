# Cleaning the data -------------------------------------------------------
# visualised the data and then designed a cleaning protocol for each dataset
# e.g., clipping misread rections, removing obvious outliers, etc.

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
data <- na.omit(data)

# plot the data and save to disk
output_path <- file.path(base_path, "AccelerometerData", species, "original_data.png")
p <- ggplot(data, aes(x = seq_len(nrow(data)), y = Accel.X)) +
  geom_line()
ggsave(filename = output_path, plot = p, width = 15, height = 5, dpi = 300)










# 
# 
# # Define sampling frequency and cutoff frequency
# freq <- as.numeric(dataset_variables[Name == species]$Frequency) # Hz
# 
# Wn <- 0.05
# 
# # Design a 4th-order low-pass Butterworth filter
# filter_coeffs <- butter(n = 2, W = Wn, type = "low")
# 
# # Apply the filter using filtfilt for zero-phase filtering
# data$filtered_X <- filtfilt(filter_coeffs, data$Accel.X)
# data$filtered_Y <- filtfilt(filter_coeffs, data$Accel.Y)
# data$filtered_Z <- filtfilt(filter_coeffs, data$Accel.Z)
# 
# 
# ggplot(data, aes(x = seq(1:nrow(data)), y = Accel.X)) + geom_line()
# ggplot(data, aes(x = seq(1:nrow(data)), y = filtered_X)) + geom_line()
# 
# fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))
