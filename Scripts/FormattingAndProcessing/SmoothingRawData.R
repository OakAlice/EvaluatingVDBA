# Smoothing the outliers --------------------------------------------------
# Applying the butterworth filter to the data to remove outliers and noise

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

data <- na.omit(data)
# Define sampling frequency and cutoff frequency
freq <- as.numeric(dataset_variables[Name == species]$Frequency) # Hz

cutoff_freq <- 0.5
# Calculate normalised cutoff frequency
Wn <- cutoff_freq / (freq / 2)

# Design a 4th-order low-pass Butterworth filter
filter_coeffs <- butter(n = 2, W = Wn, type = "low")

# Apply the filter using filtfilt for zero-phase filtering
data$filtered_X <- filtfilt(filter_coeffs, data$Accel.X)
data$filtered_Y <- filtfilt(filter_coeffs, data$Accel.Y)
data$filtered_Z <- filtfilt(filter_coeffs, data$Accel.Z)


# ggplot(data, aes(x = seq(1:nrow(data)), y = Accel.X)) + geom_line()
ggplot(data, aes(x = seq(1:nrow(data)), y = filtered_X)) + geom_line()

fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))
