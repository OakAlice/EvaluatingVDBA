# Cleaning the data -------------------------------------------------------
# visualised the data and then designed a cleaning protocol for each dataset
# e.g., clipping misread rections, removing obvious outliers, etc.

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
data <- na.omit(data)

if (species == "Annett_Possum" | species == "Neis_Cow" 
    | species == "Pagano_Bear" | species == "Studd_Lynx" |
    species == "Sparkes_Koala"){
  data <- data[1:(nrow(data)/4),]
}

if (!file.exists(output_path)){
  # plot the data and save to disk
  # rakes forever so only do it if necessary
  output_path <- file.path(base_path, "AccelerometerData", species, "original_data.png")
  p <- ggplot(data, aes(x = seq_len(nrow(data)), y = Accel.X)) +
    geom_line()
  ggsave(filename = output_path, plot = p, width = 15, height = 5, dpi = 300)
}

# remove whatever was wrong with the data
if (species == "Dickinson_Goat"){
  # remove the outliers
  clean_data <- data[!(data$Accel.X > 4 | data$Accel.X < -3.5), ]
  clean_data <- clean_data[!(clean_data$Accel.Y > 4 | clean_data$Accel.Y < -3.5), ]
  clean_data <- clean_data[!(clean_data$Accel.Z > 4 | clean_data$Accel.Z < -4.5), ]
} else if (species == "Dickinson_Ibex"){
  clean_data <- data[!(data$Accel.X > 4 | data$Accel.X < -4), ]
  clean_data <- clean_data[!(clean_data$Accel.Y > 4 | clean_data$Accel.Y < -5), ]
  clean_data <- clean_data[!(clean_data$Accel.Z > 5 | clean_data$Accel.Z < -4), ]
} else if (species == "Gaschk_Quoll"){
  clean_data <- data[-(50000000:65000000)]
} else if (species == "HarveyCaroll_Pangolin"){
  clean_data <- data[!(data$Accel.X > 2.5 | data$Accel.X < -3.5), ]
  clean_data <- clean_data[!(clean_data$Accel.Y > 3 | clean_data$Accel.Y < -3.5), ]
  clean_data <- clean_data[!(clean_data$Accel.Z > 3 | clean_data$Accel.Z < -4), ]
} else if (species == "Clemente_Kudu"){
  clean_data <- data[1950000:2800000,]
} else if (species == "Williams_Squirrel"){
  clean_data <- data[-(2000000:7500000)]
}

downsampled <- clean_data[seq(1, nrow(clean_data), by = 100), ]
  
ggplot(downsampled, aes(x = seq_len(nrow(downsampled)))) +
  geom_line(aes(y = Accel.X), colour = "red")


ggplot(clean_data, aes(x = seq_len(nrow(clean_data)))) +
  geom_line(aes(y = Accel.X), colour = "red") +
  geom_line(aes(y = Accel.Y), colour = "blue") +
  geom_line(aes(y = Accel.Z), colour = "green")



fwrite(clean_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_cleaned_reformatted.csv")))



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
