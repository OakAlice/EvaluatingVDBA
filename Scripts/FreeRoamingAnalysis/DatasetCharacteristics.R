# Extracting Characteristics of the datasets ----------------------------
# just trying to understand the data I have

## 1: Volume of data
data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

# what was the sampling rate?
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

# volume
hours_per_id <- data %>% group_by(ID) %>% summarise(volume = ((n()/freq)/60)/60)

# write that out and save for later
fwrite(hours_per_id, file.path(base_path, "AccelerometerData", species, paste0(species, "_volume.csv")))

