# Extracting Characteristics of the datasets ----------------------------
# just trying to understand the data I have

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))

# what was the sampling rate?
freq <- as.numeric(dataset_variables[Name == species]$Frequency)

# volume
hours_per_id <- data %>% group_by(ID) %>% summarise(volume = ((n()/freq)/60)/60)
