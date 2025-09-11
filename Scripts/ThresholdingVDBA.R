# Finding the active / inactive threshold for each dataset ----------------

# load in the data

data <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))



ggplot(data, aes(x = Time, y = minVDBA))+
  geom_point()


