# Checking the goat data --------------------------------------------------


species <- "Dickinson_Goat"

data <- fread(file.path(base_path, "AccelerometerData", species, "raw", dataset_dictionary[[species]]))

data <- data %>%
  mutate(NewTime = as.POSIXct(paste(Date, Time))) %>%
  select(NewTime, Accx, Accy, Accz, Behaviour, ID) %>%
  rename(Time = NewTime,
         Accel.X = Accx,
         Accel.Y = Accy,
         Accel.Z = Accz,
         Activity = Behaviour,
         ID = ID)

data_subset <- data #[1:1000000]


ggplot(data_subset, aes(x = seq(1:nrow(data_subset)), y = Accel.X)) + geom_line()



