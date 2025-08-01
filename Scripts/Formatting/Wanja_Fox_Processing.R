# Formatting / Processing the Wanja_Fox data ------------------------------

data <- fread(file.path(base_path, "AccelerometerData", "Wanja_Fox", "wanja_fox.csv"))


# Reformat ----------------------------------------------------------------
# this one is straight forward
data <- data %>%
  rename(Accel.X = X,
         Accel.Y = Y,
         Accel.Z = Z,
         Activity = Bev)


