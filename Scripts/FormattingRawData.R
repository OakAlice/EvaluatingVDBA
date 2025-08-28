# Formatting each of the raw data sources into standard structure ---------
# And generate the VDBA for each time instance



# Dataset dictionary ------------------------------------------------------
# all the files have weird names so I need to define them
dataset_dictionary <- list(
  "Wanja_Fox" = "wanja_fox.csv",
  "Nuijten_BewickSwans" = "Biotelemetry of Bewick's swans-acceleration.csv",
  "Khaewphakdee_FishingCat" = "Fishing cat (Prionairulus viverrinus) study in Khao Sam Roi Yot Thailand-acc.csv",
  "Schweitzer_WoodStork" = "NC Wood Stork Tracking-acceleration.csv",
  "Schloesing_FruitBat" = "Hammer-headed fruit bats (Hypsignathus monstrosus) in the Republic of Congo-acc.csv",
  "Dickinson_Goat" = "Goat10hz_processed.csv",
  "Dickinson_Ibex" = "Ibex10hz_processed.csv",
  "Chimienti_Razorbills" = "RAZO.txt",
  "Chimienti_Guillemots" = "COGU.txt",
  "Dunford_Cat" = "Dunford_et_al._Cats_calibrated_data.csv"
)

# Functions ---------------------------------------------------------------
reformat_eobs_data <- function(data){
  long <- data[, .(
    acc = as.numeric(unlist(strsplit(`eobs:accelerations-raw`, " ")))
  ), by = .(Event.ID = `event-id`, 
            timestamp, 
            ID = `individual-local-identifier`,
            freq = `eobs:acceleration-sampling-frequency-per-axis`)]
  
  long[, idx := rep(1:(.N/3), each = 3), by = .(Event.ID, timestamp)]
  long[, axis := rep(c("X", "Y", "Z"), times = .N/3), by = .(Event.ID, timestamp)]
  
  wide <- dcast(long, Event.ID + timestamp + ID + freq + idx ~ axis, value.var = "acc")
  
  wide[, Time := timestamp + (idx - 1) * (1/freq)]
  
  data2 <- wide %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z) %>%
    select(Accel.X, Accel.Y, Accel.Z, Time, Event.ID, ID) %>%
    mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS"))
  
  return(data2)
}

# Reformat the data -------------------------------------------------------
# if its just the eobs format or something very easy, format here, otherwise run alternate script for other cases

data <- fread(file.path(base_path, "AccelerometerData", species, dataset_dictionary[[species]]))

if (species == "Wanja_Fox"){
  data <- data %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z,
           Time = timestamp,
           Activity = Bev) %>%
    mutate(Time = as.POSIXct(Time, format = "%d.%m.%Y %H:%M:%S"))
  
} else if (species == "Nuijten_BewickSwans"){
  data <- data %>%
    rename(`eobs:acceleration-sampling-frequency-per-axis` = `acceleration-sampling-frequency-per-axis`,
         `eobs:accelerations-raw` = `accelerations-raw`)
  data <- reformat_eobs_data(data)

} else if (species == "Dickinson_Goat" | species == "Dickinson_Ibex") {
  data <- data %>%
    mutate(NewTime = as.POSIXct(paste(Date, Time))) %>%
    select(NewTime, Accx, Accy, Accz, Behaviour, ID) %>%
    rename(Time = NewTime,
           Accel.X = Accx,
           Accel.Y = Accy,
           Accel.Z = Accz,
           Activity = Behaviour,
           ID = ID)
  
} else if (species == "Chimienti_Razorbills" | species == "Chimienti_Guillemots"){
  data <- data %>%
    mutate(Time = as.POSIXct(paste0(Date, Time), 
               format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")) %>%
    select(X, Y, Z, Time) %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z)
  
} else if (species == "Dunford_Cat") {
  data <- data %>%
    rename(Accel.X = AccX,
           Accel.Y = AccY,
           Accel.Z = AccZ,
           Activity = Behaviour)


} else { # everything else
  data <- reformat_eobs_data(data)
  
}

fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
