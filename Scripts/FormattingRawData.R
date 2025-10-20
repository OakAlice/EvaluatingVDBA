# Formatting each of the raw data sources into standard structure ---------

# Functions ---------------------------------------------------------------
reformat_eobs_data <- function(data){
  long <- data[, .(
    acc = as.numeric(unlist(strsplit(`eobs:accelerations-raw`, " ")))
  ), by = .(Event.ID = `event-id`, 
            timestamp, 
            ID = `individual-local-identifier`,
            freq = `eobs:acceleration-sampling-frequency-per-axis`)]
  
  if (data$`eobs:acceleration-axes`[1] == "XYZ"){
    long[, idx := rep(1:floor(.N/3), each = 3)[1:.N], by = .(Event.ID, timestamp)]
    long[, axis := rep(c("X", "Y", "Z"), length.out = .N), by = .(Event.ID, timestamp)]
    
  } else if (data$`eobs:acceleration-axes`[1] == "XY"){
    long[, idx := rep(1:(.N/2), each = 2)[1:.N], by = .(Event.ID, timestamp)]
    long[, axis := rep(c("X", "Y"), times = .N/2), by = .(Event.ID, timestamp)]  
  } else {
    print("there is a different number of axes in this case")
  }
  
  wide <- dcast(long, Event.ID + timestamp + ID + freq + idx ~ axis, value.var = "acc")
  
  wide[, Time := timestamp + (idx - 1) * (1/freq)]
  
  if (data$`eobs:acceleration-axes`[1] == "XYZ"){
    data2 <- wide %>%
      rename(Accel.X = X,
             Accel.Y = Y,
             Accel.Z = Z) %>%
      select(Accel.X, Accel.Y, Accel.Z, Time, Event.ID, ID) %>%
      mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS"))
  } else if (data$`eobs:acceleration-axes`[1] == "XY"){
    data2 <- wide %>%
      rename(Accel.X = X,
             Accel.Y = Y) %>%
      select(Accel.X, Accel.Y, Time, Event.ID, ID) %>%
      mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%OS"))
  }
  
  return(data2)
}

reformat_clemente_data <- function(x){
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
  dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
}

# Reformat the data -------------------------------------------------------
# if its just the eobs format or something very easy, format here, otherwise run alternate script for other cases

# most species just read in directly but there are a few that need a bit more
if (species != "Seriyes_Bobcat" && species != "Acacio_Stork" &&
    species != "Minasandra_Hyena" && !species %in% unlist(source_dictionary[["Clemente_Data"]])){
  data <- fread(file.path(base_path, "AccelerometerData", species, dataset_dictionary[[species]]))
}

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
         `eobs:accelerations-raw` = `accelerations-raw`,
         `eobs:acceleration-axes` = `acceleration-axes`)
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

} else if (species == "Seriyes_Bobcat"){
  files <- list.files(file.path(base_path, "AccelerometerData", species), full.names = TRUE, pattern = "of9\\.csv$")
  dfs <- lapply(files, function(x){
    reformat_eobs_data(fread(x))
  })
  data <- rbindlist(dfs, fill = TRUE)

} else if (species == "Acacio_Stork"){
  files <- list.files(file.path(base_path, "AccelerometerData", species), full.names = TRUE, pattern = "_part\\.csv$")
  
  df1 <- fread(file.path(base_path, "AccelerometerData/Acacio_Stork/White Stork Adults 2017_part.csv")) %>%
    rename(Accel.X = `acceleration-raw-x`,
             Accel.Y = `acceleration-raw-y`,
             Accel.Z = `acceleration-raw-z`,
             ID = `tag-local-identifier`,
             Time = timestamp,
             Event.ID = `event-id`) %>%
    select(Accel.X, Accel.Y, Accel.Z, Time, ID, Event.ID)
  
  df2 <- fread(file.path(base_path, "AccelerometerData/Acacio_Stork/White Stork Adults 2018_part.csv")) %>%
    mutate(DateTime = paste0("01/01/2018 01:", timestamp)) %>%
    rename(Accel.X = `acceleration-raw-x`,
           Accel.Y = `acceleration-raw-y`,
           Accel.Z = `acceleration-raw-z`,
           ID = `tag-local-identifier`,
           Time = DateTime,
           Event.ID = `event-id`) %>%
    select(Accel.X, Accel.Y, Accel.Z, Time, ID, Event.ID) %>%
    mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M:%S"))
  
  data <- rbind(df1, df2)

} else if (species == "Rautiainen_Reindeer"){
  data <- data %>%
    mutate(Time = as.POSIXct(Timestamp, 
                             format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")) %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z,
           ID = TagID)

} else if (species == "Minasandra_Hyena"){
  # these are h5 files so require being unpacked before read
  files <- list.files(file.path(base_path, "AccelerometerData", species), full.names = TRUE, pattern = "\\.h5$")
  
  dfs <- lapply(files, function(x){
    # List top-level groups/datasets and select the one you want
    # h5ls(x)
    
    # get the accel data out
    data <- h5read(x, "A")
    colnames(data) <- c("Accel.X", "Accel.Y", "Accel.Z")
    
    # get the timestamps
    Times <- h5read(x, "UTC")
    StartTime <- as.POSIXct(
      paste0(Times[1,1], "/", Times[1,2], "/", Times[1,3], " ", Times[1,4], ":", Times[1,5], ":", Times[1,6]),
      format = "%Y/%m/%d %H:%M:%OS")
    
    # get the sampling rate
    Fs <- h5read(x, "fs")
    
    # use the sampling rate to generate a string of times
    Time <- seq(
      from = StartTime, 
      by   = 1/Fs, # seconds between each of the saemples assuming constant rate
      length.out = nrow(data)
    )
    
    # get the name of the individual 
    ID_name <- tools::file_path_sans_ext(basename(x))
    ID <- rep(ID_name, nrow(data))
    
    # stitch it all together
    data <- as.data.table(data)
    data[, `:=`(Time = Time, ID = ID)]
    
    H5close() # close the file
  })
  
  data <- rbindlist(dfs)
  rm(dfs)

} else if (species %in% unlist(source_dictionary[["Clemente_Data"]])){
  
  if (species == "Sparkes_Koala"){
    # only select the first 100 files from each individual
      all_files <- list.files(file.path(base_path, "AccelerometerData/Sparkes_Koala", "Data"),
                              recursive = TRUE, full.names = TRUE)
      subdirs <- dirname(all_files) %>% basename()
      files_by_subdir <- split(all_files, subdirs)
      first40_per_subdir <- lapply(files_by_subdir, function(x) head(x, 40))
      files <- unlist(first40_per_subdir, use.names = FALSE)
  } else {
    pattern_dictionary <- list("Annett_Kangaroo" = "Collar[0-9]*\\.csv$",
                               "Annett_Possum" = "^[0-9]{5}_.+\\.csv$",
                               "Clemente_Impala" = "Collar_[0-9]*_.+\\.csv$",
                               "DiCicco_Perentie" = "^[0-9]{5}.+\\.csv$",
                               "Galea_Cat" = "_[0-9]{1}.csv$",
                               "Gaschk_Quoll" = "_Accel")
    
    files <- list.files(file.path(base_path, "AccelerometerData", species), full.names = TRUE, pattern = pattern_dictionary[[species]], ignore.case = TRUE)
  }
  
  dfs <- lapply(files, function(x){
    reformat_clemente_data(x)
  })
  data <- rbindlist(dfs)
  rm(dfs)
  
} else if (species %in% c("Mauny_Goat", "Smit_Cat", "Ladds_Seal", "Studd_Squirrel")){
  
  data <- data %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z)
  
} else { # everything else
  data <- reformat_eobs_data(data)
}

# add in ID if not already there so I can subset
if (!"ID" %in% colnames(data)){
  data$ID <- rep("Unsure", nrow(data))
}

# crop the files to a maximum size
one_day <- as.numeric(dataset_variables[Name == species]$Frequency) * (60*60*max_samples)
data <- data[, .SD[1:min(one_day, .N)], by = ID]

# only seelct the columns we want
data <- data[, c("ID", "Time", "Accel.X", "Accel.Y", "Accel.Z")]

# writing it to disk
fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

rm(data)
gc()
