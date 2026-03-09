# Formatting each of the raw data sources into standard structure ---------

# Reformat the data -------------------------------------------------------

if (species == "Wanja_Fox"){
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", "wanja_fox.csv"))
  data <- data %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z,
           Time = timestamp,
           Activity = Bev) %>%
    mutate(Time = as.POSIXct(Time, format = "%d.%m.%Y %H:%M:%S"))
  
} else if (species == "Dickinson_Goat" | species == "Dickinson_Ibex") {
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", 
                          ifelse(species == "Dickinson_Goat", "Goat10hz.csv", "Ibex10hz.csv")))
  data <- data %>%
    mutate(NewTime = as.POSIXct(paste(Date, Time))) %>%
    select(NewTime, Accx, Accy, Accz, Behaviour, ID) %>%
    rename(Time = NewTime,
           Accel.X = Accx,
           Accel.Y = Accy,
           Accel.Z = Accz,
           Activity = Behaviour,
           ID = ID)
  
} else if (species == "Dunford_Cat") {
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", "Dunford_et_al._Cats_calibrated_data.csv"))
  data <- data %>%
    rename(Accel.X = AccX,
           Accel.Y = AccY,
           Accel.Z = AccZ,
           Activity = Behaviour)

} else if (species == "Seriyes_Bobcat"){
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE, pattern = "of9\\.csv$")
  dfs <- lapply(files, function(x){
    reformat_eobs_data(fread(x))
  })
  data <- rbindlist(dfs, fill = TRUE)

} else if (species == "Rautiainen_Reindeer"){
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", "acceleration.csv"))
  data <- data %>%
    mutate(Time = as.POSIXct(Timestamp, 
                             format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")) %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z,
           ID = TagID) %>%
    select(ID, Time, Accel.X, Accel.Y, Accel.Z)

} else if (species == "Minasandra_Hyena"){
  # these are h5 files so require being unpacked before read
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE, pattern = "\\.h5$")
  
  dfs <- lapply(files, function(x){
    # List top-level groups/datasets and select the one you want
    # h5ls(x)
    
    # get the accel data out
    data <- h5read(x, "A")
    data <- as.data.table(data)
    colnames(data) <- c("Accel.X", "Accel.Y", "Accel.Z")
    
    # get the timestamps
    Times <- h5read(x, "UTC")
    StartTime <- as.POSIXct(
      paste0(Times[1,1], "/", Times[1,2], "/", Times[1,3], " ", Times[1,4], ":", Times[1,5], ":", Times[1,6]),
      format = "%Y/%m/%d %H:%M:%OS")
  
    # use the sampling rate to generate a string of times
    Time <- seq(
      from = StartTime, 
      by   = 1/25, # 25 is the sampling rate
      length.out = nrow(data)
    )
    
    # get the name of the individual 
    ID_name <- tools::file_path_sans_ext(basename(x))
    ID <- rep(ID_name, nrow(data))
    
    # stitch it all together
    data[, `:=`(Time = Time, ID = ID)]
    data <- data[1:(nrow(data)/5), ]
    
    H5close() # close the file
    data
  })
  
  data <- rbindlist(dfs)
  rm(dfs)
  
}  else if (species == "Sparkes_Koala"){
  # only select the first 100 files from each individual
  dat <- fread("C:/Users/PC/Documents/EvaluatingVDBA/AccelerometerData/Sparkes_Koala/raw/labelled_data.csv")
  dat <- dat[, c(1:4, 8)]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z", "ID")

  data <- dat
  

} else if (species %in% c("Annett_Possum", "DiCicco_Perentie", "Galea_Cat", 
                          "Annett_Bettong", "Annett_Wallaby")){
  
  pattern_dictionary <- list("Annett_Possum" = "^[0-9]{5}_.+\\.csv$",
                            "DiCicco_Perentie" = "^[0-9]{5}.+\\.csv$",
                            "Galea_Cat" = "_[0-9]{1}.csv$",
                            "Annett_Wallaby" = ".csv",
                            "Annett_Bettong" = ".csv")
    
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE, pattern = pattern_dictionary[[species]], ignore.case = TRUE)

  dfs <- lapply(files, function(x){
    dat <- reformat_clemente_data(x)
    if (species == "Galea_Cat"){
      dat <- dat[1:(nrow(dat)/4),]
    }
    dat
  })
  data <- rbindlist(dfs)

} else if (species == "Annett_Glider"){
  
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE, pattern = ".txt", ignore.case = TRUE)

  dfs <- lapply(files, function(x){
    reformat_clemente_data(x)
  })
  data <- rbindlist(dfs)
  data$ID <- str_extract(data$ID, "Flip|Gilberta")
  
} else if (species == "Clemente_Kudu"){
  
    dat <- fread(file.path(base_path, "AccelerometerData", species, "raw", "6017913_0000000000.csv"))
    dat <- dat[, 1:4]
    colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
    dat$Time <- as.POSIXct(dat$Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    dat$ID <- "6017913_0000000000"
    data <- dat
    
} else if (species %in% c("Smit_Cat", "Studd_Squirrel", "Clemente_Echidna", "HarveyCaroll_Pangolin", "Vehkaoja_Dog")){
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", 
                          if (species == "Smit_Cat"){ "Smit_Cat_reformatted.csv"
                          } else if(species == "Studd_Squirrel"){ "Studd_Squirrel_reformatted.csv"
                          } else if (species == "Clemente_Echidna"){ "Clemente_Echidna_reformatted.csv"
                          } else if (species == "HarveyCaroll_Pangolin"){"HarveyCaroll_Pangolin_reformatted.csv"
                          } else if (species == "Vehkaoja_Dog"){"Vehkaoja_Dog_reformatted"}
  ))
  data <- data %>%
    rename(Accel.X = X,
           Accel.Y = Y,
           Accel.Z = Z)
  
} else if (species == "Mauny_Goat"){
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    
    fread(x) %>%
      select(TIME, ACCx, ACCy, ACCz) %>%
      rename(Time = TIME,
             Accel.X = ACCx,
             Accel.Y = ACCy,
             Accel.Z = ACCz) %>%
      mutate(ID = str_split(tools::file_path_sans_ext(basename(x)), "_")[[1]][3])
  })
  data <- rbindlist(data)
  
} else if (species == "Friedlaender_Whale"){
  
  files <- list.files(file.path(base_path, "AccelerometerData", "Friedlaender_Whale"), 
                      pattern = "prh.mat", recursive = TRUE, full.names = TRUE)
  data <- lapply(files, function(x){
    mat <- readMat(x)
    df <- as.data.table(mat$A)  # extract the accelerometer data
    df[, ID := tools::file_path_sans_ext(basename(x))]
    colnames(df) <- c("Accel.X", "Accel.Y", "Accel.Z", "ID")
    N <-nrow(df)
    df$Time <- seq(
      from = as.POSIXct("2000-01-01 01:01:00", format = "%Y-%m-%d %H:%M:%OS"),
      by   = 1 / 5,  # seconds between samples
      length.out = N
    )
    df
  })
  data <- rbindlist(data)
  
} else if (species == "Chakravarty_Meerkat"){
  
  file_path <- file.path(base_path, "AccelerometerData", "Chakravarty_Meerkat", "raw", "labelledTriaxialAccData.mat")
  datasets <- h5ls(file_path)
  datasets <- datasets[datasets$otype == "H5I_DATASET", ]
  refs <- datasets$name
  refs <- refs[1:82606]
  
  accel_list <- lapply(seq_along(refs), function(i) {
    ref_path <- refs[[i]]
    print(i)
   
    dat <- h5read(file_path, paste0("/#refs#/", ref_path))
    tryCatch({
      dat <- as.data.table(dat)
      
      setnames(dat, c("Accel.X", "Accel.Y", "Accel.Z"))
      
      dat[, Time := seq(
        from = as.POSIXct("2000-01-01 01:01:00", tz = "UTC"),
        by = 1 / as.numeric(dataset_variables[Name == species]$Frequency),
        length.out = .N
      )]
      
      numeric_id <- str_extract(ref_path, "^.{1,2}")
      dat[, ID := numeric_id]
      dat
      
    }, error = function(e) {
      message("Skipping: not data")
      NULL
    })
    
  })
  data <- rbindlist(accel_list, use.names = TRUE)

} else if (species == "Dissanayake_Calf"){
  data <- fread(file.path(base_path, "AccelerometerData", species, "raw", "AcTBeCalf.csv"))
  data <- data %>%
    rename(Time = dateTime,
           ID = calfId,
           Accel.X = accX,
           Accel.Y = accY,
           Accel.Z = accZ)
  
  
} else if (species == "Kamminga_Horse"){
  files <- list.files(file.path(base_path, "AccelerometerData", "Kamminga_Horse", "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    fread(x) %>% 
      rename(Accel.X = Ax,
             Accel.Y = Ay,
             Accel.Z = Az,
             Time = segment) %>%
      mutate(ID = str_split(basename(x), "_")[[1]][3])
  })
  data <- rbindlist(data)

} else if (species == "Ladds_Seal"){

  files <- list.files(file.path(base_path, "AccelerometerData", "Ladds_Seal", "raw"), full.names = TRUE, recursive = TRUE)
  dfs <- lapply(files, function(x){
      dat <- fread(x) %>%
      select(time, x, y, z) %>%
      rename(Time = time,
             Accel.X = x,
             Accel.Y = y,
             Accel.Z = z) %>%
      mutate(ID = basename(dirname(x)))
    
      if (!inherits(dat$Time, "POSIXct")) return(NULL)
      dat
  })
  data <- rbindlist(dfs)
  
} else if (species == "Gaschk_Quoll"){
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE)
  
  data <- lapply(files, function(file) {
    
      collar_number <- tools::file_path_sans_ext(basename(file))
      
      # Species-specific collar ID extraction
      collar_number <- str_split(collar_number, "_")[[1]][1]
      
      # Format the data into a standardised format
      dat <- reformat_clemente_data(file)
      
      # add in the ID
      dat[, ID := collar_number]
      dat <- dat[1:(nrow(dat)/4),]
    })
  
  data <- rbindlist(data)
  
} else if (species == "Pagano_Bear"){
  
  data <- fread(file.path(base_path, "AccelerometerData", "Pagano_Bear", "raw", "PolarBear_archival_logger_data_southernBeaufortSea_2014_2016_revised.csv"))
  data <- data %>%
    rename(ID = Bear,
           Time = Datetime,
           Accel.X = Int_aX,
           Accel.Y = Int_aY,
           Accel.Z = Int_aZ)
  
  # convery from mps2 to Gs
  data[, `:=`(
    Accel.X = Accel.X / 9.80665,
    Accel.Y = Accel.Y / 9.80665,
    Accel.Z = Accel.Z / 9.80665
  )]
  

} else if (species == "Studd_Lynx"){
  files <- list.files(file.path(base_path, "AccelerometerData", "Studd_Lynx", "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    dat <- fread(x)
    dat <- dat %>%
      rename(Time = V1, 
             Accel.X = V2,
             Accel.Y = V3,
             Accel.Z = V4) %>%
      mutate(ID = str_split(tools::file_path_sans_ext(basename(x)), "_")[[1]][1])
    dat
  })
  data <- rbindlist(data)
  
} else if (species == "Williams_Squirrel"){
  
  files <- list.files(file.path(base_path, "AccelerometerData", "Williams_Squirrel", "raw"), full.names = TRUE)
  data <- lapply(files, \(x) {
    dat <- fread(x, col.names = c(
      "year", "month", "day", "hour", "min", "sec", "doy", "uniqueID",
      "Sex", "Site", "acc_x", "acc_y", "acc_z", "odba"
    ))[1:(.N/2)]  # take first half of rows (just because the files are really big)
    
    dat %>%
      mutate(
        Time = as.POSIXct(sprintf("%04d/%02d/%02d %02d:%02d:%02d",
                                  year, month, day, hour, min, sec),
                          format = "%Y/%m/%d %H:%M:%S")
      ) %>%
      transmute(
        ID = uniqueID,
        Time,
        Accel.X = acc_x,
        Accel.Y = acc_y,
        Accel.Z = acc_z
      )
  })
  
  data <- rbindlist(data)

} else if (species == "Harris_Sheep"){
  files <- list.files(file.path(base_path, "AccelerometerData", "Harris_Sheep", "raw"), full.names = TRUE)[1:19]
  data <- lapply(files, function(x){
    df <- fread(x)
    
    x_cols <- grep("^x_", names(df), value = TRUE)
    y_cols <- grep("^y_", names(df), value = TRUE)
    z_cols <- grep("^z_", names(df), value = TRUE)
    n_samples <- length(x_cols)
    
    # Melt to long format
    df_long <- melt(
      df,
      id.vars = c("sheep_number", "time_stamp"),
      measure = list(Accel.X = x_cols, Accel.Y = y_cols, Accel.Z = z_cols),
      variable.name = "sample_index"
    )
    
    # Add per-sample timestamp (30 Hz = every 1/30 sec)
    df_long[, time_stamp := as.POSIXct(time_stamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
    df_long[, Time := time_stamp + (as.numeric(sample_index) / 30)]
    
    # Reorder and clean up
    setorder(df_long, sheep_number, Time)
    df_long <- df_long[, .(sheep_number, Time,
                           Accel.X, Accel.Y, Accel.Z)]
    
    df_long <- na.omit(df_long)
    df_long$ID <- str_split(tools::file_path_sans_ext(basename(x)), "_")[[1]][2]
    
    df_long
  })
  
  data <- rbindlist(data)
  
} else if (species == "Neis_Cow"){
  files <- list.files(file.path(base_path, "AccelerometerData", "Neis_Cow", "raw"), full.names = TRUE, recursive = TRUE)
  data <- lapply(files, function(x){
    df <- fread(x) %>%
      rename(Time = timestamp,
             Accel.X = accel_x_mps2,
             Accel.Y = accel_y_mps2,
             Accel.Z = accel_z_mps2) %>%
      select(Time, Accel.X, Accel.Y, Accel.Z) %>%
      mutate(ID = basename(x))
    
    # convery from mps2 to Gs
    df[, `:=`(
      Accel.X = Accel.X / 9.80665,
      Accel.Y = Accel.Y / 9.80665,
      Accel.Z = Accel.Z / 9.80665
    )]
    
    df
  })
  data <- rbindlist(data)
  
} else if (str_detect(species, "Buchmann") %in% TRUE){
  
  files <- list.files(file.path(base_path, "AccelerometerData", species, "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    df <- fread(x, header = FALSE) %>%
      rename(Time = V4,
             Accel.X = V1,
             Accel.Y = V2,
             Accel.Z = V3)
    
    df$ID <- str_split(tools::file_path_sans_ext(basename(x)), "_")[[1]][2]
    
    # convert into Gs
    df[, `:=`(
      Accel.X = Accel.X / 9.80665,
      Accel.Y = Accel.Y / 9.80665,
      Accel.Z = Accel.Z / 9.80665
    )]
  
    df
  })
  data <- rbindlist(data)

} else if (species == "Annett_Kangaroo"){
  
  files <- list.files(file.path(base_path, "AccelerometerData", "Annett_Kangaroo", "Individual_Analyses"), full.names = TRUE)
  data <- lapply(files, function(x){
    fread(x) %>%
      select(ID, Time, Accel.X, Accel.Y, Accel.Z)
  })
  data <- rbindlist(data)

} else if (species == "Clemente_Impala"){
  
  files <- list.files(file.path(base_path, "AccelerometerData", "Clemente_Impala", "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    dat <- fread(x)
    colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
    dat$ID <- str_split(basename(x), "_")[[1]][2]
    dat <- dat[1:(nrow(dat)/4),]
    dat
  })
  data <- rbindlist(data)
  
} else if (species == "Annett_Bettong"){
  files <- list.files(file.path(base_path, "AccelerometerData", "Annett_Bettong", "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    dat <- fread(x)
    dat <- dat[, 1:4]
    colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
    dat$ID <- str_split(basename(x), "_", simplify = TRUE)[1]
    dat
  })
  data <- rbindlist(data)
  
} else if (species == "HARTH_Human"){
  files <- list.files(file.path(base_path, "AccelerometerData", "HARTH_Human", "raw"), full.names = TRUE)
  data <- lapply(files, function(x){
    dat <- fread(x)
    dat <- dat[, 1:4]
    colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
    dat <- dat %>%
      mutate(Accel.X = as.numeric(Accel.X))
    dat$ID <- tools::file_path_sans_ext(basename(x))
    dat
  })
  data <- rbindlist(data)
  
} else { # everythindataset_variables} else { # everything else
  data <- reformat_eobs_data(data)
}

# add in ID if not already there so I can subset
if (!"ID" %in% colnames(data)){
  data$ID <- rep("Unsure", nrow(data))
}

# crop the files to a maximum size ## REMOVE THIS LINE TO SEE WHAT HAPPENS
# one_day <- as.numeric(dataset_variables[Name == species]$Frequency) * (60*60*max_samples)
# data <- data[, .SD[1:min(one_day, .N)], by = ID]

# only seelct the columns we want
data <- data[, c("ID", "Time", "Accel.X", "Accel.Y", "Accel.Z")]

# writing it to disk
fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))

rm(data)
gc()
