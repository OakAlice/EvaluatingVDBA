# Formatting each of the raw data sources into standard structure ---------
# I dont want to have to store the raw data in multiple places, therefore, I have linked these back to their original locations

reformat_axivity <- function(x){
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
}

# "Annett_Bettong", "Annett_Wallaby", "Clemente_Kudu" were already completed so I just saved the final versions
locations <- list("Annett_Possum" = "D:/LabelledDataSets/Annett_Possum/raw",
                  "Annett_Kangaroo" = "D:/OurDatasets/Annett_Kangaroo/Annett_Kangaroo/raw",
                  "Clemente_Impala" = "D:/OurDatasets/Clemente_Impala/Clemente_Impala/raw",
                  "Galea_Cat" = "D:/OurDatasets/Galea_Cat/Galea_Cat_Axivity",
                  "Gaschk_Quoll" = file.path(base_path, "Data/AxivityAccelerometer/Gaschk_Quoll/raw"),
                  "Sparkes_Koala" = "D:/OurDatasets/Sparkes_Koala/raw"
                  )

sampling_times <- list("Annett_Kangaroo" = file.path(base_path, "Data/AxivityAccelerometer/Annett_Kangaroo/Sampling_Times.csv"),
                       "Clemente_Impala" = file.path(base_path, "Data/AxivityAccelerometer/Clemente_Impala/Sampling_Times.csv")
                       )


files <- list.files(locations[[species]], recursive = TRUE, full.names = TRUE)
  
if (species %in% c("Annett_Kangaroo", "Clemente_Impala")){

  # figure out the dates
  samps <- fread(sampling_times[[species]]) 
  samps$CollarDate <- as.Date(as.character(samps$CollarDate), format = "%Y%m%d")
  samps$DropOffDate <- as.Date(as.character(samps$DropOffDate), format = "%Y%m%d")
  
  lapply(files, function(x){
    data <- reformat_axivity(x)
    ID <- data$ID[1]
    
    data$Date <- as.Date(as.POSIXct((data$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
    
    # crop dates
    start <- samps$CollarDate[samps$Name == ID]
    end   <- samps$DropOffDate[samps$Name == ID]
    data <- data[Date > start & Date < end]
    
    # and save
    fwrite(data, file.path(base_path, "Data", "AxivityAccelerometer", species, paste0(species, "_", ID, "_reformatted.csv")))
  })
  
  } else if (species %in% c("Galea_Cat", "Gaschk_Quoll", "Sparkes_Koala")){
    
    lapply(files, function(x){
      data <- reformat_axivity(x)
      ID <- data$ID[1]
      fwrite(data, file.path(base_path, "Data", "AxivityAccelerometer", species, paste0(species, "_", ID, "_reformatted.csv")))
    })
    
  } else {
  # the simple small ones
  dfs <- lapply(files, function(x){
    dat <- reformat_axivity(x)
    dat
  })
  data <- rbindlist(dfs)
  
  fwrite(data, file.path(base_path, "Data", "AxivityAccelerometer", species, paste0(species, "_reformatted.csv")))
}

  








# Reformat the data -------------------------------------------------------
# Axivity datasets specifically
# need to make sure the names of the individuals matches what's in the files



if (species == "Sparkes_Koala"){
  # previously formatted. only selected the first 100 files from each individual
  data <- fread(file.path(base_path, "/Data/Accelerometer/Sparkes_Koala/raw/labelled_data.csv"))
  data <- data[, c(1:4, 8)]
  colnames(data) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z", "ID")

} else if (species %in% c("Galea_Cat",
                          "Annett_Possum",  "Annett_Kangaroo", "Annett_Bettong", "Annett_Wallaby",
                          "Clemente_Impala", "Clemente_Kudu")){
  
  files <- list.files(file.path(base_path, "Data/AxivityAccelerometer", species, "raw"), full.names = TRUE)
  
  dfs <- lapply(files, function(x){
    
    dat <- reformat_axivity(x)
    
    # subsample the big datasets
    if (species %in% c("Galea_Cat", "Annett_Kangaroo", "Clemente_Impala")){
      dat <- dat[1:(nrow(dat)/4),]
    }
    
    dat
  })
  
  data <- rbindlist(dfs)
  
} else if (species == "Gaschk_Quoll"){
  files <- list.files(file.path(base_path, "Data/AccelerometerData", species, "raw"), full.names = TRUE)
  
  data <- lapply(files, function(x) {
    collar_number <- tools::file_path_sans_ext(basename(x))
    collar_number <- str_split(collar_number, "_")[[1]][1]
    # Format the data like the others
    dat <- reformat_axivity(x)
    # add in the ID
    dat[, ID := collar_number]
    dat <- dat[1:(nrow(dat)/4),]
  })
  data <- rbindlist(data)
  
} 

# and there are some that are already done
# "Annett_Glider"

fwrite(data, file.path(base_path, "Data", "AxivityAccelerometer", species, paste0(species, "_reformatted.csv")))
