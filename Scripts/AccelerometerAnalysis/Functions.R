

# Get vedba ---------------------------------------------------------------
get_vedba <- function(dat, freq){
  # VDBA window
  win <- 1 * freq 
  
  # calculate the static accelerations
  ax_static <- frollmean(dat$X, n = win, align = "center", fill = NA)
  ay_static <- frollmean(dat$Y, n = win, align = "center", fill = NA)
  az_static <- frollmean(dat$Z, n = win, align = "center", fill = NA)
  
  # get the dynamic component 
  ax_dynamic <- dat$X - ax_static
  ay_dynamic <- dat$Y - ay_static
  az_dynamic <- dat$Z - az_static
  
  # generate and return
  dat$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
  dat
}


# Mode --------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Get just the locomotion data --------------------------------------------
get_locomotion <- function(dat, freq, stride_window, list_locomotion_labels){
  setDT(dat)
  
  win <- stride_window * freq 
  
  # make a per individual window id
  dat[, wind_id := (seq_len(.N) - 1) %/% win, by = ID]
  
  # get the most common behaviour per window 
  dat_win <- dat[, .(Behaviour = get_mode(Activity)), by = .(ID, wind_id)]
  
  # Keep only locomotion windows
  dat_win <- dat_win[Behaviour %in% list_locomotion_labels, ]
  
  # Filter original data
  dat <- dat[dat_win, on = .(ID, wind_id), nomatch = 0]
  
  dat
}


# Format each dataset -----------------------------------------------------

format_labelled_data <- function(species){
  
  if(species == "Agarwal_WildDog"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Agarwal_WildDog/matched_acceleration_data_out.csv")) %>%
      rename(X = acc_x, Y = acc_y, Z = acc_z, Activity = behavior)
    ID <- fread(file.path(base_path, "Data/Accelerometer/Agarwal_WildDog/matched_acceleration_metadata_out.csv"))
    data$ID <- ID$`individual ID`
    
    parse_vec <- function(x) as.numeric(strsplit(gsub("\\[|\\]", "", x), ",\\s*")[[1]])
    X_list <- lapply(data$X, parse_vec)
    Y_list <- lapply(data$Y, parse_vec)
    Z_list <- lapply(data$Z, parse_vec)
    n <- lengths(X_list)
    melted <- data.table(
      Activity       = rep(data$Activity, n),
      behavior_start = rep(data$behavior_start, n),
      behavior_end   = rep(data$behavior_end, n),
      duration       = rep(data$duration, n),
      Source         = rep(data$Source, n),
      ID             = rep(data$ID, n),
      X = unlist(X_list),
      Y = unlist(Y_list),
      Z = unlist(Z_list)
    )
    melted[, sample_idx := sequence(n) - 1L]
    melted[, Time := behavior_start + sample_idx / 16]
    
    data <- melted %>% select(Activity, ID, Time, X, Y, Z)
  }
  
  if(species == "Annett_Glider"){
    
    raw_files <- list.files(
      path = file.path(base_path, "Data/Accelerometer/Annett_Glider/raw"),
      recursive = TRUE,
      pattern = "\\.txt$",
      full.names = TRUE)
    
    alldata <- lapply(raw_files, function(x){
      data <- fread(x)
      ind <- ifelse(grepl("Flip", x), "Flip", "Gilberta") # Take ID names from files
      data <- data %>% 
        # Generating a generic time stamp for the start time
        mutate(
          time=as.POSIXct((V1 - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
      
      data <- data %>% 
        select(V2, V3, V4, V5, time) %>% 
        rename(X = V2,
               Y = V3,
               Z = V4,
               Activity = V5,
               Time = time) %>%
        mutate(ID = ind) 
      
      data <- dplyr::filter(data, Activity != 0) 
      data
    })
    data <- rbindlist(alldata)
  }
  
  if(species == "Bonneau_Goat"){
    
    data <- fread(file.path(base_path, "Data/Accelerometer/Bonneau_Goat/raw/raw.txt")) %>%
      rename(ID = Animal_id,
             Activity = Behaviour) %>%
      select(ID, Time, Activity, X, Y, Z)
  }
  
  if(species == "Caramaschi_Human"){
    accel_files <- list.files(file.path(base_path, "Data/Accelerometer/Caramaschi_Human"), pattern = "motion.csv", recursive = TRUE, full.names = TRUE)
    data <- lapply(accel_files, function(x){
      # skip the empty ones]
      if (file.size(x) < 5) {
        message("Skipping empty file: ", x)
        return(NULL)
      }
      
      fread(x) %>%
        rename(X = accelX, Y = accelY, Z = accelZ, Time = ms) %>%
        mutate(ID = basename(dirname(dirname(x))),
               Activity = "Locomotion")
    })
    data <- rbindlist(data)
    data <- data %>% select(ID, Time, Activity, X, Y, Z)
  }
  
  if(species == "Chakravarty_Meerkat"){
    # this was difficulet for me as I am not used to this file format
    # couldn't figure it out in R, so did it in matlab...
    
    data <- fread(file.path(base_path, "Data/Accelerometer/Chakravarty_Meerkat/meerkat_acceleration_labels.csv")) %>%
      rename(ID = Session, Activity = Behaviour) %>%
      group_by(ID) %>%
      mutate(Time = row_number())
    
  }
  
  if(species == "Christensen_Baboon"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Christensen_Baboon/rsos221103_si_005.txt")) %>%
      rename(X = stX, Y = stY, Z = stZ,
             Activity = Category) %>%
      mutate(Time = row_number()) %>%
      select(ID, Time, X, Y, Z, Activity)
  }
  
  if(species == "Dickinson_Ibex"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Dickinson_Ibex/Ibex10hz.csv")) %>%
      select(ID, Timestamp, Behaviour, Accx, Accy, Accz) %>%
      rename(X = Accx,
             Y = Accy,
             Z = Accz,
             Activity = Behaviour)
  }
  
  if(species == "Dickinson_Goat"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Dickinson_Goat/Goat10hz.csv")) %>%
      select(ID, Timestamp, Behaviour, Accx, Accy, Accz) %>%
      rename(X = Accx,
             Y = Accy,
             Z = Accz,
             Activity = Behaviour)
  }
  
  if(species == "Dunford_Cat"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Dunford_Cat/Dunford_et_al._Cats_calibrated_data.csv")) %>%
      rename(X = AccX, Y = AccY, Z = AccZ, Activity = Behaviour)
  }
  
  if(species == "Galea_Cat"){
    print("this one only runs from my personal desktop (data too big)")
    
    predictions <- fread(file.path(base_path, "Data/Accelerometer/Galea_Cat/Final_predictions.csv")) %>%
      select(ID, time, prediction) %>%
      rename(Time = time,
             Activity = prediction)
    
    raw_files <- list.files("C:/Users/PC/Documents/Catdata", full.names = TRUE, recursive = FALSE)[2:11]
    raw <- lapply(raw_files, function(x){
      cat <- str_split(basename(x), "_", simplify = T)[1]
      print(cat)
      
      dat <- fread(x)
      colnames(dat) <- c("Time", "X", "Y", "Z")
      dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
      
      # join them together
      setkey(dat, Time)
      preds <- predictions %>% dplyr::filter(ID == cat)
      setkey(preds, Time)
      
      dat <- preds[dat, on = "Time", roll = TRUE]
      
      dat
    })
    data <- rbindlist(raw)
  }
  
  if(species == "HARTH_Human"){
    files <- list.files(file.path(base_path, "Data/Accelerometer/HARTH_Human/raw"), full.names = TRUE)
    dat <- lapply(files, function(x){
      dat <- fread(x)
      dat <- dat[,c(1:4,8)]
      colnames(dat) <- c("Time", "X", "Y", "Z", "Activity")
      dat$ID <- tools::file_path_sans_ext(basename(x))
      dat
    })
    data <- rbindlist(dat)
  }
  
  if(species == "Harris_Sheep"){
    files <- list.files(file.path(base_path, "Data/Accelerometer/Harris_Sheep/raw"), full.names = TRUE)
    data <- lapply(files, function(x){
      df <- fread(x)
      
      x_cols <- grep("^x_", names(df), value = TRUE)
      y_cols <- grep("^y_", names(df), value = TRUE)
      z_cols <- grep("^z_", names(df), value = TRUE)
      
      n_samples <- length(x_cols)
      
      df_long <- melt(
        df,
        id.vars = c("sheep_number", "time_stamp", "walking", "sitting", "standing",	"grazing",	"ruminating"),
        measure.vars = list(X = x_cols, Y = y_cols, Z = z_cols),
        variable.name = "sample_index",
        value.name = c("X", "Y", "Z")
      )
      
      # collapse the activity labels
      df_long$Activity <- data.table::fcase(
        df_long$walking == 1, "Walking",
        df_long$grazing == 1, "Grazing",
        df_long$sitting == 1, "Sitting",
        df_long$standing == 1, "Standing",
        df_long$ruminating == 1, "ruminating",
        default = "other"
      )
      
      df_long <- df_long[, -c("walking", "sitting", "standing",	"grazing",	"ruminating")]
      
      df_long[, sample_index := as.integer(sample_index)]
      # Sort so it reads in order
      setorder(df_long, sheep_number, time_stamp, sample_index)
      
      # now format and rename
      df_long <- df_long %>% select(!sample_index)
      colnames(df_long) <- c("ID", "Time", "X", "Y", "Z", "Activity")
      df_long
    })
    data <- rbindlist(data)
  }
  
  if(species == "HarveyCaroll_Pangolin"){
    files <- list.files(file.path(base_path, "Data/Accelerometer/HarveyCaroll_Pangolin/raw"), full.names = TRUE)
    data <- lapply(files, function(x){
      dat <- fread(x)
      dat$ID <- str_split(basename(x), "_", simplify = T)[1]
      dat <- dat[, c("ID", "time", "X", "Y", "Z", "Behavior")]
      dat <- dat %>% rename(Activity = Behavior)
      dat
    })
    data <- rbindlist(data)
  }
  
  if(species == "Marcato_Dog"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Marcato_Dog/df_raw.csv"))
    # save 3 other files
    chest_data <- data %>%
      rename(X = Chest.Acc.X, Y = Chest.Acc.Y, Z = Chest.Acc.Z, Activity = Position, Time = Timestamp, ID = Subject) %>%
      select(Time, ID, Activity, X, Y, Z)
    fwrite(chest_data, file.path(base_path, "Data/Accelerometer/Marcato_Dog/Chest_Marcato_Dog_reformatted.csv"))
    
    back_data <- data %>%
      rename(X = Back.Acc.X, Y = Back.Acc.Y, Z = Back.Acc.Z, Activity = Position, Time = Timestamp, ID = Subject) %>%
      select(Time, ID, Activity, X, Y, Z)
    fwrite(back_data, file.path(base_path, "Data/Accelerometer/Marcato_Dog/Back_Marcato_Dog_reformatted.csv"))
    
    collar_data <- data %>%
      rename(X = Neck.Acc.X, Y = Neck.Acc.Y, Z = Neck.Acc.Z, Activity = Position, Time = Timestamp, ID = Subject) %>%
      select(Time, ID, Activity, X, Y, Z)
    fwrite(back_data, file.path(base_path, "Data/Accelerometer/Marcato_Dog/Collar_Marcato_Dog_reformatted.csv"))
    
    data <- chest_data # this is the one to save as the main file
  }
  
  if(species == "MoralesVargas_Cow"){
    files <- list.files(file.path(base_path, "Data/Accelerometer/MoralesVargas_Cow"), full.names = TRUE)
    data <- lapply(files, function(x){
      df <- fread(x) %>%
        select(Time, BNO055_AX, BNO055_AY, BNO055_AZ)  %>%
        rename(X = BNO055_AX,
               Y = BNO055_AY,
               Z = BNO055_AZ) %>%
        mutate(X = X/9.8,
               Y = Y/9.8,
               Z = Z/9.8)
      df$ID <- str_split(basename(x), "_", simplify = TRUE)[3]
      df
    })
    data <- rbindlist(data)
    # all of this data was from walking trials
    data$Activity <- "Locomotion"
  }
  
  if(species == "Pagano_Bear"){
    # partially but not fully formatted elsewhere
    data <- fread(file.path(base_path, "Data/Accelerometer/Pagano_Bear/Pagano_Bear_formatted.csv")) %>%
      mutate(X = X/9.81,
             Y = Y/9.81,
             Z = Z/9.81)
  }
  
  if(species == "Smit_Cat"){
    
    data_types <- c("Chest", "Collar")
    
    for (type in data_types){
      files <- list.files(file.path(base_path, "Data/Accelerometer/Smit_Cat", type), full.names = TRUE)
      anno <- fread(file.path(base_path, "Data/Accelerometer/Smit_Cat/Annotations.csv"))
      
      data <- lapply(files, function(x){
        dat <- fread(x) %>%
          dplyr::rename(Time = Timestamp,
                        X = `Accelerometer X`,
                        Y = `Accelerometer Y`,
                        Z = `Accelerometer Z`)
        
        # ensure that the timezone for this data is set correctly
        # set the tz to UTC (DOUBLE CHECK THIS DOESNT CHANGE THE VALUE)
        dat[, Time := as.POSIXct(Time, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")]
        
        # select the data that falls within the annotation times
        relevant_annotations <- anno %>%
          dplyr::filter(ID == str_split(tools::file_path_sans_ext(basename(x)), "-")[[1]][3]) 
        
        Strt <- relevant_annotations %>% arrange(Time) %>% slice(1) %>% select(Time)
        Ed <- relevant_annotations %>% arrange(desc(Time)) %>% slice(1) %>% select(Time)
        
        # are the annotations while the cat is wearing the device?
        if (dat$Time[1]<Strt$Time){
          print("accel starts before the annotations do (good)")
        } else {
          print("something is wrong (annotations before accel)")
        }
        dat2 <- dat[Time > Strt$Time & Time < Ed$Time]
        
        setDT(dat2)
        setDT(relevant_annotations)
        
        # annotations were made per second whereas data is in 30Hz so we need to do some steps
        # add second-level time keys to join them together
        dat2[, Time_sec := as.POSIXct(floor(as.numeric(Time)), tz = "UTC")]
        relevant_annotations[, Time_sec := as.POSIXct(floor(as.numeric(Time)), tz = "UTC")]
        # ensure there is only one annotation per second (just in case)
        relevant_annotations <- relevant_annotations[ , .SD[1], by = Time_sec]
        # join
        labelled_dat <- merge(dat2, relevant_annotations,
                              by = "Time_sec",
                              all.x = TRUE
        )
        
        # remove the NA observations and clean up a bit
        labelled_dat <- labelled_dat %>%
          na.omit() %>%
          dplyr::rename(Time = Time.x) %>%
          select(-Time.y, -Time_sec)
        
        return(labelled_dat) # return this object
      })
      data <- rbindlist(data)
        
      # Recoding the behaviours -------------------------------------------------
      data <- data %>%
        mutate(Activity = case_when(
          # General Active
          Activity %in% c("Active_Climbing", "Active_Playfight.Fighting", "Active_Rubbing") ~ "Active",
          # Locomotion
          Activity %in% c("Active_Trotting","Active_Walking") ~ "Locomotion",
          Activity %in% c("Active_Jumping.Horizontal", "Active_Jumping.Vertical") ~ "Jumping",
          # Lying
          Activity %in% c("Inactive_Lying.Crouch", "Inactive_Lying.Down") ~ "Lying",
          # Sitting
          Activity %in% c("Inactive_Sitting.Down", "Inactive_Sitting.Stationary", "Inactive_Sitting.Up") ~ "Sitting",
          # Standing
          Activity %in% c("Inactive_Standing.Stationary", "Inactive_Standing.Up") ~ "Standing",
          # Grooming
          Activity %in% c("Maintenance_Grooming", "Maintenance_Scratching", "Maintenance_Shake.Body", "Maintenance_Shake.Head") ~ "Grooming",
          # Littering
          Activity %in% c("Maintenance_Littering.Digging", "Maintenance_Littering.None", "Maintenance_Littering.Urinating") ~ "Littering",
          # Eating
          Activity == "Maintenance_Nutrition.Eating" ~ "Eating",
          # other / synchronisation stuff
          Activity == "Other_Social.Allogrooming" ~ "Social",
          Activity %in% c("Other_Start", "Other_Outofsight", "Other_Other") ~ "Other"
        ))
      
        # Save the file -----------------------------------------------------------
        fwrite(data, file.path(base_path, "Data/Accelerometer/Smit_Cat", paste0(type, "_Smit_Cat_formatted.csv")))
    }
  }
  
  if(species == "Studd_Squirrel"){
    # partailly formatted as well
    data <- fread(file.path(base_path, "Data/Accelerometer/Studd_Squirrel/Studd_Squirrel_formatted.csv")) %>%
      select(Time, X, Y, Z, ID, FuncActivity) %>%
      rename(Activity = FuncActivity)
  }
  
  if(species == "Vehkaoja_Dog"){
    
    list_locomotion_labels <- c("Walking", "Pacing", "Galloping", "Trotting")
    
    data <- fread(file.path(base_path, "Data/Accelerometer/Vehkaoja_Dog/DogMoveData.csv")) %>% 
      mutate(Activity = ifelse(
        Behavior_1 %in% list_locomotion_labels | Behavior_2 %in% list_locomotion_labels | Behavior_3 %in% list_locomotion_labels,
        "Locomotion",
        "Other" # just set the remainder to other
      )) %>%
      mutate(Time = row_number())
    
    chest_data <- data %>% 
      rename(ID = DogID,
             X = ABack_x,
             Y = ABack_y,
             Z = ABack_z) %>%
      select(ID, Time, X, Y, Z, Activity)
    fwrite(chest_data, file.path(base_path, "Data/Accelerometer/Vehkaoja_Dog/Back_Vehkaoja_Dog_reformatted.csv"))
    
    # and now repeat it again for the other data
    neck_data <- data %>%
      rename(ID = DogID,
             X = ANeck_x,
             Y = ANeck_y,
             Z = ANeck_z) %>%
      select(ID, Time, X, Y, Z, Activity)
    fwrite(neck_data, file.path(base_path, "Data/Accelerometer/Vehkaoja_Dog/Collar_Vehkaoja_Dog_reformatted.csv"))
    
    data <- chest_data # set this one to be default returned
  }
  
  if(species == "Wijers_Lion"){
    data <- fread(file.path(base_path, "Data/Accelerometer/Wijers_Lion/SHUMBA_RAW_ACC.csv")) %>%
      rename(X = ACC_rawX, Y = ACC_rawY, Z = ACC_rawZ,
             Time = ACC_UTC,
             ID = lion,
             Activity = behaviour) %>%
      select(ID, Time, X, Y, Z, Activity)
    # rearrange so its a long dataframe 
    parse_vec <- function(x) as.numeric(strsplit(gsub("\\[|\\]", "", x), ",\\s*")[[1]])
    data <- data[, {
      x <- parse_vec(X)
      y <- parse_vec(Y)
      z <- parse_vec(Z)
      n <- length(x)
      .(Time = Time + seq_len(n) / 100,   # arbitraryu decomalisation so stays in thre right order
        X = x, Y = y, Z = z,
        Activity = Activity)
    }, by = .(ID, Time)]
    # now convert these numbers to Gs
    data <- data[, c(1:2, 4:7)]
    data <- data %>%
      mutate(X = X/9.8, Y = Y/9.8, Z = Z/9.8)
  }
  
  # datasets I formatted in an alternate github and am yet to copy the code over for
  if(species %in% c("Clemente_Echidna", "Mauny_Goat")){
    data <- fread(file.path(base_path, "Data/Accelerometer", species, paste0(species, "formatted.csv")))
  }
  
  
  return(data)
}


format_unlabelled_data <- function(species){
  
  if(species == "Buchmann_Ungulates"){
    
    freq <- 33.3
    # this deals with a heap of species
    species <- list.dirs(file.path(base_path, "Data/Accelerometer/Buchmann_Ungulates/"), recursive = FALSE)
    for (sp in species){
      
      # and deal with the 3 different ways it was collected
      for (position in c("breast", "leg", "neck")){
        files <- list.files(file.path(sp, "raw"), pattern = position, full.names = TRUE)
        df <- lapply(files, function(x){
          dat <- fread(x)
          colnames(dat) <- c("X", "Y", "Z", "Time")
          
          # convert to Gs
          dat$X <- dat$X / 9.8
          dat$X <- dat$X / 9.8
          dat$X <- dat$X / 9.8
          
          # assign the label based on the threshold
          dat <- get_vedba(dat, freq)
          dat$Activity <- ifelse(dat$vedba > 1, "Locomotion", "Other")
          
          # get the ID
          dat$ID <- str_split(basename(x), "_", simplify = T)[2]
            
          dat <- na.omit(dat)
          dat
        })
        df <- rbindlist(df)
        # adjust the names
        if(position == "breast"){ 
          position = "Chest"
          data <- df # for this case save as the final as well
        } else if (position == "neck"){
          position = "Collar"
        }
        fwrite(df, file.path(sp,  paste0(position, "_", basename(sp), "_reformatted.csv")))
      }
      fwrite(data, file.path(sp,  paste0(basename(sp), "_reformatted.csv")))
    }
  }
    
  if(species %in% c("Annett_Bettong", "Annett_Wallaby")){
    files <- list.files(file.path(base_path, "Data/Accelerometer", species), full.names = TRUE)
    dat <- lapply(files, function(x){
      raw <- fread(x)[, c(1:4)]
      colnames(raw) <- c("Time", "X", "Y", "Z")
      raw$ID <- tools::file_path_sans_ext(basename(x))
      
      # crop the first hour off
      crop_off <- 50*60*60
      raw <- raw[crop_off:nrow(raw), ]
      
      # vedba threshold for detecting hopping # experimented with threshold at bottom
      raw <- get_vedba(raw, freq)
      raw$Activity <- ifelse(raw$vedba > 1, "Locomotion", "Other")
      
      raw
    })
    data <- rbindlist(dat)
  }
  
  if(species == "Kangaroo_Annett"){
    source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Kangaroo.R"))
    return(NULL)
  }
  
    return(data)
}
