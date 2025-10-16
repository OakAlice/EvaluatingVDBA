# Dataset Specific Testing ------------------------------------------------
# These daatsets are too big to be processed typically, therefore I had to break it up
# Impala ------------------------------------------------------------------
species <- "Clemente_Impala"
mass_of_individuals <- fread(file.path(base_path, "AccelerometerData", "Clemente_Impala", "Mass_of_Individuals.csv"))

# raw files
impala_files <- list.files(file.path(base_path, "AccelerometerData", "Clemente_Impala"), pattern = "^Collar_", full.names = TRUE)


# impala and kangaroo files are too big, system crashes if I do them all at once therefore, doing it file by file
# workflow for processing the individual data
data <- lapply(impala_files, function(x){
  
  # x <- impala_files[1]
  collar_number <- str_split(basename(x), "_")[[1]][2]
  
  # check if its already been done
  if (file.exists(file.path(base_path, "AccelerometerData", "Clemente_Impala", paste("Clemente_Impala", collar_number, "summary.csv", sep = "_")))){
    vedba_stats <- fread(file.path(base_path, "AccelerometerData", "Clemente_Impala", paste("Clemente_Impala", collar_number, "summary.csv", sep = "_")))
    
  } else {
    
    # load in the VDBA data directly
    dat <- reformat_clemente_data(x)
    
    # calculating the VDBA
    window <- ifelse(as.numeric(dataset_variables[Name == species]$Frequency)>5, 2, 5)
    window_samples <- window * as.numeric(dataset_variables[Name == species]$Frequency)
    
    processed_data <- process_cont_VDBA(dat, window_length = window_samples)
    
    # threshold it
    
    data <- processed_data %>%
      select(ID, vedba, odba) %>%
      na.omit()
    
    # find the vedba of the peak
    dens <- density(data$vedba, na.rm = TRUE)
    vedba_peak <- dens$x[which.max(dens$y)]
    
    inactive_vedba <- vedba_peak * 2 # double it for the threshold
    
    data <- data %>%
      mutate(threshold = ifelse(vedba > inactive_vedba, "active", "inactive"))
    
    # now we should take the means
    summary <- data %>%
      group_by(ID, threshold) %>%
      summarise(meanVDBA = mean(vedba),
                minVDBA = min(vedba),
                maxVDBA = max(vedba),
                meanODBA = mean(odba),
                minODBA = min(odba),
                maxODBA = max(odba)
      )
    
    overall_summary <- data %>%
      group_by(ID) %>%
      summarise(meanVDBA = mean(vedba),
                minVDBA = min(vedba),
                maxVDBA = max(vedba),
                meanODBA = mean(odba),
                minODBA = min(odba),
                maxODBA = max(odba)
      )
    overall_summary$threshold <- "all"
    
    vedba_stats <- rbind(summary, overall_summary)
  
    # save the summary
    fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, paste0(species, "_", collar_number, "_summary.csv")))
  }
  
  vedba_stats
})

data <- rbindlist(data)
fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))
# yay, save that and now it can be used in the rest of the plots

data$Collar <- as.numeric(sapply(str_split(data$ID, "_"), `[`, 2))
data <- merge(data, mass_of_individuals, by = "Collar")

ggplot(data, aes(x = Log_Mass, y = minVDBA, colour = threshold)) + geom_point() + geom_smooth(method = "lm")





# Annett Kangaroo ---------------------------------------------------------
species <- "Annett_Kangaroo"
mass_of_individuals <- fread(file.path(base_path, "AccelerometerData", "Annett_Kangaroo", "Mass_of_Individuals.csv"))

# raw files
kangaroo_files <- list.files(file.path(base_path, "AccelerometerData", "Annett_Kangaroo"), 
                             pattern = "[Cc]ollar[0-9]{1,2}\\.csv$", 
                             full.names = TRUE)



data <- lapply(kangaroo_files, function(x){
  
  # x <- impala_files[1]
  collar_number <- tools::file_path_sans_ext(basename(x))
  
  # check if its already been done
   
    # load in the VDBA data directly
    dat <- reformat_clemente_data(x)
    
    # calculating the VDBA
    window <- ifelse(as.numeric(dataset_variables[Name == species]$Frequency)>5, 2, 5)
    window_samples <- window * as.numeric(dataset_variables[Name == species]$Frequency)
    
    processed_data <- process_cont_VDBA(dat, window_length = window_samples)
    
    # threshold it
    
    data <- processed_data %>%
      select(ID, vedba, odba) %>%
      na.omit()
    
    # find the vedba of the peak
    dens <- density(data$vedba, na.rm = TRUE)
    vedba_peak <- dens$x[which.max(dens$y)]
    
    inactive_vedba <- vedba_peak * 2 # double it for the threshold
    
    data <- data %>%
      mutate(threshold = ifelse(vedba > inactive_vedba, "active", "inactive"))
    
    # now we should take the means
    summary <- data %>%
      group_by(ID, threshold) %>%
      summarise(meanVDBA = mean(vedba),
                minVDBA = min(vedba),
                maxVDBA = max(vedba),
                meanODBA = mean(odba),
                minODBA = min(odba),
                maxODBA = max(odba)
      )
    
    overall_summary <- data %>%
      group_by(ID) %>%
      summarise(meanVDBA = mean(vedba),
                minVDBA = min(vedba),
                maxVDBA = max(vedba),
                meanODBA = mean(odba),
                minODBA = min(odba),
                maxODBA = max(odba)
      )
    overall_summary$threshold <- "all"
    
    vedba_stats <- rbind(summary, overall_summary)
    
    # save the summary
    fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, paste0(species, "_", collar_number, "_summary.csv")))
  
  vedba_stats
})

data <- rbindlist(data)
fwrite(data, file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))

data$Collar <- as.numeric(str_extract(data$ID, "[0-9]+"))
data <- merge(data, mass_of_individuals, by = "Collar")

ggplot(data, aes(x = Log_Mass, y = log(meanVDBA), colour = threshold)) + geom_point() + geom_smooth(method = "lm")


