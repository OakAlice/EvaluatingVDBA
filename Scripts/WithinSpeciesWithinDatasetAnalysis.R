# Dataset Specific Testing ------------------------------------------------
# These daatsets are too big to be processed typically, therefore I had to break it up


# Function to process the large datasets ----------------------------------

process_species_vdba <- function(species,
                                 base_path,
                                 file_pattern = ".*\\.csv$",
                                 window_threshold = 5) {

  # Load mass of individuals
  mass_file <- file.path(base_path, "AccelerometerData", species, "Mass_of_Individuals.csv")
  mass_of_individuals <- fread(mass_file)
  
  # List raw files
  files <- list.files(file.path(base_path, "AccelerometerData", species),
                      pattern = file_pattern,
                      full.names = TRUE)
  
  all_data <- lapply(files, function(x) {
    
    collar_number <- tools::file_path_sans_ext(basename(x))
    
    summary_file <- file.path(base_path, "AccelerometerData", species,
                              paste0(species, "_", collar_number, "_summary.csv"))
    
    if (file.exists(summary_file)) {
      # Already processed
      vedba_stats <- fread(summary_file)
    } else {
      # Load and optionally subset data
      dat <- reformat_clemente_data(x)
      
      # Determine window length
      freq <- as.numeric(dataset_variables[Name == species]$Frequency)
      window <- ifelse(freq > window_threshold, 2, 5)
      window_samples <- window * freq
      
      # Process VDBA
      processed_data <- process_cont_VDBA(dat, window_length = window_samples)
      
      # Save processed raw file
      fwrite(processed_data,
             file.path(base_path, "AccelerometerData", species,
                       paste0(species, "_", collar_number, "_processed.csv")))
      
      # Threshold VDBA
      df <- processed_data %>%
        select(ID, vedba, odba) %>%
        na.omit()
      
      dens <- density(df$vedba, na.rm = TRUE)
      vedba_peak <- dens$x[which.max(dens$y)]
      inactive_vedba <- vedba_peak * 2
      
      df <- df %>%
        mutate(threshold = ifelse(vedba > inactive_vedba, "active", "inactive"))
      
      # Summarize
      summary <- df %>%
        group_by(ID, threshold) %>%
        summarise(
          meanVDBA = mean(vedba),
          minVDBA = min(vedba),
          maxVDBA = max(vedba),
          meanODBA = mean(odba),
          minODBA = min(odba),
          maxODBA = max(odba),
          .groups = "drop"
        )
      
      overall_summary <- df %>%
        group_by(ID) %>%
        summarise(
          meanVDBA = mean(vedba),
          minVDBA = min(vedba),
          maxVDBA = max(vedba),
          meanODBA = mean(odba),
          minODBA = min(odba),
          maxODBA = max(odba),
          .groups = "drop"
        ) %>%
        mutate(threshold = "all")
      
      vedba_stats <- rbind(summary, overall_summary)
      
      # Save summary
      fwrite(vedba_stats, summary_file)
    }
    
    vedba_stats
  })
  
  # Combine all files
  data <- rbindlist(all_data)
  
  # Save overall species summary
  fwrite(data, file.path(base_path, "AccelerometerData", species,
                         paste0(species, "_summary.csv")))
  
  # Add Collar number and merge with mass
  data$Collar <- as.numeric(str_extract(data$ID, "[0-9]+"))
  data <- merge(data, mass_of_individuals, by = "Collar")
  
  # Plot example
  ggplot(data, aes(x = Log_Mass, y = log(meanVDBA), colour = threshold)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle(paste("VDBA summary for", species))
  
  return(data)
}


# Code --------------------------------------------------------------------
# Impala
impala_data <- process_species_vdba(
  species = "Clemente_Impala",
  base_path = base_path,
  file_pattern = "^Collar_",
  window_threshold = 5
)

# Kangaroo
kangaroo_data <- process_species_vdba(
  species = "Annett_Kangaroo",
  base_path = base_path,
  file_pattern = "[Cc]ollar[0-9]{1,2}\\.csv$",
  window_threshold = 5
)

individual_kangaroo_files <- list.files(file.path(base_path, "AccelerometerData", "Annett_Kangaroo", "Individual_analysis"),
                                        pattern = "[Cc]ollar[0-9]{1,2}_processed\\.csv$",
                                        full.names = TRUE)
individual_impala_files <- list.files(file.path(base_path, "AccelerometerData", "Clemente_Impala", "Individual_analysis"),
                                        pattern = "_processed\\.csv$",
                                        full.names = TRUE)
files <- individual_impala_files

one_day <- as.numeric(dataset_variables[Name == species]$Frequency) * (60*60*max_samples)
subset_data <- lapply(files, function(x){
  dat <- fread(x)
  dat <- dat[, .SD[1:min(one_day, .N)]]
  dat
})
subset_data <- rbindlist(subset_data)
fwrite(subset_data, file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))
