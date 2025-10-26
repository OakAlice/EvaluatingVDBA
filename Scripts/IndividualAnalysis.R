# Dataset Specific Testing ------------------------------------------------
# These daatsets are too big to be processed typically, therefore I had to break it up

# Function to process the large datasets ----------------------------------
process_individual_vdba <- function(species,
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
    if(species == "Gaschk_Quoll"){
      collar_number <- str_split(collar_number, "_")[[1]][1]
    } else if (species == "Clemente_Impala"){
      collar_number <- str_split(collar_number, "_")[[1]][2]
    } else if (species == "Annett_Kangaroo"){
      collar_number <- str_extract(tools::file_path_sans_ext(collar_number), "\\d+")
    }
    
    summary_file <- file.path(base_path, "AccelerometerData", species,
                              paste0(species, "_", collar_number, "_summary.csv"))
    
    if (file.exists(summary_file)) {
      # Already processed
      vedba_stats <- fread(summary_file)
    } else {
      # Load and optionally subset data
      dat <- reformat_clemente_data(x)
      dat$ID <- collar_number
      
      # Determine window length
      freq <- as.numeric(dataset_variables[Name == species]$Frequency)
      win <- 10 * freq
      
      # Calculate the vdba ------------------------------------------------------
      # Process VDBA
        ax_static <- frollmean(dat$Accel.X, n = win, align = "center", fill = NA)
        ay_static <- frollmean(dat$Accel.Y, n = win, align = "center", fill = NA)
        az_static <- frollmean(dat$Accel.Z, n = win, align = "center", fill = NA)
        
        # static_accel <- mean(sqrt(ax_static^2 + ay_static^2 + az_static^2), na.rm = TRUE)
        
        # get the dynamic component 
        ax_dynamic <- dat$Accel.X - ax_static
        ay_dynamic <- dat$Accel.Y - ay_static
        az_dynamic <- dat$Accel.Z - az_static
        
        vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
        dat$vedba <- vedba
      
      # Save processed raw file
      fwrite(dat,
              file.path(base_path, "AccelerometerData", species, "Individual_Analyses",
                        paste0(species, "_", collar_number, "_processed.csv")))
        
      # Threshold VDBA ----------------------------------------------------------
      df <- dat %>%
        select(ID, vedba) %>%
        na.omit()
      
      # ggplot(dat, aes(x = vedba)) + geom_density()
      # ggplot(dat[1:10000, ], aes(x = seq(1:10000), y = Accel.X, colour = threshold)) + geom_point()
      
      # find the max of the static periods
      dat$rolling_sd <- roll_sd(vedba, n = win, fill = NA, align = "center") # get the sd of the rolling windows
      static_idx <- which(dat$rolling_sd < quantile(dat$rolling_sd, 0.25, na.rm = TRUE)) # periods of flat
      static_accel <- dat[static_idx, ]
      threshold_pct <- max(static_accel$vedba, na.rm = TRUE) # max acceleration in the flat periods
      
      df <- df %>%
        mutate(threshold = ifelse(vedba > threshold_pct, "active", "inactive"))
      
      # ggplot(df[1:10000, ], aes(x = seq(1:10000), y = vedba, colour = threshold)) + geom_point()
      
      fwrite(df, file.path(base_path, "AccelerometerData", species, 
                             "Individual_Analyses",
                             paste0(species, collar_number, "_summary.csv")))
    
      # Summarise ---------------------------------------------------------------
      summary <- df %>%
        group_by(ID, threshold) %>%
        summarise(
          meanVDBA = mean(vedba),
          minVDBA = min(vedba),
          maxVDBA = max(vedba),
          .groups = "drop"
        )
      
      overall_summary <- df %>%
        group_by(ID) %>%
        summarise(
          meanVDBA = mean(vedba),
          minVDBA = min(vedba),
          maxVDBA = max(vedba),
          .groups = "drop"
        ) %>%
        mutate(threshold = "all")
      
      vedba_stats <- rbind(summary, overall_summary)
    }
    
    vedba_stats
  })
  
  # Combine all files
  data <- rbindlist(all_data)
  
  # Add Collar number and merge with mass
  if (species == "Clemente_Impala"){
    data$ID <- as.character(data$ID)
    mass_of_individuals$ID <- as.character(mass_of_individuals$ID)
    data <- merge(data, mass_of_individuals, by = "ID")
  } else if (species == "Gaschk_Quoll"){
    data$ID <- tolower(data$ID)
    data <- merge(data, mass_of_individuals, by = "ID")
  } else if (species == "Annett_Kangaroo"){
    data$ID <- as.character(data$ID)
    mass_of_individuals$ID <- as.character(mass_of_individuals$ID)
    data <- merge(data, mass_of_individuals, by = "ID")
  }
  
  # Save overall species summary
  fwrite(data, file.path(base_path, "AccelerometerData", species, 
                         "Individual_Analyses",
                         paste0(species, "_summary.csv")))
  
  return(data)
}

reformat_clemente_data <- function(x){
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
  dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
}

# Code --------------------------------------------------------------------
# Impala
impala_data <- process_individual_vdba(
  species = "Clemente_Impala",
  base_path = base_path,
  file_pattern = "^Collar_",
  window_threshold = 5
)

# Kangaroo
kangaroo_data <- process_individual_vdba(
  species = "Annett_Kangaroo",
  base_path = base_path,
  file_pattern = "[Cc]ollar[0-9]{1,2}\\.csv$",
  window_threshold = 5
)

quoll_data <- process_individual_vdba(
  species = "Gaschk_Quoll",
  base_path = base_path,
  file_pattern = "_[0-9]{5}.csv$",
  window_threshold = 5
)

# Analysis ----------------------------------------------------------------

impala_data <- fread(file.path(base_path, "AccelerometerData", "Clemente_Impala",
                        "Individual_Analyses", "Clemente_Impala_summary.csv")) %>%
  mutate(Dataset = "Clemente_Impala")

kangaroo_data <- fread(file.path(base_path, "AccelerometerData", "Annett_Kangaroo",
                                 "Individual_Analyses", "Annett_Kangaroo_summary.csv")) %>%
  mutate(Dataset = "Annett_Kangaroo")

quoll_data <- fread(file.path(base_path, "AccelerometerData", "Gaschk_Quoll",
                                 "Individual_Analyses", "Gaschk_Quoll_summary.csv")) %>%
  mutate(Dataset = "Gaschk_Quoll")

all_data <- rbind(impala_data, kangaroo_data, quoll_data, fill = TRUE) %>%
  select(Dataset, ID, Sex, threshold, meanVDBA, minVDBA, maxVDBA, LogMass)


# Plot each species independently
p1 <- ggplot(all_data, aes(x = LogMass, y = log(meanVDBA), colour = threshold, shape = Sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Dataset, scales = "free") +
  theme_minimal()

# Save the plot
ggsave(
  filename = file.path(base_path, "Output", "Plots", "Species_Individuals_VDBA_Mass.png"),
  plot = p1,
  width = 8, height = 6, dpi = 300 
)

# plot all the individuals togetehr
p2 <- ggplot(all_data, aes(x = LogMass, y = log(meanVDBA), colour = threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Save the plot
ggsave(
  filename = file.path(base_path, "Output", "Plots", "All_Individuals_VDBA_Mass.png"),
  plot = p2,
  width = 8, height = 6, dpi = 300 
)

