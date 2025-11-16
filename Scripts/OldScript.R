# Dataset Specific Testing ------------------------------------------------
# These daatsets are too big to be processed typically, therefore I had to break it up

# Function to process the large datasets ----------------------------------
process_individual_vdba <- function(
    species,
    base_path,
    file_pattern = ".*\\.csv$",
    window_threshold = 5
) {
  #Load
  mass_file <- file.path(base_path, "Mass_of_individuals", paste0(species, ".csv"))
  if (!file.exists(mass_file)) stop("Mass file not found: ", mass_file)
  mass_of_individuals <- fread(mass_file)
  
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  if (is.na(freq)) stop("Frequency missing for species: ", species)
  win <- 10 * freq  # smoothing window
  
  #Process each individual file (generally too big to do all at once)
  files <- list.files(
    file.path(base_path, "AccelerometerData", species, "raw"),
    full.names = TRUE
  )
  
  all_data <- lapply(files, function(file) {
    tryCatch({
      collar_number <- tools::file_path_sans_ext(basename(file))
      
      # Species-specific collar ID extraction
      collar_number <- switch(
        species,
        "Gaschk_Quoll"     = str_split(collar_number, "_")[[1]][1],
        "Clemente_Impala"  = str_split(collar_number, "_")[[1]][2],
        "Annett_Kangaroo"  = str_extract(collar_number, "\\d+"),
        collar_number
      )
      
      # Format the data into a standardised format
      dat <- fread(file)
      dat <- dat[, 1:5]
      colnames(dat) <- c("ID", "Time", "Accel.X", "Accel.Y", "Accel.Z")
      # dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
      dat[, ID := collar_number]
      
      output_dir <- file.path(base_path, "AccelerometerData", species, "raw")
      # dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      fwrite(
        dat,
        file.path(output_dir, paste0(species, "_", collar_number, "_raw.csv"))
      )
      
      # calculate VDBA across each rolling window
      dat[, `:=`(
        ax_static = frollmean(Accel.X, n = win, align = "center", fill = NA),
        ay_static = frollmean(Accel.Y, n = win, align = "center", fill = NA),
        az_static = frollmean(Accel.Z, n = win, align = "center", fill = NA)
      )]
      
      dat[, `:=`(
        vedba = sqrt((Accel.X - ax_static)^2 + 
                       (Accel.Y - ay_static)^2 + 
                       (Accel.Z - az_static)^2)
      )]
      
      # threshold into active and inactive 
      dat[, rolling_sd := roll_sd(vedba, n = win, fill = NA, align = "center")]
      
      static_idx <- which(dat$rolling_sd < quantile(dat$rolling_sd, 0.25, na.rm = TRUE))
      threshold_pct <- max(dat[static_idx, vedba], na.rm = TRUE)
      
      dat[, threshold := ifelse(vedba > threshold_pct, "active", "inactive")]
      
      # save that
    
      fwrite(
        dat,
        file.path(output_dir, paste0(species, "_", collar_number, "_processed.csv"))
      )
      
      # Summaries for each individual
      summary_active <- dat[, .(
        meanVDBA = mean(vedba, na.rm = TRUE),
        minVDBA  = min(vedba, na.rm = TRUE),
        maxVDBA  = max(vedba, na.rm = TRUE)
      ), by = .(ID, threshold)]
      
      summary_overall <- dat[, .(
        meanVDBA = mean(vedba, na.rm = TRUE),
        minVDBA  = min(vedba, na.rm = TRUE),
        maxVDBA  = max(vedba, na.rm = TRUE)
      ), by = ID][, threshold := "all"]
      
      rbind(summary_active, summary_overall)
      
    }, error = function(e) {
      message("Skipping file ", basename(file), ": ", conditionMessage(e))
      NULL
    })
  })
  
  # returning this from the lapply
  rbindlist(Filter(Negate(is.null), all_data), fill = TRUE)
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
fwrite(kangaroo_data, file.path(base_path, "AccelerometerData", "Annett_Kangaroo", "Annett_Kangaroo_summary2.csv"))

quoll_data <- process_individual_vdba(
  species = "Gaschk_Quoll",
  base_path = base_path,
  file_pattern = "_[0-9]{5}.csv$",
  window_threshold = 5
)

# Analysis ----------------------------------------------------------------
masses <- fread(file.path(base_path, "Mass_of_individuals", "Annett_Kangaroo.csv"))
kangaroo_data <- fread(file.path(base_path, "AccelerometerData", "Annett_Kangaroo",
                                 "Annett_Kangaroo_summary2.csv")) %>%
  mutate(ID = as.numeric(ID))
kangaroo_data <- merge(kangaroo_data, masses, by = "ID")




# Plot each species independently
p1 <- ggplot(kangaroo_data, aes(x = LogMass, y = log(meanVDBA), colour = threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
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