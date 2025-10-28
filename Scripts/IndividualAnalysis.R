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
  mass_file <- file.path(base_path, "AccelerometerData", species, "Mass_of_Individuals.csv")
  if (!file.exists(mass_file)) stop("Mass file not found: ", mass_file)
  mass_of_individuals <- fread(mass_file)
  
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  if (is.na(freq)) stop("Frequency missing for species: ", species)
  win <- 10 * freq  # smoothing window
  
  #Process each individual file (generally too big to do all at once)
  files <- list.files(
    file.path(base_path, "AccelerometerData", species),
    pattern = file_pattern,
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
      dat <- reformat_clemente_data(file)
      dat[, ID := collar_number]
      
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
      
      static_idx <- which(rolling_sd < quantile(rolling_sd, 0.25, na.rm = TRUE))
      threshold_pct <- max(dat[static_idx, vedba], na.rm = TRUE)
      
      dat[, threshold := ifelse(vedba > threshold_pct, "active", "inactive")]
      
      # save that
      output_dir <- file.path(base_path, "AccelerometerData", species, "Individual_Analyses")
      # dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      
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

