# Main script for multi-species Vdba analysis ----------------------------

# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"
source(file.path(base_path, "Scripts/config.R"))

# Get all datasets into a consistent format -------------------------------
for (species in species_list){
  if(!file.exists(file.path(base_path, "Data/AxivityAccelerometer", species, paste0(species, "_reformatted.csv")))){
    # formatting
    source(file = file.path(base_path, "Scripts", "FreeRoamingAnalysis", "FormattingAndProcessing", "FormattingRawData.R"))

    # filtering and cleaning
    # source(file = file.path(base_path, "Scripts", "FormattingAndProcessing", "CleanFormattedData.R"))
  }
}

# Generating VBDA ---------------------------------------------------------
for (species in species_list){
  print(species)
  
  window_seconds <- 1
  smooth_window_seconds <- 5
  freq <- as.numeric(dataset_variables[Name == species]$Frequency)
  
  files <- list.files(file.path(base_path, "Data/AxivityAccelerometer", species), pattern = "_reformatted.csv", full.names = TRUE)
  
  summaries <- lapply(files, function(x){
    accel <- fread(x)
    accel <- generate_vdba(accel, freq, window_seconds)
    accel <- smooth_vdba(accel, freq, smooth_window_seconds)
    vedba_stats <- summarise_vdba(accel, freq, window_seconds) 
  })
  summaries <- rbindlist(summaries)
  
# Add in the mass ---------------------------------------------------------
  if(file.exists(file.path(base_path, "Data/AxivityAccelerometer", species, "Mass_of_individuals.csv"))){
    ind_masses <- fread(file.path(base_path, "Data/AxivityAccelerometer", species, "Mass_of_individuals.csv"))
    summaries <- merge(summaries, ind_masses, by = "ID")
  } else {
    mass <- as.numeric(dataset_variables[Name == species]$Mass_kg)
    summaries$Mass <- mass
  }

  fwrite(summaries, file.path(base_path, "Output", paste0(species, "_summary.csv")))
}

# Analysis -----------------------------------------------------------------
files <- list.files(file.path(base_path, "Output"), pattern = "_summary\\.csv$", full.names = TRUE)
all_summaries <- rbindlist(
  lapply(files, function(x){
    fread(x) %>% mutate(species = gsub("_summary.csv", "", basename(x)))
    })
)
all_summaries$LogMass <- log(all_summaries$Mass)
all_summaries <- na.omit(all_summaries)

# ggplot(all_summaries, aes(x = LogMass, y = meanVDBA, colour = species)) +
#   geom_point(size = 4) +
#   geom_smooth(method = "lm", colour = "darkgrey") +
#   scale_colour_manual(values = selected_colours) +
#   labs(x = "nlog body mass (kg)", y = "Mean nlog sVDBA (g)") +
#   my_theme()

ggplot(all_summaries %>% dplyr::filter(species %in% c("Annett_Kangaroo", "Clemente_Impala", "Gaschk_Quoll", "Sparkes_Koala")),
       aes(x = LogMass, y = meanVDBA, colour = species)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", colour = "darkgrey") +
  scale_colour_manual(values = selected_colours) +
  labs(x = "nlog body mass (kg)", y = "Mean nlog sVDBA (g)") +
  my_theme() + 
  facet_wrap(~species, scales = "free")
