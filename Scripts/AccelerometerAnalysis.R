# Accelerometer Analysis --------------------------------------------------
# Undertaking VDBA analysis of strides for the accelerometer datasets
# Each of these scripts loads in the data, makes a walking detection model (various),
# extracts the walking, and calculates the VDBA


dataset_variables <- fread(file.path(base_path, "Data/Accelerometer/Dataset_Variables.csv"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Functions.R"))


labelled_species <- basename(list.dirs(file.path(base_path, "Data/Accelerometer"), recursive = FALSE))
# TODO: figure out a workarpound for the threshold species...

for (species in labelled_species){
  
  if(species %in% c("Annett_Kangaroo", "Sparkes_Koala")){
    # note that, for now, Annett_Kangaroo and Sparkes_Koala still have custom scripts
    source(file.path(base_path, "Scripts/AccelerometerAnalysis", paste0(species, ".R")))
    next
  }
  
  # variables
  freq <- dataset_variables$Frequency[dataset_variables$Name == species]
  stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == species]
  list_locomotion_labels <- dataset_variables$LocomotionLabels[dataset_variables$Name == species]
  
  # function that formats the data if necessary
  data <- format_labelled_data(species) 
  fwrite(data, file.path(base_path, "Data/Accelerometer", species, paste0(species, "_reformatted.csv")))
  
  # Get Vedba
  if(species == "Studd_Squirrel"){
    win <- 5 * freq 
    ax_static <- frollmean(data$X, n = win, align = "center", fill = NA)
    ay_static <- frollmean(data$Y, n = win, align = "center", fill = NA)
    az_static <- frollmean(data$Z, n = win, align = "center", fill = NA)
    ax_dynamic <- data$X - ax_static
    ay_dynamic <- data$Y - ay_static
    az_dynamic <- data$Z - az_static
    data$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
  } else {
    data <- get_vedba(data, freq)
  }
  
  # Get locomotion
  data <- get_locomotion(data, freq, stride_window, list_locomotion_labels)
  
  # Summarise
  # now take a mean across each window
  summary <- data %>%
    group_by(ID, wind_id) %>%
    summarise(mean_vedba = mean(vedba, na.rm = TRUE),
              max_vedba = max(vedba, na.rm = TRUE))
  
  # Add animal mass
  if(file.exists(file.path(base_path, "Data/Accelerometer/", species, "Mass_of_individuals.csv"))){
    animal_mass <- fread(file.path(base_path, "Data/Accelerometer/", species, "Mass_of_individuals.csv")) %>%
      select(ID, LogMass)
    summary <- merge(summary, animal_mass, by = "ID")
  } else {
    animal_mass <- dataset_variables$LogMass[dataset_variables$Name == species]
    summary$LogMass <- animal_mass
  }
  
  # Final summarisation
  summ_stats <- summary %>%
    group_by(ID, LogMass) %>%
    summarise(
      mean_vedba_raw = mean(mean_vedba, na.rm = TRUE),
      sd_vedba_raw = sd(mean_vedba, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se_vedba_raw = sd_vedba_raw / sqrt(n),
      logmean = log10(mean_vedba_raw),
      log_upper = log10(mean_vedba_raw + se_vedba_raw),
      log_lower = log10(mean_vedba_raw - se_vedba_raw)
    ) %>%
    mutate(ID = as.character(ID))
  
  fwrite(summ_stats, file.path(base_path, "Output", paste0(species, "Accelerometer_summary_stats.csv")))
  
}



# Compare them in one place -----------------------------------------------
files <- list.files(file.path(base_path, "Output"), pattern = "_summary_stats.csv", full.names = TRUE)
all_data <- lapply(files, function(x){
  dat <- fread(x)
  dat$Species <- gsub("Accelerometer_summary_stats.csv", "", basename(x))
  dat
})
all_data <- rbindlist(all_data, fill = TRUE)
merged_data <- merge(all_data, dataset_variables, by.x = "Species", by.y = "Name")
merged_data <- merged_data %>% 
  mutate(Category = ifelse(Category == "Marsupial_Quadruped", "Mammal_Quadruped", Category)) %>%
 dplyr::filter(!Species == "Caramaschi_Human") # this one is on a different scale

# Labels
ggplot(merged_data %>% dplyr::filter(LocomotionDetection == "Labels" | LocomotionType == "BipedalHopper"), 
       aes(x = LogMass.x, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")



# Non-collars
ggplot(merged_data %>% dplyr::filter(!DeviceAttachment == "Collar"), 
       aes(x = LogMass.x, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


# Axivity only
ggplot(merged_data %>% dplyr::filter(Device == "Axivity"), 
       aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


# Buchmann only
ggplot(merged_data %>% dplyr::filter(Device == "Buchmann"), 
       aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


