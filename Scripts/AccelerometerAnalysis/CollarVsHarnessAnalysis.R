# Collar vs Harness Analysis ----------------------------------------------
# Side analysis to see whether the collar data can be used or has too much shake

# Compile species trhat have harness and collar data
# Generate both, and compare


double_species <- c("Smit_Cat", 
                    "Vehkaoja_Dog", 
                    "Marcato_Dog", 
                    "Buchmann_Ungulates/Buchmann_Camel",
                    "Buchmann_Ungulates/Buchmann_CameroonSheep",
                    "Buchmann_Ungulates/Buchmann_Cow",
                    "Buchmann_Ungulates/Buchmann_Donkey",
                    "Buchmann_Ungulates/Buchmann_DwarfGoat",
                    "Buchmann_Ungulates/Buchmann_Goat",
                    "Buchmann_Ungulates/Buchmann_Horse",
                    "Buchmann_Ungulates/Buchmann_MerinoSheep",
                    "Buchmann_Ungulates/Buchmann_Mule",
                    "Buchmann_Ungulates/Buchmann_Pig"
                    )

summary_sets <- list()
for (sp in double_species){
  
  print(sp)
  
  # variables
  is_buchmann <- grepl("Buchmann", sp)
  freq <- if (is_buchmann) 33.3 else dataset_variables$Frequency[dataset_variables$Name == sp]
  animal_name <- if (is_buchmann) basename(sp) else sp
  stride_window <- 3
  list_locomotion_labels <- if (is_buchmann) "Locomotion" else dataset_variables$LocomotionLabels[dataset_variables$Name == sp]
  
  # pull oput the options
  options <- list.files(file.path(base_path, "Data/Accelerometer", sp), pattern = "formatted.csv", full.names = TRUE)
  options <- options[grepl(paste(c("Chest", "Collar", "Back", "leg"), collapse = "|"), options)]
  
  # calculate and summarise the vedba
  summary_data <- lapply(options, function(x){
    df <- fread(x)
    df <- get_vedba(df, freq)
    df <- get_locomotion(df, freq, stride_window, list_locomotion_labels)
    
    # now take a mean across each window
    summary <- df %>%
      group_by(ID, wind_id) %>%
      summarise(mean_vedba = mean(vedba, na.rm = TRUE),
                max_vedba = max(vedba, na.rm = TRUE))
    
    # Add animal mass
    if(file.exists(file.path(base_path, "Data/Accelerometer/", sp, "Mass_of_individuals.csv"))){
      animal_mass <- fread(file.path(base_path, "Data/Accelerometer/", sp, "Mass_of_individuals.csv")) %>%
        select(ID, LogMass)
      summary <- merge(summary, animal_mass, by = "ID")
    } else {
      # get the generic masses
      animal_mass <- dataset_variables$LogMass[dataset_variables$Name == animal_name]
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
    
    summ_stats$Species = animal_name
    summ_stats$Position = str_split(basename(x), "_", simplify = T)[1]
    
    return(summ_stats)
  })
  
  summary_data <- rbindlist(summary_data)
  
  summary_sets[[sp]] <- summary_data
}

summary_datasets <- rbindlist(summary_sets)
fwrite(summary_datasets, file.path(base_path, "Output", paste0("Collar_vs_Harness_summary_stats.csv")))


# POlots ------------------------------------------------------------------
ggplot(summary_datasets, 
       aes(x = Position, y = logmean, colour = Species)) + 
  geom_line(aes(group = ID)) +
  geom_point(size = 3) +
  my_theme() + 
  labs(x = "Device Position", y = "Log mean VDBA (g)") +
  theme(legend.position = "none") + 
  facet_wrap(~Species, scales = "free")


ggplot(summary_datasets %>% dplyr::filter(grepl("Buchmann", Species)), 
       aes(x = LogMass, y = logmean, colour = Species)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right")+ 
  facet_wrap(~Position, scales = "free")

ggplot(summary_datasets %>% dplyr::filter(grepl("Buchmann", Species)), 
       aes(x = LogMass, y = logmean, colour = Species, shape = Position)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = Position, linetype = Position),
              colour = "black", se = FALSE, linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  my_theme() + 
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right")
