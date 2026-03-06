# Checking for outliers --------------------------------------------------

if (species == "Clemente_Impala" | species == "Annett_Kangaroo"){
    
  accel <- fread(file.path(base_path, "AccelerometerData", species, 
                           paste0(species, "_", window_seconds, "_processed.csv")))
  
  # remove outliers
  accel <- accel %>%
    dplyr::filter(seconds_VDBA < 2)
  
  
  ggplot(accel, aes(x = seconds_VDBA, fill = species)) + 
    geom_histogram(binwidth = 0.1) +
    scale_fill_manual(values = fave_colours) +
    my_theme() +
    theme(legend.position = "none")
  

  
}
