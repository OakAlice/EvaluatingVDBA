# generate the vdba and summary -------------------------------------------

# Gs_scaling <- fread(file.path(base_path, "Output", "G_scaling.csv"))
# species_list <- na.omit(unique(unlist(Gs$species)))
# Calib_species <- na.omit(unique(unlist(Gs$species[Gs$rescale == "Other"])))
# Gs_species <- na.omit(unique(unlist(Gs$species[Gs$rescale == "G"])))
# 
# if (species %in% Gs_species){
   accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_smoothed.csv")))
# } else {
#  accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_recalibrated.csv")))
# }

accel <- generate_vdba(accel, species, dataset_variables)
  
# save the processed files -- intermediary so I can do other things with it later
fwrite(accel, file.path(base_path, "AccelerometerData", species, 
                      paste0(species, "_processed.csv")))
  
vedba_stats <- summarise_vdba(accel, species, dataset_variables)
  
fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, 
                                paste0(species, "_summary.csv")))
