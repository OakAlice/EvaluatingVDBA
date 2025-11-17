# generate the vdba and summary -------------------------------------------

cleaned_file <- file.path(base_path, "AccelerometerData", species, paste0(species, "_cleaned_reformatted.csv"))
if (file.exists(cleaned_file)){
   accel <- fread(cleaned_file)
} else {
  accel <- fread(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))
}

accel <- generate_vdba(accel, species, dataset_variables)

# save the processed files -- intermediary so I can do other things with it later
# fwrite(accel, file.path(base_path, "AccelerometerData", species, 
#                       paste0(species, "_processed.csv")))

sampling_style <- dataset_variables[Name == species]$SamplingStyle
if (sampling_style == "Continuous"){
  accel <- smooth_vdba(accel, species, dataset_variables, window = 1)
}

vedba_stats <- summarise_vdba(accel, species, dataset_variables)
  
fwrite(vedba_stats, file.path(base_path, "AccelerometerData", species, 
                                paste0(species, "_summary.csv")))
