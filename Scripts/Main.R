# Main script for processing the other scripts ----------------------------

# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

# Packages and settings ---------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(pacman)
p_load(tidyverse,
       data.table,
       tsfeatures,
       future,
       RcppRoll,
       arrow,
       zoo,
       R.matlab,
       rhdf5
       )

source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))

# Within Species and Dataset Analysis -------------------------------------
source(file = file.path(base_path, "Scripts", "WithinSpeciesWithinDatasetAnalysis.R"))

# Within Species Across Datsets Analysis ----------------------------------
source(file = file.path(base_path, "Scripts", "WithinSpeciesAcrossDatasetAnalysis.R"))





# Main Multi-Species Comparison -------------------------------------------
max_samples <- 24 # in hours # the maximum samples from any individual
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")

species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

for (dataset in species_list){
  
  species <- basename(dataset)
  print(species)
  
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo", "Minasandra_Hyena", "Kamminga_Horse")){
    next
  }
  
  # Formatting all data sources into same structure
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))){
    print("already formatted")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))
  }
  
  # Accounting for different brands and acceleration scales and calculating vdba
  #if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))){
  #  print("already rescaled")
  #} else {
    source(file = file.path(base_path, "Scripts", "CalibratingDevices.R"))
  #}
  
  # Finding the threshold between active and inactive for each species  
  #if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))){
  #  print("already summarised")
  #} else {
    # source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))
  #}
  
}



# Checking the rescaling --------------------------------------------------
# load in the rescaled files
processed_files <- lapply(species_list, function(x) {
   list.files(x, pattern = "_processed.csv$", full.names = TRUE)
})
processed_files <- unlist(processed_files)

summary_stats <- lapply(processed_files, function(x) {
  print(paste0("processing ", x))
  fread(x) %>%
    summarise(
      MaxX = max(Accel.X, na.rm = TRUE),
      MinX = min(Accel.X, na.rm = TRUE),
      MaxDynX = max(ax_dynamic, na.rm = TRUE),
      MinDynX = min(ax_dynamic, na.rm = TRUE),
      MaxStatX = max(ax_static, na.rm = TRUE),
      MinStatX = min(ax_static, na.rm = TRUE),
      MaxVDBA = max(vedba, na.rm = TRUE),
      MinVDBA = min(vedba, na.rm = TRUE)
    ) %>%
    mutate(dataset = paste(str_split(basename(x), "_")[[1]][1],
                           str_split(basename(x), "_")[[1]][2], sep = "_"))
})
summary <- rbindlist(summary_stats)
dataset_variables$dataset <- dataset_variables$Name
summary <- merge(summary, dataset_variables, by = "dataset")

# plot this
ggplot(summary %>% filter(Type == "Mam"), aes(x = dataset, y= MaxX, colour = Device)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))







# Plotting these results
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))
