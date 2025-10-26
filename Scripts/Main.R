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
       rhdf5,
       glmmTMB
       )

source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))
dataset_variables <- fread(file.path(base_path, "Dataset_Variables.csv"))



# Experimenting with Sampling Rate ----------------------------------------
source(file = file.path(base_path, "Scripts", "SamplingRateExperiment.R"))



# Within Species and Dataset Analysis -------------------------------------
source(file = file.path(base_path, "Scripts", "IndividualAnalysis.R"))



# Main Multi-Species Comparison -------------------------------------------
max_samples <- 24 # in hours # the maximum samples from any individual
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")

species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)

for (dataset in species_list){
  
  species <- basename(dataset)
  print(species)
  
  if (species %in% c("Clemente_Impala", "Annett_Kangaroo")){
    next
  }
  
  # Formatting all data sources into same structure
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_reformatted.csv")))){
    print("already formatted")
  } else {
    source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))
  }
  
  # Accounting for different brands and acceleration scales and calculating vdba
  if (file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_processed.csv")))){
    print("already rescaled")
  } else {
    source(file = file.path(base_path, "Scripts", "CalibratingDevices.R"))
  }
  
  # Finding the threshold between active and inactive for each species  
  if(file.exists(file.path(base_path, "AccelerometerData", species, paste0(species, "_summary.csv")))){
    print("already summarised")
  } else {
     source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))
  }
  
}












# Checking the rescaling --------------------------------------------------
# load in the rescaled files
processed_files <- lapply(species_list, function(x) {
   list.files(x, pattern = "_processed.csv$", full.names = TRUE)
})
processed_files <- unlist(processed_files)
excluded_species <- c("Clemente_Impala", "Annett_Kangaroo", "Minasandra_Hyena", "Kamminga_Horse")
processed_files <- processed_files[!basename(dirname(processed_files)) %in% excluded_species]

data <- lapply(processed_files, function(x) {
  print(paste0("processing ", x))
  dat <- fread(x) 
  # Filter only columns that exist in dat (because non-rescaled ones didnt change)
  # fill if they were empty (i.e., not reca;librated)
  for (i in 2:5) {
    col_name <- paste0("vedba", i)
    
    if (!col_name %in% names(dat)) {
      # If column doesn't exist, create it as a copy of vedba1
      dat[[col_name]] <- dat$vedba1
    }
  }
  
  summary_exprs <- map(c("vedba1", "vedba2", "vedba3", "vedba4", "vedba5"), function(col) {
    exprs(
      !!paste0("min_", col) := min(!!sym(col), na.rm = TRUE),
      !!paste0("mean_", col) := mean(!!sym(col), na.rm = TRUE),
      !!paste0("max_", col) := max(!!sym(col), na.rm = TRUE)
    )
  }) |> unlist(recursive = FALSE)
  summary <- dat %>% summarise(!!!summary_exprs)
  summary$dataset <- paste(str_split(basename(x), "_")[[1]][1],
                                     str_split(basename(x), "_")[[1]][2], sep = "_")
  summary
})

summary <- rbindlist(data, fill = TRUE)
dataset_variables$dataset <- dataset_variables$Name
summary <- merge(summary, dataset_variables, by = "dataset")


# plot this
ggplot(summary %>% filter(Type == "Mam"), aes(x = dataset, y= mean_vedba2, colour = Device)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggplot(summary, aes(x = LogMass, y = mean_vedba5)) +
  geom_point() +
  geom_smooth(method = "lm", group = 1) +
  theme_minimal()




# Plotting these results
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))
