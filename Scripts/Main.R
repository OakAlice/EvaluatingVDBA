# Main script for processing the other scripts ----------------------------

base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/EvaluatingVDBA"

# Packages and settings ---------------------------------------------------
library(pacman)
p_load(tidyverse,
       data.table,
       tsfeatures)

selected_axes <- c("Accel.X", "Accel.Y", "Accel.Z")

# Formatting all data sources into same structure -------------------------
source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))

