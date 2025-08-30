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
       future)
library(rhdf5)

# Species options ---------------------------------------------------------
species_options <- c(
  "Wanja_Fox", "Nuijten_BewickSwans", "Khaewphakdee_FishingCat", "Schweitzer_WoodStork", "Schloesing_FruitBat",
  "Dickinson_Goat", "Dickinson_Ibex", "Chimienti_Razorbills", "Chimienti_Guillemots", "Dunford_Cat", "Isbell_OliveBaboon",
  "Kayes_Coatis", "Kays_Toucan", "Rautiainen_Reindeer", "Seriyes_Bobcat", "Acacio_Stork", "Minasandra_Hyena"
)

species <- species_options[1]


# Variables dictionaries --------------------------------------------------
source(file = file.path(base_path, "Scripts", "VariableDictionaries.R"))

# Formatting all data sources into same structure -------------------------
source(file = file.path(base_path, "Scripts", "FormattingRawData.R"))


# Generating VDBA for all datasets ----------------------------------------
selected.axes <- c("Accel.X", "Accel.Y", "Accel.Z")
source(file = file.path(base_path, "Scripts", "GeneratingVDBA.R"))

