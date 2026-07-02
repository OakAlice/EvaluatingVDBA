# Main file for the VDBA analysis -----------------------------------------

# In this study we attempted to find the relationship between mass and VDBA
# We tried to do this in many different ways
# One of which was by looking at trends over dozens of species... however this was abandoned
# Folder "FreeRoamingAnalysis(Old) contains legacy code for the unused data
# Final output relied only on:
  # KangarooAnalysis.R -> Analysis of accelerometer data from free-roaming kangaroos
  # SimulationAnalysis.R -> Analysis of simulation data from humans of differing masses
  # MotionTrackingAnalysis.R -> Analysis of motion tracking lizard data

# Set Up ------------------------------------------------------------------
library(pacman)
p_load(tidyverse,
       data.table,
       zoo,
       glmmTMB,
       patchwork
)

base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

my_theme <- function() {
  theme_minimal(base_size = 15, base_family = "serif") +
    theme(
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20) 
    )
}

fave_colours <- c("firebrick3", "tan2", "goldenrod2", "khaki3", "palegreen3", "aquamarine3", "darkcyan",
                  , "cornflowerblue", "powderblue","plum", "rosybrown1","lavenderblush2")
fave_colours_big <- c(
  "firebrick3", "coral", "tan2", "goldenrod2", "khaki3", 
  "olivedrab4","palegreen3", "aquamarine3","darkcyan", 
  "skyblue4","dodgerblue4", "cornflowerblue", "powderblue", "lightslateblue", 
  "mediumpurple3", "plum", "thistle", "rosybrown1",
  "mistyrose3", "lavenderblush2"
)

# Run analyses ------------------------------------------------------------
# Motion Tracking
source(file.path(base_path, "Scripts/MotionTrackingAnalysis.R"))
# Simulation
source(file.path(base_path, "Scripts/SimulationAnalysis.R"))
# Kangaroo Data
source(file.path(base_path, "Scripts/AccelerometerAnalysis.R"))



# Making figures for paper ------------------------------------------------
source(file.path(base_path, "Scripts/MakingAFigure.R"))

