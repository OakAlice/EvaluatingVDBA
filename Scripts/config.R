# Packages and settings ---------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")

pacman::p_load(tidyverse,
               data.table,
               tsfeatures,
               future,
               RcppRoll,
               arrow,
               plotly,
               zoo,
               R.matlab,
               rhdf5,
               ctmm,
               signal,
               lmerTest,
               mgcv,
               glmmTMB
)


# Variables #####
dataset_variables <- fread(file.path(base_path, "Data", "Dataset_Variables.csv"))

# these are the datasets small enough to fit into a single file
species_list <- c("Annett_Possum", "Annett_Bettong", "Annett_Wallaby", "Clemente_Kudu")
                  # "Annett_Kangaroo", "Clemente_Impala", "Gaschk_Quoll", "Galea_Cat","Sparkes_Koala")

source(file.path(base_path, "Scripts", "FreeRoamingAnalysis", "FormattingRawData.R"))
source(file.path(base_path, "Scripts", "FreeRoamingAnalysis", "VDBAFunctions.R"))



# Plotting ----------------------------------------------------------------

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

selected_colours <- c(
  "firebrick3","coral", "goldenrod2", "palegreen3","darkcyan",  "skyblue4", "plum", "lightpink1", "mistyrose3"
)


fave_colours <- c(
  "firebrick3", "tomato", "coral", "darkorange2", "sienna3", "tan2",
  "goldenrod2", "lemonchiffon2", "khaki2", "gold1",
  "springgreen3", "darkseagreen4", "seagreen", "olivedrab4", "darkcyan", "aquamarine3", "palegreen3",
  "skyblue4", "deepskyblue3", "cornflowerblue", "powderblue", "royalblue4", "slateblue2", "lightslateblue", "lightcyan4",
  "purple4", "mediumpurple3", "mediumorchid", "plum", "orchid3", "thistle",
  "deeppink3", "hotpink", "lightpink1", "rosybrown1",
  "mistyrose3", "lavenderblush2", "seashell2"
)
