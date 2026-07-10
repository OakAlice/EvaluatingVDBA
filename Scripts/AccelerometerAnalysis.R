# Accelerometer Analysis --------------------------------------------------
# Undertaking VDBA analysis of strides for the accelerometer datasets
# Each of these scripts loads in the data, makes a walking detection model (various),
# extracts the walking, and calculates the VDBA


dataset_variables <- fread(file.path(base_path, "Data/Accelerometer/Dataset_Variables.csv"))


# Functions ---------------------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Functions.R"))

# Datasets with labels ----------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Sparkes_Koala.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Galea_Cat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Studd_Squirrel.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Smit_Cat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Dunford_Cat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Vehkaoja_Dog.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/HarveyCaroll_Pangolin.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Mauny_Goat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Dickinson_PygmyGoat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Dickinson_Ibex.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Harris_Sheep.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Pagano_Bear.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/HARTH_Human.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Glider.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Clemente_Impala.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Clemente_Echidna.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Bonneau_Goat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/MoralesVargas_Cow.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Wijers_Lion.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Christensen_Baboon.R"))

# Thresholded -------------------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Gaschk_Quoll.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Possum.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Wallaby.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Kangaroo.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Bettong.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Clemente_Kudu.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Rautiainen_Reindeer.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Neis_Cow.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Williams_Squirrel.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Gunner_Lion.R"))


# Buchmann datasets -------------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Buchmann_Datasets.R"))





# Compare them in one place -----------------------------------------------
files <- list.files(file.path(base_path, "Output"), pattern = "_summary_stats.csv", full.names = TRUE)
all_data <- lapply(files, function(x){
  dat <- fread(x)
  dat$Species <- gsub("Accelerometer_summary_stats.csv", "", basename(x))
  dat
})
all_data <- rbindlist(all_data, fill = TRUE)
merged_data <- merge(all_data, dataset_variables %>% select(Name, AnimalType, LocomotionType, Category, Device, LocomotionDetection), by.x = "Species", by.y = "Name")
merged_data <- merged_data %>% 
  mutate(Category = ifelse(Category == "Marsupial_Quadruped", "Mammal_Quadruped", Category)) # %>%
  # dplyr::filter(!Species == "Pagano_Bear") # this one is on a different scale

# plot ---------------------------------------------------------------------
# All 
# ggplot(merged_data, aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
#   geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
#   geom_point(size = 3) +
#   geom_smooth(method = "lm", aes(group = Category, linetype = Category), 
#               colour = "black", se = FALSE, linewidth = 1) +
#   my_theme() + 
#   # scale_colour_manual(values = fave_colours_big) + 
#   scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
#   labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
#   theme(legend.position = "right",
#         legend.box = "vertical")

# Labels
ggplot(merged_data %>% dplyr::filter(LocomotionDetection == "Labels" | LocomotionType == "BipedalHopper"), 
       aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


# Axivity only
ggplot(merged_data %>% dplyr::filter(Device == "Axivity"), 
       aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


# Buchmann only
ggplot(merged_data %>% dplyr::filter(Device == "Buchmann"), 
       aes(x = LogMass, y = logmean, colour = Species, shape = AnimalType)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  # scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")


