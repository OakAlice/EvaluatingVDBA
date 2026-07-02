# Accelerometer Analysis --------------------------------------------------
# Undertaking VDBA analysis of strides for the accelerometer datasets
# Each of these scripts loads in the data, makes a walking detection model (various),
# extracts the walking, and calculates the VDBA


dataset_variables <- fread(file.path(base_path, "Data/Accelerometer/Dataset_Variables.csv"))

# Non-ungulates -----------------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Kangaroo.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Annett_Bettong.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Sparkes_Koala.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Galea_Cat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Smit_Cat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Studd_Squirrel.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Vehkaoja_Dog.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/HarveyCaroll_Pangolin.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Pagano_Bear.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/HARTH_Human.R"))



# Not done yet ------------------------------------------------------------
# Quoll data from Gaschk et al., 2023 
# Custom walking detection model
#source(file.path(base_path, "Scripts/AccelerometerAnalysis/Quoll.R"))

# Impala data from Wilson et al., unpublished
# Custom walking detection model
#source(file.path(base_path, "Scripts/AccelerometerAnalysis/Impala.R"))



# Ungulates ---------------------------------------------------------------
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Mauny_Goat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Dickinson_PygmyGoat.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Dickinson_Ibex.R"))
source(file.path(base_path, "Scripts/AccelerometerAnalysis/Harris_Sheep.R"))




# Compare them in one place -----------------------------------------------
files <- list.files(file.path(base_path, "Output"), pattern = "_summary_stats.csv", full.names = TRUE)
all_data <- lapply(files, function(x){
  dat <- fread(x)
  dat$Species <- gsub("Accelerometer_summary_stats.csv", "", basename(x))
  dat
})
all_data <- rbindlist(all_data, fill = TRUE)

# add in the categories
merged_data <- merge(all_data, dataset_variables %>% select(Name, AnimalType, LocomotionType, Category), by.x = "Species", by.y = "Name")

# exclude Species
merged_data <- merged_data %>% dplyr::filter(
  !Species == "Studd_Squirrel" # sampling rate too low, not comparable
) %>%
  mutate(Category = ifelse(Category == "Marsupial_Quadruped", "Mammal_Quadruped", Category))


# plot
mean_plot <- ggplot(merged_data, aes(x = LogMass, y = logmean, colour = Species)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = Species), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = Category, linetype = Category), 
              colour = "black", se = FALSE, linewidth = 1) +
  my_theme() + 
  scale_colour_manual(values = fave_colours_big) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)") +
  theme(legend.position = "right",
        legend.box = "vertical")

mean_plot
