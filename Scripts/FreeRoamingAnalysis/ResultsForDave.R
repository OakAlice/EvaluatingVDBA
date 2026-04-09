
# Set up ------------------------------------------------------------------
pacman::p_load(data.table, 
               tidyverse, 
               glmmTMB, 
               ggnewscale, 
               mgcv, 
               patchwork, 
               broom.mixed)

base_path <- "C:/Users/PC/Documents/EvaluatingVDBA/ForDave"
dataset_variables <- read_csv(file.path(base_path, "Dataset_Variables.csv"), col_select = c(Name, Species, LogMass, Device, DeviceAttachment), show_col_types  = FALSE)
all_volumes <- fread(file.path(base_path, "volumes.csv"))
ind_weight <- fread(file.path(base_path, "Individual_Weights.csv")) %>% select(Dataset, ID, LogMass) %>% rename(Name = Dataset, IndLogMass = LogMass)

# Functions and themes ----------------------------------------------------
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
fave_colours <- c(
  "firebrick3", "tomato", "coral", "darkorange2", "sienna3", "tan2",
  "goldenrod2", "lemonchiffon2", "khaki2", "gold1",
  "springgreen3", "darkseagreen4", "seagreen", "olivedrab4", "darkcyan", "aquamarine3", "palegreen3",
  "skyblue4", "deepskyblue3", "cornflowerblue", "powderblue", "royalblue4", "slateblue2", "lightslateblue", "lightcyan4",
  "purple4", "mediumpurple3", "mediumorchid", "plum", "orchid3", "thistle",
  "deeppink3", "hotpink", "lightpink1", "rosybrown1",
  "mistyrose3", "lavenderblush2", "seashell2"
)

# Make a summary of the data ----------------------------------------------
# took all the individually processed datasets and put them together and then summarised:

# all_data <- fread(file.path(base_path, "All_windows_data.csv")) %>% rename(Name = Species)
# ind_summary_data <- all_data %>%
#   group_by(Name, ID, threshold) %>%
#   summarise(IndMean = mean(seconds_VDBA), IndMax = max(seconds_VDBA))
# summary_data <- ind_summary_data %>%
#   group_by(Name, threshold) %>%
#   summarise(SpeciesMean = mean(IndMean), SpeciesMeanMax = max(IndMax))
# summary_data <- merge(ind_summary_data, summary_data, by = c("Name", "threshold"))
# summary_data <- merge(summary_data, dataset_variables, by = "Name")
# now add in the active minutes
# minute_data <- file.path(base_path, "Active_time_data.csv")
# summary_data <- merge(summary_data, minute_data, by = c("ID", "Name"))

# exclude some datasets
summary_data <- summary_data %>% dplyr::filter(!Name == "HARTH_Human")

# save that
# fwrite(summary_data, file.path(base_path, "All_summarised_data.csv"))
summary_data <- fread(file.path(base_path, "All_summarised_data.csv"))

# make some subsets
buchmann_species <- dataset_variables$Name[str_detect(dataset_variables$Name, "Buchmann") %in% TRUE]
Buchmann_data <- summary_data %>% dplyr::filter(Name %in% buchmann_species)
active_data <- summary_data %>% dplyr::filter(!Name %in% c(buchmann_species, "Vehkaoja_Dog"))

long_volumes <- all_volumes[all_volumes$volume > 12, ]
long_species <- unique(long_volumes$Species)
long_data <- summary_data %>% dplyr::filter(Name %in% long_species)

# Part One: Averages vs body mass ----------------------------------------------
# Hypothesis: There will be a significant effect of Body mass on mean and max VDBA

## Plots ####
### All thd data ####
pmean <- ggplot() +
  geom_point(data = active_data %>% dplyr::filter(threshold == "active"), 
             aes(x = LogMass, y = log10(SpeciesMean), colour = Name), size = 4) +
  scale_colour_manual(values = fave_colours) +
  
  ggnewscale::new_scale_colour() +  # reset colour scale for the lines
  
  geom_smooth(data = active_data  %>% dplyr::filter(threshold == "active"), 
              aes(x = LogMass, y = log10(SpeciesMean), colour = "All datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_smooth(data = long_data %>% dplyr::filter(threshold == "active"), 
              aes(x = LogMass, y = log10(SpeciesMean), colour = "Long datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_smooth(data = active_data %>% dplyr::filter(threshold == "active",
                                                   DeviceAttachment == "Collar"), 
              aes(x = LogMass, y = log10(SpeciesMean), colour = "Collar datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c(
      "All datasets"           = "firebrick3",
      "Long datasets"          = "goldenrod2", 
      "Collar datasets"        = "springgreen3"
    )
  ) +
  labs(x = "Log Mass (grams)", y = "Dataset Mean VDBA (G)") +
  my_theme()

pmax <- ggplot() +
  geom_point(data = active_data %>% dplyr::filter(threshold == "active"), 
             aes(x = LogMass, y = log10(SpeciesMeanMax), colour = Name), size = 4) +
  scale_colour_manual(values = fave_colours) +
  
  ggnewscale::new_scale_colour() +  # reset colour scale for the lines
  
  geom_smooth(data = active_data %>% dplyr::filter(threshold == "active"), 
              aes(x = LogMass, y = log10(SpeciesMeanMax), colour = "All datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_smooth(data = long_data %>% dplyr::filter(threshold == "active"), 
              aes(x = LogMass, y = log10(SpeciesMeanMax), colour = "Long datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_smooth(data = active_data %>% dplyr::filter(threshold == "active",
                                                   DeviceAttachment == "Collar"), 
              aes(x = LogMass, y = log10(SpeciesMeanMax), colour = "Collar datasets"),
              method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c(
      "All datasets"           = "firebrick3",
      "Long datasets"          = "goldenrod2", 
      "Collar datasets"        = "springgreen3"
    )
  ) +
  labs(x = "Log Mass (grams)", y = "Dataset Mean Max VDBA (G)") +
  my_theme()
  
(pmean + pmax) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )

### clemente and buchmann data ####
Clemente_data <- active_data %>% dplyr::filter(Device == "Axivity", threshold == "active")
clem <- mean_max_plotting(Clemente_data)
buch <- mean_max_plotting(Buchmann_data  %>% dplyr::filter(threshold == "active"))

(clem / buch) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )
  
### Individual masses ####
ind_species <- c("Annett_Kangaroo", "Clemente_Impala", "Clemente_Kudu", "Gaschk_Quoll", "HarveyCaroll_Pangolin",  "Mauny_Goat", "Pagano_Bear", "Sparkes_Koala", "Vehkaoja_Dog")
ind_data <- summary_data %>% dplyr::filter(Name %in% ind_species, threshold == "active")
ind_data$ID[ind_data$Name == "Vehkaoja_Dog"] <-
  str_split_i(ind_data$ID[ind_data$Name == "Vehkaoja_Dog"], "_", 1)
ind_data <- merge(ind_data, ind_weight, by = c("Name", "ID"))

### Even further separated
ind_data <- ind_data %>%
  dplyr::filter(Name != "Clemente_Kudu")
p7mean <- ggplot(ind_data, aes(x = IndLogMass, y = log10(IndMean), shape = threshold)) +
  geom_point(aes(colour = Name), size = 3) +
  xlab("Log mass (g)") + ylab("Log mean VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none") +
  facet_wrap(~Name, scales = "free")

p7max <- ggplot(ind_data %>% dplyr::filter(threshold == "active"), aes(x = IndLogMass, y = log10(IndMax))) +
  geom_point(aes(colour = Name), size = 3) +
  xlab("Log mass (g)") + ylab("Log max VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() +
  geom_smooth(method = "lm", colour = "lightcoral", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")+
  facet_wrap(~Name, scales = "free")

(p7mean + p7max)

## Stats ####
summary_data <- summary_data %>% dplyr::filter(threshold == "active",
                                               !Name %in% c(buchmann_species, "Vehkaoja_Dog"))

full_mean_model <- glmmTMB(
  log(IndMean) ~ LogMass +
    (1 | Name) +
    (1 | Device) +
    (1 | Name:ID),
  data = summary_data
)
simple_mean_model <- glmmTMB(
  log(IndMean) ~ LogMass +
    (1 | Name) + 
    (1 | Device) +
    (1 | DeviceAttachment),
  data = summary_data
)
simplest_mean_model <- glmmTMB(
  log(IndMean) ~ LogMass +
    (1 | Device) + 
    (1 | Name),
  data = summary_data
)

AIC(full_mean_model, simple_mean_model, simplest_mean_model)
anova(full_mean_model, simple_mean_model, simplest_mean_model)
# from playing around with this, it looked like Device was the most important explanatory factor

simple_max_model <- glmmTMB(
  log(IndMax) ~ LogMass +
    (1 | Name) + 
    (1 | Device) +
    (1 | DeviceAttachment),
  data = summary_data
)


summary(simple_mean_model)
summary(simple_max_model)

# Conclusion, there is no effect of body mass on the mean (marginal) or max VDBA (non sig)

# Part 2: Animal activity ####
# Hypothesis, there will be a significant difference in how big vs small animals behave
# with larger animals spending less time in high activity


# find the minutes per day that the animals are active
# plot this
summary_data$prop_active <- 1 - summary_data$prop_rest
# this is somewhat arbitrary because I chose the threshold between active and inactive
ggplot(long_data, aes(x = LogMass, y = prop_active, colour = Name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Log Mass (g)") +
  ylab("Proportion of timeseries active") +
  scale_color_manual(values = fave_colours) +
  my_theme() +
  theme(legend.position = "bottom")


# Understanding the distribution of activity

# only include datasets with more than 5 hours
selected_data <- all_data %>%
  group_by(Name) %>%
  dplyr::filter(n() > 5 * 60 * 60) %>% # 5 hours worth of data
  ungroup() %>%
  dplyr::filter(!Name %in% c("HARTH_Human", "Vehkaoja_Dog", "Mauny_Goat"))

# now plot the distributions
ggplot(selected_data, aes(x = log10(seconds_VDBA), fill = Name)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.01) +
  scale_fill_manual(values = fave_colours) +
  scale_x_continuous(limits = c(-5, 1)) +
  labs(y = "Proportion of counts", x = "Log VDBA (g)") +
  my_theme() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~Name, scales = "free_y")


# density-based peak finding per dataset
# Chris said to do this... but I don't fully understand it 
library(pracma)  # for findpeaks()
peaks <- selected_data %>%
  group_by(Name) %>%
  summarise(active_peak = {
    d <- density(log10(seconds_VDBA))
    d$x[findpeaks(d$y)[,2]]  # x position of peaks
  })

peaks <- merge(peaks, dataset_variables, by = "Name")
ggplot(peaks, aes(x = LogMass, y = active_peak, colour = Name)) + geom_point()


# no longer have any idea what I'm doing here.
