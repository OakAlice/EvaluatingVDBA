
# Set up ------------------------------------------------------------------
pacman::p_load(data.table, tidyverse, lmerTest, ggnewscale, mgcv, patchwork, broom.mixed)

base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

window_seconds <- 1 # redefine that here

dataset_variables <- read_csv(file.path(base_path, "Dataset_Variables.csv"), col_select = c(Name, Species, LogMass, Device, DeviceAttachment), show_col_types  = FALSE)

species_list <- list.dirs(file.path(base_path, "AccelerometerData"), recursive = FALSE)


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

threshold_cols <- c(
  active   = "lightcoral", 
  all = "lightcyan3"
)

# plot functions
mean_max_plotting <- function(data){
  
  p1mean <- ggplot(data, aes(x = LogMass, y = log10(SpeciesMean), shape = threshold)) +
    geom_point(aes(colour = Name), size = 4) +
    xlab("Log mass (g)") + ylab("Log mean VDBA (g)") +
    scale_color_manual(values = fave_colours) +
    ggnewscale::new_scale_colour() + # reset the colour scale
    geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
    scale_color_manual(values = threshold_cols) +
    my_theme() +
    theme(legend.position = "none")
  
  p1max <- ggplot(data %>% dplyr::filter(threshold == "active"), aes(x = LogMass, y = log10(SpeciesMeanMax))) +
    geom_point(aes(colour = Name), size = 4) +
    xlab("Log mass (g)") + ylab("Log max VDBA (g)") +
    scale_color_manual(values = fave_colours) +
    ggnewscale::new_scale_colour() +
    geom_smooth(method = "lm", colour = "lightcoral", se = FALSE, linewidth = 1.2) +
    scale_color_manual(values = threshold_cols) +
    my_theme() +
    theme(legend.position = "none")
  
  plot <- (p1mean + p1max) +
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
  
  return(plot)
}

quick_stats <- function(data){
  
  mean_model <- glmmTMB(log(meanVDBA) ~ LogMass * threshold + (1|ID), data = data)
  mean_res <- broom.mixed::tidy(mean_model, effects = "fixed")
  mean_results <- mean_res[grep("LogMass", mean_res$term), ]
  mean_results$sig <- ifelse(mean_results$p.value < 0.05, "sig", "not sig")
  mean_results$variable <- "mean"
  
  max_data <- data %>% dplyr::filter(threshold == "active")
  max_model <- glmmTMB(log(maxVDBA) ~ LogMass + (1|ID), data = max_data)
  max_res <- broom.mixed::tidy(max_model, effects = "fixed")
  max_results <- max_res[grep("LogMass", max_res$term), ]
  max_results$sig <- ifelse(max_results$p.value < 0.05, "sig", "not sig")
  max_results$variable <- "max"
  
  logmass_results <- rbind(mean_results, max_results)
  
  return(logmass_results)
}

quick_stats2 <- function(data){
  
  mean_model <- lm(log(SpeciesMean) ~ LogMass * threshold, data = data)
  mean_res <- broom.mixed::tidy(mean_model, effects = "fixed")
  mean_results <- mean_res[grep("LogMass", mean_res$term), ]
  mean_results$sig <- ifelse(mean_results$p.value < 0.05, "sig", "not sig")
  mean_results$variable <- "mean"
  
  max_data <- data %>% dplyr::filter(threshold == "active")
  max_model <- lm(log(SpeciesMeanMax) ~ LogMass, data = max_data)
  max_res <- broom.mixed::tidy(max_model, effects = "fixed")
  max_results <- max_res[grep("LogMass", max_res$term), ]
  max_results$sig <- ifelse(max_results$p.value < 0.05, "sig", "not sig")
  max_results$variable <- "max"
  
  logmass_results <- rbind(mean_results, max_results)
  
  return(logmass_results)
}

# Make a summary of the data ----------------------------------------------
# take all the individually processed datasets and put them together
summary_files1 <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "1_summary\\.csv$", full.names = TRUE)
summary_files3 <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "3_summary\\.csv$", full.names = TRUE)
summary_files <- c(summary_files1, summary_files3)

summary_data <- lapply(summary_files, function(x){
  dat <- fread(x) %>% na.omit()
  dat$ID <- as.character(dat$ID)
  dat$Name <- basename(dirname(x))
  dat
})
summary_data <- rbindlist(summary_data, fill = TRUE)

summary_data <- merge(summary_data, dataset_variables, by = 'Name') %>%
  dplyr::filter(!Name == "HARTH_Human") %>%
  mutate(mean_MSVDBA = meanVDBA / 10^LogMass,
         max_MSVDBA = maxVDBA / 10^LogMass) %>%
  dplyr::filter(threshold != "inactive") %>%
  group_by(Name, threshold) %>%
  mutate(SpeciesMean = mean(meanVDBA), SpeciesMeanMax = mean(maxVDBA))
fwrite(summary_data1, file.path(base_path, "Output", paste0("All_1_summary_data.csv")))


# Compare 5 seconds and 1 second windows 
To demonstarte that these are interchangable and results not affected by the length of the window


# now can compare them
win1 <- ggplot(summary_data1, aes(x = LogMass, y = log10(meanVDBA), shape = threshold)) +
  geom_point(aes(colour = Name)) +
  xlab("Log mass (g)") + ylab("Log mean VDBA \n over 1 sec (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

win1.1 <- ggplot(summary_data1, aes(x = LogMass, y = log10(meanInt), shape = threshold)) +
  geom_point(aes(colour = Name)) +
  xlab("Log mass (g)") + ylab("Log mean integral VDBA \n over 1 sec (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

win5 <- ggplot(summary_data5, aes(x = LogMass, y = log10(meanVDBA), shape = threshold)) +
  geom_point(aes(colour = Name)) +
  xlab("Log mass (g)") + ylab("Log mean VDBA \n over 5 sec (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

win5.1 <- ggplot(summary_data5, aes(x = LogMass, y = log10(meanInt), shape = threshold)) +
  geom_point(aes(colour = Name)) +
  labs(x = "Log mass (g)", y = "Log mean integral VDBA \n over 5 sec (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

plotX <- ((win1 | win5) / (win1.1 | win5.1) + plot_layout(guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )

plotX



## All species pooled
All species, with approximate/average masses. 
{r, echo = FALSE}
# Plots ------------------------------------------------------------------------
plot1 <- mean_max_plotting(summary_data1)
plot1

stats1 <- quick_stats(summary_data1)
stats1



## Only Datasets With Long Data
Maybe some of the small datasets are skewing it... Exclude datasets that had less than 24 hrs worth of data.

# select only the big datasets
all_volumes <- lapply(species_list, function(x){
  vol <- fread(file.path(x, paste0(basename(x), "_volume.csv")))
  vol$Species <- basename(x)
  vol
})
all_volumes <- rbindlist(all_volumes)
long_volumes <- all_volumes[all_volumes$volume > 12, ]
long_species <- unique(long_volumes$Species)
long_volumes_data <- summary_data1 %>% dplyr::filter(Name %in% long_species)

plot2 <- mean_max_plotting(long_volumes_data)
plot2

stats2 <- quick_stats(long_volumes_data)
stats2



## Collar Data Only
And then only the sepcies that were on a collar.

collar_data <- summary_data1 %>% dplyr::filter(DeviceAttachment == "Collar")

plot3 <- mean_max_plotting(collar_data)
plot3

stats3 <- quick_stats(collar_data)
stats3



## Single system analysis
What is the differences in sampling protocol are throwing it out? Collected only data collected in the same way by the same lab.
# V1: Clemente
There were insufficint individuals, so removed ID as an effect and just had a linear model.
{r echo = FALSE}
Clemente_data <- summary_data1 %>% dplyr::filter(Device == "Axivity")

plot4 <- mean_max_plotting(Clemente_data)
plot4

stats4 <- quick_stats2(Clemente_data)
stats4


# V2: Buchmann
Similarly ID was not a random effect here as too few individuals.
{r echo = FALSE}
buchmann_species <- dataset_variables$Name[str_detect(dataset_variables$Name, "Buchmann") %in% TRUE]
Buchmann_data <- summary_data1 %>% dplyr::filter(Name %in% buchmann_species)

plot5 <- mean_max_plotting(Buchmann_data)
plot5

stats5 <- quick_stats2(Buchmann_data)
stats5


## Specific Masses
# Pooled
Maybe the species masses are too vauge... what if we only use the datasets where we know the exact mass?
  Again, just a linear model with no random effect for ID.

ind_species <- c("Annett_Kangaroo", "Clemente_Impala", "Clemente_Kudu", "Gaschk_Quoll", "HarveyCaroll_Pangolin",  "Mauny_Goat", "Pagano_Bear", "Sparkes_Koala", "Vehkaoja_Dog")

# Analysis ----------------------------------------------------------------
ind_summaries <- lapply(ind_species, function(x){
  weights <- fread(file.path(base_path, "Mass_of_individuals", paste0(x, ".csv"))) %>%
    mutate(ID = as.character(ID))
  summary <- fread(file.path(base_path, "AccelerometerData", x, paste0(x, "_", window_seconds, "_summary.csv"))) %>%
    mutate(Dataset = x,
           ID = as.character(ID))
  dat <- merge(summary, weights, by = "ID")
  dat
})

ind_data <- rbindlist(ind_summaries, fill = TRUE) %>%
  dplyr::filter(threshold != "inactive")

# Plots ------------------------------------------------------------------------------
p6mean <- ggplot(ind_data, aes(x = LogMass, y = log10(meanVDBA), shape = threshold)) +
  geom_point(aes(colour = Dataset), size = 4) +
  xlab("Log mass (g)") + ylab("Log mean VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

p6max <- ggplot(ind_data %>% dplyr::filter(threshold == "active"), aes(x = LogMass, y = log10(maxVDBA))) +
  geom_point(aes(colour = Dataset), size = 4) +
  xlab("Log mass (g)") + ylab("Log max VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() +
  geom_smooth(method = "lm", colour = "lightcoral", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")

plot6 <- (p6mean + p6max + plot_layout(guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )
plot6

mean_model <- lm(log(meanVDBA) ~ LogMass * threshold, data = data)
mean_res <- broom.mixed::tidy(mean_model, effects = "fixed")
mean_results <- mean_res[grep("LogMass", mean_res$term), ]
mean_results$sig <- ifelse(mean_results$p.value < 0.05, "sig", "not sig")
mean_results$variable <- "mean"

max_data <- data %>% dplyr::filter(threshold == "active")
max_model <- lm(log(maxVDBA) ~ LogMass, data = max_data)
max_res <- broom.mixed::tidy(max_model, effects = "fixed")
max_results <- max_res[grep("LogMass", max_res$term), ]
max_results$sig <- ifelse(max_results$p.value < 0.05, "sig", "not sig")
max_results$variable <- "max"

rbind(mean_results, max_results)



# Even further separated
Where the masses of the exact individual are known. Per species plot and then all known individuals on the same graph.
{r echo = FALSE}

ind_data <- ind_data %>%
  dplyr::filter(Dataset != "Clemente_Kudu")

# Plots -------------------------------------------------------------------
p7mean <- ggplot(ind_data, aes(x = LogMass, y = log10(meanVDBA), shape = threshold)) +
  geom_point(aes(colour = Dataset), size = 3) +
  xlab("Log mass (g)") + ylab("Log mean VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() + # reset the colour scale
  geom_smooth(method = "lm", aes(colour = threshold), se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none") +
  facet_wrap(~Dataset, scales = "free")

p7max <- ggplot(ind_data %>% dplyr::filter(threshold == "active"), aes(x = LogMass, y = log10(maxVDBA))) +
  geom_point(aes(colour = Dataset), size = 3) +
  xlab("Log mass (g)") + ylab("Log max VDBA (g)") +
  scale_color_manual(values = fave_colours) +
  ggnewscale::new_scale_colour() +
  geom_smooth(method = "lm", colour = "lightcoral", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = threshold_cols) +
  my_theme() +
  theme(legend.position = "none")+
  facet_wrap(~Dataset, scales = "free")

plot7 <- (p7mean + p7max) +
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

plot7



## Understanding the animal activity
# Find the active minutes per day
Is there a mass-mediated effect on activity patterns generally

# find the minutes per day that the animals are active
all_minutes_files <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "*_active_minutes\\.csv$", full.names = TRUE)
minute_data <- lapply(all_minutes_files, function(x){
  dat <- fread(x) %>% na.omit()
  dat$Name <- basename(dirname(x))
  dat
})
minute_data <- rbindlist(minute_data, fill = TRUE)
minute_data <- minute_data %>% dplyr::filter(Name %in% long_species)
merged_data <- merge(summary_data1, minute_data, by = c("ID", "Name"))

# plot this
merged_data$prop_active <- 1 - merged_data$prop_rest
p8total <- ggplot(merged_data, aes(x = LogMass, y = prop_active, colour = Name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Log Mass (g)") +
  ylab("Proportion of timeseries active") +
  scale_color_manual(values = fave_colours) +
  my_theme() +
  theme(legend.position = "bottom")

# average it across the individuals
plot_data <- merged_data %>%
  group_by(Name, LogMass) %>%
  summarise(mean_high = mean(prop_high),
            mean_med = mean(prop_med),
            mean_low = mean(prop_low),
            mean_rest = mean(prop_rest))

# rotate it to allow all the activity levels
plot_data <- plot_data %>%
  pivot_longer(cols = c(mean_high, mean_med, mean_low, mean_rest),
               names_to = "activity",
               values_to = "prop")
plot_data$activity <- factor(plot_data$activity, levels = c("mean_high", "mean_med", "mean_low", "mean_rest"))

p8props <- ggplot(plot_data,  aes(x = LogMass, y = prop, colour = activity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Log Mass (g)") +
  ylab("Proportion of timeseries per activity level") +
  scale_colour_manual(values = c("mean_high" = "lightcoral",
                                 "mean_med" = "tan2",
                                 "mean_low" = "lightcyan3",
                                 "mean_rest" = "ivory3")) +
  my_theme() +
  theme(legend.position = "bottom")

plot8 <- (p8total + p8props) +
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

plot8



## Understanding the distribution of activity
Does the distibution of high and low VDBA differ with size?
  
  
# load in all the vdba data
all_files <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "_1_processed.csv", full.names = TRUE)
all_data <- lapply(all_files, function(x){
  dat <- fread(x)
  dat$Species <- basename(dirname(x))
  
  dat
})
all_data <- rbindlist(all_data)

# only include datasets with more than 10 hours (10 * 60 * 60)
selected_data <- all_data %>%
  group_by(Species) %>%
  dplyr::filter(n() > 5 * 60 * 60) %>% # 5 hours worth of data
  ungroup()

# now plot the distributions
ggplot(selected_data, aes(x = log10(seconds_VDBA), fill = Species)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 0.01) +
  scale_fill_manual(values = fave_colours) +
  scale_x_continuous(limits = c(-5, 1)) +
  labs(y = "Proportion of counts", x = "Log VDBA (g)") +
  my_theme() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~Species, scales = "free_y")



