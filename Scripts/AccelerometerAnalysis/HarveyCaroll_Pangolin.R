# Analysis of the pangolin data -------------------------------------------
# has behavioural labels
freq <- dataset_variables$Frequency[dataset_variables$Name == "HarveyCaroll_Pangolin"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "HarveyCaroll_Pangolin"]
list_locomotion_labels <- c("walk")

# Set up the data ---------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/HarveyCaroll_Pangolin/raw"), full.names = TRUE)
data <- lapply(files, function(x){
  dat <- fread(x)
  dat$ID <- str_split(basename(x), "_", simplify = T)[1]
  dat <- dat[, c("ID", "time", "X", "Y", "Z", "Behavior")]
  dat <- dat %>% rename(Activity = Behavior)
  dat
})
dat <- rbindlist(data)
  
# Get Vedba ---------------------------------------------------------------
dat <- get_vedba(dat, freq)

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/HarveyCaroll_Pangolin", "Mass_of_Individuals.csv"))
summary <- merge(summary, animal_mass %>% select(ID, LogMass), by = "ID")

# final summary -------------------------------------------------------
summ_stats <- summary %>%
  group_by(ID, LogMass) %>%
  summarise(
    mean_vedba_raw = mean(mean_vedba, na.rm = TRUE),
    sd_vedba_raw = sd(mean_vedba, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_vedba_raw = sd_vedba_raw / sqrt(n),
    logmean = log10(mean_vedba_raw),
    log_upper = log10(mean_vedba_raw + se_vedba_raw),
    log_lower = log10(mean_vedba_raw - se_vedba_raw)
  )

fwrite(summ_stats, file.path(base_path, "Output/HarveyCaroll_PangolinAccelerometer_summary_stats.csv"))

# Plots -------------------------------------------------------------------
mean_plot <- ggplot(summ_stats, aes(x = LogMass, y = logmean, colour = ID)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = ID), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  my_theme() + 
  scale_colour_manual(values = fave_colours) + 
  theme(legend.position = "none") +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)")

mean_plot

# save
img_output <- file.path(base_path, "Output", "HarveyCaroll_PangolinAccelerometer.png")
ggsave(img_output, mean_plot)

# Statistics --------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ LogMass, data = summ_stats)
summary(mean_model)

mean_model2 <- glmmTMB(log10(mean_vedba) ~ LogMass, data = summary)
summary(mean_model2)



