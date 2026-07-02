# Analysis of the dog data (already labelled) -----------------------------
# Behcause this data is a;lready labelled, we can pull out the specific instances
# note that there was an accelerometer on the back and the collar but I used the back
# because closer to the COM

freq <- dataset_variables$Frequency[dataset_variables$Name == "Vehkaoja_Dog"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Vehkaoja_Dog"]
list_locomotion_labels <- c("Walking", "Pacing", "Galloping", "Trotting")

# Set up data -------------------------------------------------------------
dat <- fread(file.path(base_path, "Data/Accelerometer/Vehkaoja_Dog/raw/DogMoveData.csv")) %>% 
  select(DogID, ABack_x, ABack_y, ABack_z, Behavior_1, Behavior_2, Behavior_3) %>%
  mutate(Activity = ifelse(
    Behavior_1 %in% list_locomotion_labels | Behavior_2 %in% list_locomotion_labels | Behavior_3 %in% list_locomotion_labels,
    "Locomotion",
    "Other" # just set the remainder to other
  )) %>%
  mutate(Time = row_number()) %>%
  rename(ID = DogID,
         X = ABack_x,
         Y = ABack_y,
         Z = ABack_z) %>%
  select(ID, Time, X, Y, Z, Activity)

# Get Vedba ---------------------------------------------------------------
dat <- get_vedba(dat, freq)

# Get locomotion ----------------------------------------------------------
# remmeber to swtich it over to the new list
dat <- get_locomotion(dat, freq, stride_window, "Locomotion")

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Vehkaoja_Dog", "Mass_of_Individuals.csv"))
summary <- merge(summary, animal_mass %>% select(ID, LogMass), by = "ID")

# Final summary -----------------------------------------------------------
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

fwrite(summ_stats, file.path(base_path, "Output/Vehkaoja_DogAccelerometer_summary_stats.csv"))

# Plots -------------------------------------------------------------------
mean_plot <- ggplot(summ_stats, aes(x = LogMass, y = logmean)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  my_theme() + 
  theme(legend.position = "none") +
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)")

mean_plot

# save
img_output <- file.path(base_path, "Output", "Vehkaoja_DogAccelerometer.png")
ggsave(img_output, mean_plot)


# Statistics --------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ LogMass, data = summ_stats)
summary(mean_model)

mean_model2 <- glmmTMB(log10(mean_vedba) ~ LogMass, data = summary)
summary(mean_model2)
