# Simulation Data ---------------------------------------------------------

# Prepare the data --------------------------------------------------------
data <- fread(file.path(base_path, "Data/HumanSimulation", "VDBA_output.csv")) %>%
  select(-file, -V1)
# summary
sum_data <- data %>%
  group_by(Mass) %>%
  summarise(mean = mean(mean_pelvis),
            se = sd(mean_pelvis, na.rm = TRUE) / sqrt(n()),
            max = max(mean_pelvis)) %>%
  mutate(logmean = log10(mean),
         log_upper = log10(mean + se),
         log_lower = log10(mean - se),
         logmax = log10(max),
         logmass = log10(Mass*1000))

# Plot --------------------------------------------------------------------
mean_plot <- ggplot(sum_data, aes(x = logmass, y = logmean, colour = as.factor(Mass))) +
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper), width = 0.02) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  scale_colour_manual(values = fave_colours) +
  my_theme() +
  labs(x = "Log Mass (grams)", y = "Log Mean Acceleration (g)", colour = NULL) +
  theme(legend.position = "none")

mean_plot

# save
img_output <- file.path(base_path, "Output", "HumanSimulation.png")
ggsave(img_output, mean_plot)

# Stats -------------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ logmass, data = sum_data)
summary(mean_model)

mean_model2 <- glmmTMB(log10(mean_pelvis) ~ log10(Mass * 1000), data = data)
summary(mean_model2)
