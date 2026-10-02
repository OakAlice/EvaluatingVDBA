# Analysing the motion tracking dtaa --------------------------------------

# Load in the data --------------------------------------------------------
data <- fread(file.path(base_path, "Data/LizardMotionTracking/Compiled data 2026.csv")) %>%
  mutate(LogMass = log10(Mass),
         MeanAccelGs = `av accel`/9.8,
         MaxAccelGs = `max accel`/9.8)

summary <- data %>%
  group_by(Species, LogMass) %>%
  summarise(mean = mean(MeanAccelGs),
            se = sd(MeanAccelGs, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  mutate(logmean = log10(mean),
         log_upper = log10(mean + se),
         log_lower = log10(mean - se))

# Plot --------------------------------------------------------------------
mean_plot <- ggplot(summary, aes(x = LogMass, y = logmean, colour = Species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper), width = 0.02) +
  geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  my_theme() +
  xlab("Log Mass (grams)") + ylab("Log Mean Acceleration (g)") +
  scale_colour_manual(values = fave_colours_big) + 
  theme(legend.position = "none")

mean_plot

# save image
img_output <- file.path(base_path, "Output", "LizardMotionTracking.png")
ggsave(img_output, mean_plot)

# Stats -------------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ LogMass, data = summary)
summary(mean_model)

mean_model2 <- glmmTMB(log10(MeanAccelGs) ~ log10(Mass), data = data)
summary(mean_model2)



ggplot(summary, aes(x = LogMass, y = logmean, colour = Species)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper), width = 0.02) +
  geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  my_theme() +
  xlab("Log Mass (grams)") + ylab("Log Mean Acceleration (g)") +
  scale_colour_manual(values = fave_colours_big) + 
  theme(legend.position = "none") + 
  facet_wrap(~Species, scales = "free")
