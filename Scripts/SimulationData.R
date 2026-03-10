# Simulation Data ---------------------------------------------------------
base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"
# load in the data 
data <- fread(file.path(base_path, "Human_Simulation", "VDBA_output.csv")) %>%
  select(-file, -V1)
# summary
sum_data <- data %>%
  group_by(Mass) %>%
  summarise(mean = mean(mean_pelvis), max = max(mean_pelvis)) %>%
  mutate(logmean = log10(mean), logmax = log10(max), logmass = log10(Mass))

# make a plot
mean <- ggplot(sum_data, aes(x = logmass)) +
  geom_point(aes(y = logmean, colour = "Mean"), size = 2) +
  geom_smooth(method = "lm", aes(y = logmean, colour = "Mean"), se = FALSE, linewidth = 2) +
  scale_colour_manual(values = c("Mean" = "lightcoral")) +
  my_theme() +
  labs(x = "Log Mass (kg)", y = "Log Mean Acceleration (g)", colour = NULL) +
  theme(legend.position = "none")

max <- ggplot(sum_data, aes(x = logmass)) +
  geom_point(aes(y = logmax, colour = "Max"), size = 2) +
  geom_smooth(method = "lm", aes(y = logmax, colour = "Max"), se = FALSE, linewidth = 2) +
  scale_colour_manual(values = c("Max" = "lightcyan3")) +
  my_theme() +
  labs(x = "Log Mass (kg)", y = "Log Max Acceleration (g)", colour = NULL) +
  theme(legend.position = "none")

mean + max


# Stats -------------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ logmass, data = sum_data)
summary(mean_model)

max_model <- glmmTMB(logmax ~ logmass, data = sum_data)
summary(max_model)
