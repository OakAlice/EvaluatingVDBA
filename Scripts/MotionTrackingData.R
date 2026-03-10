# Analysing the motion tracking dtaa --------------------------------------

pacman::p_load(tidyverse, data.table, patchwork, glmmTMB)

base_path <- "C:/Users/PC/Documents/EvaluatingVDBA"

# Load in the data --------------------------------------------------------
data <- fread("MotionTracking/Compiled data 2026.csv") %>%
  mutate(LogMass = log10(Mass),
        MeanAccelGs = `av accel`/9.8,
        MaxAccelGs = `max accel`/9.8)

# Plot --------------------------------------------------------------------
mean_plot <- ggplot(data, aes(x= LogMass, y = log10(MeanAccelGs))) +
  geom_point(aes(colour = Species), size = 3) +
  geom_smooth(method = "lm", colour = "coral", se = FALSE, linewidth = 1.2) +
  my_theme() +
  xlab("Log mass (g)") + ylab("Log mean acceleration (g)") +
  scale_colour_manual(values = fave_colours)

max_plot <- ggplot(data, aes(x= LogMass, y = log10(MaxAccelGs))) +
  geom_point(aes(colour = Species), size = 3) +
  geom_smooth(method = "lm", colour = "coral", se = FALSE, linewidth = 1.2) +
  my_theme() +
  xlab("Log mass (g)") + ylab("Log max acceleration (g)") +
  scale_colour_manual(values = fave_colours)

plot <- (mean_plot + max_plot + plot_layout(guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.key.size = unit(0.4, "lines"),
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )
plot

# Stats -------------------------------------------------------------------
mean_model <- glmmTMB(log(MeanAccelGs) ~ LogMass, data = data)
mean_res <- broom.mixed::tidy(mean_model, effects = "fixed")
mean_results <- mean_res[grep("LogMass", mean_res$term), ]
mean_results$sig <- ifelse(mean_results$p.value < 0.05, "sig", "not sig")
mean_results$variable <- "mean"

max_model <- glmmTMB(log(MaxAccelGs) ~ LogMass, data = data)
max_res <- broom.mixed::tidy(max_model, effects = "fixed")
max_results <- max_res[grep("LogMass", max_res$term), ]
max_results$sig <- ifelse(max_results$p.value < 0.05, "sig", "not sig")
max_results$variable <- "max"

rbind(mean_results, max_results)

