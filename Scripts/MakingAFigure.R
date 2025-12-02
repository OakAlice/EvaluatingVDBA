# making a figure for the paper

# packages
library(data.table)
library(tidyverse)
library(patchwork)

# theme details
my_theme <- function() {
  theme_minimal(base_size = 10, base_family = "serif") +
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
sample_colours <- c(
  "firebrick3", "coral",  "goldenrod2",  "khaki2", 
  "seagreen", "darkcyan", "cornflowerblue", "slateblue2", 
  "hotpink",  "rosybrown1"
)

# code
dat <- fread(file.path(base_path, "AccelerometerData", "Smit_Cat", "raw", "Formatted_raw_data.csv"))


# extract the sections of data I want to use
walking <- dat %>% 
  dplyr::filter(ID == "Cho",
                Activity == "Active_Walking") %>% 
  slice(210:410)
inactive <- dat %>% 
  dplyr::filter(ID == "Cho",
                Activity == "Inactive_Sitting.Stationary") %>% 
  slice(250:450)
jump <- dat %>% 
  dplyr::filter(ID == "Cho",
                Activity %in% c("Active_Jumping.Horizontal", "Active_Jumping.Vertical")) %>% 
  slice(1:200)
# check the sections independently
#ggplot(jump, aes(x = seq_len(nrow(jump)), y = X)) + geom_line()

# combine them
subset <- rbind(inactive, walking, jump)

# generate the metrics
win <- 1 * 25  # smoothing window over 25hz

# calculate the static accelerations
subset$ax_static <- frollmean(subset$X, n = win, align = "center", fill = NA)
subset$ay_static <- frollmean(subset$Y, n = win, align = "center", fill = NA)
subset$az_static <- frollmean(subset$Z, n = win, align = "center", fill = NA)

# get the dynamic component 
subset$ax_dynamic <- subset$X - subset$ax_static
subset$ay_dynamic <- subset$Y - subset$ay_static
subset$az_dynamic <- subset$Z - subset$az_static

subset$vedba <- sqrt(subset$ax_dynamic^2 + subset$ay_dynamic^2 + subset$az_dynamic^2)

# smooth it
subset[, smooth_vdba := frollmean(vedba, n = win, align = "center", fill = NA)]



# plots 
raw <- ggplot(subset, aes(x = seq_len(nrow(subset)))) +
  geom_line(aes(y = X), colour = "lightcoral", linewidth = 2) +
  geom_line(aes(y = Y), colour = "khaki2", linewidth = 2) +
  geom_line(aes(y = Z), colour = "seagreen", linewidth = 2) +
  my_theme() +
  ylab("Acceleration (g)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

vdba <- ggplot(subset, aes(x = seq_len(nrow(subset)))) +
  geom_line(aes(y = vedba, colour = "Raw"), linewidth = 2) +
  geom_line(aes(y = smooth_vdba, colour = "Smoothed"), linewidth = 2) +
  scale_colour_manual(values = c("Raw" = "#0097b2",
                                 "Smoothed" = "#505a6d")) +
  my_theme() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1)
  ) +
  labs(x = "Time", y = "VDBA")

raw / vdba




