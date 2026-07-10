# Analysis of the Pangano Bear dataset ------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Pagano_Bear"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Pagano_Bear"]
list_locomotion_labels <- c("walking")


# Data --------------------------------------------------------------------
# Data was already reformatted in the DataReformatting github
dat <- fread(file.path(base_path, "Data/Accelerometer/Pagano_Bear/Pagano_Bear_formatted.csv")) %>%
  mutate(X = X/9.81,
         Y = Y/9.81,
         Z = Z/9.81)

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

# Add mass ----------------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Pagano_Bear", "Mass_of_Individuals.csv"))
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
  ) %>%
  mutate(ID = as.character(ID))

fwrite(summ_stats, file.path(base_path, "Output/Pagano_BearAccelerometer_summary_stats.csv"))

# Plots -------------------------------------------------------------------
# mean_plot <- ggplot(summ_stats, aes(x = LogMass, y = logmean, colour = as.factor(ID))) + 
#   geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = ID), width = 0.01) +
#   geom_point(size = 3) +
#   geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
#   my_theme() + 
#   scale_colour_manual(values = fave_colours) + 
#   theme(legend.position = "none") +
#   labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)")
# 
# mean_plot
# 
# # save
# img_output <- file.path(base_path, "Output", "Pagano_BearAccelerometer.png")
# ggsave(img_output, mean_plot)
# 
# # Statistics --------------------------------------------------------------
# mean_model <- glmmTMB(logmean ~ LogMass, data = summ_stats)
# summary(mean_model)
# 
# mean_model2 <- glmmTMB(log10(mean_vedba) ~ LogMass, data = summary)
# summary(mean_model2)




# Play --------------------------------------------------------------------
# Determine the scaling ---------------------------------------------------
# static <- dat %>% dplyr::filter(Activity %in% c("walking"))
# 
# plotdat <- static %>%
#    group_by(ID) %>%
#    mutate(row_idx = row_number()) %>%
#    ungroup()
# 
#  ggplot(plotdat, aes(x = row_idx)) +
#    geom_path(aes(y = X, colour = "X")) +
#    geom_path(aes(y = Y, colour = "Y")) +
#    geom_path(aes(y = Z, colour = "Z")) +
#    facet_grid(~ID, scales = "free")


