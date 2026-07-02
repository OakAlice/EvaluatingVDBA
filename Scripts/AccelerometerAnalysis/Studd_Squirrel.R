# Analysis of Studd Squirrel dataset --------------------------------------

# labelled training data
freq <- dataset_variables$Frequency[dataset_variables$Name == "Studd_Squirrel"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Studd_Squirrel"]
list_locomotion_labels <- c("Locomotion")

# Prepare data ------------------------------------------------------------
# Already formatted in the DataReformatting github
dat <- fread(file.path(base_path, "Data/Accelerometer/Studd_Squirrel/Studd_Squirrel_formatted.csv")) %>%
  select(Time, X, Y, Z, ID, FuncActivity) %>%
  rename(Activity = FuncActivity)

# Get Vedba ---------------------------------------------------------------
# this has to be manual because I need a longer window
win <- 5 * freq 
ax_static <- frollmean(dat$X, n = win, align = "center", fill = NA)
ay_static <- frollmean(dat$Y, n = win, align = "center", fill = NA)
az_static <- frollmean(dat$Z, n = win, align = "center", fill = NA)
ax_dynamic <- dat$X - ax_static
ay_dynamic <- dat$Y - ay_static
az_dynamic <- dat$Z - az_static
dat$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Studd_Squirrel", "Mass_of_Individuals.csv")) %>%
  pull(LogMass)
summary$LogMass <- animal_mass

# Final summarisation -----------------------------------------------------
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

fwrite(summ_stats, file.path(base_path, "Output/Studd_SquirrelAccelerometer_summary_stats.csv"))




# Check whether the 0 is actually 0 ---------------------------------------
# plot <- dat %>% dplyr::filter(Activity == "Stationary")
# plot2 <- plot[1350:1650]
# ggplot(plot2, aes(x = seq(1:nrow(plot2)))) +
#       geom_path(aes(y = X, colour = "X")) +
#       geom_path(aes(y = Y, colour = "Y")) +
#       geom_path(aes(y = Z, colour = "Z")) +
#       geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5)
#       #geom_hline(yintercept = 1, linetype="dashed", color = "red") 
# 
# 
# mean(plot2$vedba)
# 
# ax_static <- frollmean(plot2$X, n = win, align = "center", fill = NA)
# ay_static <- frollmean(plot2$Y, n = win, align = "center", fill = NA)
# az_static <- frollmean(plot2$Z, n = win, align = "center", fill = NA)
# 
# sqrt(mean(ax_static, na.rm=T)^2 + mean(ay_static, na.rm=T)^2 + mean(az_static, na.rm=T)^2)
# 
