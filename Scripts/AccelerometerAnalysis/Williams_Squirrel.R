# Williams Squirrel -------------------------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Williams_Squirrel"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Williams_Squirrel"]
list_locomotion_labels <- c("Locomotion")

# Prepare the data --------------------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Williams_Squirrel/raw/"), full.names = TRUE)
data <- lapply(files, function(x) {
  dat <- fread(x, col.names = c(
    "year", "month", "day", "hour", "min", "sec", "doy", "uniqueID",
    "Sex", "Site", "acc_x", "acc_y", "acc_z", "odba"
  ))[1:(.N/2)]  # take first half of rows (just because the files are really big)

  dat <- dat %>%
    group_by(uniqueID) %>%
    mutate(Time = row_number()) %>%
    rename(ID = uniqueID, X = acc_x, Y = acc_y, Z = acc_z) %>%
    select(ID, Time, X, Y, Z) %>%
    ungroup()
  
  dat
})
dat <- rbindlist(data)

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

# define a locomotion threshold
dat$Activity <- ifelse(dat$vedba > 0.5, "Locomotion", "Other")

# Get locomotion ----------------------------------------------------------
dat <- get_locomotion(dat, freq, stride_window, list_locomotion_labels)

# Summarise ---------------------------------------------------------------
# now take a mean across each window
summary <- dat %>%
  group_by(ID, wind_id) %>%
  summarise(mean_vedba = mean(vedba, na.rm = TRUE),
            max_vedba = max(vedba, na.rm = TRUE))

# Add animal mass ---------------------------------------------------------
animal_mass <- dataset_variables$LogMass[dataset_variables$Name == "Williams_Squirrel"]
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

fwrite(summ_stats, file.path(base_path, "Output/Williams_SquirrelAccelerometer_summary_stats.csv"))






plot <- dat[30000:35000,]
ggplot(plot, aes(x = Time)) +
    geom_path(aes(y = X, colour = "X")) +
    geom_path(aes(y = Y, colour = "Y")) +
    geom_path(aes(y = Z, colour = "Z")) +
    geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
    geom_hline(yintercept = 1, linetype="dashed", color = "red") +
    my_theme()




