# Specific kangaroo analysis ----------------------------------------------
# Extracting the strides from the kangaroo
# To make the data comparable with the lizard and simulation data, I have to exract locomotion periods only
freq <- dataset_variables$Frequency[dataset_variables$Name == "Annett_Kangaroo"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Annett_Kangaroo"]


# Load in the data and metadata -------------------------------------------
files <- list.files(file.path(base_path, "Data/Accelerometer/Annett_Kangaroo", "raw"), full.names = TRUE)
sampling_times <- fread(file.path(base_path, "Data/Accelerometer/Annett_Kangaroo", "Sampling_Times.csv")) %>%
  mutate(CollarDate = as.POSIXct(as.character(CollarDate), format = "%Y%m%d"),
         DropOffDate = as.POSIXct(as.character(DropOffDate), format = "%Y%m%d"))
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Annett_Kangaroo", "Mass_of_Individuals.csv"))

#Prepare the data ---------------------------------------------------------
# define a butterworth filter
bw_cutoff = 5
bw_order = 4
fs = 50
bf <- butter(bw_order, bw_cutoff/(fs/2), type = "low")

# Processing loop
lapply(files, function(x){
  
  ID <- tools::file_path_sans_ext(basename(x))
  print(ID) # just to show me where we're up to
  
  if(file.exists(file.path(base_path, "Data/Accelerometer/Annett_Kangaroo/processed", paste0(ID, "_summary.csv")))){
    print("alrerady processed")
    return(NULL)
  }
  
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "X", "Y", "Z")
  
  # Crop the dates she wants ----------------------------------------------
  # convert the matlab time to normal time
  dat$Date <- as.Date(as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
  # crop only within the sampling days
  start <- sampling_times[sampling_times$Name == ID, CollarDate]
  end <- sampling_times[sampling_times$Name == ID, DropOffDate]
  dat <- dat[Date > start & Date < end]
  
  # butterworth filter to remove noise ------------------------------------
  dat$X.cl <- filtfilt(bf, dat$X)
  dat$Y.cl <- filtfilt(bf, dat$Y)
  dat$Z.cl <- filtfilt(bf, dat$Z)
  
  # calculate VDBA --------------------------------------------------------
  win <- 1 * 50  # smoothing window of 1 seocnd by 50 Hz
  # calculate the static accelerations
  ax_static <- frollmean(dat$X.cl, n = win, align = "center", fill = NA)
  ay_static <- frollmean(dat$Y.cl, n = win, align = "center", fill = NA)
  az_static <- frollmean(dat$Z.cl, n = win, align = "center", fill = NA)
  # get the dynamic component 
  ax_dynamic <- dat$X.cl - ax_static
  ay_dynamic <- dat$Y.cl - ay_static
  az_dynamic <- dat$Z.cl - az_static
  
  dat$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
  
  # threshold to define locomotion periods --------------------------------
  # this was figured out in the commented plotting section at bottom of file
  dat$activity <- ifelse(dat$vedba > 1, "locomotion", "other")
  
  dat$ID <- ID
  
  # save this for use later
  fwrite(dat, file.path(base_path, "Data/Accelerometer/Annett_Kangaroo/processed", paste0(ID, "_processed.csv")))
  
  # Summarise vedba of the locomotion periods -------------------------------
  # To make this data comparable to the other studies, we need to do over 3 second windows
  win <- 3 * 50 # now over 3 seconds
  dat[, wind_id := (seq_len(.N) - 1) %/% win]
  
  # select only windows with locomotion
  # initially got when they were mostly locomotion
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  dat_win <- dat[, .(Activity = get_mode(activity)), by = wind_id]
  dat_win <- dat_win[Activity == "locomotion", ]
  
  # then changed to finding windows that were 80% locomotionn
  # dat_win <- dat[, .(prop_loco = mean(activity == "locomotion"),
  #                    mean_vedba = mean(vedba, na.rm = TRUE)),
  #                by = wind_id][prop_loco >= 0.8]
  
  dat <- dat %>% dplyr::filter(wind_id %in% dat_win$wind_id)
  
  # summarise
  # now take a mean across each window
  summary <- dat %>%
    group_by(wind_id) %>%
    summarise(mean_vedba = mean(vedba, na.rm = TRUE),
              max_vedba = max(vedba, na.rm = TRUE))
  
  fwrite(summary, file.path(base_path, "Data/Accelerometer/Annett_Kangaroo/processed", paste0(ID, "_summary.csv")))
  
  return(NULL)
})

# Combine the summaries ---------------------------------------------------
summary_files <- list.files(file.path(base_path, "Data/Accelerometer/Annett_Kangaroo/processed"), pattern = "summary", full.names = TRUE)
summaries <- lapply(summary_files, function(x){
  df <- fread(x)
  df$ID <- gsub("_summary", "", tools::file_path_sans_ext(basename(x))) # left out ID in the previous step lol
  df
})
summaries <- rbindlist(summaries)

# now add in their mass
summaries <- merge(summaries, animal_mass %>% select("ID", "LogMass"), by = "ID") 

# note the different deployments
summaries$deployment <- ifelse(summaries$ID %in% c("Banjo", "Elton_John", "Kahn", "Rainman"), "2", "1")
summaries <- summaries %>% dplyr::filter(deployment == "1") # only select the first deployment

# and then mean them down further
summ_stats <- summaries %>%
  group_by(ID, LogMass, deployment) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/Annett_KangarooAccelerometer_summary_stats.csv"))


# Plots -------------------------------------------------------------------
# ggplot(summaries, aes(x = LogMass, y = log10(mean_vedba))) + 
#   geom_boxplot(aes(colour = ID, group = ID, fill = deployment)) + 
#   geom_smooth(method = "lm", aes(group = 1)) + 
#   my_theme()

mean_plot <- ggplot(summ_stats, aes(x = LogMass, y = logmean, colour = ID)) + 
  geom_errorbar(aes(ymin = log_lower, ymax = log_upper, colour = ID), width = 0.01) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), colour = "dodgerblue4", se = FALSE, linewidth = 2) +
  my_theme() + 
  theme(legend.position = "none") +
  scale_colour_manual(values = fave_colours) + 
  labs(x = "Log Mass (grams)", y = "Log mean VDBA (g)")

mean_plot

# save
img_output <- file.path(base_path, "Output", "Annett_KangarooAccelerometer.png")
ggsave(img_output, mean_plot)

# Statistics --------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ LogMass, data = summ_stats)
summary(mean_model)

mean_model2 <- glmmTMB(log10(mean_vedba) ~ LogMass, data = summaries)
summary(mean_model2)





# Playing -----------------------------------------------------------------
# romeo_data
# summary(romeo_data$vedba)
# which.max(romeo_data$vedba)
# data2 <- romeo_data[4525530:4525950]
# 
# data2 <- dat[150000:200000][seq(1, .N, 10)]
# 
# # summary(dat$vedba)
# # summary(data2$vedba)
# # 
# ggplot(data2, aes(x = Time)) +
#   geom_path(aes(y = X.cl, colour = "X")) +
#   geom_path(aes(y = Y.cl, colour = "Y")) +
#   geom_path(aes(y = Z.cl, colour = "Z")) +
#   geom_path(aes(y = vedba, colour = "vedba"), linewidth = 2, alpha = 0.5) +
#   geom_point(aes(y = vedba, colour = activity)) +
#   geom_hline(yintercept = 1, linetype="dashed", color = "red") +
#   my_theme()
# 
# # ggplot(data2, aes(x = vedba)) + geom_histogram(bins = 100) + 
# #   geom_vline(xintercept = 1, linetype="dashed", color = "red") +
# #   my_theme()
