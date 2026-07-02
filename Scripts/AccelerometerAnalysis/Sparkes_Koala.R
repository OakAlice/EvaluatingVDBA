# Analysis of the koala data ----------------------------------------------
freq <- dataset_variables$Frequency[dataset_variables$Name == "Sparkes_Koala"]
stride_window <- dataset_variables$StrideWindow[dataset_variables$Name == "Sparkes_Koala"]
list_locomotion_labels <- c("Walking", "Trotting", "Running")

# butterworth filter
bw_cutoff = 5
bw_order = 4
fs = 50
bf <- butter(bw_order, bw_cutoff/(fs/2), type = "low")


# Prepare the data --------------------------------------------------------
# Call in the data from where it's stored and join the raw data with the predictions
data_path <- "C:/Users/PC/Documents/KoalaAnalysis/KoalaAnalysis"
koalas <- str_split(basename(list.dirs(file.path(data_path, "Raw_Wild_Data"))), "_", simplify = T)[,1][2:10]

# go through all the koalas
lapply(koalas, function(x){
  
  print(x) # name of the koala
  
  if(file.exists(file.path(base_path, "Data/Accelerometer/Sparkes_Koala/processed", paste0(x, "_summary.csv")))){
    print("alrerady processed")
    return(NULL)
  }
  
  chunks <- list.files(file.path(data_path, "Predicted_Wild_Data", x), full.names = TRUE)
  raw_chunks <- list.files(file.path(data_path, "Raw_Wild_Data", paste0(x, "_Chunked")), full.names = TRUE)
  
  # load in the preds, smooth the preds, add to the raw data
  joined_data <- list()
  for (chunk in chunks){
    
    # get the preds
    preds <- fread(chunk) %>% select(Time, predicted_classes)
    
    # get the raw data
    number <- str_split(basename(chunk), "_", simplify = T)[2]
    raw <- fread(grep(number, raw_chunks, value = TRUE))[,2:5]
    colnames(raw) <- c("Time", "X", "Y", "Z")
    
    # join them together
    setkey(raw, Time)
    setkey(preds, Time)
    
    raw <- preds[raw, on = "Time", roll = TRUE]
    
    # butterworth filter to remove noise 
    raw$X.cl <- filtfilt(bf, raw$X)
    raw$Y.cl <- filtfilt(bf, raw$Y)
    raw$Z.cl <- filtfilt(bf, raw$Z)
    
    # calculate VDBA 
    win <- 1 * 50  # smoothing window of 1 seocnd by 50 Hz
    # calculate the static accelerations
    ax_static <- frollmean(raw$X.cl, n = win, align = "center", fill = NA)
    ay_static <- frollmean(raw$Y.cl, n = win, align = "center", fill = NA)
    az_static <- frollmean(raw$Z.cl, n = win, align = "center", fill = NA)
    # get the dynamic component 
    ax_dynamic <- raw$X.cl - ax_static
    ay_dynamic <- raw$Y.cl - ay_static
    az_dynamic <- raw$Z.cl - az_static
    
    raw$vedba <- sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)
    
    joined_data[[number]] <- raw
  }
  joined_data <- rbindlist(joined_data)
  
  # remove the first and last day (handling effects)
  joined_data$Date <- as.Date(as.POSIXct((joined_data$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC"))
  joined_data <- joined_data[Date > min(joined_data$Date) & Date < max(joined_data$Date)]
  
  # save it
  fwrite(joined_data, file.path(base_path, "Data/Accelerometer/Sparkes_Koala/processed", paste0(x, "_processed.csv")))
  
  # select only windows with locomotion
  win <- 3 * 50 # over 3 seconds
  joined_data[, wind_id := (seq_len(.N) - 1) %/% win]
  
  # fidn when mostly a locomotion window
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  dat_win <- joined_data[, .(Activity = get_mode(predicted_classes)), by = wind_id]
  dat_win <- dat_win[Activity == "Walking", ]
  
  joined_data <- joined_data %>% dplyr::filter(wind_id %in% dat_win$wind_id)
  
  # summarise
  # now take a mean across each window
  summary <- joined_data %>%
    group_by(wind_id) %>%
    summarise(mean_vedba = mean(vedba, na.rm = TRUE),
              max_vedba = max(vedba, na.rm = TRUE))
  
  # also save
  fwrite(summary, file.path(base_path, "Data/Accelerometer/Sparkes_Koala/processed", paste0(x, "_summary.csv")))
  
  return(NULL)
   
})

# Combine the summaries ---------------------------------------------------
summary_files <- list.files(file.path(base_path, "Data/Accelerometer/Sparkes_Koala/processed"), pattern = "summary", full.names = TRUE)
summaries <- lapply(summary_files, function(x){
  df <- fread(x)
  df$ID <- gsub("_summary", "", tools::file_path_sans_ext(basename(x))) # left out ID in the previous step lol
  df
})
summaries <- rbindlist(summaries)

# now add in their mass
animal_mass <- fread(file.path(base_path, "Data/Accelerometer/Sparkes_Koala", "Mass_of_Individuals.csv"))
summaries <- merge(summaries, animal_mass %>% select("ID", "LogMass", "Sex"), by = "ID") 

# and then mean them down further
summ_stats <- summaries %>%
  group_by(ID, LogMass, Sex) %>%
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

fwrite(summ_stats, file.path(base_path, "Output/Sparkes_KoalaAccelerometer_summary_stats.csv"))

# Plots -------------------------------------------------------------------
# ggplot(summaries, aes(x = LogMass, y = log10(mean_vedba))) +
#   geom_boxplot(aes(colour = ID, group = ID)) +
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
img_output <- file.path(base_path, "Output", "Sparkes_KoalaAccelerometer.png")
ggsave(img_output, mean_plot)

# Statistics --------------------------------------------------------------
mean_model <- glmmTMB(logmean ~ LogMass, data = summ_stats)
summary(mean_model)

mean_model2 <- glmmTMB(log10(mean_vedba) ~ LogMass, data = summaries)
summary(mean_model2)
