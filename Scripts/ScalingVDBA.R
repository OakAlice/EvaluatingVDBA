# Plot the VDBA against the body size -------------------------------------

summary_files <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "*_summary\\.csv$", full.names = TRUE)

vdba_data <- lapply(summary_files, function(x){
  
  dat <- fread(x) %>% na.omit()
  dat$ID <- as.character(dat$ID)
  species <- basename(dirname(x)) 
  
  dat$dataset <- species
  dat

})
vdba_data <- rbindlist(vdba_data)

dataset_variables$dataset <- dataset_variables$Name
vdba_stuff <- merge(vdba_data, dataset_variables, by = 'dataset')

# remove the stork data
vdba_stuff <- vdba_stuff %>% filter(!Type == "Bird")

ggplot(vdba_stuff, aes(x = dataset, y = meanVDBA, colour = threshold)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(vdba_stuff, aes(x = Log_Mass, y = log(meanVDBA), colour = dataset)) +
  geom_boxplot() +
  facet_wrap(~ threshold)
