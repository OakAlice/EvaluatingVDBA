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

# summarise by dataset
vdba_data <- vdba_data %>% group_by(dataset, threshold) %>%
  summarise(mean_vedba = mean(meanVDBA))

dataset_variables$dataset <- dataset_variables$Name
vdba_stuff <- merge(vdba_data, dataset_variables, by = 'dataset')

# remove the non-mammal data
vdba_stuff <- vdba_stuff %>% filter(Type == "Mam")

ggplot(vdba_stuff, aes(x = dataset, y = mean_vedba, colour = threshold)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(vdba_stuff, aes(x = LogMass, y = log(mean_vedba))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ threshold)
