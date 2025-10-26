# Plot the VDBA against the body size -------------------------------------

summary_files <- list.files(file.path(base_path, "AccelerometerData"), recursive = TRUE, pattern = "*_summary\\.csv$", full.names = TRUE)

vdba_data <- lapply(summary_files, function(x){
  
  dat <- fread(x) %>% na.omit()
  dat$ID <- as.character(dat$ID)
  species <- basename(dirname(x)) 
  
  dat$dataset <- species
  dat

})
vdba_data <- rbindlist(vdba_data, fill = TRUE)

vdba_data <- vdba_data %>%
  filter(dataset %in% Gs_species) %>%
  select(ID, threshold, meanVDBA, minVDBA, maxVDBA, dataset)

dataset_variables$dataset <- dataset_variables$Name
vdba_stuff <- merge(vdba_data, dataset_variables, by = 'dataset')

# summarise by dataset
vdba_stuff <- vdba_stuff %>% group_by(dataset, threshold, LogMass, Name) %>%
  summarise(meanVDBA = mean(meanVDBA),
            maxVDBA = mean(maxVDBA),
            minVDBA = mean(minVDBA)
            )

# remove the non-mammal data
# vdba_stuff <- vdba_stuff %>% filter(Type == "Mam")

ggplot(vdba_stuff, aes(x = dataset, y = maxVDBA, colour = threshold)) +
  geom_point() +
  #scale_y_continuous(limits = c(0, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(vdba_stuff, aes(x = LogMass, y = log10(meanVDBA), shape = threshold, colour = Name)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = threshold, colour = threshold)) +
  theme_minimal() #+
 # facet_wrap(~DeviceAttachment)

