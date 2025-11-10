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
  select(ID, threshold, meanVDBA, minVDBA, maxVDBA, dataset)

dataset_variables$dataset <- dataset_variables$Name
vdba_stuff1 <- merge(vdba_data, dataset_variables, by = 'dataset')

# find the species calibration settings
Gs <- fread(file.path(base_path, "Output", "G_scaling.csv"))
Gs_species <- unique(unlist(Gs$species[Gs$rescale == "G"]))
Gs_species <- na.omit(Gs_species)


# remove the non-mammal data 
vdba_stuff <- vdba_stuff1 %>% 
  # filter(Type == "Mam") %>%
  # filter(Zone == "Land") %>%
  dplyr::filter(dataset %in% Gs_species) %>%
  dplyr::filter(DeviceAttachment %in% c("Harness", "Collar")) %>%
  dplyr::filter(!dataset %in% c("Chakravarty_Meerkat", "Schloesing_FruitBat", "Nuijten_BewickSwans"))

# summarise by dataset
# vdba_stuff <- vdba_stuff %>% group_by(dataset, threshold, LogMass, Name) %>%
#   summarise(meanVDBA = mean(meanVDBA),
#             maxVDBA = mean(maxVDBA),
#             minVDBA = mean(minVDBA)
#             )

ggplot(vdba_stuff, aes(x = dataset, y = log10(maxVDBA), colour = threshold)) +
  geom_point(position = "jitter") +
  #scale_y_continuous(limits = c(0, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(vdba_stuff, aes(x = LogMass, y = log10(meanVDBA), shape = threshold, colour = Name)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = threshold, colour = threshold)) +
  theme_minimal() #+
 # facet_wrap(~DeviceAttachment)

summary(lm(log10(meanVDBA) ~ LogMass, data = vdba_stuff))




vdba_grouoed <- vdba_stuff %>% group_by(dataset, threshold, LogMass, Name) %>%
     summarise(meanVDBA = mean(meanVDBA),
             maxVDBA = mean(maxVDBA),
               minVDBA = mean(minVDBA)
               )
ggplot(vdba_grouoed, aes(x = LogMass, y = log10(maxVDBA), shape = threshold, colour = Name)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = threshold, colour = threshold)) +
  theme_minimal()





summary(glmmTMB(maxVDBA ~ LogMass, vdba_stuff, family = gaussian()))

