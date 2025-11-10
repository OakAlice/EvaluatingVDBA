# Dataset Specific Testing ------------------------------------------------
# these are the datasets where I know the mass of every individual

species <- c("Annett_Kangaroo", "Clemente_Impala", "Gaschk_Quoll", "Sparkes_Koala")

# Analysis ----------------------------------------------------------------
all_summaries <- lapply(species, function(x){
  weights <- fread(file.path(base_path, "AccelerometerData", x, "Mass_of_Individuals.csv")) %>%
    mutate(ID = as.character(ID))
  summary <- fread(file.path(base_path, "AccelerometerData", x, paste0(x, "_summary.csv"))) %>%
    mutate(Dataset = x,
           ID = as.character(ID))
  dat <- merge(summary, weights, by = "ID")
  dat
})


all_data <- rbindlist(all_summaries, fill = TRUE) %>%
  select(Dataset, ID, threshold, meanVDBA, minVDBA, maxVDBA, LogMass)


# Plot each species independently
p1 <- ggplot(all_data, aes(x = LogMass, y = log10(meanVDBA), colour = threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Dataset, scales = "free") +
  theme_minimal()

p1
# Save the plot
ggsave(
  filename = file.path(base_path, "Output", "Plots", "Species_Individuals_VDBA_Mass.png"),
  plot = p1,
  width = 8, height = 6, dpi = 300 
)

active_data <- all_data %>% dplyr::filter(threshold == "active")
model <- lmer(log10(meanVDBA) ~ LogMass + (1|Dataset), data = active_data)
summary(model)

# plot all the individuals togetehr
p2 <- ggplot(all_data, aes(x = LogMass, y = log10(meanVDBA), colour = threshold)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

p2
# Save the plot
ggsave(
  filename = file.path(base_path, "Output", "Plots", "All_Individuals_VDBA_Mass.png"),
  plot = p2,
  width = 8, height = 6, dpi = 300 
)

