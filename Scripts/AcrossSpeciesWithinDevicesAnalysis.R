# Same species but multiple datasets --------------------------------------

# find the files

device_dictionary

axivity_directory <- dataset_variables %>% filter(Device == "Axivity") %>% pull(Name)
eobs_directory <- dataset_variables %>% filter(Device == "E-obs GmbH") %>% pull(Name)

directory <- axivity_directory

files <- lapply(directory, function(dir) {
  list.files(
    file.path(base_path, "AccelerometerData", dir),
    pattern = "_summary.csv",
    full.names = TRUE
  )
})
files <- unlist(files, use.names = FALSE)

# read it together
data <- lapply(files, function(x){
  dat <- fread(x)
  dat$dataset <- paste(str_split(basename(x), "_")[[1]][1],  str_split(basename(x), "_")[[1]][2], sep = "_")
  dat
}) 
data <- rbindlist(data)

# add the weights in
Mass <- dataset_variables[, c("Name", "Log_Mass")]
colnames(Mass) <- c("dataset", "Log_Mass")
data <- merge(data, Mass, by = "dataset")

ggplot(data, aes(x = threshold, y = meanVDBA, colour = dataset)) + geom_boxplot()

ggplot(data, aes(x = Log_Mass, y = log(meanVDBA), colour = threshold)) + 
  geom_point() + 
  geom_smooth(method = "lm")
