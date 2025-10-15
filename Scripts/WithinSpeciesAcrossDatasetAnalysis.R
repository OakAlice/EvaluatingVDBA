# Same species but multiple datasets --------------------------------------

# find the files
directory <- c("Galea_Cat", "Smit_Cat", "Dunford_Cat")
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
  dat$dataset <- str_split(basename(x), "_")[[1]][1]
  dat
}) 
data <- rbindlist(data)


ggplot(data, aes(x = threshold, y = meanVDBA, colour = dataset)) + geom_boxplot()
