# Same species but multiple datasets --------------------------------------

# find the files
directory <- c("Annett_Kangaroo", "Annett_Possum", "Clemente_Impala", "DiCicco_Perentie",
               "Sparkes_Koala", "Galea_Cat")
files <- lapply(directory, function(dir) {
  list.files(
    file.path(base_path, "AccelerometerData", dir),
    pattern = "[A-Za-z]+_summary\\.csv$",
    full.names = TRUE
  )
})
files <- unlist(files, use.names = FALSE)

# read it together
data <- list()
data <- lapply(files, function(x){
  dat <- fread(x)
  dat$dataset <- paste(str_split(basename(x), "_")[[1]][1], str_split(basename(x), "_")[[1]][2], sep = "_")
  dat
}) 
data <- rbindlist(data)

ggplot(data, aes(x = threshold, y = meanVDBA, colour = dataset)) + geom_boxplot()
