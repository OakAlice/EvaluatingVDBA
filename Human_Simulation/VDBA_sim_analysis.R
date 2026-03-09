


library(R.matlab)
library(stringr)

setwd('G:/Project files/gravity sims')

merge_dat<- read.csv('merged_df.csv')



#get list of files
setwd('G:/Project files/gravity sims/Ioana GravitySims/Sims')

fileslist<-list.files(pattern='VDBA.mat',
                      recursive = TRUE,
                      full.names = TRUE)


results <- lapply(fileslist, function(f) get_mean_vedba_safe(f, merge_dat))
results <- Filter(Negate(is.null), results)
library(dplyr)
final_df <- bind_rows(results)

write.csv(final_df, "VDBA_output.csv")

plot(mean_pelvis~Mass, final_df, pch=20)

library(plotly)


plot_3d <- plot_ly(
  final_df,
  x = ~log10(Mass),
  y = ~mean_speed,
  z = ~mean_pelvis,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 10, color = ~mean_pelvis, colorscale = "Viridis")
  
) %>%
  layout(
    scene = list(
      xaxis = list(title = "log Mass (kg)"),
      yaxis = list(title = "Speed (m/s)"),
      zaxis = list(title = "Pelvis VeDBA")
    )
  )

htmlwidgets::saveWidget(plot_3d, "VDBA_3dplot.html", selfcontained = TRUE)
browseURL("VDBA_3dplot.html")





library(mgcv)
library(plotly)

# Your data frame should look like:
final_df$Mass  <- as.numeric(final_df$Mass)      
final_df$mean_speed  
final_df$mean_pelvis  

# Fit a smooth GAM surface
gam_fit <- gam(mean_pelvis ~ s(Mass, mean_speed), data = final_df)

# Create grid for the surface
mass_seq  <- seq(min(final_df$Mass),       max(final_df$Mass),       length.out = 40)
speed_seq <- seq(min(final_df$mean_speed), max(final_df$mean_speed), length.out = 40)

grid <- expand.grid(
  Mass       = mass_seq,
  mean_speed = speed_seq
)

# Predict pelvis VeDBA across the grid
grid$pred <- predict(gam_fit, newdata = grid)

# Convert predictions to matrix for plotly
z_matrix <- matrix(grid$pred,
                   nrow = length(mass_seq),
                   ncol = length(speed_seq))

# 3D plot
P <- plot_ly() %>%
  
  # Data points
  add_markers(
    data = final_df,
    x = ~Mass,
    y = ~mean_speed,
    z = ~mean_pelvis,
    marker = list(size = 4, color = "black"),
    name = "Data"
  ) %>%
  
  # Smooth GAM surface
  add_surface(
    x = mass_seq,
    y = speed_seq,
    z = z_matrix,
    colorscale = "Viridis",
    opacity = 0.7,
    name = "GAM Surface"
  ) %>%
  
  # Axis labels
  layout(
    scene = list(
      xaxis = list(title = "Mass"),
      yaxis = list(title = "Speed (m/s)"),
      zaxis = list(title = "Pelvis VeDBA"),
      camera = list(eye = list(x = 1.4, y = 1.4, z = 1.2))
    )
  )



htmlwidgets::saveWidget(P, "VDBA_3dplot.html", selfcontained = TRUE)
browseURL("VDBA_3dplot.html")











get_mean_vedba_safe <- function(fname, merge_dat) {
  tryCatch({
    
    # strip "_VDBA.mat" → ".mat"
    mat_core <- str_replace(fname, "_VDBA\\.mat$", ".mat")
    
    # find matching row
    row_idx <- grep(mat_core, merge_dat$file)
    if (length(row_idx) == 0) {
      message("No matching row found for ", fname, " — skipping.")
      return(NULL)
    }
    
    # check status
    if (merge_dat$status[row_idx] != "ok") {
      message("Status not OK for ", fname, " — skipping.")
      return(NULL)
    }
    
    # load MATLAB file
    dat <- readMat(fname)
    
    mass <- sub(".*scale([0-9]+).*", "\\1", fname)
    
    return(list(
      file = fname,
      Mass = mass,
      mean_pelvis = mean(dat$VeDBA.pelvis, na.rm = TRUE),
      mean_torso  = mean(dat$VeDBA.torso, na.rm = TRUE),
      mean_speed = mean(dat$speedPelvis.body[1:50], na.rm = TRUE),
      row = row_idx
    ))
    
  }, error = function(e) {
    message("Error reading ", fname, ": ", conditionMessage(e), " — skipping.")
    return(NULL)
  })
}




