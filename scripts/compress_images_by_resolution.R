# Loop though all the images and compress to a maximum dimension
# try 400px for starters
library(imager)

images <- list.files('www/images/wft/', full.names = TRUE)

# A function that takes an image path, resizes it and saves it
resize_image <- function(path, max_dim = 400, saveDir,
                         thumbnail = TRUE, thumb_max_dim = 120){
  
  # Load the image
  image <- load.image(path)
  
  # Calculate the rescaling ratio
  ratio <- max(dim(image)[1:2]) / max_dim
  
  # resize
  image_resized <- resize(image,
                          size_x = dim(image)[1]/ratio,
                          size_y = dim(image)[2]/ratio)
  
  # Save the image using the same name but a new location
  if(!dir.exists(saveDir))dir.create(saveDir, showWarnings = FALSE)
  save.image(im = image_resized,
             file = file.path(saveDir, basename(path)))
  
  if(thumbnail){
    
    # Calculate the rescaling ratio
    tratio <- dim(image)[2] / thumb_max_dim
    
    # resize
    image_resized <- resize(image,
                            size_x = dim(image)[1]/tratio,
                            size_y = dim(image)[2]/tratio)
    
    # Save the image using the same name but a new location
    if(!dir.exists(saveDir)) dir.create(saveDir, showWarnings = FALSE)
    save.image(im = image_resized,
               file = file.path(saveDir,
                                paste('thumbnail',
                                      basename(path),
                                      sep = '_')))
    
  }
  
  return(NULL)
             
}

library(parallel)
library(snowfall)

sfInit(parallel = TRUE, type = 'SOCK', cpus = detectCores())

sfExportAll()
sfLibrary(imager)

shef_data <- sfClusterApplyLB(images,
                              resize_image,
                              max_dim = 400,
                              saveDir = 'www/images/wft_400',
                              thumbnail = TRUE,
                              thumb_max_dim = 150)
sfStop()

# Did they all get done?
basename(images) %in% basename(list.files('www/images/wft_400/'))

# Yep!