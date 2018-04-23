# Change the thumbnail compression
library(jpeg)
library(imager)

# thumbnail height dimension
minD <- 150

thumbnails <- list.files(full.names = TRUE, path = 'www/images/species', recursive = TRUE, pattern = '^thumbnail')

for(i in thumbnails){
  
  org_path <- file.path(dirname(i), gsub('^thumbnail_', '', basename(i)))
  cat(paste(grep(i, thumbnails), basename(org_path), '\n'))
  
  if(!file.exists(org_path)){
    stop('File', i, 'none thumbnail does not exist')
  }
  
  # Load the image
  image <- load.image(org_path)
  
  # plot(image)
  
  ratio <- dim(image)[2] / minD
  
  # resize
  image_resized <- resize(image,
                          size_x = dim(image)[1]/ratio,
                          size_y = dim(image)[2]/ratio)
  
  # plot(image_resized)
  
  save.image(im = image_resized,
             file = i, quality = 1)

}
