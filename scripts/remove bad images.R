rm(list = ls())
library(imager)

load('data/UKMoths/speciesData_newNames2017.rdata')

# Using Marks list of 'bad' images go through and move these to the archive
classifications <- read.csv('picture_classifier/classifications.csv')

head(classifications)

# loop through the images and remove the bad ones
for(i in tail(gsub('^../', '', as.character(classifications$path[classifications$classification == 'bad'])), -11)){
  
  cat(basename(i),'\n')
  
  # is it a thumbnail
  if(grepl('thumbnail$', dirname(i))){
  
    # Remove the image and the thumbnail
    i_thumb <- file.path(dirname(i), paste('thumbnail', basename(i), sep = '_'))
    if(!file.exists(i_thumb)) stop('thumbnail does not exist')
    if(!file.exists(i)) stop('image does not exist')
    unlink(i)
    unlink(i_thumb)
    
    # HERE REMOVE IMAGE FROM speciesDataRaw 
    # Not this file is not actually used to build the app it is 
    # the file structure that is used to build the app
    i_name <- gsub('_', ' ', regmatches(gsub('/thumbnail','',dirname(i)),regexpr('(?<=species//)[[:alnum:][:punct:]]+', i, perl = TRUE)))
    i_images <- strsplit(speciesDataRaw[speciesDataRaw$NAME == i_name, 'image'], split = ',')[[1]]
    speciesDataRaw[speciesDataRaw$NAME == i_name, 'image'] <- paste(i_images[!i_images %in% basename(i)], collapse = ',')
    save(speciesDataRaw, file = 'data/UKMoths/speciesData_newNames2017.rdata')
    
    # If a thumbnail is removed make sure this is replaced with another thumbnail
    potential_thumbs <- list.files(path = gsub('/thumbnail','',dirname(i)),
               full.names = TRUE,
               include.dirs = FALSE, pattern = '.jpg$')
    potential_thumbs[!potential_thumbs %in% gsub('^../', '', as.character(classifications$path[classifications$classification == 'bad']))]
    
    if(length(potential_thumbs) == 0){
      warning(paste('No replacement thumbnail for', basename(i)))
    } else {
      move_to <- file.path(dirname(potential_thumbs[1]),
                           'thumbnail',
                           basename(potential_thumbs[1]))
      file.rename(from = potential_thumbs[1],
                  to = move_to)
      # Create thumbnail
      image <- load.image(move_to)
      tratio <- dim(image)[2] / 150
      
      # resize
      image_resized <- resize(image,
                              size_x = dim(image)[1]/tratio,
                              size_y = dim(image)[2]/tratio)
      
      # Save the image
      save.image(im = image_resized,
                 file = file.path(dirname(move_to),
                                  paste('thumbnail',
                                        basename(move_to),
                                        sep = '_')))
      
    }
    
  } else {
    
    # remove the file
    unlink(i)
    
    i_name <- gsub('_', ' ', regmatches(dirname(i),regexpr('(?<=species//)[[:alnum:][:punct:]]+', i, perl = TRUE)))
    
    i_images <- strsplit(speciesDataRaw[speciesDataRaw$NAME == i_name, 'image'], split = ',')[[1]]
    
    speciesDataRaw[speciesDataRaw$NAME == i_name, 'image'] <- paste(i_images[!i_images %in% basename(i)], collapse = ',')
    
    save(speciesDataRaw, file = 'data/UKMoths/speciesData_newNames2017.rdata')
    
  }
  
}
# When an image is removed make sure it is also removed from the species info .csv