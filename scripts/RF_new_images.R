# Compress Richards new images
library('jpeg')
library(imager)

files <- list.files(path = 'www/images/From Richard Fox', pattern = 'jpg$', full.names = TRUE)

for(file in files){
  
  print(file)
  img <- readJPEG(source = file)
  writeJPEG(image = img,
            target = file.path(dirname(file),
                               'compressed',
                               basename(file)),
            quality = 0.3)
  
}

# I have put the image in place but now need to crete the thumbnails.

# Find all the thumbnail directories that dont have a thumbnail but do have a big image
dirs <- list.dirs('www/images/species', recursive = TRUE, full.names = TRUE)
tdirs <- dirs[basename(dirs) == 'thumbnail']

tn <- NULL

for (i in tdirs){
  fs <- list.files(i, full.names = TRUE)
  if(length(fs) > 0){
    if(!any(grepl('^thumbnail', basename(fs)))){
      tn <- c(tn, fs)
    }
  }
}

tn
minD <- 150

# Once done you need o edit the file additiona_images.csv in the data folder

for(i in tn){
  
  org_path <- i

  if(!file.exists(org_path)) stop('File', i, 'thumbnail does not exist')
  
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
             file = file.path(dirname(i), paste0('thumbnail_', basename(i))),
             quality = 1)
  
}
