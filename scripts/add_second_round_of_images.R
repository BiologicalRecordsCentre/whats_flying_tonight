################################
## Add second round of images ##
################################

# First add the image names to the .csv of images
spDat <- read.csv(file = 'data/UKMoths/spDatImages.csv', stringsAsFactors = FALSE)
new_images <- read.csv(file = 'data/UKMoths/second_image_set/wft_images_additional/images_additonal_with_filenames.csv',
                       stringsAsFactors = FALSE)

# How many are missing? 290
nrow(spDat[spDat$image == '', ])

# Adding 78 species
length(unique(new_images$NAME))

# Do we have any of these already? 1 
spDat[spDat$NAME %in% new_images$NAME & spDat$image != '',]

# Do any names not match? Nope
new_images$NAME[!new_images$NAME %in% spDat$NAME]

# Loop through the new images and add them to the rows
for(i in unique(new_images$NAME)){
  spDat$image[spDat$NAME == i] <- paste(c(spDat$image[spDat$NAME == i],
                                          new_images$FILENAME[new_images$NAME == i]),
                                        collapse = ',')
}

# Get rid of leading comma
spDat$image <- gsub(pattern = '^,', '', spDat$image)

# How many still missing? 219
nrow(spDat[spDat$image == '', ])

# This should be 212...?
missingImages <- spDat$NAME[spDat$image == '']
missingImages[missingImages %in% new_images$NAME]

# Not sure what is going on but none of those missing images
# are in the new dataset

# Add NA image
spDat$image[spDat$image == ''] <- 'NAimage.png'

# Write
write.csv(spDat, file = 'data/UKMoths/spDatImages2.csv',
          row.names = FALSE)

## Now compress all the new images ##
library('jpeg')

images <- list.files(path = 'data/UKMoths/second_image_set/wft_images_additional',
                     pattern = '.jpg$', full.names = TRUE)

for(i in images){
  
  print(basename(i))
  img <- readJPEG(source = i)
  
  writeJPEG(image = img,
            target = file.path('www/images/second_image_set/compressed',
                               basename(i)),
            quality = 0.3)
}

## Create the thumbnails ##
library(imager)

# A function that takes an image path, resizes it and saves it
resize_image <- function(path, saveDir, thumb_max_dim = 120){
  
  # Load the image
  image <- load.image(path)
  
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

for(i in list.files(path = 'www/images/second_image_set/wft_images_additional',
                    pattern = '.jpg$', full.names = TRUE)){
  
  cat(i, '\n')
  resize_image(path = i, saveDir = 'www/images/second_image_set/thumbnails')
  
}

## Move the new images into the species folders ##
# Here are the species to work 
new_spDat <- spDat[spDat$NAME %in% unique(new_images$NAME), ]

for(i in 1:nrow(new_spDat)){
  
  sp <- gsub(' ', '_', new_spDat$NAME[i])
  cat(sp,'\n')
  
  files <- unlist(strsplit(new_spDat$image[i], split = ','))
  
  for(j in files){
    # If no thumbnail make the first one the thumbnail
    if(!dir.exists(file.path('www/images/species', sp, 'thumbnail'))){
      
      dir.create(file.path('www/images/species', sp, 'thumbnail'))
      
      # copy large image
      file.copy(from = file.path('www/images/second_image_set/compressed', j),
                to = file.path('www/images/species', sp, 'thumbnail', j))
      
      # copy thumbnail
      file.copy(from = file.path('www/images/second_image_set/thumbnails',
                                 paste('thumbnail', j, sep = '_')),
                to = file.path('www/images/species', sp, 'thumbnail',
                               paste('thumbnail', j, sep = '_')))
      
    } else {
      # copy only if not there already
      if(!j %in% basename(list.files(file.path('www/images/species', sp), recursive = TRUE))){ 
        file.copy(from = file.path('www/images/second_image_set/compressed', j),
                  to = file.path('www/images/species', sp, j)) 
      }
    }
  }
}

# And done!
