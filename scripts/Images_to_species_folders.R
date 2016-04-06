# Create species folders to contain images
spim <- read.csv('data/UKMoths/images/Images_requested_with_filenames.csv', stringsAsFactors = FALSE)
spdat <- read.csv('data/UKMoths/speciesData.csv', stringsAsFactors = FALSE)

# remove genera
spdat <- spdat[spdat$RANK != 'Genus',]

move_images <- function(i){
  
  print(i)
  
  dir.create(file.path('www/images/species', i))
  
  # Find the images for this species
  image_names <- spim$FILENAME[spim$NAME == i]
  
  # Copy these files and their thumbnails to the new folders
  file.copy(from = file.path('www/images/wft_400', image_names),
            to = file.path('www/images/species', i, image_names))
            
  file.copy(from = file.path('www/images/wft_400', paste('thumbnail',
                                                         image_names,
                                                         sep = '_')),
            to = file.path('www/images/species', i, paste('thumbnail',
                                                          image_names,
                                                          sep = '_')))
}

lapply(sort(unique(spdat$NAME)), FUN = move_images)