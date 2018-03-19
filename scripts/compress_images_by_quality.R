rm(list=ls())

images <- list.files(path = 'www/images/wft/',
                     pattern = '.jpg$',
                     full.names = TRUE,
                     recursive = TRUE)

images <- images[!basename(images) %in% list.files(path  = 'www/images/wft_0.3/')]


library('jpeg')

for(i in images){

  print(i)
  img <- readJPEG(source = i)

  writeJPEG(image = img,
            target = file.path('www/images/wft_0.3',
                               basename(i)),
            quality = 0.3)
}

# Now go through all of the images that I have already and
# replace matches with these low quality but hig res images
sp_images <- list.files(path = 'www/images/species/',
                        pattern = '.jpg$',
                        full.names = TRUE,
                        recursive = TRUE)

head(sp_images)

sp_images_gal <- sp_images[!grepl('^thumbnail', basename(sp_images))]
gd_im <- list.files(path = 'www/images/wft_0.3',
                    pattern = '.jpg$',
                    full.names = TRUE,
                    recursive = TRUE)

# Replace all the gallery images with the compressed ones
for(i in sp_images_gal){
  
  print(basename(i))
  file.copy(from = gd_im[basename(gd_im) == basename(i)], 
            to = i, overwrite = TRUE)
  
}


# Load in and compress the thumbnails
sp_images_thumb <- sp_images[grepl('^thumbnail', basename(sp_images))]

for(i in sp_images_thumb){
  
  print(basename(i))
  img <- readJPEG(source = i)
  writeJPEG(image = img, target = i, quality = 0.4)
  
}
