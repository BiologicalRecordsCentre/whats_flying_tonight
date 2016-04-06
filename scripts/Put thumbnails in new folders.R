# Place the thumbnail image in a thumbnail folder

load('data/UKMoths/speciesData.rdata')

speciesDataRaw$NAME <- gsub('\\.$', '', speciesDataRaw$NAME)

head(speciesDataRaw)

folders <- dir('www/images/species/', full.names = TRUE)

root <- getwd()

for(spPath in file.path(root, folders)){
	
  print(basename(spPath))
	
  if(length(list.files(spPath)) > 0){
    
    if(!dir.exists(file.path(spPath, 'thumbnail'))){
      
      thumbnail_image <- speciesDataRaw$image[speciesDataRaw$NAME == basename(spPath)]
      thumbnail_image <- strsplit(thumbnail_image, split = ',')[[1]][1]
      
      dir.create(file.path(spPath, 'thumbnail'))
      
      for(file in file.path(spPath, c(thumbnail_image,
                                      paste('thumbnail', thumbnail_image, sep = '_')))){
        suc <- file.copy(from = file,
                  to = file.path(file.path(dirname(file)), file.path('thumbnail', basename(file))))
        if(suc) file.remove(file)
      }
      
      # remove other thumbnails
      images <- list.files(spPath, pattern = '.jpg$', full.names = TRUE)
      thumbs <- images[grepl(pattern = '^thumbnail_', basename(images))]
      
      for(thumb in thumbs){
        
        file.remove(thumb)
        
      }
    }
  }
}
