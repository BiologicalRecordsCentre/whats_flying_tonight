spim <- read.csv('data/UKMoths/images/Images_requested_with_filenames.csv', stringsAsFactors = FALSE)
spdat <- read.csv('data/UKMoths/speciesData.csv', stringsAsFactors = FALSE)
spdat$image <- NA

head(spdat)

for(i in spdat$NAME){
  
  spdat$image[spdat$NAME == i] <- paste(spim$FILENAME[spim$NAME == i], collapse = ',')
  
}

spdat$image[spdat$image == ''] <- 'NAimage.png'

write.csv(spdat, file = 'data/UKMoths/spDatImages.csv',
          row.names = FALSE)