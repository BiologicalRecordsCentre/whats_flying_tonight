# Create speciesData
rm(list = ls())

recData <- read.csv(file = 'data/NMRS/NMRS No rec spp_10km by dayno.csv', stringsAsFactors = FALSE)

str(recData)

concepts <- sort(unique(recData$CONCEPT))

rm(list = 'recData')

source('scripts/getNames.R')

fullnames <- lapply(concepts, getNames)

speciesData <- do.call(rbind, fullnames)

# Add the Phenology plots
for(i in 1:nrow(speciesData)){
  
  name <- speciesData[i, 'NAME']
  valid <- speciesData[i, 'VALID']
  smallPath <- paste0('www/phenology/', valid, gsub(' ','_',name), '.png')
  bigPath <- paste0('www/phenology/', valid, gsub(' ','_',name), '_big.png')
  
  if(file.exists(smallPath)){
    speciesData$phenosmall[i] <- gsub('^www/', '', smallPath)
  } else {
    speciesData$phenosmall[i] <- NA
  }
  
  if(file.exists(bigPath)){
    speciesData$phenobig[i] <- gsub('^www/', '', bigPath)
  } else {
    speciesData$phenobig[i] <- NA
  }
}

# Add image links
images <- c('images/10429767534_3a000555cc_q.jpg',
          'images/19617047581_b812b682f1_q.jpg',
          'images/20316434971_e059769d80_q.jpg',
          'images/4979188787_e8ce74d81e_q.jpg',
          'images/6296358535_100788c5ce_q.jpg',
          'images/8274925926_2dce14d248_q.jpg')

speciesData$image <- sample(images, nrow(speciesData), replace = TRUE)

write.csv(speciesData, file = 'data/speciesData.csv', row.names = FALSE)
