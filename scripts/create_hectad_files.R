## Build record counts dataset
rm(list = ls())

# old version
#recData <- read.csv(file = 'data/NMRS/NMRS No rec spp_10km by dayno.csv', stringsAsFactors = FALSE)

# new version
recData <- read.table('data/NMRS/NMRS summary_T August/NMRS summary data.txt',
           stringsAsFactors = FALSE,
           header = TRUE,
           sep = '\t')

str(recData)

## For the new version of data only
# Keep only species & columns of value
table(recData$RANK)
recData <- recData[recData$RANK == 'Species', 
                   c('CONCEPT', 'SQ_10', 'DAYNO', 'N_RECS')]
n <- length(sort(unique(recData$SQ_10)))
# This dataset needs to be cut up into small chunks for loading on the go
for(hec in sort(unique(recData$SQ_10))){
  
  cat(grep(paste('^',hec, sep = ''), sort(unique(recData$SQ_10))), 'of', n, hec, '\n')
  temp_dat <- recData[recData$SQ_10 == hec, c('CONCEPT','DAYNO','N_RECS')]
  
  save(temp_dat, 
       file = paste('data/hectad_counts/',
                    hec,
                    '.rdata', sep = ''))
  
  rm(list = c('temp_dat'))
       
}

########
# Divide hectad files by day
# May or maynot be needed
## THIS IS NOT NEEDED ##

# Create table of breaks
breaks <- data.frame(start = seq(1, 361, 10),
                     end = seq(10, 370, 10))

# loop through the files
for(file in list.files('data/hectad_counts/', full.names = TRUE)){
  
  load(file)
  
  # loop through the breaks
  for(i in 1:nrow(breaks)){
    
    dayData <- temp_dat[temp_dat$DAYNO %in% breaks[i,1]:breaks[i,2], ]
    
    if(nrow(dayData) != 0){
      
      print(paste(file, '-', i))
      save(dayData, file = gsub('.rdata', paste0('_', i, '.rdata'), file))
      
    }
  }
}

system.time({load('data/hectad_counts/SK41.rdata')})
