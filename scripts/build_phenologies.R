### Create the data ###
rm(list = ls())

## Species data
load(file = 'www/speciesData.rdata')
write.csv(speciesData, row.names = FALSE, file = 'speciesData.csv')

## Create Phenology
source('scripts/getNames.R')
speciesData <- read.csv('data/NMRS/NMRS sp week counts.csv',
                        stringsAsFactors = FALSE)
library(ggplot2)
library(RColorBrewer)
library(grid)

#n_dist is number of distinct site date records

for(i in unique(sort(speciesData$CONCEPT))[820:length(unique(sort(speciesData$CONCEPT)))]){
  
  cat(i, '... ')
  
  sp_name <- getNames(i)
  
  if(is.null(sp_name)){
    
    latinName <- i
    commonName <- NA
    valid <- 'M'
    
  } else {
  
    if(sp_name$RANK != 'Species'){
      
      latinName <- as.character(sp_name$NAME)
      commonName <- NA
      valid <- as.character(sp_name$VALID)
      
    } else {
      
      latinName <- as.character(sp_name$BINOMIAL)
      commonName <- as.character(sp_name$NAME_ENGLISH)
      valid <- as.character(sp_name$VALID)
      
    }
  }
  
  cat(valid, latinName, commonName)
  
  tempDat <- speciesData[speciesData$CONCEPT == i, ]
  
  if(nrow(tempDat) != 53){
    tempDat <- rbind(tempDat[,c('CONCEPT', 'WEEKNO', 'N_DIST')],
                     data.frame(CONCEPT = i,
                                WEEKNO = c(1:53)[!c(1:53) %in% tempDat$WEEKNO],
                                N_DIST = 0))
  }
  
  # Create a custom colour pallette for the calendar plots
  myPalette <- colorRampPalette(brewer.pal(9, 'YlOrBr'))

  png(filename = paste('www/phenology/',
                       valid,
                       gsub(' ', '_', gsub('/', '_', latinName)),
                       '.png', sep = ''),
      width = 235, height = 60, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = WEEKNO, y = CONCEPT, fill = N_DIST)) +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(50)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq(52/12, 52, 52/12) - (52/12)/2,
                       labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
    scale_y_discrete(expand = c(0, 0)) +
    # geom_vline(xintercept = c(thisWeek - 0.5, thisWeek + 0.5)) +
    ylab('') +
    xlab('') +
    # geom_hline(yintercept = c(1.5, 2.5), colour = 'white') +
    theme_bw() +
    theme(text = element_text(size = 12),
          legend.position = "none",
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = unit(c(0.1,0.1,-0.3,-0.8), "cm"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) 
  plot(p)
  
  dev.off()
  
  png(filename = paste('www/phenology/', 
                       valid,
                       gsub(' ', '_', gsub('/', '_', latinName)),
                       '_big.png', sep = ''),
      width = 600, height = 120, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = WEEKNO, y = CONCEPT, fill = N_DIST)) +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(50)) +
    scale_x_continuous(expand = c(0, 0),
                     breaks = seq(52/12, 52, 52/12) - (52/12)/2,
                     labels = c('Jan','Feb','Mar','Apr','May','Jun',
                                'Jul','Aug','Sep','Oct','Nov','Dec')) +
    scale_y_discrete(expand = c(0, 0)) +
    # geom_vline(xintercept = c(thisWeek - 0.5, thisWeek + 0.5)) +
    # geom_hline(yintercept = c(1.5, 2.5), colour = 'white') +
    theme_bw() +
    ylab('') +
    xlab('') +
    theme(text = element_text(size = 12),
          legend.position = "none",
          axis.text.y = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) 
  plot(p)
  
  dev.off()
  
  cat(' ... done', '\n')
  
}

