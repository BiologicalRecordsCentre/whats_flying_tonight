### Create the data ###

## Species data
load(file = 'www/speciesData.rdata')
write.csv(speciesData, row.names = FALSE, file = 'speciesData.csv')


## Create Phenology
calData <- NULL

speciesData <- read.csv('data/speciesData.csv')

thisWeek <- 30

for(i in speciesData$latinName){
  
  num <- rnorm(n = 8000, mean = rnorm(1), sd = 0.2)
  h <- as.numeric(table(cut(num, breaks = 52, include.lowest = TRUE)))
  ly <- as.numeric(table(cut(sample(num, size = 6000, replace = TRUE), breaks = 52, include.lowest = TRUE)))
  ty <- as.numeric(table(cut(sample(num, size = 6000, replace = TRUE), breaks = 52, include.lowest = TRUE)))
  
  historic <- data.frame(species = i,
                         value = h/max(h),
                         id = 'Historic',
                         stringsAsFactors = FALSE)
  lastYear <- data.frame(species = i,
                         value = ly/max(ly),
                         id = 'Last year',
                         stringsAsFactors = FALSE)
  thisYear <- data.frame(species = i,
                         value = ty/max(ty),
                         id = 'This year',
                         stringsAsFactors = FALSE)
  thisYear$value[(thisWeek+1):52] <- 0
  calData_temp <- rbind(thisYear, lastYear, historic)
  calData_temp$Week <- rep(1:52, 3)
  
  calData <- rbind(calData, calData_temp)
  
}

write.csv(calData, file = 'data/phenologyData.csv', row.names = FALSE)


## Create phenology plots

library(ggplot2)
library(RColorBrewer)
library(grid)

# Create a custom colour pallette for the calendar plots
myPalette <- colorRampPalette(brewer.pal(9, 'YlOrBr'))

speciesData <- read.csv('data/speciesData.csv')
calData <- read.csv('data/phenologyData.csv')

for(i in 1:nrow(speciesData)){
  
  tempDat <- calData[calData$species == speciesData$latinName[i], ]
  
  png(filename = paste('www/phenology/',
                       gsub(' ', '_', speciesData[i, 'latinName']),
                       '.png', sep = ''),
      width = 235, height = 80, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = Week, y = id, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(52)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = seq(52/12, 52, 52/12) - (52/12)/2,
                       labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
    scale_y_discrete(expand = c(0, 0)) +
    geom_vline(xintercept = c(thisWeek - 0.5, thisWeek + 0.5)) +
    ylab('') +
    xlab('') +
    geom_hline(yintercept = c(1.5, 2.5), colour = 'white') +
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
                       gsub(' ', '_', speciesData[i, 'latinName']),
                       'big.png', sep = ''),
      width = 600, height = 250, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = Week, y = id, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(52)) +
    scale_x_continuous(expand = c(0, 0),
                     breaks = seq(52/12, 52, 52/12) - (52/12)/2,
                     labels = c('Jan','Feb','Mar','Apr','May','Jun',
                                'Jul','Aug','Sep','Oct','Nov','Dec')) +
    scale_y_discrete(expand = c(0, 0)) +
    geom_vline(xintercept = c(thisWeek - 0.5, thisWeek + 0.5)) +
    geom_hline(yintercept = c(1.5, 2.5), colour = 'white') +
    theme_bw() +
    ylab('') +
    xlab('') +
    theme(text = element_text(size = 12),
          legend.position = "none",
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.ticks.x = element_blank()) 
  plot(p)
  
  dev.off()
  
}

