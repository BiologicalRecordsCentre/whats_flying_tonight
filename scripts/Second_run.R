## Gather new data, create phenologies and hectad count data ##
rm(list = ls())

# This file should be re-runable. You shouldn't have to change anything in this
# file, just update the sql queries in the NMRS folder so that they include the 
# year range of interest. Note the info in the app will also need to be 
# updated

##################
# Get Count Data #
##################

# Setup DB connection
library(RODBC)
if(exists("channel") == FALSE) channel = odbcConnect("BRC", uid = "tomaug", pwd = "DoHitDedKu")

# Run an sql query to gather the hectad data files
get_all_query <- paste(readLines('data/NMRS/NMRS summary sp agg 2017.sql'), collapse = ' ')

all_data <- sqlQuery(channel = channel, query = get_all_query)

# save the data
write.table(all_data,
            file = 'data/NMRS/NMRS_summary_data.csv',
            row.names = FALSE,
            sep = ',')

# Write a species summary for MB
species_table <- unique(all_data[, c('CONCEPT', 'NAME', 'RANK')])
# Add to this the species other data
load('data/UKMoths/speciesData_newNames.rdata')
speciesDataRaw <- speciesDataRaw[, c('CONCEPT',
                                     'new_englishname',
                                     'NAME_ENGLISH',
                                     'BINOMIAL')]

names(speciesDataRaw) <- c('CONCEPT', 'english_name', 'old_englishname', 'old_binomial')
species_table_plus <- merge(x = species_table,
                            y = speciesDataRaw, 
                            by.x = 'CONCEPT',
                            by.y = 'CONCEPT',
                            all = T)
write.csv(x = species_table,
          file = 'data/NMRS/species_extracted.csv',
          row.names = FALSE)
write.csv(x = species_table_plus,
          file = 'data/NMRS/species_extracted_translation.csv',
          row.names = FALSE)

rm(list = c('all_data'))

#####################
# Cut up count data #
#####################

recData <- read.table('data/NMRS/NMRS_summary_data.csv',
                      stringsAsFactors = FALSE,
                      header = TRUE,
                      sep = ',')

n <- length(sort(unique(recData$SQ_10)))

# parallelise
library(snowfall)
sfInit(cpus = 3, parallel = TRUE)
sfExport('n', 'recData')

# This dataset needs to be cut up into small chunks for loading on the go
sfClusterApplyLB(sort(unique(recData$SQ_10)), fun = function(hec){
  
  cat(grep(paste('^',hec, sep = ''), sort(unique(recData$SQ_10))), 'of', n, hec, '\n')
  temp_dat <- recData[recData$SQ_10 == hec, c('CONCEPT','DAYNO','N_RECS')]
  
  save(temp_dat, 
       file = paste('data/hectad_counts/',
                    hec,
                    '.rdata', sep = ''))
  
  rm(list = c('temp_dat'))
  
})

sfStop()

##################################################
# Create phenology plots and add missing species #
##################################################

# Get the data
rm(list = ls())
library(RODBC)
if(exists("channel") == FALSE) channel = odbcConnect("BRC", uid = "tomaug", pwd = "DoHitDedKu")
get_weeks_query <- paste(readLines('data/NMRS/NMRS sp week counts 2017.sql'), collapse = ' ')
all_weeks_data <- sqlQuery(channel = channel, query = get_weeks_query)

# save the data
write.table(all_weeks_data,
            file = 'data/NMRS/NMRS_weeks_counts.csv',
            row.names = FALSE,
            sep = ',')
rm(list = c('all_weeks_data'))

speciesData <- read.csv('data/NMRS/NMRS_weeks_counts.csv',
                        stringsAsFactors = FALSE)
library(ggplot2)
library(RColorBrewer)
library(grid)

load(file = 'data/UKMoths/speciesData_newNames2017.rdata')

# reset phenology columns
speciesDataRaw$phenosmall <- NA
speciesDataRaw$phenobig <- NA

for(n in seq_along(unique(speciesData$CONCEPT))){
  
  i <- unique(speciesData$CONCEPT)[n]
  
  cat(i, '... ')
  
  if(!i %in% speciesDataRaw$new_concept){
    
    new_name_data <- sqlQuery(channel = channel,
                              query = paste0(
                                "select * from brc.taxa_taxon_register where concept = '",
                                i,
                                "' AND valid = 'V'"))
    temp <- speciesDataRaw[1,]
    temp[,] <- NA
    temp[,c('NAME', 'BINOMIAL', 'new_binomial')] <- as.character(new_name_data$BINOMIAL)
    temp[,c('NAME_ENGLISH', 'new_englishname')] <- as.character(new_name_data$NAME_ENGLISH)
    temp[,c('CONCEPT','new_concept')] <- as.character(i)
    temp$URL <- 'http://ukmoths.org.uk'
    temp$changed <- 'new'
    temp$VALID <- 'V'
    cat(' * NEW * ')
    
    speciesDataRaw <- rbind(speciesDataRaw, temp)
    
  }

  sp_name <- speciesDataRaw[speciesDataRaw$new_concept == i & !is.na(speciesDataRaw$new_concept), ]
  
  latinName <- as.character(sp_name$new_binomial)
  commonName <- as.character(sp_name$new_englishname)
  valid <- as.character(sp_name$VALID)
    
  cat(valid, latinName, commonName)
  
  #n_recs is number of distinct site date records
  tempDat <- speciesData[speciesData$CONCEPT == i, ]
  
  if(nrow(tempDat) != 53){
    tempDat <- rbind(tempDat[,c('CONCEPT', 'WEEKNO', 'N_RECS')],
                     data.frame(CONCEPT = i,
                                WEEKNO = c(1:53)[!c(1:53) %in% tempDat$WEEKNO],
                                N_RECS = 0))
  }
  
  # Create a custom colour pallette for the calendar plots
  myPalette <- colorRampPalette(brewer.pal(9, 'YlOrBr'))
  
  small_filename <- paste('phenology/',
                          valid,
                          gsub(' ', '_', gsub('/', '_', latinName)),
                          '.png', sep = '')

  speciesDataRaw[speciesDataRaw$new_concept == i & !is.na(speciesDataRaw$new_concept),
                 'phenosmall'] <- small_filename
  
  png(filename = small_filename,
      width = 235, height = 60, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = WEEKNO, y = CONCEPT, fill = N_RECS)) +
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
  
  big_filename <- paste('phenology/', 
                        valid,
                        gsub(' ', '_', gsub('/', '_', latinName)),
                        '_big.png', sep = '')
  
  speciesDataRaw[speciesDataRaw$new_concept == i & !is.na(speciesDataRaw$new_concept),
                 'phenobig'] <- big_filename
  
  png(filename = big_filename,
      width = 600, height = 120, bg = "transparent")
  
  p <- ggplot(tempDat, aes(x = WEEKNO, y = CONCEPT, fill = N_RECS)) +
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

# Save out the new species names table
# unique(speciesData$CONCEPT[!speciesData$CONCEPT %in% speciesDataRaw$new_concept])
# x <- speciesDataRaw[!speciesDataRaw$new_concept %in% unique(speciesData$CONCEPT),]

save(speciesDataRaw, file = 'data/UKMoths/speciesData_newNames2017.rdata')