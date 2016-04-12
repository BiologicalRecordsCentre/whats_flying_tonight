## A fuction that when given a hectad and Julian day
# returns the species summary for a area of x rings around
# the named hectad (x=1 is 9 hectads)
# hectad - the users location
# jday - the current day of the year
# radius - the number of hectad rings to search in (1 = 9 hectads, 2 = 25 hectads, ...)
# dayBuffer - the number of days on either side of hte current day to search in

gatherData <- function(hectad, jDay, radius = 1, dayBuffer = 4){
  
  require(BRCmap)
  
  # Get a the names of all the hectad we want data for
  hectads <- c(hectad, neigh_smooth(hectad, smooth_radius = radius))
  
  master <- NULL
  
  # load in the datasets (fast version)
  data1 <- lapply(hectads, function(x){
    
    if(file.exists(paste0('data/hectad_counts/', x, '.rdata'))){
      
      load(file.path('data/hectad_counts', paste(x, '.rdata', sep = '')))
     
      return(temp_dat)
    }
  })
  
  combo <- do.call(rbind, data1)

  ## Subset to day range
  dRange <- (jDay - dayBuffer):(jDay + dayBuffer)
  
  # deal with days beyond the end of the year (366 becomes 1 and so on)
  dRange[dRange > 365] <- dRange[dRange > 365] - 365 
  
  # Subset
  combo2 <- combo[combo$DAYNO %in% dRange, ]

  # Now summarise across species
  rec_tab <- sort(tapply(combo2$N_RECS, combo2$CONCEPT, sum), decreasing = TRUE)
  species_recs <- data.frame(species = names(rec_tab),
                             nrec = as.numeric(rec_tab))
  
  # Return this table
  return(species_recs)
  
}