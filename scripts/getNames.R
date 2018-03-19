getNames <- function(concept){
  
  library(RODBC)
  channel <- odbcConnect(dsn = 'BRC', uid = 'tomaug', pwd = 'DoHitDedKu')
  
  taxReg <- sqlQuery(channel = channel, 
                     query = paste("select NAME, NAME_ENGLISH, CONCEPT, BINOMIAL, RANK, VALID
                     from BRC.taxa_taxon_register
                     where valid in ('V','P')
                     and CONCEPT = '", concept, "'", sep = ''))
  
  if(nrow(taxReg) == 0){
    return(NULL)
  } else {
    # Match the concepts across the two tables
    names <- taxReg[match(x = concept, table = taxReg$CONCEPT), ]
    
    return(names)
  }
}