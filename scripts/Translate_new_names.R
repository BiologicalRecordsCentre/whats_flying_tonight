# Translate the old names to the new names and assess the difference

load('data/UKMoths/speciesData.rdata')

concepts <- speciesDataRaw$CONCEPT

library(RODBC)

if(exists("channel") == FALSE) channel = odbcConnect("BRC", uid = "tomaug", pwd = "DoHitDedKu")

for(concept in concepts){
  cat(concept, '\n')
  query <- paste("select concept, concept_rec, binomial, name_english, valid",
                 "from BRC.taxa_taxon_register",
                 "where concept =",
                 paste0("'", concept, "'"),
                 "and valid = 'V'")
  name <- sqlQuery(channel, query)
  if(nrow(name) == 0) name[1,] <- NA # add NAs if nothing matching found
  if(nrow(name) > 1) stop('return has more than one row')
  speciesDataRaw$new_concept[speciesDataRaw$CONCEPT == concept] <- as.character(name$CONCEPT_REC)
  speciesDataRaw$new_binomial[speciesDataRaw$CONCEPT == concept] <- as.character(name$BINOMIAL)
  speciesDataRaw$new_englishname[speciesDataRaw$CONCEPT == concept] <- as.character(name$NAME_ENGLISH)
  
}

head(speciesDataRaw, 20)

# about 140 rows have changed

speciesDataRaw$changed <- speciesDataRaw$BINOMIAL != speciesDataRaw$new_binomial | speciesDataRaw$NAME_ENGLISH != speciesDataRaw$new_englishname | speciesDataRaw$CONCEPT != speciesDataRaw$new_concept

save(speciesDataRaw, file = 'data/UKMoths/speciesData_newNames.rdata')
write.csv(speciesDataRaw, file = 'temp_for_MB.csv')
