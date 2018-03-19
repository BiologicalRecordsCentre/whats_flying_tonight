setwd("W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Shiny BRC/whats_flying_tonight/www/images/species")

for(i in list.dirs(recursive = FALSE)){
 
  print(i)
  file.rename(i, gsub(' ', '_', i))
  
}

