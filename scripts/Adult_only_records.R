# Here I try to find out how to identify adult only records

rm(list = ls())

library(RODBC)
if(exists("channel") == FALSE) channel = odbcConnect("BRC", uid = "tomaug", pwd = "DoHitDedKu")

# Run an sql query to gather the hectad data files
get_ab <- 'select TO_ABUNDANCE from BRC.rd_nmrs_data'

all_ab <- sqlQuery(channel = channel, query = get_ab)$TO_ABUNDANCE
all_ab <- as.character(all_ab)

# split into words and remove numbers
terms <- unlist(sapply(all_ab, FUN = strsplit, split = ' '), use.names = FALSE)

words <- terms[!grepl('[[:digit:]]+', terms)]
sort(table(words), decreasing = TRUE)
u_words <- unique(words)
as.data.frame(sort(table(words), decreasing = TRUE))

head(terms)
floc <- grep('Feeding', terms)
lapply(tail(floc, 100), function(x) terms[(x-3):(x+3)])
 
exclusion_terms <- c('Larvae', 'Pupae', 'Larval', 'Egg', 'Cocoon', 'Larvae;',
                     'Pupal', 'Pupae;', 'Mine', 'Web', 'Emergence', 'Holes',
                     'Exuvia', 'Gall', 'Spinning', 'Case', 'Tube', 'Nest',
                     'Feeding')

# Lets try an SQL query that removes these
get_ab <- paste0('select TO_ABUNDANCE from BRC.rd_nmrs_data ',
                "where not regexp_like(to_abundance, '(",
                paste0(exclusion_terms, collapse = ')|('),
                ")')")

# Run the extraction again
all_ab <- sqlQuery(channel = channel, query = get_ab)$TO_ABUNDANCE
all_ab <- as.character(all_ab)

# split into words and remove numbers
terms <- unlist(sapply(all_ab, FUN = strsplit, split = ' '), use.names = FALSE)

words <- terms[!grepl('[[:digit:]]+', terms)]
sort(table(words), decreasing = TRUE)
u_words <- unique(words)
as.data.frame(sort(table(words), decreasing = TRUE))

# works a treat
