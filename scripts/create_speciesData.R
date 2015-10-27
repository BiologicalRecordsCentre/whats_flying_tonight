## generate underlying data about species ##
rm(list = ls())

# Get species images: URLs #
image_paths <- list.files('www/images/', full.names = TRUE) 
image_paths <- gsub('www/', '', image_paths)


# Get species latin names #
latinName <- replicate(n = length(image_paths),
                       paste(sample(letters, round(runif(1,8,15))),
                             collapse = ''))

# Get Species common names #
commonName <- replicate(n = length(image_paths),
                        paste(sample(letters, round(runif(1,8,15))),
                              collapse = ''))

# Save data in a table #
speciesData <- data.frame(image = image_paths,
                          latinName,
                          commonName,
                          stringsAsFactors = FALSE)
save(speciesData, file = 'www/speciesData.rdata')
