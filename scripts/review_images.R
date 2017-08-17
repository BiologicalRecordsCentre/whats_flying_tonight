# This script reviews all the cached images and
# highlights species with missing images that appear 
# frequently in the app
rm(list = ls())

# Create a table of the number of images for each species
species_folders <- list.files(path = 'www/images/species/', full.names = TRUE)

count_images <- function(path){
  
  # gallery images
  n_gal <- length(list.files(path, pattern = '.jpg'))
  
  # thumbnail
  thumb <- ifelse(test = dir.exists(file.path(path, 'thumbnail')),
                  yes = 1,
                  no = 0)
  
  # return row
  row <- data.frame(species = basename(path),
                    n_images = n_gal + thumb)
  
}

# Apply this function across species
count_out <- lapply(species_folders, FUN = count_images)

sp_im <- do.call(rbind, count_out)

sp_im

# Now for each species work out how often they appear
data_files <- list.files(path = 'data/hectad_counts',
                         full.names = TRUE,
                         pattern = '.rdata$')

sp_names <- NULL

for(i in data_files){
  
  load(i)
  
  sp_names <- c(sp_names, unique(temp_dat$CONCEPT))
  
  rm(list = 'temp_dat')
  
}

# Match concepts to names
load('data/UKMoths/speciesData_newNames2017.rdata')

sp_freq <- as.data.frame(table(sp_names))

sp_freq$name <- gsub('.', '',
                     gsub('/', '',
                          gsub(' ', '_',
                               speciesDataRaw$NAME[match(sp_freq$sp_names, speciesDataRaw$new_concept)])),
                     fixed = TRUE)

head(sp_freq, 100)

sp_review <- merge(x = sp_im,
                   y = sp_freq,
                   by.x = 'species',
                   by.y = 'name',
                   all.x = FALSE,
                   all.y = TRUE,
                   incomparables = NA)

hist(sp_review$n_images)

# add speceies english names
sp_review$english_name <- speciesDataRaw$new_englishname[match(sp_review$sp_names, speciesDataRaw$new_concept)]

# add taxanomic rank
sp_review$rank <- speciesDataRaw$RANK[match(sp_review$sp_names, speciesDataRaw$new_concept)]

## add new binomial
sp_review$binomial <- speciesDataRaw$new_binomial[match(sp_review$sp_names, speciesDataRaw$new_concept)]

colnames(sp_review) <- c("name", "number_of_images", "concept", "number_of_hectads",
                         "english_name", "rank", "binomial")
# reorder
sp_review <- sp_review[,c("binomial", "english_name", "concept", "number_of_images", 
                          "number_of_hectads", "rank", "name")]

## 1. Species that have no image folder
# Should these be in the data?
no_folder <- sp_review[is.na(sp_review$number_of_images), ]
no_folder <- no_folder[order(-no_folder$number_of_hectads), ]

nrow(no_folder)
head(no_folder)
no_folder$species
write.csv(no_folder,
          file = 'data/no_folder_species.csv',
          row.names = FALSE)

## 2. Species that have 0 images
# Where can we get images from
no_images <- sp_review[sp_review$number_of_images == 0, ]
no_images <- no_images[order(-no_images$number_of_hectads), ]

nrow(no_images)
head(no_images)
hist(no_images$number_of_hectads)
write.csv(no_images,
          file = 'data/no_images_species.csv',
          row.names = FALSE)

## 3. Species that have only thumbnail
only_thumb <- sp_review[sp_review$number_of_images == 1, ]
only_thumb <- only_thumb[order(-only_thumb$number_of_hectads), ]

nrow(only_thumb)
head(only_thumb)
hist(only_thumb$number_of_hectads)
write.csv(only_thumb,
          file = 'data/only_thumb_species.csv',
          row.names = FALSE)

## 4. The whole lot
write.csv(sp_review,
          file = 'data/all_images_species.csv',
          row.names = FALSE)
