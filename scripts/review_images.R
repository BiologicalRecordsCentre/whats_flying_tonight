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
data_files <- list.files(path = 'data/hectad_counts', full.names = TRUE)

sp_names <- NULL

for(i in data_files){
  
  load(i)
  
  sp_names <- c(sp_names, unique(temp_dat$CONCEPT))
  
  rm(list = 'temp_dat')
  
}

# Match concepts to names
load('data/UKMoths/speciesData.rdata')

sp_freq <- as.data.frame(table(sp_names))

sp_freq$name <- gsub(' ', '_', speciesDataRaw$NAME[match(sp_freq$sp_names, speciesDataRaw$CONCEPT)])

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
sp_review$english_name <- speciesDataRaw$NAME_ENGLISH[match(sp_review$sp_names, speciesDataRaw$CONCEPT)]

# add taxanomic rank
sp_review$rank <- speciesDataRaw$RANK[match(sp_review$sp_names, speciesDataRaw$CONCEPT)]

## 1. Species that have no image folder
# Should these be in the data?
no_folder <- sp_review[is.na(sp_review$n_images), ]
no_folder <- no_folder[order(-no_folder$Freq), ]

nrow(no_folder)
head(no_folder)
no_folder$species
write.csv(no_folder,
          file = 'data/no_folder_species.csv',
          row.names = FALSE)

## 2. Species that have 0 images
# Where can we get images from
no_images <- sp_review[sp_review$n_images == 0, ]
no_images <- no_images[order(-no_images$Freq), ]

nrow(no_images)
head(no_images)
hist(no_images$Freq)
write.csv(no_images,
          file = 'data/no_images_species.csv',
          row.names = FALSE)

## 3. Species that have only thumbnail
only_thumb <- sp_review[sp_review$n_images == 1, ]
only_thumb <- only_thumb[order(-only_thumb$Freq), ]

nrow(only_thumb)
head(only_thumb)
hist(only_thumb$Freq)
write.csv(only_thumb,
          file = 'data/only_thumb_species.csv',
          row.names = FALSE)

## 4. The whole lot
write.csv(sp_review,
          file = 'data/all_images_species.csv',
          row.names = FALSE)