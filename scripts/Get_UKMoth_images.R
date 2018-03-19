# This script uses my permissions spreadsheet to retrieve
# all the images that I have permissions for

# Read in permissions
permissions <- read.csv(file = 'data/UKMoths/emails.csv', header = TRUE,
                        stringsAsFactors = FALSE)

# get emails
email_permissions <- permissions$Email[permissions$permission == 'Y']


# Load in all the image information
image_information <- read.csv(file = 'data/UKMoths/speciesData_IK_v2.csv',
                              header = TRUE,
                              stringsAsFactors = FALSE)

nrow(image_information) #2656

# Get ownly rows for which I have permission
image_permission <- image_information[image_information$EMAIL %in% email_permissions, ]

nrow(image_permission) #1725

# So I got about 64%... not too bad

# Generate table to send to Ian
write.csv(image_permission, file = 'data/UKMoths/Images_requested.csv',
          row.names = FALSE)

