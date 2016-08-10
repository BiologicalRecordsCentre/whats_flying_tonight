# There was a mistake in the production of the list of images available.
# The version 3 file contains additional images. This script finds the
# additional images and then works out which ones we have author
# permissions for, finally outputing a new images request file for Ian
v2 <- read.csv(file = 'data/UKMoths/speciesData_IK_v2.csv')
v3 <- read.csv(file = 'data/UKMoths/speciesData_IK_v3.csv')

# get the new ones
new_images <- v3[!v3$UKMOTHSURL %in% v2$UKMOTHSURL, ]

# something is off
nrow(v2) == length(unique(v2$UKMOTHSURL))
nrow(v3) == length(unique(v3$UKMOTHSURL))

# There are some duplicaters in v3
dups <- names(table(v3$UKMOTHSURL)[table(v3$UKMOTHSURL)>1])

# Looks like this might be on purpose as some consider
# the species to be the same
v3[v3$UKMOTHSURL %in% dups, ]

# numbers still dont quite match...
# there could be new speices that use the same images as 
# old ones
new_images2 <- v3[(!v3$UKMOTHSURL %in% v2$UKMOTHSURL) | (!v3$BINOMIAL %in% v2$BINOMIAL),]

# This works

# Now, which of these do I have permissions for?
# Read in permissions
permissions <- read.csv(file = 'data/UKMoths/emails.csv', header = TRUE,
                        stringsAsFactors = FALSE)

# get emails
email_permissions <- permissions$Email[permissions$permission == 'Y']

nrow(new_images2) #295

# Get only rows for which I have permission
image_permission <- new_images2[new_images2$EMAIL %in% email_permissions, ]

nrow(image_permission) #195

# So I got about 66%

# Generate table to send to Ian
write.csv(image_permission, file = 'data/UKMoths/Images_requested_additional.csv',
          row.names = FALSE)