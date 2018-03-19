# Get all unique email address and write out as a .csv and a .txt file

addresses <- read.csv('data/UKMoths/speciesData_IK_v2.csv')

emails <- sort(unique(addresses$EMAIL))

write.csv(x = emails, file = 'data/UKMoths/emails.csv',
          row.names = FALSE)

write(paste(emails, collapse = ';'), file = 'data/UKMoths/emails.txt')
