
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# images <- list.files('../www/images/species/',
#                      recursive = TRUE,
#                      pattern = '.jpg$',
#                      full.names = TRUE)
# head(images)
# 
# images <- images[!grepl('^thumbnail_', basename(images))]
# 
# df <- data.frame(path = images,
#                  basename = basename(images),
#                  classification = NA)
# 
# write.table(sep = ',', df, file = 'classifications.csv')
# 
# df <- read.csv('classifications.csv', stringsAsFactors = FALSE)

shinyServer(function(input, output) {

  values <- reactiveValues(df = read.table('classifications.csv', sep = ',',
                                           stringsAsFactors = FALSE, header = TRUE)[,c('path','basename','classification')])
  
  species <- reactive({

    values$df$path[is.na(values$df$classification)][1]

  })   
  
  taxa <- reactive({
    
    folder <- basename(gsub(basename(species()), '', species()))    
    
    if(!folder == 'thumbnail'){
      return(folder)
    } else {
      return(basename(gsub(file.path(folder, basename(species())), '', species())))
    }
    
  })   
  
  output$image <- renderImage({
    # Return a list containing the filename and alt text
    list(src = species(),
         alt = basename(species()))
    
  }, deleteFile = FALSE)
  
  output$progress <- renderText({
    
    progress <- paste(nrow(values$df[is.na(values$df$classification),]),
                      'images of',
                      nrow(values$df),
                      'remain to be classified.\n',
                      '   Image:', basename(species()), '\n',
                      '   Species:', taxa())
  })
  
  observeEvent(input$goodClass, {
    values$df$classification[values$df$path == species()] <- 'good'
    write.table(x = values$df, sep = ',', row.names = FALSE,
                file = 'classifications.csv')
  })
  
  observeEvent(input$badClass, {
    values$df$classification[values$df$path == species()] <- 'bad'
    write.table(x = values$df, sep = ',',  row.names = FALSE,
                file = 'classifications.csv')
  })
  
  index <- reactive({

    input$badClass + input$goodClass + 1

  })
  
  
})
