# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# load datasets
speciesData <- read.csv('data/speciesData.csv')

set.seed(1)
# Create location specific data
speciesData$nrec <- round(runif(n = nrow(speciesData), min = 1, max = 1000))
speciesData$colour[speciesData$nrec >= quantile(speciesData$nrec, probs = 0.5)] <- 'green'
speciesData$colour[speciesData$nrec < quantile(speciesData$nrec, probs = 0.5)] <- 'red'
# sort by records
speciesData <- speciesData[order(speciesData$nrec, decreasing = TRUE),]

shinyServer(function(input, output) {

  output$UI <- renderUI({

    html <- list()
    
    for(i in 1:nrow(speciesData)){
      
      big_phenology <- paste('phenology/', gsub(' ', '_', speciesData[i, 'latinName']), 'big.png', sep = '')
      small_phenology <- paste('phenology/', gsub(' ', '_', speciesData[i, 'latinName']), '.png', sep = '')
      
      temp_html <- tags$div(id = 'species',
                            align = 'center',
                       
                       ## left image
                       tags$div(id = 'image',
                                a(href = speciesData[i,'image'],
                                  'data-lightbox' = speciesData[i,'image'],
                                  'data-title' = paste(speciesData[i,'commonName'],
                                                       speciesData[i,'latinName'],
                                                       sep = ' - '),
                                  img(src = speciesData[i,'image'],
                                      tabindex = 1,
                                      align = 'middle',
                                      height = '100%',
                                      alt = speciesData[i,'commonName']))
                       ),
                       
                       ## Right text
                       tags$div(id = 'text',
                                a(href = 'http://ukmoths.org.uk/',
                                  target = '_blank',
                                  strong(speciesData[i,'commonName'])),
                                em(speciesData[i,'latinName']),
                                br(),
                                tags$span(paste(speciesData[i,'nrec'],
                                                'records'),
                                          style=paste("color",
                                                      speciesData[i, 'colour'],
                                                      sep = ':')),
                                br(),
                                ## Phenology plot
                                a(href = big_phenology,
                                  'data-lightbox' = big_phenology,
                                  'data-title' = paste(speciesData[i,'commonName'],
                                                       'phenology'),
                                  img(src = small_phenology,
                                      align = 'middle',
                                      tabindex = 1,
                                      width = '100%',
                                      alt = paste(speciesData[i,'commonName'],
                                                  'phenology')))
                       )
      )
        
      html <- append(html, list(temp_html))
        
    }
    
    tagList(html)
 
  })

})
