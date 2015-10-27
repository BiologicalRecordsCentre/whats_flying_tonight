
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)

locationData <- data.frame(nrec = round(runif(n = 6, 10, 1000)))
locationData$colour <- ifelse(locationData$nrec > 500, 'green', 'red')
locationData <- locationData[rev(order(locationData$nrec)), ] 
load(file = 'www/speciesData.rdata')
speciesData <- cbind(speciesData, locationData)

yearData <- data.frame(month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

colours <- paste("['#eae9d4', '#eae9d4', '#eae9d4',",
                 "'#eae9d4', '#eae9d4', '#eae9d4',",
                 "'#000000', '#eae9d4', '#eae9d4',",
                 "'#eae9d4', '#eae9d4', '#eae9d4']")
                 
for(i in speciesData$commonName){

  x <- table(cut(rnorm(n = 1000, mean = rnorm(1), sd = 0.2), breaks = 12))
  yearData[, i] <- as.numeric(x)
  
}

shinyServer(function(input, output) {

  output$UI <- renderUI({

    html <- list()
    
    for(i in 1:nrow(speciesData)){
      
      Col <- paste(gvisColumnChart(yearData, xvar="month",
                                   yvar = speciesData[i, 'commonName'],
                                   options = list(legend = "{position:'none'}",
                                                  hAxis = "{textPosition:'none'}",
                                                  vAxis = "{textPosition:'none'}",
                                                  colors = "['black']",
                                                  backgroundColor = "transparent",
                                                  gridlines = "{color: 'transparent'}",
                                                  chartArea = "{width:'100%',height:'100%'}",
                                                  width = "200px",
                                                  height = "50px"))$html$chart,
                    collapse = "\n")
      
      
      temp_html <- tags$div(id = 'species',
                       
                       tags$div(id = 'image',
                                a(href = speciesData[i,'image'],
                                  img(src = speciesData[i,'image'],
                                      tabindex = 1,
                                      width = '100%',
                                      alt = speciesData[i,'commonName']))
                       ),
                       
                       tags$div(id = 'text',
                                a(href = 'http://ukmoths.org.uk/',
                                  strong(speciesData[i,'commonName'])),
                                em(speciesData[i,'latinName']),
                                br(),
                                tags$span(paste(speciesData[i,'nrec'],
                                                'records'),
                                          style=paste("color",
                                                      speciesData[i, 'colour'],
                                                      sep = ':'))
                       ),
                       
                       tags$div(id = 'chart',
                                align = 'center',
                                HTML(Col)        
                       )
                       
      )
        
      html <- append(html, list(temp_html))
        
    }
    
    tagList(html)
 
  })

})
