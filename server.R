# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# library(googleVis)
library(ggplot2)
library(RColorBrewer)
library(grid)

locationData <- data.frame(nrec = round(runif(n = 6, 10, 1000)))
locationData$colour <- ifelse(locationData$nrec > 500, 'green', 'red')
locationData <- locationData[rev(order(locationData$nrec)), ] 
load(file = 'www/speciesData.rdata')
speciesData <- cbind(speciesData, locationData)

yearData <- data.frame(month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
# used for google hist
# colours <- paste("['#eae9d4', '#eae9d4', '#eae9d4',",
#                  "'#eae9d4', '#eae9d4', '#eae9d4',",
#                  "'#000000', '#eae9d4', '#eae9d4',",
#                  "'#eae9d4', '#eae9d4', '#eae9d4']")
# for(i in speciesData$commonName){
#   
#   x <- table(cut(rnorm(n = 1000, mean = rnorm(1), sd = 0.2), breaks = 12))
#   yearData[, i] <- as.numeric(x)
#   
# }

calData <- list() 

for(i in speciesData$commonName){
  
  num <- rnorm(n = 8000, mean = rnorm(1), sd = 0.2)
  h <- as.numeric(table(cut(num, breaks = 52, include.lowest = TRUE)))
  ly <- as.numeric(table(cut(sample(num, size = 6000, replace = TRUE), breaks = 52, include.lowest = TRUE)))
  
  historic <- data.frame(value = h/max(h),
                         id = 'Historic',
                         stringsAsFactors = FALSE)
  lastYear <- data.frame(value = ly/max(ly),
                         id = 'Last year',
                         stringsAsFactors = FALSE)
  calData[[i]] <- rbind(lastYear, historic)
  calData[[i]]$Week <- rep(1:52, 2)
  
}

# What week are we in? 
thisWeek <- 30
         
# Create a custom colour pallette for the calendar plots
myPalette <- colorRampPalette(brewer.pal(9, 'YlOrBr'))


shinyServer(function(input, output) {

  output$UI <- renderUI({

    html <- list()
    
    for(i in 1:nrow(speciesData)){
      
      outfile <- tempfile(fileext='.png', tmpdir = 'www')
      
      tempDat <- calData[[speciesData$commonName[i]]]
      
      png(filename = outfile, width = 200, height = 100, bg = "transparent")

        p <- ggplot(tempDat, aes(x = Week, y = id, fill = value)) +
         geom_tile() +
         scale_fill_gradientn(colours = myPalette(52)) +
         scale_x_discrete(expand = c(0, -2), breaks = c(seq(10,50,10))) +
         scale_y_discrete(expand = c(0, 0)) +
         geom_vline(xintercept = c(thisWeek - 0.5, thisWeek + 0.5)) +
         ylab('') +
         theme_bw() +
         theme(text = element_text(size = 16),
               legend.position = "none",
               plot.background = element_rect(fill = "transparent", colour = NA),
               plot.margin = unit(c(0.1,0.1,0.1,-0.9), "cm"),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank()) 
        plot(p)
      dev.off()

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
                                                      sep = ':')),
                                img(src = basename(outfile),
                                    align = 'middle',
                                    tabindex = 1,
                                    width = '48%',
                                    alt = paste(speciesData[i,'commonName'],
                                                'phenology'))
                       )
                       
      )
        
      html <- append(html, list(temp_html))
        
    }
    
    tagList(html)
 
  })

})
