# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(BRCmap)
library(sparta)
source(file = 'scripts/gatherData.R')

# load datasets
speciesDataRaw <- read.csv('data/UKMoths/speciesData.csv')

# Add species URL
image_information <- read.csv(file = 'data/UKMoths/speciesData_IK_v2.csv',
                              header = TRUE,
                              stringsAsFactors = FALSE)

sp_URLs <- tapply(image_information$UKMOTHSURL, image_information$BINOMIAL,
                  FUN = function(x) unique(dirname(x)))

speciesDataRaw$URL <- gsub('NA$',
                           '',
                           paste('http://ukmoths.org.uk',
                                 sp_URLs[as.character(speciesDataRaw$BINOMIAL)],
                                  sep = '')
                          )

set.seed(1)
# # Create location specific data
# speciesData$nrec <- round(runif(n = nrow(speciesData), min = 1, max = 1000))
# speciesData$colour[speciesData$nrec >= quantile(speciesData$nrec, probs = 0.5)] <- 'green'
# speciesData$colour[speciesData$nrec < quantile(speciesData$nrec, probs = 0.5)] <- 'red'
# # sort by records
# speciesData <- speciesData[order(speciesData$nrec, decreasing = TRUE),]

shinyServer(function(input, output) {
  
  # Get hectad using location
  hectad_loc <- reactive({
    if(!is.null(input$lat)){
      gr <- sparta::gps_latlon2gr(latitude = input$lat, longitude = input$long)
      hectad <- reformat_gr(gr$GRIDREF, prec_out = 10000)
    }
  }) 

  # Select hetad to use
  hectad <- reactive({
    if(!is.null(input$lat) & !input$use_man){
      hectad_loc()
    } else if(input$use_man){
      input$hectad_man
    }
  })
  
  # If geolocation is not give this is displayed
  output$geolocation_denied <- renderUI({
    if(!is.null(input$geolocation) & is.null(input$lat)){
      if(!input$geolocation){
        
        h5(id = 'geolocation_denied',
           align = 'center',
           "You have denied access to your location. To allow access clear your cache for this page and then select 'allow' when prompted")
        
      }
    } 
  })
  
  # Gather the data
  speciesData <- reactive({
    if(!is.null(input$lat)){
      recData <- gatherData(hectad = hectad(),
                            jDay = as.POSIXlt(Sys.time())$yday,
                            radius = 1,
                            dayBuffer = 4)
      
      if(nrow(recData) == 0){
        return(NULL)
      } else {
        speciesData <- merge(x = recData, y = speciesDataRaw,
                             by.x = 'species', by.y = 'CONCEPT',
                             all.x = TRUE, all.y = FALSE, sort = FALSE)
      }
    }
  }) 

  output$UI <- renderUI({
    
    if(!is.null(input$lat)){
      
      html <- list()
      
      speciesData <- speciesData()
      
      # If data are present build the species panels
      if(!is.null(speciesData)){
        
        for(i in 1:nrow(speciesData)){
          
          big_phenology <- speciesData[i, 'phenobig']
          small_phenology <- speciesData[i, 'phenosmall']
          
          # Create species gallery links
          galleryLinks <- list()
          
          for(im in file.path('images/gallery', list.files('www/images/gallery'))){
            gal_temp <- tags$a(href = im,
                               'data-lightbox' = speciesData[i, 'NAME'],
                               'data-title' = paste(speciesData[i, 'NAME_ENGLISH'],
                                                    speciesData[i, 'NAME'],
                                                    sep = ' - '))
            galleryLinks <- append(galleryLinks, list(gal_temp))
          }
          
          gallery <- tagList(galleryLinks)
          
          # Create the species div
          temp_html <- tags$div(id = 'species',
                                align = 'center',
                           
                           ## left image
                           tags$div(id = 'image',
                                    a(href = 'images/gallery/21744826158_c8fc9881e9_z.jpg',
                                      'data-lightbox' = speciesData[i,'NAME'],
                                      'data-title' = paste(speciesData[i,'NAME_ENGLISH'],
                                                           speciesData[i,'NAME'],
                                                           sep = ' - '),
                                      img(src = speciesData[i,'image'],
                                          tabindex = 1,
                                          align = 'middle',
                                          height = '100%',
                                          alt = speciesData[i,'NAME_ENGLISH']))
                                    ,
                                    HTML(as.character(htmltools::renderTags(gallery)$html))
    #                                 a(href = 'images/gallery/19488613634_efbaa545f3_b.jpg',
    #                                   'data-lightbox' = speciesData[i,'latinName'],
    #                                   'data-title' = paste(speciesData[i,'commonName'],
    #                                                        speciesData[i,'latinName'],
    #                                                        sep = ' - '))
                                    
                                    
                           ),
                           
                           ## Right text
                           tags$div(id = 'text',
                                    a(href = speciesData[i, 'URL'],
                                      target = '_blank',
                                      strong(speciesData[i,'NAME_ENGLISH'])),
                                    br(),
                                    em(speciesData[i,'NAME']),
                                    br(),
                                    tags$span(paste(speciesData[i,'nrec'],
                                                    'records')
  #                                             style=paste("color",
  #                                                         speciesData[i, 'colour'],
  #                                                         sep = ':')
                                              ),
                                    br(),
                                    ## Phenology plot
                                    a(href = big_phenology,
                                      'data-lightbox' = big_phenology,
                                      'data-title' = paste('Phenology:',
                                                           speciesData[i,'NAME_ENGLISH'],
                                                           speciesData[i,'NAME'],
                                                           sep = ' - '),
                                      img(src = small_phenology,
                                          align = 'middle',
                                          tabindex = 1,
                                          width = '100%',
                                          alt = paste('Phenology:',
                                                      speciesData[i,'NAME_ENGLISH'],
                                                      speciesData[i,'NAME'],
                                                      sep = ' - ')))
                           )
          )
            
          html <- append(html, list(temp_html))
            
        } # end of species loop
        
      } else { # No data available
        
        temp_html <- tags$div(id = 'nodata',
                              align = 'center',
                              tags$span('There are no records of moths in this area at this time of year')
                              )
        
        html <- list(temp_html)
        
      }
      # Create bottom box
#       bot_box <- tags$div(id = 'bottom-box',
#                           align = 'center',
#                           tags$span(paste('You are located in hectad',
#                                           hectad())))
#       
#       html <- append(html, list(bot_box))
      
      tagList(html)
    }
  })

})
