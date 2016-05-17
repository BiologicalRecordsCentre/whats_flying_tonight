# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)

# load tranformation objects
# load('data/datum_vars.rdata')
# load('data/helmert_trans_vars.rdata')

source_scripts <- list.files('scripts/internal/', full.names = TRUE)
for(i in source_scripts) source(i)

# # load datasets
# speciesDataRaw <- read.csv('data/UKMoths/spDatImages.csv', stringsAsFactors = FALSE)
# # Add species URL
# # Might be missing some species and therefore URLs
image_information <- read.csv(file = 'data/UKMoths/images/Images_requested_with_filenames.csv',
                              header = TRUE,
                              stringsAsFactors = FALSE)
# # Create this species URL object from the raw species images file
# # save and load
# sp_URLs <- tapply(image_information$UKMOTHSURL, image_information$BINOMIAL,
#                   FUN = function(x) unique(dirname(x)))
# speciesDataRaw$URL <- gsub('NA$',
#                            '',
#                            paste('http://ukmoths.org.uk',
#                                  sp_URLs[as.character(speciesDataRaw$BINOMIAL)],
#                                   sep = '')
#                           )
# save(speciesDataRaw, file = 'data/UKMoths/speciesData.rdata')

load('data/UKMoths/speciesData.rdata')

shinyServer(function(input, output) {
  
  # Get hectad using location
  hectad_loc <- reactive({
    if(!is.null(input$lat)){
      gr <- gps_latlon2gr(latitude = input$lat, longitude = input$long)
      hectad <- reformat_gr(gr$GRIDREF, prec_out = 10000)
    }
  }) 

  # Select hectad to use
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
  speciesData_raw <- reactive({
    if(!is.null(input$lat)){
      
      # Set date
      if(!input$use_date){
        
        jDay <- as.POSIXlt(Sys.time())$yday
        
      } else if(input$use_date){
        
        jDay <- as.POSIXlt(input$date_man)$yday
        
      }
      
      recData <- gatherData(hectad = hectad(),
                            jDay = jDay,
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
  
  
  # Sort the data
  speciesData <- reactive({
    
    if(input$sortBy == 'records'){
      return(speciesData_raw())
    } else if(input$sortBy == 'english'){
      return(speciesData_raw()[order(speciesData_raw()$NAME_ENGLISH), ])
    } else if(input$sortBy == 'latin'){
      return(speciesData_raw()[order(speciesData_raw()$NAME), ])
    }
    
  })
  
  # how many species to show
  n_to_show <- reactive({
    
    if(input$NtoShow == 'All'){
      return(nrow(speciesData()))
    } else if(input$NtoShow == 'Top 10'){
      return(min(nrow(speciesData()), 10))
    } else if(input$NtoShow == 'Top 25'){
      return(min(nrow(speciesData()), 25))
    } else if(input$NtoShow == 'Top 50'){
      return(min(nrow(speciesData()), 50))
    } else if(input$NtoShow == 'Top 100'){
      return(min(nrow(speciesData()), 100))
    }
    
  })

  output$UI <- renderUI({
    
    if(!is.null(input$lat)){
      
      html <- list()
      
      speciesData <- speciesData()
      
      # If data are present build the species panels
      if(!is.null(speciesData)){
        
        for(i in 1:n_to_show()){
          
          big_phenology <- speciesData[i, 'phenobig']
          small_phenology <- speciesData[i, 'phenosmall']
          
          # Create species gallery links
          galleryLinks <- list()
          
          # Species images
#           im_tab <- image_information[image_information$NAME == speciesData[i, 'NAME'],]          
#           if(nrow(im_tab) == 0){
#             im_tab <- data.frame(FILENAME = c('NAimage.png'),
#                                  CONTRIBUTOR = c(''))
#           }
#           
#           im_tab$FILENAME <- file.path('images/wft_400', im_tab$FILENAME)
          
          sp_name <- gsub(' ', '_', speciesData[i, 'NAME'])
          images_dir <- 'www/images/species'
          species_dir <- file.path(images_dir, sp_name)
          thumb_dir <- file.path(images_dir, sp_name, 'thumbnail')
          
          if(dir.exists(species_dir)){ 
            # there is a folder for this species
            if(dir.exists(thumb_dir)){
              # thumbnail dir exists
              # add thumbnail
              thumb_images <- list.files(thumb_dir, pattern = 'jpg$')
              thumb_small <- thumb_images[grep('^thumbnail_', thumb_images)][1]
              thumb_big <- thumb_images[grep('^thumbnail_', thumb_images, invert = TRUE)][1]
              thumb_credit <- image_information$CONTRIBUTOR[image_information$FILENAME == thumb_big]
              
              gal_temp <-  tags$a(href = gsub('^www/', '', file.path(thumb_dir, thumb_big)),
                                  'data-lightbox' = speciesData[i,'NAME'],
                                  'data-title' = paste(speciesData[i,'NAME_ENGLISH'],
                                                       speciesData[i,'NAME'],
                                                       paste('Credit: ', thumb_credit[1]),
                                                       sep = ' - '),
                                  style = 'width: 100%',
                                  # img(src = gsub('^www/', '', file.path(thumb_dir, thumb_small)),
                                  #     tabindex = 1,
                                  #     align = 'middle',
                                  #     height = '100%',
                                  #     alt = speciesData[i,'NAME_ENGLISH'])
                                  div(style = paste("background: url('",
                                                    gsub('^www/', '', file.path(thumb_dir, thumb_small)),
                                                    "') no-repeat center center; width: 100%; height: 118px;", sep = ''))
                                  )
              
              galleryLinks <- append(galleryLinks, list(gal_temp))
              
              # Then add the rest of the gallery
              if(length(list.files(species_dir, pattern = 'jpg$')) > 0 ){
                # If there are gallery images
                for(j in list.files(species_dir, pattern = 'jpg$')){
                  
                  im_credit <- image_information$CONTRIBUTOR[image_information$FILENAME == j]
                  
                  gal_temp <- tags$a(href = gsub('^www/', '', file.path(species_dir, j)),
                                     'data-lightbox' = speciesData[i, 'NAME'],
                                     'data-title' = paste(speciesData[i, 'NAME_ENGLISH'],
                                                          speciesData[i, 'NAME'],
                                                          paste('Credit: ', im_credit[1]),
                                                          sep = ' - '))
                  galleryLinks <- append(galleryLinks, list(gal_temp))
                  
                }
              }
            } else {
              # If no thumbnail exists
              # use 'no image'
              thumb_small <- 'images/thumbnail_NAimage.png'
              thumb_big <- 'images/NAimage.png'
              
              gal_temp <-  tags$a(href = thumb_big,
                                  'data-lightbox' = speciesData[i, 'NAME'],
                                  'data-title' = paste(speciesData[i,'NAME_ENGLISH'],
                                                       speciesData[i,'NAME'],
                                                       'No image available', sep = ' - '),
                                  img(src = thumb_small,
                                      tabindex = 1,
                                      align = 'middle',
                                      height = '100%',
                                      alt = 'No species'))
              galleryLinks <- append(galleryLinks, list(gal_temp))
              
            }
            
          } else {
            # there is no image folder for this species
            # use 'no image'
            thumb_small <- 'images/thumbnail_NAimage.png'
            thumb_big <- 'images/NAimage.png'
            
            gal_temp <-  tags$a(href = thumb_big,
                                'data-lightbox' = speciesData[i, 'NAME'],
                                'data-title' = paste(speciesData[i,'NAME_ENGLISH'],
                                                     speciesData[i,'NAME'],
                                                     ' - No image available'),
                                img(src = thumb_small,
                                    tabindex = 1,
                                    align = 'middle',
                                    height = '100%',
                                    alt = 'No species'))
            galleryLinks <- append(galleryLinks, list(gal_temp))
          }

          gallery <- tagList(galleryLinks)
          
          # Create the species div
          temp_html <- tags$div(id = 'species',
                                align = 'center',
                           
                               ## left image
                               tags$div(id = 'image',
                                        HTML(as.character(htmltools::renderTags(gallery)$html))
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
          
          # If this the last species say what show length we are working with
          if(i == n_to_show()){
            
            # Create the species div
            show_n_html <- tags$div(id = 'show_n',
                                  align = 'center',
                                  
                                  ## left image
                                  tags$div(id = 'show_length',
                                           paste('Showing', input$NtoShow, '- this can be changed in settings')
                                  ))
            
            html <- append(html, list(show_n_html))
            
          }
            
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
  
  # Settings box
#   output$settings_js <- renderUI({
#     
#     if(input$setting_button %% 2 != 0){
#       
#       if(input$about_button %% 2 != 0){
#         
#         div(includeScript(path = 'about_off.js'),
#             includeScript(path = 'settings_on.js'))
#         
#       } else {
#         
#         includeScript(path = 'settings_on.js')
#         
#       }
#       
#     } else {
#       
#       if(input$about_button %% 2 != 0){
#       
#         div(includeScript(path = 'settings_off.js'),
#             includeScript(path = 'about_on.js'))
#         
#       } else {
#         
#         includeScript(path = 'settings_off.js')
#         
#       }
#     }
#   })
  
  # Settings box
  output$button_js <- renderUI({
    
    # Deal with about button clicks
    # on
    if(input$about_button %% 2 != 0 & input$setting_button %% 2 == 0){
      
      div(includeScript(path = 'settings_off.js'),
          includeScript(path = 'about_on.js'))
          
    } else if(input$about_button %% 2 == 0 & input$setting_button %% 2 != 0){
          
      div(includeScript(path = 'about_off.js'),
          includeScript(path = 'settings_on.js'))          
      
    } else if(input$about_button %% 2 == 0 & input$setting_button %% 2 == 0){
      
      div(includeScript(path = 'about_off.js'),
          includeScript(path = 'settings_off.js'))
      
    }
  })
})
