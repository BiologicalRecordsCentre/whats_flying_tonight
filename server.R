# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinyjs)

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
  # and the loading text is hidden
  observe({
    if(!is.null(input$geolocation) & is.null(input$lat)){
      if(!input$geolocation){
        hide('loading', anim = FALSE)
        show('geolocation_denied', anim = TRUE, animType = 'fade')
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

  # Build species divs
  divList <- reactive({
    
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
                                  div(style = paste("background: url('",
                                                    gsub('^www/', '', file.path(thumb_dir, thumb_small)),
                                                    "') no-repeat center center; width: 100%; height: 124px;", sep = ''))
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
              thumb_small <- 'images/no_image_thumb.gif'
              thumb_big <- 'images/no_image.gif'
              
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
            thumb_small <- 'images/no_image_thumb.gif'
            thumb_big <- 'images/no_image.gif'
            
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
                                    p(a(href = speciesData[i, 'URL'],
                                      target = '_blank',
                                      strong(speciesData[i,'NAME_ENGLISH'])),
                                      em(paste0('(', speciesData[i,'NAME'], ')')),
                                      style = 'margin: 0px 0 0px;'),
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

      tagList(html)
    }
  })

  # output species divs
  output$UI <- renderUI({
    
    divList()
    
  })
  
  # Loading div
  observe({
    if(!is.null(divList())){
      hide('loading', anim = FALSE)
    }
  })
  
  # Settings box
  observe({
    
    # Deal with about button clicks
    # on
    if(input$about_button %% 2 != 0 & input$setting_button %% 2 == 0){
      
      # div(includeScript(path = 'settings_off.js'),
      #     includeScript(path = 'about_on.js'))
      hide(id = 'settings_display', anim = TRUE,
           animType = 'fade', time = 0.2)
      show(id = 'about_display',  anim = TRUE,
           animType = 'fade', time = 0.2)
      hide(id = 'setting_button', anim = TRUE,
           animType = 'fade', time = 0.2)
      
      
    } else if(input$about_button %% 2 == 0 & input$setting_button %% 2 != 0){
          
      # div(includeScript(path = 'about_off.js'),
      #     includeScript(path = 'settings_on.js'))          
      show(id = 'settings_display', anim = TRUE,
           animType = 'fade', time = 0.2)
      hide(id = 'about_button', anim = TRUE,
           animType = 'fade', time = 0.2)
      hide(id = 'about_display', anim = TRUE,
           animType = 'fade', time = 0.2)
      
    } else if(input$about_button %% 2 == 0 & input$setting_button %% 2 == 0){
      
      # div(includeScript(path = 'about_off.js'),
      #     includeScript(path = 'settings_off.js'))
      hide(id = 'about_display', anim = TRUE,
           animType = 'fade', time = 0.2)
      hide(id = 'settings_display', anim = TRUE,
           animType = 'fade', time = 0.2)
      show(id = 'about_button', anim = TRUE,
           animType = 'fade', time = 0.2)
      show(id = 'setting_button', anim = TRUE,
           animType = 'fade', time = 0.2)
      
      
    }
  })
})
