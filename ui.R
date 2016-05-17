
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    
    # Include our custom CSS
    includeCSS("styles.css"),
    includeCSS("lightbox.css"),
    includeScript("jquery-2.1.4.js"),
    # htmlOutput('settings_js'),
    # htmlOutput('about_js'),
    htmlOutput('button_js'),
    tags$script('
        $(document).ready(function () {
          navigator.geolocation.getCurrentPosition(onSuccess, onError);
          
          function onError (err) {
            Shiny.onInputChange("geolocation", false);
          }
          
          function onSuccess (position) {
            setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
            }, 1100)
          }
         });
        ')
  ),
  
  htmlOutput('UI'),
  htmlOutput('geolocation_denied'),
  div(id = 'bottom-box'),
  actionButton('setting_button', 'Settings'),
  actionButton('about_button', 'About'),
  # if odd ie clicked on
  div(id = 'settings_display',
      tags$b(id = 'location_title', 'Location'),
      br(),
      checkboxInput('use_man',
                    'Use different location?',
                    FALSE),
      selectInput('hectad_man',
                  '',
                  sort(gsub('.rdata$',
                            '',
                            list.files('data/hectad_counts/'))),
                  selectize = FALSE,
                  multiple = FALSE),
      div(id = 'date_title', tags$b('Date')),
      checkboxInput('use_date',
                    'Use different date?',
                    FALSE),
      dateInput('date_man',
                '',
                weekstart = 1,
                min = paste(format(Sys.Date(), '%Y'), '-01-01', sep = ''),
                max = paste(format(Sys.Date(), '%Y'), '-12-31', sep = ''),
                startview = 'year',
                format = "dd-M"),
      radioButtons('sortBy',
                   label = 'Sort by',
                   choices = list('Number of records' = 'records',
                                  'English name' = 'english',
                                  'Latin name' = 'latin')),
      tags$b(id = 'date_title', 'Show...'),
      br(),
      selectInput('NtoShow',
                  label = NULL,
                  c('All', 'Top 10', 'Top 25', 'Top 50', 'Top 100'),
                  selectize = FALSE,
                  multiple = FALSE,
                  selected = 'Top 10')
      ),
  div(id = 'about_display',
      img(src = 'BClogo.gif', style = 'width: 100%; max-width: 300px'),
      p(paste("What's flying tonight uses data gathered by the National Moth",
            "Recording scheme [link] from 2005 to 2015. We summarise the NMRS data",
            "for your 10km grid-square combined with neigbouring grid-squares",
            "and present the number of records for each moth species, in this area,",
            "for a 9 day period centered on today's date."),
        style = 'color: white; '),
      p("Data used with permission of Butterfly Conservation", 
        style = 'color: white'),
      p("Site built by the Biological Records Centre",
        style = 'color: white'),
      img(src = 'BRClogo.png', style = 'width: 100%; max-width: 300px')),
  
  includeScript("lightbox.js")
  
))
