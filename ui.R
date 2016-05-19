
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
  useShinyjs(),
  
  tags$head(
    
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1, user-scalable=no"/>
          <meta name="mobile-web-app-capable" content="yes">
          <meta name="apple-mobile-web-app-capable" content="yes">
          <meta name="apple-mobile-web-app-title" content="Whats flying tonight">
          <meta name="application-name" content="Whats flying tonight">')
    ),
    # Include our custom CSS
    includeCSS("styles.css"),
    includeCSS("lightbox.css"),
    includeScript("jquery-2.1.4.js"),
    # htmlOutput('settings_js'),
    # htmlOutput('about_js'),
    # htmlOutput('button_js'),
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
        '),
    
  
  # Loading text
  div(id = 'loading',
      h3('Loading...'),
      p("Using your location and today's date to build your custom report"),
      img(src = 'images/startup.gif', alt = 'loading', style = 'margin-top: 20px')),
  # No gelocation text
  hidden(h5(id = 'geolocation_denied',
            align = 'center',
            "You have denied access to your location. To allow access clear your cache for this page and then select 'allow' when prompted")
  ),
  htmlOutput('UI'),
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
                  selectize = TRUE,
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
                  selected = 'Top 25')
      ),
  div(id = 'about_display',
      a(href = 'http://butterfly-conservation.org/',
        target = '_blank',
        img(src = 'BClogo.gif', style = 'width: 93%; max-width: 300px; display: block; margin-top: 8px; margin-left: auto; margin-right: auto;')),
      p(HTML(paste("What's flying tonight uses data gathered by the <a class = 'about', href =",
              "'http://www.mothscount.org/text/27/national_moth_recording_scheme.html'",
              " target='_blank'> National Moth Recording scheme</a> from 2005 to 2015. We summarise the NMRS data",
              "for your 10km grid-square combined with neigbouring grid-squares",
              "and present the number of records for each moth species, in this area,",
              "for a 9 day period centered on today's date.")),
              style = 'width: 98%; max-width: 300px; text-align: center; color: white; display: block; background-color: dimgray; margin-top: 10px; margin-left: auto; margin-right: auto;'),
      p(HTML("Data used with permission of <a class = 'about', href = 'http://butterfly-conservation.org/', target = '_blank'>Butterfly Conservation</a>"), 
        style = 'width: 98%; max-width: 300px; text-align: center; color: white; display: block; background-color: dimgray; margin-left: auto; margin-right: auto;'),
      p(HTML("Site built by the <a class = 'about', href = 'http://www.brc.ac.uk/', target = '_blank'>Biological Records Centre</a>"),
        style = 'width: 98%; max-width: 300px; text-align: center; color: white; display: block; background-color: dimgray; margin-left: auto; margin-right: auto;'),
      a(href = 'http://www.brc.ac.uk/',
        target = '_blank',
        img(src = 'BRClogo.png', style = 'width: 93%; max-width: 300px; display: block; margin-left: auto; margin-right: auto;'))),
  
  includeScript("lightbox.js")
  
))
