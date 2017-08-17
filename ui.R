
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
          <meta name="application-name" content="Whats flying tonight">
          <link rel="icon" href="favicon.ico" type="image/x-icon" >
          <link rel="apple-touch-icon" sizes="57x57" href="apple-touch-icon-57x57.png" >
          <link rel="apple-touch-icon" sizes="72x72" href="apple-touch-icon-72x72.png" >
          <link rel="apple-touch-icon" sizes="76x76" href="apple-touch-icon-76x76.png" >
          <link rel="apple-touch-icon" sizes="114x114" href="apple-touch-icon-114x114.png" >
          <link rel="apple-touch-icon" sizes="120x120" href="apple-touch-icon-120x120.png" >
          <link rel="apple-touch-icon" sizes="144x144" href="apple-touch-icon-144x144.png" >
          <link rel="apple-touch-icon" sizes="152x152" href="apple-touch-icon-152x152.png" >
          <link rel="apple-touch-icon" sizes="180x180" href="apple-touch-icon-180x180.png" >'),
    # Include our custom CSS
    includeCSS("styles.css"),
    includeCSS("lightbox.css"),
    includeCSS('addtohomescreen.css'),
    includeScript("google_analytics.js"),
    includeScript("jquery-2.1.4.js"),
    includeScript(path = 'www/location.js'),
    includeScript(path = 'www/addtohomescreen.js'),
    tags$script('addToHomescreen({
                                  icon: false
                             });')
  ),

  # Loading text
  div(id = 'loading',
      h1('BETA'),
      h3('Loading...'),
      p("Using your location and today's date to build your custom report"),
      img(src = 'images/startup.gif', alt = 'loading', style = 'margin-top: 20px')),
  # No gelocation text
  hidden(h5(id = 'geolocation_denied',
            align = 'center',
            paste("You have denied access to your location.",
                  "To allow access clear your cache for this page", 
                  "and then select 'allow' when prompted"))
  ),
  htmlOutput('UI'),
  div(id = 'bottom-box'),
  actionButton('setting_button', 'Settings'),
  actionButton('about_button', 'About'),
  hidden(actionButton('about_exit', 'X')),
  hidden(actionButton('settings_exit', 'X')),
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
                            list.files('data/hectad_counts/',
                                       pattern = '.rdata$'))),
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
        img(src = 'BClogo.gif', style = 'width: 93%; max-width: 300px; display: block; margin-top: 8px; margin-left: auto; margin-right: auto; border: 10px; border-style: solid; border-color: white;')),
      p(HTML(paste("What's flying tonight uses UK sightings of larger (macro-) moths gathered by the <a class = 'about', href =",
              "'http://www.mothscount.org/text/27/national_moth_recording_scheme.html'",
              " target='_blank'> National Moth Recording scheme</a> from 2005 to 2013.",
              "The list shows the larger moth species previously recorded in this area (a block of nine 10km x 10km grid squares centred on your location) at around this date (a nine-day period centred on today’s date) and the number of sightings of each.")),
              style = 'width: 98%; max-width: 300px; font-size: small; text-align: center; color: white; display: block; background-color: dimgray; margin-top: 5px; margin-bottom: 5px; margin-left: auto; margin-right: auto; padding: 1px 1px 1px 1px;'),
      p(HTML(paste0("Site built by the <a class = 'about', href = 'http://www.brc.ac.uk/', target = '_blank'>Biological Records Centre</a>,",
                    " supported by the <a class = 'about', href = 'http://www.ceh.ac.uk/', target = '_blank'>Centre for Ecology & Hydrology</a>",
                    ", <a class = 'about', href = 'http://butterfly-conservation.org/', target = '_blank'>Butterfly Conservation</a>",
                    ", <a class = 'about', href = 'http://www.ukmoths.org.uk/', target = '_blank'>UKmoths</a>",
                    ", and the <a class = 'about', href = 'http://jncc.defra.gov.uk/', target = '_blank'>Joint Nature Conservation Committee</a>.",
                    " This application is hosted by the <a class = 'about', href = 'http://www.ceh.ac.uk/our-science/science-areas/environmental-informatics', target = '_blank'>CEH’s Environment Informatics Programme</a>.")),
        style = 'width: 98%; max-width: 300px; font-size: small; text-align: center; color: white; display: block; background-color: dimgray; margin-top: 5px; margin-bottom: 5px; margin-left: auto; margin-right: auto; padding: 1px 1px 1px 1px;'),
      p(HTML(paste("<a class = 'about', href = 'http://www.ukmoths.org.uk/use-of-images/', target = '_blank'>Images are the copyright of their owners</a>")),
        style = 'width: 98%; max-width: 300px; font-size: small; text-align: center; color: white; display: block; background-color: dimgray; margin-top: 5px; margin-bottom: 5px; margin-left: auto; margin-right: auto; padding: 1px 1px 1px 1px;'),
      a(href = 'http://www.brc.ac.uk/',
        target = '_blank',
        img(src = 'BRClogo.png', style = 'width: 93%; max-width: 300px; display: block; margin-left: auto; margin-right: auto;'))),
  
  includeScript("lightbox.js")
  
))
