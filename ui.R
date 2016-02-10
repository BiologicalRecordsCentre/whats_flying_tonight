
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
  checkboxInput('use_man',
                'Use different location?',
                FALSE),
  div(id = 'dropBoxs',
      selectInput('hectad_man',
                  '',
                  sort(gsub('.rdata$',
                            '',
                            list.files('data/hectad_counts/'))),
                  selectize = FALSE,
                  multiple = FALSE,
                  width = '85px')),
  
  includeScript("lightbox.js")
  
))
