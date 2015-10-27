
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css")
  ),
  
  htmlOutput('UI')
  
))
