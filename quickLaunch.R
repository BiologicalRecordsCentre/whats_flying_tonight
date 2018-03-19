## Run using:
# source('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Shiny BRC/whats_flying_tonight/quickLaunch.R')

if(!require(devtools)) install.packages('devtools')
require(devtools)

source_gist(9112634)

package(c('shiny'))

library(shiny)

shiny::runApp('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Shiny BRC/whats_flying_tonight')