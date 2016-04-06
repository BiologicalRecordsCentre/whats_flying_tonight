## Run using:
# source('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Shiny BRC/whats_flying_tonight/quickLaunch.R')

if(!require(devtools)) install.packages('devtools')
require(devtools)

source_gist(9112634)

package(c('shiny', 'ggplot2', 'RColorBrewer', 'grid', 'lme4',
          'plyr', 'dplyr', 'sp', 'reshape2', 'ggplot2', 'gdata'))

if(!require(sparta)) install_github('biologicalrecordscentre/sparta', force = TRUE)
require(sparta)

library(shiny)

shiny::runApp('W:/PYWELL_SHARED/Pywell Projects/BRC/Tom August/Shiny BRC/whats_flying_tonight')