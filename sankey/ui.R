#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("DQ Reasons"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('types','Select media types',
                         choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit','PR'),
                         selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit','PR'))
    ),


    # Show a plot of the generated distribution
    mainPanel(
      radioButtons('media_group','Select Groups',list("Group Pre-Screen"='group',"Media Types"='types',"Post-Screen"='post')),
      showOutput("sankey",'d3_sankey')
    )
  )
))
