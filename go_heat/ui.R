
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('types','Select media types',
                         choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'),
                         selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit')),
    
    radioButtons('measure','Select measurement',list("CPI"='cpi',"CPRef"='cpref',"CPRand"='cprand'))
    ),


    # Show a plot of the generated distribution
    mainPanel(
      showOutput("heatmap",'polycharts')
    )
  )
))
