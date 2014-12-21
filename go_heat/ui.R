
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Cost Metrics by Site and Type"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons('measure','Select measurement',list("CPI"='cpi',"CPRef"='cpref',"CPRand"='cprand')),
      #uiOutput('types'),
      #checkboxInput('selected','Select All',value=TRUE),  
      checkboxGroupInput('types','Select media types',
                         choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'),
                         selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit')),
      dateRangeInput('dateRange',
      label = 'Date range input: yyyy-mm-dd',
      start = '2013-01-01', end = Sys.Date() 
    ),
      downloadButton("downloadData","Download")
    ),


    # Show a plot of the generated distribution
    mainPanel(
      checkboxInput('best','Check to show only best performer'),
      radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
      conditionalPanel(condition="input.table=='graphic'",
        showOutput("heatmap",'polycharts')),
      conditionalPanel(condition="input.table=='table'",
        dataTableOutput("table"))
    )
  )
))
