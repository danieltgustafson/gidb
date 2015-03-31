
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
     conditionalPanel(condition="input.tabs=='Heat'",
      radioButtons('measure','Select measurement',c("CPI"='cpi',"CPRef"='cpref',"CPRand"='cprand'),selected='cprand'),
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
    conditionalPanel(condition="input.tabs=='Bar'",
       selectInput('LAME','Select a measurement',list("Inquiries"='inquiries',"Randomizations"='rands',"Referrals"='referrals'))
    )
),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Heat",
          checkboxInput('best','Check to show only best performer'),
          radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          conditionalPanel(condition="input.table=='graphic'",
            showOutput("heatmap",'polycharts')),
          conditionalPanel(condition="input.table=='table'",
            selectInput('total','Total Options',list("No Total"='all',"By Medium"='media',"By Site"='site','Overall'='total')),
            dataTableOutput("table"))
        ),
        tabPanel("Bar",
          showOutput('bars','highcharts'))
    )
  ))
))
