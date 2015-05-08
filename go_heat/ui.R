
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
    sidebarPanel("Heatmaps provide quick visual indications of the best performing sites and media types by user-defined metric types",
     conditionalPanel(condition="input.tabs=='Heat'",
      radioButtons('measure','Select measurement',c("CPI"='cpi',"CPRef"='cpref',"CPRand"='cprand'),selected='cprand'),
      "The user selects from the metrics above that are inputs to the heatmap.  In this case we are looking at the cost per
      Inquiry (acquisition of a potential candidate interested in the trial), Referral (a candidate that has past initial screening),
      and Randomization (a candidate that is enrolled).  You can see visually how the cost varies by these various measurements and 
      by media type and location.",
      #uiOutput('types'),
      #checkboxInput('selected','Select All',value=TRUE),  
      checkboxGroupInput('types','Select media types',
                         choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'),
                         selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit')),
      "Users can choose to limit the media types (above) in the analysis as well as the dates considered (below).  This allows user control 
      over zooming in on performance over specific time periods or for a particular medium",
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
          "The first option is for the visualization - do we want to show only the best performing media type.  This will give the user a 
          quick way of identifying what is working",
          checkboxInput('best','Check to show only best performer'),
          radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          "We provide the option for the user to see the visualization or simple tabular data",
          conditionalPanel(condition="input.table=='graphic'",
            "Each square represents the cost per (inquiry/referral/randomization - depending on user input) for a particular site + media
            combination",
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
