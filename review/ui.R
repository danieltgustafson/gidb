
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Overview"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tabs=='Main'",
      
        uiOutput('sites'),
        checkboxInput('selected','Select All',value=TRUE) 
      

    ),
      conditionalPanel(condition="input.tabs=='Upload'",
        fileInput('file1', 'Choose file to upload')
    )),


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Main",
          checkboxInput('cumsum','Check for cumulative sums'),
          radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          conditionalPanel(condition="input.table=='graphic'",
            selectInput('measure','Select Measurement',list('Inquiries'='Inquiries','Referrals'='Referrals','Screens'='Screens')),
            showOutput("day2",'highcharts'),
            showOutput("day3",'highcharts')),
          conditionalPanel(condition="input.table=='table'",
            dataTableOutput("table"))
    ),
        tabPanel("Upload",
          dataTableOutput("preview"))
  )
)
)
))