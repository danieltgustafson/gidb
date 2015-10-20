
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Usage"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput('ui_orgs')
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("First Use vs Time",
          dataTableOutput("test"),
          showOutput("usage",'highcharts')
    )
        #,tabPanel("Retention",
          #radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          #conditionalPanel(condition="input.table=='graphic'",
            #tags$style('.highcharts {height: 650px}'),
            #tags$style('.highcharts {width: 1100px}'),
            #showOutput("retention",'highcharts'),
          #conditionalPanel(condition="input.table=='table'",
           #dataTableOutput('retentest')
           #)

)
))
))