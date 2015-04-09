
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("FITBIT!"),

  # Sidebar with a slider input for number of bins


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("User Growth",
            showOutput("day2",'highcharts')
    ),
        tabPanel("Retention",
          #radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          #conditionalPanel(condition="input.table=='graphic'",
            tags$style('.highcharts {height: 650px}'),
            tags$style('.highcharts {width: 1100px}'),
            showOutput("retention",'highcharts'),
          #conditionalPanel(condition="input.table=='table'",
           dataTableOutput('retentest')
    ),
        tabPanel("Engagement",
          showOutput("engagement",'highcharts')
  )
)
)
))