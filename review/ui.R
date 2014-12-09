
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
      
      uiOutput('sites'),
      checkboxInput('selected','Select All',value=TRUE) 
      

    ),


    # Show a plot of the generated distribution
    mainPanel(
      checkboxInput('cumsum','Check to cumulative sums'),
      radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
      conditionalPanel(condition="input.table=='graphic'",
        showOutput("day2",'highcharts')),
      conditionalPanel(condition="input.table=='table'",
        dataTableOutput("table"))
    )
  )
))
