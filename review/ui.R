
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
      "Below we list all of the available sites - users can select which sites to focus on or leave all available",
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
          "This web-application provides high level summary data to the user for a selected list of sites.  BELOW we provide an 
          option to provide cumulative data rather than for individual time points.",
          checkboxInput('cumsum','Check for cumulative sums'),
          "Data can be shown in simple tabular format or as a graphic via the toggle below",
          radioButtons('table','Show Table or Graphic?',list("Table"='table',"Graphic"='graphic')),
          conditionalPanel(condition="input.table=='graphic'",
            "With Graphic selected we provide a number of options for which metrics to show in the graph.  The graphics summarize the
            selected data visually",
            selectInput('measure','Select Measurement',list('Inquiries'='Inquiries','Referrals'='Referrals','Screens'='Screens')),
            showOutput("day2",'highcharts'),
            showOutput("day3",'highcharts')),
          conditionalPanel(condition="input.table=='table'",
            dataTableOutput("table"))
    ),
        tabPanel("Upload",
          "This second tab provides a user-interface for uploading new data that powers the visualizations in the first page.  This currently
          only accepts CSVs and requires a previously defined file layout.  This restriction could be relaxed in the future.  A preview of the 
          uploaded data is shown below for verification before submitting to the database.",
          dataTableOutput("preview"))
  )
)
)
))