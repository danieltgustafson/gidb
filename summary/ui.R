
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Summary Dashboard"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

   
        dateRangeInput('dateRange',
      label = 'Date range input: yyyy-mm-dd',
      start = '2013-01-01', end = Sys.Date() 
    ),
       selectInput('measure','Select a measurement',list("Inquiries"='inquiries',"Randomizations"='rands',"Referrals"='referrals'))
    
),

    # Show a plot of the generated distribution
    mainPanel(
    
        textOutput('text'),
          showOutput('bars','highcharts')
    
  ))
))
