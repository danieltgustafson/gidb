#

library(shiny)
library(rCharts)


shinyUI(fluidPage(

  # Application title
  titlePanel("DQ Reasons"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons('site_type',"Group by Site / Media or DQ reason?",list('Media'='media','Site'='site','DQ'='dq')),
     conditionalPanel(condition="input.site_type=='media'",
        uiOutput("types")),
      conditionalPanel(condition="input.site_type=='site'",
        uiOutput('sites')),
      #conditionalPanel(condition="input.site_type=='dq'",
       # uiOutput('reasons')),
      checkboxInput('selected',"Select All",value=TRUE)
    
    ),


    # Show a plot of the generated distribution
    mainPanel(
      radioButtons('media_group','Select Groups',
        list("Group Pre-Screen"='group',"Types/Sites"='types',"Post-Screen"='post','Full'='full')),
      checkboxInput("table","Check to show output as Table"),
      conditionalPanel(condition="input.table == false",
        showOutput("sankey",'d3_sankey')
      ),
      conditionalPanel(condition="input.table == true",
        dataTableOutput("test")
    )
    )
  )
))
