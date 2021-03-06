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
    mainPanel("This tool is to help visualize the 'funnel' flow for candidates entering the enrollment process.  Where are they falling out?  
      What are the most common reason for their disqualifications?  Is there a certain site or media type that tends to disqualify for a particular reason?",
      radioButtons('media_group','Select Groups',
        list("Group Pre-Screen"='group',"Types/Sites"='types',"Post-Screen"='post','Full'='full')),
      "The options above help navigate the graphic (which can be unwieldy if there are a lot of steps.  We can choose to group together all of the sources
        for example, and focus only on the aggregate counts of disqualification reasons.  Alternatively we may want to focus only on users that passed the pre-screening
        process and analyze their destinations.",
      checkboxInput("table","Check to show output as Table"),
      conditionalPanel(condition="input.table == false",
        htmlOutput('pie'),
        showOutput("sankey",'d3_sankey')
      ),
      conditionalPanel(condition="input.table == true",
        dataTableOutput("test")
    )
    )
  )
))
