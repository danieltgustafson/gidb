
library(shiny)
library(rCharts)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Screeners"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      conditionalPanel(condition="input.tabs=='Main'",
        selectInput('lmeasure','Select Line Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "DQ/Reached"='dqreach',"Ref/Reached"='refreached')),
         selectInput('cmeasure','Select Bar Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "DQ/Reached"='dqreach',"Ref/Reached"='refreached'),selected='dqs')

  
      

    ),
      conditionalPanel(condition="input.tabs=='Upload'",
        fileInput('file1', 'Upload tracking file for JENNE'),
        fileInput('file2', 'Upload tracking file for KIM'),
        fileInput('file3', 'Upload tracking file for CHERYL'),
        actionButton("submit","Submit")
    )),


    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Main",
                checkboxInput("table","Show as table?"),
        conditionalPanel(condition="input.table",
         dataTableOutput("table")),
        #conditionalPanel(condition="input.table==false",
        showOutput("day2",'highcharts')
    ),
        tabPanel("Upload",
          actionButton("submit2","Review and Submit to database"),
          conditionalPanel(condition="input.submit2",
            textOutput("send")
            ),
          conditionalPanel(condition="input.submit2==false",
            dataTableOutput("preview")
            )
  )
)
)
)
))