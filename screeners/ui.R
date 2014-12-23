
library(shiny)
library(rCharts)
library(shinyIncubator)


shinyUI(fluidPage(
  progressInit(),
  # Application title
  titlePanel("Screeners"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      conditionalPanel(condition="input.tabs=='Main'",
        selectInput('lmeasure','Select Line Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "Randomizations"='randomized',
          "DQ/Reached"='dqreach',"Ref/Reached"='refreached')),
         selectInput('cmeasure','Select Bar Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "Randomizations"='randomized',
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
        showOutput("day2",'highcharts'),
        actionButton('get',"Get Pie Chart"),
        conditionalPanel(condition="input.get",
        #radioButtons("name","Name for pie chart output",list("Jenne"='Jenne',"Kim"='Kim',"Cheryl"="Cheryl")),
        showOutput("pie_kim",'highcharts'), showOutput("pie_jenne",'highcharts'), showOutput("pie_cheryl",'highcharts'))
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