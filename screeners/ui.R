
library(shiny)
library(rCharts)
library(shinyIncubator)


shinyUI(fluidPage(
  progressInit(),
  # Application title
  titlePanel("Screeners"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel("This is one of the more customizable applications providing a vast number of user-options for the metrics to visualize.  
      The focus of this one is on 'screeners', which are invidual representatives that are responsible for performing phone-interviews on
      candidates that have passed through the initial screening process.  They are in place as an additional filter for potentially unqualified
      candidates - before they go through the battery of expensive testing.",

      conditionalPanel(condition="input.tabs=='Main'",
        "This menu allows the user to select which metric is represented by the line in the graph to the right.  Options include counting the
        number of candidates called (per week), reached by phone, referred on to the next step, disqualified, or ultimately enrolled.  
        There are also ratios of these values available and the same metric list can be graphed as a column for comparison.  All data is split
        by the individual screening person responsible.",
        selectInput('lmeasure','Select Line Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "Randomizations"='randomized',
          "DQ/Reached"='dqreach',"Ref/Reached"='refreached',"Rand/Referal"='randref',"None"='None')),
         selectInput('cmeasure','Select Bar Measure',list("Reached"='reached',"Called"='called',"Referrals"='referrals',"DQs"='dqs',
          "Randomizations"='randomized',
          "DQ/Reached"='dqreach',"Ref/Reached"='refreached',"Rand/Referal"='randref',"None"='None'),selected='dqs')
    ),
      conditionalPanel(condition="input.tabs=='Pie'",
        uiOutput('ui_sites'),
        checkboxInput('selected','Select All',value=TRUE),
        dateInput('date',"Screen Date > : ",value='2014-01-01')
      ),
      conditionalPanel(condition="input.tabs=='Upload'",
        fileInput('file1', 'Upload tracking file for JENNE'),
        fileInput('file2', 'Upload tracking file for KIM'),
        fileInput('file3', 'Upload tracking file for CHERYL'),
        actionButton("submit","Submit")
    )),

    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Main", "As always, we provide the option of graphic or tabular views of the data",
                checkboxInput("table","Show as table?"),
        conditionalPanel(condition="input.table",
         dataTableOutput("table")),
        conditionalPanel(condition="input.table==false",
        showOutput("day2",'highcharts')),
        checkboxInput("monthly","Group time to randomization by Month?"),
        #dataTableOutput("timechart"),
        showOutput("timechart",'highcharts')
        ),
        tabPanel("Pie","Here we provide a 'drilldown' into the reasons that candidates were ultimately disqualified from the process after a 
          screener had referred them on.  It can help us identify screeners that may systematically be missing a reason for excluding the candidate
          and potentially re-train them on what to check for ",
        actionButton('get',"Get Pie Charts"),
        conditionalPanel(condition="input.get",
        #radioButtons("name","Name for pie chart output",list("Jenne"='Jenne',"Kim"='Kim',"Cheryl"="Cheryl")),
        showOutput("pie_kim",'highcharts'), showOutput("pie_jenne",'highcharts'), showOutput("pie_cheryl",'highcharts'))
    ),
        tabPanel("Upload", "This second tab again allows the user to upload files that will be loaded into the database and impact the
          graphics in the first tab.",
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