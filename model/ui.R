
library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Inquiries/Randomization model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      sliderInput('spend_fb',"Enter Facebook Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_gg',"Enter Google/YMSN Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_tr',"Enter Transit Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_tv',"Enter TV Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_ra',"Enter Radio Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_pr',"Enter Print Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      dateInput('max_date',"Enter max model date"),

      #checkboxInput('site_check',"Check for statistically significant site Differences"),

        uiOutput('select_site')
    ),


    # Show a plot of the generated distribution
    mainPanel(

      showOutput('box','highcharts'),
      conditionalPanel('!is.na(output.bar)',
        checkboxInput('inqs','Check to show predicted inquiries')
      showOutput("bar","nvd3")
            )

    )
  )
))
