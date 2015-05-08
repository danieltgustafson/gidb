
library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("Inquiries/Randomization model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel("The highlight of the platfrom is the back-end providing the analysis that is visualized by this application.
      Here we predict, for any user-defined level of spend in various mediums (below), how many inquiries and randomizations they
      will ultimately get.  This allows the company to best understand where to spend their advertising dollars, as well as estimate 
      how much will need to be spent in order to reach the goal number of enrollees.",

      sliderInput('spend_fb',"Enter Facebook Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_gg',"Enter Google/YMSN Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_tr',"Enter Transit Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_tv',"Enter TV Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_ra',"Enter Radio Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      sliderInput('spend_pr',"Enter Print Spend",min=0,max=75000,value=1000,ticks=TRUE,step=2500),
      "Below the user can restrict the date range of historical date for which the model is based.  This enables the user to exclude
      older data that may no longer be relevant.",
      dateInput('max_date',"Enter max model date"),

      #checkboxInput('site_check',"Check for statistically significant site Differences"),
      "As a whole, individual sites were not deemed to be a signficant variable in determining the outcome.  However, some individual outlying
      sites are impactful to the model.  Any that are impactful are listed here - if one selects a site, it reconstructs the model based on that 
      specific inputted site.",
        uiOutput('select_site')
    ),


    # Show a plot of the generated distribution
    mainPanel("'Box and Whisker' plots are useful ways of displaying distribution data.  Below we have data on the distribution of time
      from initial user inquiry through ultimate enrollment in the trial split by sites.  This allows us to visualize the difference in performance
      with respect to time, by site.  Furthermore, this type of graph aids in displaying the entire distribution of time by site as opposed to a simple 
      average.  Averages can easily be skewed by outliers - but the box and whisker shows both the average as well as the distribution.
      We also provide the option to aggregate the data across sites and just display the distribution for the entire dataset.",
      checkboxInput('all','Check to show all-site trend'),
      showOutput('box','highcharts'),
      conditionalPanel('!is.na(output.bar)',"Option to switch the model prediction between Inquiries and Enrollments",
        checkboxInput('inqs','Check to show predicted inquiries'),
      "BELOW: visual output of the model.  The center bar is the predicted number of enrollments[inquiries]. The outer bars are the 95% confidence high/low 
      end estimates of that predection.",
      showOutput("bar","nvd3")
            )

    )
  )
))
