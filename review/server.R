
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RMySQL)
library(rCharts)
library(plyr)
library(data.table)

shinyServer(function(input, output) {
  


con=dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='localhost', port=3306)
  
  
 
data<-reactive({
  dbGetQuery(con,"
select a.site_name,rand_date,
count(distinct if(100*year(`Pre-Screen Date`)+month(`Pre-Screen Date`)=rand_date,patient_id,null)) inquiries,
count(distinct if(100*year(`Randomization`)+month(`Randomization`)=rand_Date,patient_id,null)) rands,
count(distinct if(status<>'Failed Pre-Screen'and 100*year(`Pre-Screen Date`)+month(`Pre-Screen Date`)=rand_date,patient_id,null))as referrals, 
count(if(`Screening Visit` >='2001-01-01'and 100*year(`Screening Visit`)+month(`Screening Visit`)=rand_date,patient_id,null)) screens, max(cost) as cost from gidb.endo1 a
join (
select year(start_date)* 100 + month(start_date) rand_date,site_id,sum(cost) cost from gidb.media 
group by rand_date,site_id) b on a.site_id = b.site_id 
group by rand_date,site_name")

})
output$sites<-renderUI({
	
	if(input$selected){
	checkboxGroupInput('sites',"Select Sites",choices=c(paste(unique(data()$site_name))),
	selected=c(paste(unique(data()$site_name))))
	}
	else checkboxGroupInput('sites',"Select Sites",choices=c(paste(unique(data()$site_name))))
})

data_lim<-reactive({
	
	a<-subset(data(),data()$rand_date>0&data()$site_name %in% input$sites)
	a<-data.table(ddply(a,.(rand_date),summarize,rands=sum(rands),referrals=sum(referrals),inquiries=sum(inquiries),
		cost=sum(cost),screens=sum(screens)))
	a[order(rand_date),cref:=cumsum(referrals)]
	a[order(rand_date),cinq:=cumsum(inquiries)]
	a[order(rand_date),crand:=cumsum(rands)]
	a[order(rand_date),ccost:=cumsum(cost)]
	a[order(rand_date),cscreen:=cumsum(screens)]

})



 output$day2<-renderChart({
      
        theGraph <- Highcharts$new()
		theGraph$yAxis(
			    list(
			        list(
			            title = list(text = 'Randomized')
			        ),
			        list(
			            title = list(text = input$measure), 
			            opposite =TRUE
			        )
			    )
			)
        theGraph$series(
		    data = if(input$cumsum) {data_lim()$crand} else{data_lim()$rands},
		    name = "Randomized",
		    type = "column"
		)
        theGraph$series(
		    data = 
		    	if(input$measure=='Referrals')
		    	{
		    		if(input$cumsum)
				     	{data_lim()$cref}
				     else
				     	{data_lim()$referrals}
				}
				else if(input$measure=='Inquiries')
				{
				     if(input$cumsum)
				     	{data_lim()$cinq}
				     else
				     	{data_lim()$inquiries}
				}
				else
				{
					if(input$cumsum)
				     	{data_lim()$cscreen}
				     else
				     	{data_lim()$screens}
				},
		    name = input$measure,
		    type = "line",
		    yAxis=1
		)
		theGraph$xAxis(categories=data_lim()$rand_date,labels=list(rotation=-45,y=5))
        #theGraph$chart(zoomType = "xy")
        theGraph$addParams(dom='day2')
        
        return(theGraph)
      
  })
 output$day3<-renderChart({
      
        theGraph <- Highcharts$new()
		theGraph$yAxis(
			    list(
			        list(
			            title = list(text = input$measure)
			        ),
			        list(
			            title = list(text = 'Spend'), 
			            opposite =TRUE
			        )
			    )
			)
        theGraph$series(
		    data = if(input$measure=='Referrals')
		    	{
		    		if(input$cumsum)
				     	{data_lim()$cref}
				     else
				     	{data_lim()$referrals}
				}
				else if(input$measure=='Inquiries')
				{
				     if(input$cumsum)
				     	{data_lim()$cinq}
				     else
				     	{data_lim()$inquiries}
				}
				else
				{
					if(input$cumsum)
				     	{data_lim()$cscreen}
				     else
				     	{data_lim()$screens}
				},
		    name = input$measure,
		    type = "column"
		)
        theGraph$series(
		    data = if(input$cumsum) {data_lim()$ccost} else{data_lim()$cost},
		    name = "Spend",
		    type = "area",
		    fillOpacity = 0.2,
		    yAxis=1
		)
		theGraph$xAxis(categories=data_lim()$rand_date,labels=list(rotation=-45,y=5))
        #theGraph$chart(zoomType = "xy")
        theGraph$addParams(dom='day3')
        
        return(theGraph)
      
  })
contents<-reactive({
	write(input$file1,'~/endo_data.xlsx')
})

output$table<-renderDataTable({
  data_lim()
})
output$downloadData <- downloadHandler(
    filename = function() { paste('cpi_data', '.csv', sep='') },
    content = function(file) {
      write.csv(data_lim(), file)
  }    
)
})



