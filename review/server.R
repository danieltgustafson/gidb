
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
  


con=dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='54.69.26.113', port=3306)
  
  
 
data<-reactive({
  dbGetQuery(con,"
select site_name,count(distinct patient_id) inquiries,if(status<>'Randomized',year(`Pre-Screen Date`)*100+month(`Pre-Screen Date`),year(Randomization)*100+month(randomization)) rand_date,
count(if(status<>'Failed Pre-Screen',patient_id,null)) as referrals, count(if(status='Randomized',patient_id,null)) as rands from gidb.endo1
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
	a<-data.table(ddply(a,.(rand_date),summarize,rands=sum(rands),referrals=sum(referrals),inquiries=sum(inquiries)))
	a[order(rand_date),cref:=cumsum(referrals)]
	a[order(rand_date),cinq:=cumsum(inquiries)]
	a[order(rand_date),crand:=cumsum(rands)]
})



 output$day2<-renderChart({
      
        theGraph <- Highcharts$new()
		theGraph$yAxis(
			    list(
			        list(
			            title = list(text = 'Randomized')
			        ),
			        list(
			            title = list(text = 'Referarals'), 
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
		    data = if(input$cumsum) {data_lim()$cref} else{data_lim()$referrals},
		    name = "Referarals",
		    type = "line",
		    yAxis=1
		)
		theGraph$xAxis(categories=data_lim()$rand_date,labels=list(rotation=-45))
        #theGraph$chart(zoomType = "xy")
        theGraph$addParams(dom='day2')
        
        return(theGraph)
      
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



