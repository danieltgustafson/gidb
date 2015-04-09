
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RMySQL)
library(rCharts)
library(plyr)
library(reshape2)
library(lubridate)

shinyServer(function(input, output) {
  


con=dbConnect(MySQL(),username='dgustafson',password='c3808v4m', 
	#host='54.69.26.113', port=3306)
	host='localhost', port=3306)
  
  
 
data<-reactive({
  a<-dbGetQuery(con,"select * from airbnb.fitbit")
  a$date_created<-as.Date(a$date_created)
  date<-as.data.frame(seq(min(a$date_created),by=1,to=max(a$date_created)))
  colnames(date)<-'date'
  a<-merge(x=date,y=a,by.x="date",by.y="date_created",all.x=TRUE)
  return(a)
})
reten<-reactive({
	a<-dbGetQuery(con,"select year(date_created)*100+month(date_created) as month_id,count(distinct user_id) counts, 
		count(distinct if(log_date>date_created,user_id,null))/count(distinct user_id) 1_day_reten,
		count(distinct if(log_date>date_created+INTERVAL 30 day,user_id,null))/count(distinct user_id) 30_day_reten,
		count(distinct if(log_date>date_created+INTERVAL 90 day,user_id,null))/count(distinct user_id) 90_day_reten
		from airbnb.fitbit
		group by month_id")
	return(a)
})

growth<-reactive({
	a<-ddply(data(),.(date),summarize,counts=if(is.na(user_id)){max(0)}else{length(unique(user_id))})
	a$cum_sum<-cumsum(a$counts)
	a$unix<-as.numeric(as.POSIXct(a$date))*1000
	return(a)
})

engage<-reactive({
	a<-ddply(data(),.(as.Date(paste(year(date),'-',month(date),'-','01',sep="")),as.Date(paste(year(log_date),'-',month(log_date),'-','01',sep=""))),
		summarize,logs=length(unique(paste(log_date,user_id)))/length(unique(user_id)))
	colnames(a)<-c('created','log','count')
	a$months<-round((a$log-a$created)/30,0)
	return(a)
})
output$engagement<-renderChart({
	h3<-hPlot(count~months,data=engage(),group='created')
	h3$tooltip(useHTML=T,formatter="#!function(){ return 'Cohort:'+ this.series.name +'<br/> Avg. Engagements: '+ this.y.toFixed(1)}!#")
	h3$addParams(dom='engagement',width = 1000, height = 700)
	return(h3)
})

 output$day2<-renderChart({
      
       	theGraph <- Highcharts$new()
		theGraph$yAxis(
		    list(
		        list(
		            title = list(text = 'users'),min=0
		        ),
		        list(
		            title = list(text = 'cumulative'), min=0,
		            opposite =TRUE
		        )
		    )
		)
		theGraph$series(
		    data = toJSONArray2(growth()[,c('unix','counts')],names = F, json = F),
		    name = "New Users",
		    type = "column",
		    color= "#ca0707"
		)
		theGraph$series(
		    data = toJSONArray2(growth()[,c('unix','cum_sum')],names = F, json = F),
		    name = "Cumulative",
		    type = "spline",
		    groupPadding=0,
		    color= "#7ab6fa",
		    yAxis=1   
		)
        theGraph$addParams(width = 1000, height = 700,dom='day2')
        theGraph$xAxis(type='datetime',name='date',labels=list(rotation=-45,align='right',
                                                       overflow='justify'))
        theGraph$chart(zoomType = "xy")
        return(theGraph)
      
  })
 output$retention<-renderChart({
	h2<-Highcharts$new()
	h2$yAxis(
		    list(
		        list(
		            title = list(text = 'Retention Rate'),min=0
		        ),
		        list(
		            title = list(text = 'Users'), min=0,
		            opposite =TRUE
		        )
		    )
		)
	h2$series(data=reten()$counts,yAxis=1,name='Users: ',type='column')
	h2$series(data=reten()$`1_day_reten`,name='1 Day Retention: ')
	h2$xAxis(categories=reten()$month_id,labels=list(rotation=0,y=10))	
	h2$series(data=reten()$`30_day_reten`,name='30 Day Retention: ')
	h2$series(data=reten()$`90_day_reten`,name='60 Day Retention: ')
	h2$addParams(width = 1000, height = 700,dom='retention')
	h2$tooltip(useHTML=T,formatter="#!function(){ return this.series.name + this.y.toFixed(2)}!#")
	return(h2)
 })
output$retentest<-renderDataTable({
	reten()
})
 
})



