
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
library(xlsx)
library(gdata)

getConnection <- function(group) {

  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='54.69.26.113', port=3306)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='54.69.26.113', port=3306)
  }

  return(.connection)
}

shinyServer(function(input, output) {

screener_data<-reactive({
	
	a<-dbGetQuery(getConnection(),"select screener,week_start, sum(new_refs_called) called, sum(new_refs_reached+past_refs_reached)
	 reached, sum(dqs) dqs, sum(passed) referrals,sum(passed)/sum(new_refs_reached+past_refs_reached) refreached,
	 sum(dqs)/sum(new_refs_reached+past_refs_reached) dqreach
	from gidb.screeners
	group by screener, week_start")
	a$unix<-as.numeric(as.POSIXct(a$week_start))*1000
	return(a)
})
output$day2<-renderChart({
      
        theGraph <- Highcharts$new()
		theGraph$yAxis(
			    list(
			        list(
			            title = list(text = input$lmeasure),min=0
			        ),
			        list(
			            title = list(text = input$cmeasure), min=0,
			            opposite =TRUE
			        )
			    )
			)
        theGraph$series(
		    data = toJSONArray2(subset(screener_data(),screener_data()$screener=='Jenne')[,c('unix',input$lmeasure)],names = F, json = F),
		    name = "Jenne",
		    type = "spline",
		    color="#137df6"
		)
		theGraph$series(
		    data = toJSONArray2(subset(screener_data(),screener_data()$screener=='Jenne')[,c('unix',input$cmeasure)],names = F, json = F),
		    name = " ",
		    type = "column",
		     groupPadding=0,
		    color="#7ab6fa",
		    yAxis=1   
		)
        theGraph$series(
		    data =  toJSONArray2(subset(screener_data(),screener_data()$screener=='Cheryl')[,c('unix',input$lmeasure)],names = F, json = F),
		    name = "Cheryl",
		    type = "spline",
		    color= "#0ac507"
		)
		theGraph$series(
		    data =  toJSONArray2(subset(screener_data(),screener_data()$screener=='Cheryl')[,c('unix',input$cmeasure)],names = F, json = F),
		    name = " ",
		    type = "column",
		    groupPadding=0,
		    yAxis=1 ,
		    color="#5ff95d"  
		)
		theGraph$series(
		    data =  toJSONArray2(subset(screener_data(),screener_data()$screener=='Kim')[,c('unix',input$lmeasure)],names = F, json = F),
		    name = "Kim",
		    type = "spline" ,
		    color= "#ca0707"  
		)
		theGraph$series(
		    data =  toJSONArray2(subset(screener_data(),screener_data()$screener=='Kim')[,c('unix',input$cmeasure)],names = F, json = F),
		    name = " ",
		    type = "column",
		    groupPadding=0,
		    yAxis=1 ,
		    color="#f84949" 
		)
		
	
		theGraph$xAxis(type='datetime',name='Week',labels=list(rotation=-45,align='right',
			overflow='justify'))
        theGraph$chart(zoomType = "xy")
        theGraph$addParams(dom='day2')
        return(theGraph)
      
  })
output$table<-renderDataTable({
  screener_data()
})

jenne<-reactive({
	if(input$submit==0) return(NULL)
	isolate({
		infile<-input$file1
		a<-read.xlsx2(infile$datapath,1,stringsAsFactors=FALSE)
		a$Week.of<-as.Date(as.numeric(a$Week.of), origin="1904-01-01")
		a$Week.of[is.na(a$Week.of)]<-"2014-04-21"
		a$name<-'Jenne'
		return(a)
	})
})
kim<-reactive({
	if(input$submit==0) return(NULL)
	isolate({
		infile<-input$file2
		a<-read.xlsx2(infile$datapath,1,stringsAsFactors=FALSE)
		a$Week.of<-as.Date(as.numeric(a$Week.of), origin="1904-01-01")
		a$name<-'Kim'
		return(a)
	})	
})
cheryl<-reactive({
	if(input$submit==0) return(NULL)
	isolate({
		infile<-input$file3
		a<-read.xlsx2(infile$datapath,1,stringsAsFactors=FALSE)
		a$Week.of<-as.Date(as.numeric(a$Week.of), origin="1904-01-01")
		a$name<-'Cheryl'
		return(a)
	})
})
data_out<-reactive({
if(input$submit==0) return(NULL)
		isolate({
		kim<-kim()
		jenne<-jenne()
		cheryl<-cheryl()
		colnames(kim)<-colnames(jenne)
		colnames(cheryl)<-colnames(jenne)
		a<-as.data.frame(rbind(jenne,kim,cheryl))
	return(a)
	})
})


observe({
	if(input$submit==0) return(NULL)
	isolate({

	write.csv(data_out(),file='/home/ubuntu/screeners.csv',quote=TRUE,row.names=FALSE)
})
})
observe({
	if(input$submit2==0) return(NULL)
	isolate({
		dbSendQuery(getConnection(),"truncate table gidb.screeners")
		dbSendQuery(getConnection(),"LOAD DATA local infile '/home/ubuntu/screeners.csv'  into table gidb.screeners COLUMNS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' lines terminated by '\n' ignore 1 lines")
	})
})

patient_data<-reactive({
	if(input$submit==0) return(NULL)
		isolate({
			a<-data_out()
			zz<-data.frame(name=character(),site_number=character(),week=as.Date(character()),patient_id=character(),stringsAsFactors=FALSE)
			i=1
			for(i in 1:length(a[,1]))
			{
			    if(a$Patient.IDs..Priority.Patients.[i]!="")
			    {
			    	z<-cbind(a$name[i],a$Site.number[i],a$Week.of[i],data.frame(trim(unlist(strsplit(a$Patient.IDs..Priority.Patients.,',|[.]|;',perl=TRUE)[i]))))
				}
				else
				{
					z<-data.frame(name=character(),site_number=character(),week=as.Date(character()),patient_id=character(),stringsAsFactors=FALSE)
				}
			    zz<-rbind(zz,z)
			    i=i+1 
			}
			return(zz)
	})
})

output$preview<-renderDataTable({
  patient_data()
})


observe({
	if(input$submit==0) return(NULL)
	isolate({
		write.csv(patient_data(),file='/home/ubuntu/screener_patients.csv',quote=TRUE,row.names=FALSE)
})
})

output$send<-renderText({
	if(input$submit2==0) return(NULL)
	isolate({
		dbSendQuery(getConnection(),"truncate table gidb.screener_patients")
		dbSendQuery(getConnection(),"LOAD DATA local infile '/home/ubuntu/screener_patients.csv'  into table gidb.screener_patients COLUMNS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' lines terminated by '\n' ignore 1 lines")
	return('SUCCESS!')
	})


})

})

