
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
library(shinyIncubator)

getConnection <- function(group) {

  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='localhost', port=3306)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='localhost', port=3306)
  }

  return(.connection)
}

shinyServer(function(input, output,session) {

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


screener_pats<-reactive({
	if(input$get==0) return(NULL)
	isolate({
	withProgress(session, min=1, max=25, {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...')
          for (i in 1:25) {
              setProgress(value = i)
              Sys.sleep(1)
          }
	a<-dbGetQuery(getConnection(),"select count(distinct if(el_param is not null,a.patient_id,null)) counts,screener_name,if(status='Randomized','Randomized',if(el_param<>'NA' and el_param<>' ' and el_param<>'Multiple',
				trim(el_param),if(el_change not like '%HbA1%',substring(el_change,1,20),'HbA1c'))) status1
				from 
				(select c.site_name,c.patient_id,week_observed,c.screener_name,b.status from gidb.screener_patients c join
				gidb.endo1 b on b.patient_id = c.patient_id 
				) a
				join(
				select b.site_id,b.patient_id,a.`el_param`,a.`Randomized`,el_change from gidb.endo1 b
				join
				gidb.eligible_track a on concat(substring(a.`Initials`,1,1),substring(a.initials,3,1)) = substring(b.patient_id,1,2) and if(a.Randomized='Yes',1,0) =if(b.status='Randomized',1,0)
				and a.site=b.site_id) b
				on a.patient_id = b.patient_id and a.site_name=b.site_id
				where el_param is not null
				group by screener_name,status1")
	a$counts<-as.numeric(a$counts)
	if(length(a[a$status1=="",]$status1)>0){
		a[a$status1=="",]$status1<-'Unknown'
	}
	return(a)
})
})
})
selected<-reactive({
	a<-subset(screener_pats(),screener_pats()$screener_name %in% input$name)
	a<-ddply(a,.(screener_name),summarize,status=status1,value=counts/sum(counts))

	return(a[order(a$value),])
	})
output$pie<-renderChart({
	if(input$get==0) return(NULL)
	isolate({
		a <- Highcharts$new()
		a$title(text = paste(input$name," Referrals"))
		a$data(x = selected()$status, y =selected()$value*100 , type = "pie", name = "Percent")
		a$addParams(dom='pie')
		return(a)
	})
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

