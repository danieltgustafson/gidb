
library(shiny)
library(RMySQL)
library(rCharts)
library(plyr)
#library(googleVis)


getConnection <- function(group) {

  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(),username='',password='',host='localhost', port=3306)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(),username='',password='',host='localhost', port=3306)
  }

  return(.connection)
}
shinyServer(function(input, output) {
 
  
 
data_full<-reactive({
	dbGetQuery(getConnection(),"
		select type source,site_name, case when  a.status = 'Failed Pre-Screen' then dq_group else 'Post-Screen' end as target, count(distinct patient_id) value from gidb.endo1 a
		join gidb.`dim_media_type` b on a.media_type = b.detail
		left join gidb.`dim_dq_type` c on a.`Pre-Referral DQ` = c.dq_reason and a.status = 'Failed Pre-Screen'

		group by source,site_name, target

		union
		select 'Post-Screen' as source, 'Post-Screen' as site_name,case when  a.status like 'Excluded%' then dq_group else 'Site-Screen' end as target, count(distinct patient_id) value from gidb.endo1 a
		join gidb.`dim_media_type` b on a.media_type = b.detail
		left join gidb.`dim_dq_type` c on a.`Post-Referral DQ` = c.dq_reason
		where a.status <> 'Failed Pre-Screen'
		group by source,site_name, target

		union 
		select 'Site-Screen' as source, 'Site-Screen' as site_name,if(status = 'Randomized', status, 
			if(el_param is not null and el_param <> '',el_param, 'Unknown')) as target, count(distinct concat(initials,screen)) value 
		from gidb.eligible_track a
		join gidb.endo1 b on concat(substring(a.`Initials`,1,1),substring(a.initials,3,1)) = substring(b.patient_id,1,2)
		and a.site=b.site_id and if(a.Randomized='Yes',1,0) =if(b.status='Randomized',1,0)
		where b.status not like 'Excluded%' and b.status <> 'Failed Pre-Screen'
		group by source,site_name, target	"
	)
})

output$sites<-renderUI({
	a<-subset(data_full(),data_full()$site_name!='Post-Screen'&data_full()$site_name!='Site-Screen')
	if(input$selected){
	checkboxGroupInput('sites',"Select Sites",choices=c(paste(unique(a$site_name))),
	selected=c(paste(unique(a$site_name))))
	}
	else checkboxGroupInput('sites',"Select Sites",choices=c(paste(unique(a$site_name))))
	})
output$types<-renderUI({
	if(input$selected){
	 checkboxGroupInput('types','Select media types',
                         choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit','PR'),
                          selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit','PR')
                          )
	}
	else{
	 checkboxGroupInput('types','Select media types',
     choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit','PR'))
	}
})
output$reasons<-renderUI({
	if(input$selected){
	checkboxGroupInput('reasons',"Select DQ Reasons",choices=c(paste(unique(data_full()$target))),
	selected=c(paste(unique(data_full()$site_name))))
	}
	else checkboxGroupInput('reasons',"Select DQ Reasons",choices=c(paste(unique(data_full()$target))))

	})
site_type_data<-reactive({
	if(input$site_type=='site'){
		a<-ddply(data_full(),.(site_name,target),summarise,value=sum(value))
	}
	else if(input$site_type=='media')
	{ 
		a<-ddply(data_full(),.(source,target),summarise,value=sum(value))
	}
	else if(input$site_type=='dq'){
		a<-ddply(data_full,.(target),summarise,source=source,value=sum(value))
		colnames(a)<-c('')
	}
	return(a)
})

data<-reactive({
if(input$site_type=='media')
{
	a<-subset(site_type_data(),site_type_data()$source %in% input$types)
	if(input$media_group=='types'){
		b<-ddply(a,~source,summarise,target=target,value=value/sum(value))
	}
	else if(input$media_group=='group')
	{
		c<-ddply(a,.(target),summarise,value=sum(value))
		b<-ddply(c,.(),summarise,.id='source',target=target,value=value/sum(value))
		b$source<-'Inquiry'
}
	else if(input$media_group=='post')
		{
			b<-subset(site_type_data(),site_type_data()$source=='Post-Screen'&site_type_data()$target!='NA')
			b<-ddply(b,.(),summarise,.id='source',target=target,value=value/sum(value))
			b$source<-'Post-Screen'
			colnames(b)<-c('source','target','value')
		}
	else {
		b<-ddply(a,.(),summarise,.id='source',target=target,value=value/sum(value))
		b$source<-'Inquiry'
		c<-subset(site_type_data(),site_type_data()$source=='Post-Screen'&site_type_data()$target!='NA')
		c<-ddply(c,.(),summarise,.id='source',target=target,value=value/sum(value))
		c$source<-'Post-Screen'
		b<-rbind(b,c)
		colnames(b)<-c('target','source','value')
		b<-b[order(-b$value),]
	}
}
else if(input$site_type=='site')
{
	a<-subset(site_type_data(),site_type_data()$site_name %in% input$sites)
	
	if(input$media_group=='types'){
		b<-ddply(a,~site_name,summarise,target=target,value=value/sum(value))
		colnames(b)<-c('source','target','value')
	}
	else if(input$media_group=='group')
	{
		c<-ddply(a,.(target),summarise,value=sum(value))
		b<-ddply(c,.(),summarise,.id='source',target=target,value=value/sum(value))
		b$source<-'Inquiry'
}
	else if(input$media_group=='post')
		{
			b<-subset(site_type_data(),site_type_data()$site_name=='Post-Screen'&site_type_data()$target!='NA')
			b<-ddply(b,.(),summarise,.id='source',target=target,value=value/sum(value))
			b$source<-'Post-Screen'
			colnames(b)<-c('source','target','value')
		}
	else {
		b<-ddply(a,.(),summarise,.id='source',target=target,value=value/sum(value))
		b$source<-'Inquiry'
		c<-subset(site_type_data(),site_type_data()$site_name=='Post-Screen'&site_type_data()$target!='NA')
		c<-ddply(c,.(),summarise,.id='source',target=target,value=value/sum(value))
		c$source<-'Post-Screen'
		b<-rbind(b,c)
		colnames(b)<-c('target','source','value')
		b<-b[order(-b$value),]
	}
}
b$value<-round(100*b$value,2)

return(b)
})
#output$pie<-renderGvis({
#	gvisPieChart(data(),labelvar="target",numvar="value")
#	})
output$sankey <-  renderChart2({  
	a<-subset(data(),data()$value>=1)
    sankeyPlot <- rCharts$new()
    sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey")
    sankeyPlot$set(
      data = a,
      nodeWidth = 25,
      nodePadding = 15,
      layout = 31,
      width = 1250,
      height = 1250
    )
    return(sankeyPlot)
  })
output$test<-renderDataTable({
	a<-data()
	
	return(a)

})

})
