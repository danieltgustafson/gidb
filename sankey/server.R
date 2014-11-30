
library(shiny)
library(RMySQL)
library(rCharts)
library(plyr)

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
 
  
 
data_full<-dbGetQuery(getConnection(),"
		select type source, case when  a.status = 'Failed Pre-Screen' then dq_reason else 'Post-Screen' end as target, count(distinct patient_id) value from gidb.endo1 a
		join gidb.`dim_media_type` b on a.media_type = b.detail
		left join gidb.`dim_dq_type` c on a.`Pre-Referral DQ` = c.dq_reason and a.status = 'Failed Pre-Screen'

		group by source, target

		union
		select 'Post-Screen' as source, case when  a.status like 'Excluded%' then dq_reason else 'Site-Screen' end as target, count(distinct patient_id) value from gidb.endo1 a
		join gidb.`dim_media_type` b on a.media_type = b.detail
		left join gidb.`dim_dq_type` c on a.`Post-Referral DQ` = c.dq_reason
		where a.status <> 'Failed Pre-Screen'
		group by source, target

		union 
		select 'Site-Screen' as source, if(status = 'Randomized', status, if(el_param is not null and el_param <> '',el_param, 'Unknown')) as target, count(distinct concat(initials,screen)) value from gidb.eligible_track a
		join gidb.endo1 b on concat(substring(a.`Initials`,1,1),substring(a.initials,3,1)) = substring(b.patient_id,1,2)
		and a.site=b.site_id and if(a.Randomized='Yes',1,0) =if(b.status='Randomized',1,0)
		where b.status not like 'Excluded%' and b.status <> 'Failed Pre-Screen'
		group by source, target
		")



data<-reactive({

	a<-subset(data_full,data_full$source %in% input$types)
	if(input$media_group=='types'){
		b<-ddply(a,~source,summarise,target=target,value=value/sum(value))}
	else if(input$media_group=='group')
	{
		b<-ddply(a,~target,summarise,source='Inquiry',value=sum(value))
}
	else if(input$media_group=='post')
		{
			b<-subset(data_full,data_full$source=='Post-Screen'&data_full$target!='NA')
			colnames(b)<-c('source','target','value')
		}
	else {
		b<-ddply(a,~target,summarise,source='Inquiry',value=sum(value))
		b<-rbind(b,subset(data_full,data_full$source=='Post-Screen'&data_full$target!='NA'))
		colnames(b)<-c('target','source','value')
		b<-b[order(-b$value),]
	}

return(b)
})

output$sankey <-  renderChart2({  
    sankeyPlot <- rCharts$new()
    sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey")

    sankeyPlot$set(
      data = data(),
      nodeWidth = 25,
      nodePadding = 15,
      layout = 31,
      width = 1250,
      height = 1250
    )
    return(sankeyPlot)
  })
})