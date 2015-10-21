
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RPostgreSQL)
library(rCharts)
library(plyr)
library(data.table)


shinyServer(function(input, output) {
  


con=dbConnect(PostgreSQL(),user='hoveruser',password='',dbname='hover'
	#,host='54.149.77.253', port=5432)
	)
  
  
 
data<-reactive({
	a<-dbGetQuery(con,"select case when (date(a.first_model) - date(b.happened_at))<0 then 0
else (date(a.first_model) - date(b.happened_at)) end as date_gap,split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) org2,
count(distinct b.users) users from

(
select jsonb_extract_path_text(tags,'org_name') as org,jsonb_extract_path_text(tags,'org_id') as org_id,
jsonb_extract_path_text(tags,'user_email') users,min(happened_at) first_model
from metrics where name = 'model.images.count'
and jsonb_extract_path_text(tags,'org_name') not in ('Homeowner') and
jsonb_extract_path_text(tags,'org_name') not like 'RbA%'
and jsonb_extract_path_text(tags,'org_name')  not like 'Valspar%' and jsonb_extract_path_text(tags,'org_name')  
not like 'Renewal by%' and lower(jsonb_extract_path_text(tags,'org_name'))  not like 'hover%'
group by org,users,org_id) a
right join 
(select min(happened_at) happened_at,jsonb_extract_path_text(tags,'user_email') users,
substring(unnest(string_to_array(jsonb_extract_path_text(tags,'org_ids'),','))FROM '[0-9]+') as org_id,
jsonb_extract_path_text(tags,'org_names') as org
from metrics  where name = 'user.created' 
group by users,org,org_id) b
on a.users=b.users and a.org_id = b.org_id
where 
split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) not in ('Homeowner') and
split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) not like 'RbA%'
and split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) not like 'Valspar%' 
and split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) 
not like 'Renewal by%' and lower(split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1)) not like 'hover%'
group by date_gap,org2")
return(a)
})

summary<-reactive({
	a<-data()
	b<-data.table(ddply(a,.(date_gap),summarize,counts=sum(users)))
	b$pct=round(100*b[,cum:=cumsum(counts)]$cum/max(b$cum),2)
	return(b)
})

single_org<-reactive({
	a<-data()
	b<-data.table(subset(a,a$org2==input$orgs))
	b$pct=round(100*b[,cum:=cumsum(users)]$cum/max(b$cum),2)
	b<-subset(b,is.na(date_gap)==FALSE)
	return(b)
})

output$ui_orgs<-renderUI({
	a<-data()
	selectInput('orgs',"Select Org",choices=c(paste(unique(a$org2))),selected='Norandex')
})

output$test<-renderDataTable({
	single_org()[,c('date_gap','pct'),with=FALSE]
	})
output$usage<-renderChart({
      
       	theGraph <- hPlot(pct~date_gap,data=summary(),type='line',name='All Pro Avg.')
		theGraph$yAxis(title = list(text = 'Cum. % Users'),min=0)
		#theGraph$series(
		 #   data = toJSONArray2(summary()[,c('date_gap','pct'),with=FALSE],names = F, json = F),
		  #  name = "All Pro Avg",
		   # type = "line",
		    #color= "#ca0707"
		#)
		theGraph$series(
		   data = toJSONArray2(single_org()[,c('date_gap','pct'),with=FALSE],names = F, json = F),
		   name = input$orgs,
		   type = "line"
		)
        theGraph$addParams(width = 1000, height = 700,dom='usage')
       # theGraph$xAxis(type='category',name='Days from Reg.')
        #theGraph$chart(zoomType = "xy")
        return(theGraph)
      
  })
 
})



