
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
	,host='54.149.77.253', port=5432
	)
	
  
  
 
data<-reactive({
	a<-dbGetQuery(con,"select concat(date_part('year',b.happened_at),'-', date_part('month',b.happened_at)) month_id,
concat(date_part('year',b.happened_at),'-',date_part('quarter',b.happened_at)) quarter_id, case when (date(a.first_model) - date(b.happened_at))<0 then 0
else (date(a.first_model) - date(b.happened_at)) end as date_gap,split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1) org2,
count(distinct b.users) users from

(
select jsonb_extract_path_text(tags,'org_name') as org,jsonb_extract_path_text(tags,'org_id') as org_id,
jsonb_extract_path_text(tags,'user_email') users,min(happened_at) first_model
from metrics where name = 'model.images.count'
and jsonb_extract_path_text(tags,'org_name') not in ('Homeowner') and
jsonb_extract_path_text(tags,'org_name') not like 'RbA%'
and jsonb_extract_path_text(tags,'org_name')  not like 'Valspar%' and jsonb_extract_path_text(tags,'org_name')  
not like 'Renewal%' and lower(jsonb_extract_path_text(tags,'org_name'))  not like 'hover%'
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
not like 'Renewal%' and lower(split_part(regexp_replace(b.org, '[^a-zA-Z0-9>]+','','g'),'>',1)) not like 'hover%'
and b.happened_at>='2015-04-01'
group by quarter_id,month_id,date_gap,org2
")
return(a)
})
retention<-reactive({
	a<-dbGetQuery(con,"
		DROP SEQUENCE IF EXISTS serial;
		CREATE SEQUENCE serial start 1;

		SELECT a.diff,is_pro,b.reg_month,100*cast(count(distinct (
		case when maxdiff>=diff then b.email else null end)) as float)/max(c.total_reg) reten_rate
		FROM 
			(
				SELECT nextval('serial')-1 as diff 
				FROM user_metrics limit 100) a
			CROSS JOIN (
				SELECT max(date(happened_at) - date(reg)) maxdiff,a.email,
				date_part('year',min(reg))*100+date_part('month',min(reg)) as reg_month,is_pro
				FROM(
					SELECT min(happened_at) as reg, email 
					FROM user_metrics 
					GROUP by email) a 
					JOIN user_metrics b ON a.email=b.email
					JOIN 
					( 
					SELECT jsonb_extract_path_text(tags,'user_email') email, 
					jsonb_extract_path_text(tags,'user_pro') is_pro
					FROM metrics where name = 'user.created') pros on a.email=pros.email
			GROUP BY  a.email,is_pro) b 
			JOIN
			(
				SELECT date_part('year',reg)*100+date_part('month',reg) as reg_month,
				count(distinct email) as total_reg 
				FROM (
					SELECT min(happened_at) reg, email
					FROM user_metrics 
					GROUP BY email) a
				GROUP BY reg_month
			) c ON b.reg_month=c.reg_month
			where b.reg_month>=201504 
			GROUP BY b.reg_month,is_pro,a.diff")
return(a)
})

pro_reten<-reactive({
	b<-subset(retention(),retention()$is_pro=='true')
	b$reten_rate<-round(b$reten_rate,2)
	return(b)
	})
free_reten<-reactive({
	b<-subset(retention(),retention()$is_pro=='false')
	b$reten_rate<-round(b$reten_rate,2)
	return(b)
	})

summary<-reactive({
	a<-data()
	b<-data.table(ddply(a,.(date_gap),summarize,counts=sum(users)))
	b$pct=round(100*b[,cum:=cumsum(counts)]$cum/max(b$cum),2)
	return(subset(b,!is.na(date_gap)))
})

single_org<-reactive({
	a<-data()
	b<-data.table(ddply(subset(a,a$org2==input$orgs),.(date_gap),summarize,counts=sum(users)))
	b$pct=round(100*b[,cum:=cumsum(counts)]$cum/max(b$cum),2)
	b<-subset(b,is.na(date_gap)==FALSE)
	return(subset(b,!is.na(date_gap)))
})

time_groups<-reactive({

	a<-data()
	
	if(input$agg=='monthly')
	{
		b<-data.table(ddply(a,.(month_id,date_gap),summarize,counts=sum(users)))
		b[,cum:=cumsum(counts),by=month_id]
		b$pct=round(100*(b[,maxi:=max(cum),by=month_id]$cum/b$maxi),2)
	}
	else
	{
		b<-data.table(ddply(a,.(quarter_id,date_gap),summarize,counts=sum(users)))
		b[,cum:=cumsum(counts),by=quarter_id]
		b$pct=round(100*(b[,maxi:=max(cum),by=quarter_id]$cum/b$maxi),2)
	}
	return(subset(b,!is.na(date_gap)))
})

output$ui_orgs<-renderUI({
	a<-data()
	selectInput('orgs',"Select Org",choices=c(paste(unique(a$org2))),selected='Norandex')
})

output$test<-renderDataTable({
	if(input$pro=='pro'){
		pro_reten()
	}
	else{
		free_reten()
	}
	})
output$usage<-renderChart({
      
       #	theGraph <- hPlot(pct~date_gap,data=summary(),type='line',name='All Pro Avg.')
		theGraph<-Highcharts$new()
		theGraph$yAxis(title = list(text = 'Cum. % Users'),min=0)
		theGraph$series(
		 data = toJSONArray2(summary()[,c('date_gap','pct'),with=FALSE],names = F, json = F),
		 name = "All Pro Avg",
		 type = "line",
		 color= "#ca0707"
		)
		theGraph$series(
		   data = toJSONArray2(single_org()[,c('date_gap','pct'),with=FALSE],names = F, json = F),
		   name = paste(input$orgs),
		   type = "line"
		)
        theGraph$addParams(width = 1000, height = 700,dom='usage')
       # theGraph$xAxis(type='category',name='Days from Reg.')
        #theGraph$chart(zoomType = "xy")
        return(theGraph)
      
  })
output$cohorts<-renderChart({
	if(input$agg=='monthly'){
		graph2<-hPlot(pct~date_gap,data=time_groups(),type='line',group='month_id')
	}
	else{
		graph2<-hPlot(pct~date_gap,data=time_groups(),type='line',group='quarter_id')
	}
		graph2$addParams(dom='cohorts')
		graph2$yAxis(title = list(text = 'Cum. % Users'),min=0)
	return(graph2)
	})
output$retent<-renderChart({
	if(input$pro=='pro'){
		graph2<-hPlot(reten_rate~diff,data=pro_reten(),type='line',group='reg_month')
	}
	else{
		graph2<-hPlot(reten_rate~diff,data=free_reten(),type='line',group='reg_month')
	}
		graph2$addParams(dom='retent')
		graph2$yAxis(title = list(text = 'Cum. % Users'),min=0)
	return(graph2)
	})
 
})



