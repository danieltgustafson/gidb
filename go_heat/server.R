
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RMySQL)
library(rCharts)

shinyServer(function(input, output) {
  con=dbConnect(MySQL(),username='dgustafson',password='c3808v4m',host='54.69.26.113', port=3306)
  
  
 
cpi_data<-dbGetQuery(con,"
select max(inquiries) inquiries, max(cost) cost, max(referrals) referrals, sum(if(lower(status)='randomized',1,0)) as rands,
b.type,a.site_name, if(sum(if(lower(status)='randomized',1,0))>0,max(cost)/sum(if(lower(status)='randomized',1,0)),-999) as cprand,
max(cost)/max(inquiries) as cpi, max(cost)/max(referrals)as cpref
from gidb.endo1 a 
join gidb.dim_media_type b on a.media_type = b.detail
join
(select sum(inquiries) inquiries, sum(cost) cost, sum(referrals) referrals, type,site_id from gidb.media where start_date>'2014-01-01'
group by type,site_id) c
on b.type = c.type and a.site_id = c.site_id
group by a.site_name,b.type
")

data_lim<-reactive({
  subset(cpi_data,cpi_data$type %in% c(input$types))
})
output$heatmap<-renderChart({
p <- rPlot(type ~ site_name, data = data_lim(), color=paste("'",input$measure,"'",sep=""),
           type = 'tile'
              ,tooltip = paste("#!function(item){ return item.site_name + item.type + ' CP: ' + item.",input$measure," +' Count: ' + item.",
              switch(input$measure,'cpi'='inquiries','cpirand'='rands','cpref'='referrals'),"}!#",sep=""))
p$guides("{color: {scale: {type: gradient, lower: white, upper: steelblue}}}")
p$addParams(width = 1000, height = 600, dom = 'heatmap',title =
	paste("Cost Per ",switch(input$measure,'cpi'='Inquiry','cpirand'='Randomization','cpref'='Referral')))
return(p)
})

})



