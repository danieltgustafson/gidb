
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RMySQL)
library(rCharts)
library(plyr)

getConnection <- function(group) {

  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',
    #host='54.69.26.113', port=3306)
    host='localhost', port=3306)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',
    #host='54.69.26.113', port=3306)
    host='localhost', port=3306)
  }

  return(.connection)
}
shinyServer(function(input, output) {
 
cpi_data<-reactive({
  dbGetQuery(getConnection(),paste("
      select a.site_name,b.type,max(inquiries) inquiries, max(cost) cost, max(referrals) referrals, 
      sum(if(lower(status)='randomized',1,0)) as rands,round(max(cost)/max(inquiries),2) as cpi, 
      round(max(cost)/max(referrals),2)as cpref,
      round(max(cost)/sum(if(lower(status)='randomized',1,0)),2) as cprand,
      sum(if(lower(status)='randomized',1,0))/max(inquiries) as inq_2_rand,max(referrals)/max(inquiries) as inq_2_ref,
      sum(if(lower(status)='randomized',1,0))/max(referrals) as ref_2_rand
      from gidb.endo1 a 
      join gidb.dim_media_type b on a.media_type = b.detail
      join
      (select sum(inquiries) inquiries, sum(cost) cost, sum(referrals) referrals, type,site_id from gidb.media 
        where type<>'Web' and start_date between '",paste(as.character(input$dateRange), collapse = "' and '"),"'
      group by type,site_id) c
      on b.type = c.type and a.site_id = c.site_id
      where `Pre-Screen Date` between '",paste(as.character(input$dateRange), collapse = "' and '"),"'
      group by a.site_name,b.type",sep=""))

})
#output$types<-renderUI({

  #if(input$selected){
   #   checkboxGroupInput('types','Select media types',
   #                      choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'),
   #                      selected=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'))
  #}
 # else
  #{
   #     checkboxGroupInput('types','Select media types',
    #                     choices=c('Facebook','TV','Radio','Print','Google/YMSN','Transit'))
  #    }
  
#})

data_lim<-reactive({

if(input$best){
  	a<-ddply(cpi_data(), .(site_name), function(x) x[which.min(x[,input$measure]),])
}
else{
  	a<-subset(cpi_data(),cpi_data()$type %in% c(input$types))
  }
  	b<-a[!is.na(a[,input$measure]),]
  	b$site_num<-substring(b$site_name,1,4)
    return(b)

})
selector<-reactive({
  #paste(input$measures_two)
  sum<-ddply(cpi_data(),.(site_name),summarize,dat=sum(inquiries))
  sum<-sum[order(sum$dat),]
  return(sum)
})
output$bars<-renderChart({
 
  h<-hPlot(dat~site_name,data=selector(),type='bar')
  return(h)
  })


output$heatmap<-renderChart({
p <- rPlot(type ~ site_num, data = data_lim(), color=paste("'",input$measure,"'",sep=""),
           type = 'tile'
              ,tooltip = paste("#!function(item){ return item.site_name + item.type + ' CP: ' + item.",
                input$measure," +' Count: ' + item.",
              switch(input$measure,'cpi'='inquiries','cprand'='rands','cpref'='referrals'),"}!#",sep=""))
p$guides(x=list(numticks=length(unique(data_lim()$site_num)),labels=data_lim()$site_num))
p$guides(y=list(numticks=length(input$types),labels=unique(data_lim()$type)))
p$addParams(width = 1000, height = 700, dom = 'heatmap',title =
	paste("Cost Per ",switch(input$measure,'cpi'='Inquiry','cprand'='Randomization','cpref'='Referral')))
return(p)
})

output$table<-renderDataTable({
  if(input$total=='all'){
    data_lim()[,-length(data_lim())]
  }
  else if(input$total == 'media'){
    ddply(data_lim(),.(data_lim()$type),summarize,inquiries=sum(inquiries),cost=sum(cost),referrals=sum(referrals),
      rands=sum(rands),cpi=round(sum(cost)/sum(inquiries),2),cpref=round(sum(cost)/sum(referrals),2),cprand=round(sum(cost)/sum(rands),2),
      inq_2_rand=paste(round(100*sum(rands)/sum(inquiries),2),'%',sep=''),inq_2_ref=paste(round(100*sum(referrals)/sum(inquiries),2),'%',sep=''),
      ref_2_rand=paste(round(100*sum(rands)/sum(referrals),2),'%',sep=''))
  }
  else if(input$total == 'site'){
    ddply(data_lim(),.(data_lim()$site_name),summarize,inquiries=sum(inquiries),cost=sum(cost),referrals=sum(referrals),
      rands=sum(rands),cpi=round(sum(cost)/sum(inquiries),2),cpref=round(sum(cost)/sum(referrals),2),cprand=round(sum(cost)/sum(rands),2),
      inq_2_rand=paste(round(100*sum(rands)/sum(inquiries),2),'%',sep=''),inq_2_ref=paste(round(100*sum(referrals)/sum(inquiries),2),'%',sep=''),
      ref_2_rand=paste(round(100*sum(rands)/sum(referrals),2),'%',sep=''))
  }
  else{
    ddply(data_lim(),.(),summarize,inquiries=sum(inquiries),cost=sum(cost),referrals=sum(referrals),
      rands=sum(rands),cpi=round(sum(cost)/sum(inquiries),2),cpref=round(sum(cost)/sum(referrals),2),cprand=round(sum(cost)/sum(rands),2),
      inq_2_rand=paste(round(100*sum(rands)/sum(inquiries),2),'%',sep=''),inq_2_ref=paste(round(100*sum(referrals)/sum(inquiries),2),'%',sep=''),
      ref_2_rand=paste(round(100*sum(rands)/sum(referrals),2),'%',sep=''))
  }
})
output$downloadData <- downloadHandler(
    filename = function() { paste('cpi_data.csv', sep='') },
    content = function(file) {
      write.csv(data_lim(), file)
  }    
)
})



