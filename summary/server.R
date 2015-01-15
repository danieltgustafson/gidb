
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
    # host='54.69.26.113', port=3306)
    host='localhost', port=3306)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(),username='dgustafson',password='c3808v4m',
    #  host='54.69.26.113', port=3306)
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
      round(max(cost)/sum(if(lower(status)='randomized',1,0)),2) as cprand
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

output$text<-renderText({
  paste(input$measure)
  })

selector<-reactive({
  if(input$measure=='inquiries'){
    suma<-ddply(cpi_data(),.(site_name),summarize,dat=sum(inquiries),dat2=round(sum(rands)/sum(inquiries),4))
  }
  else if (input$measure=='rands'){
    suma<-ddply(cpi_data(),.(site_name),summarize,dat=sum(rands),dat2=round(sum(rands)/sum(referrals),2))
  }
  else {
    suma<-ddply(cpi_data(),.(site_name),summarize,dat=sum(referrals),dat2=round(sum(referrals)/sum(inquiries),2))
  }
    suma<-suma[order(suma$dat),]
    return(suma)
})
output$bars<-renderChart2({
 
  #h<-hPlot(dat~site_name,data=selector(),type='bar')
  h<-Highcharts$new()
  h$yAxis(
          list(
              list(
                  title = list(text = 'Count'),min=0
              ),
              list(
                  title = list(text = 'Rate'), min=0,
                  opposite =TRUE
              )
          )
      )
  h$series(
        data = toJSONArray2(selector()[,c('site_name','dat')],names = F, json = F),
        name = input$measure,
        type = "bar",
        color="#137df6"
    )
  #if(input$measure!='inquiries'){
  h$series(
        data = toJSONArray2(selector()[,c('site_name','dat2')],names = F, json = F),
        name = 'conversion',
        type = "bar",
        groupPadding=0,
        color="#0ac507",
        yAxis=1
    )
#}
  h$xAxis(type='category')
  h$addParams(dom='bars')
  h$set(width = 800, height = 800)
  
  return(h)
  })

})



