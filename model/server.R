

library(shiny)
library(RMySQL)
library(rCharts)
library(reshape2)
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

time2rand<-reactive({
	dbGetQuery(getConnection(),"select site_name,datediff(Randomization,`Pre-Screen Date` ) as diff,
		sum(if(lower(status)='randomized',1,0)) as rands
		from gidb.endo1 
		where  datediff(Randomization,`Pre-Screen Date` ) is not null and  datediff(Randomization,`Pre-Screen Date` ) >0
		group by site_name,datediff(Randomization,`Pre-Screen Date` )")
	})
full_time<-reactive({
	a<-dbGetQuery(getConnection(),"select round(avg(datediff(Randomization,`Pre-Screen Date` )),0) as diff,
		sum(if(lower(status)='randomized',1,0)) as rands,concat(year(`Pre-Screen Date`),'-',
		if(month(`Pre-Screen Date`)<10,'0',''),month(`Pre-Screen Date`),'-','01') date
		from gidb.endo1 
		where  datediff(Randomization,`Pre-Screen Date` ) is not null and  datediff(Randomization,`Pre-Screen Date` ) >0
		group by date")
	a$unix<-as.numeric(as.POSIXct(a$date))*1000
	a[order(a$unix),]
	return(a)
	})

  
 
cpi_data<-reactive({

	dbGetQuery(getConnection(),paste("
		select if(max(inquiries)>0,max(inquiries),NULL) inquiries, if(max(cost)>0,max(cost),NULL) cost, max(referrals) referrals, sum(if(lower(status)='randomized',1,0)) as rands,
		b.type,a.site_name, sum(if(lower(status)='randomized',1,0))>0,max(cost)/sum(if(lower(status)='randomized',1,0)) as cprand,
		max(cost)/max(inquiries) as cpi, max(cost)/max(referrals)as cpref
		from gidb.endo1 a 
		join gidb.dim_media_type b on a.media_type = b.detail
		join
		(select sum(inquiries) inquiries, sum(cost) cost, sum(referrals) referrals, type,site_id from gidb.media where start_date>'2014-01-01' and
			start_date<'",input$max_date,"'
		group by type,site_id) c
		on b.type = c.type and a.site_id = c.site_id
		where `Pre-Screen Date` <'",input$max_date,"' 
		group by a.site_name,b.type
	",sep=""))

})
sig_sites<-reactive({
	rand_mod<-glm(cpi_data()$rands~cpi_data()$type+cpi_data()$site_name+log(cpi_data()$inquiries),family='poisson')
	a<-substring(rownames(subset(summary(rand_mod)$coeff,
		grepl('site',paste(rownames(summary(rand_mod)$coeff)))&summary(rand_mod)$coeff[,4]<=.1)),21)
	return(a)
	})

output$select_site<-renderUI({
	selectInput("sites","Significant Sites",c("Avg.",sig_sites()))
	})

site_coeff<-reactive({
	rand_mod<-glm(cpi_data()$rands~cpi_data()$type+cpi_data()$site_name+log(cpi_data()$inquiries),family='poisson')
	site_coeff<-subset(summary(rand_mod)$coeff,grepl(input$sites,paste(rownames(summary(rand_mod)$coeff))))[1]
	if(is.na(site_coeff)){site_coeff<-0}
	return(site_coeff)
	})

inq_output<-reactive({

	rand_mod<-glm(cpi_data()$rands~cpi_data()$type+cpi_data()$site_name+log(cpi_data()$inquiries),family='poisson')
		
	fb_inq<-lm(subset(cpi_data(),cpi_data()$type=='Facebook')$inquiries~subset(cpi_data(),cpi_data()$type=='Facebook')$cost +0)
	gg_inq<-lm(subset(cpi_data(),cpi_data()$type=='Google/YMSN')$inquiries~subset(cpi_data(),cpi_data()$type=='Google/YMSN')$cost +0)
	tv_inq<-lm(subset(cpi_data(),cpi_data()$type=='TV')$inquiries~subset(cpi_data(),cpi_data()$type=='TV')$cost +0)
	tr_inq<-lm(subset(cpi_data(),cpi_data()$type=='Transit')$inquiries~subset(cpi_data(),cpi_data()$type=='Transit')$cost +0)
	ra_inq<-lm(subset(cpi_data(),cpi_data()$type=='Radio')$inquiries~subset(cpi_data(),cpi_data()$type=='Radio')$cost +0)
	pr_inq<-lm(subset(cpi_data(),cpi_data()$type=='Print')$inquiries~subset(cpi_data(),cpi_data()$type=='Print')$cost +0)




a<-as.data.frame(rbind(cbind('Facebook',fb_inq$coeff[[1]],summary(fb_inq)$coefficients[, 2],
		exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_fb*fb_inq$coeff[[1]])),
		exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_fb*fb_inq$coeff[[1]])-summary(rand_mod)$coefficients[1,2]),
		exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_fb*fb_inq$coeff[[1]])+summary(rand_mod)$coefficients[1,2]),
		input$spend_fb),
		cbind('Google/YMSN',gg_inq$coeff[[1]],summary(gg_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]-summary(rand_mod)$coefficients[2,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]+summary(rand_mod)$coefficients[2,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
			input$spend_gg),
		cbind('Google/YMSN',gg_inq$coeff[[1]],summary(gg_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]-summary(rand_mod)$coefficients[2,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeGoogle/YMSN']]+summary(rand_mod)$coefficients[2,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_gg*gg_inq$coeff[[1]])),
			input$spend_gg),
		cbind('Print',tv_inq$coeff[[1]],summary(pr_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typePrint']]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_pr*pr_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typePrint']]-summary(rand_mod)$coefficients[3,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_pr*pr_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typePrint']]+summary(rand_mod)$coefficients[3,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_pr*pr_inq$coeff[[1]])),
				input$spend_pr),
		cbind('Radio',ra_inq$coeff[[1]],summary(ra_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeRadio']]+
				rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_ra*ra_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeRadio']]-summary(rand_mod)$coefficients[4,2]+
					rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_ra*ra_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeRadio']]+summary(rand_mod)$coefficients[4,2]+
					rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_ra*ra_inq$coeff[[1]])),
				input$spend_ra),
		cbind('Transit',tr_inq$coeff[[1]],summary(tr_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTransit']]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_tr*tr_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTransit']]-summary(rand_mod)$coefficients[5,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_tr*tr_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTransit']]+summary(rand_mod)$coefficients[5,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_tr*tr_inq$coeff[[1]])),
				input$spend_tr),
		cbind('TV',tv_inq$coeff[[1]],summary(tv_inq)$coefficients[, 2],
			exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTV']]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*
				log(input$spend_tv*tv_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTV']]-summary(rand_mod)$coefficients[6,2]+
					rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_tv*tv_inq$coeff[[1]])),
				exp(rand_mod$coeff[[1]]+site_coeff()+rand_mod$coeff[['cpi_data()$typeTV']]+summary(rand_mod)$coefficients[6,2]+rand_mod$coeff['log(cpi_data()$inquiries)'][[1]]*log(input$spend_tv*tv_inq$coeff[[1]])),
				input$spend_tv)
),stringsAsFactors=FALSE)


colnames(a)<-c('type','spend_coef','spend_se','rand_exp','rand_low','rand_high','spend')
	a[2:7]<-lapply(a[2:7],as.numeric)
	a<-a[c(1,2,3,5,4,6,7)]
	return(a)

})



data<-reactive({
	if(input$inqs){
		data<-inq_output()
		data$inq_low<-data$spend*(data$spend_coef-data$spend_se)
		data$inq_exp<-data$spend*data$spend_coef
		data$inq_high<-data$spend*(data$spend_coef+data$spend_se)
		data<-melt(data[,c('type','inq_low','inq_exp','inq_high')],id='type')
	}
	else{
		data<-melt(inq_output()[-(2:3)][-(5)],id='type')
	}
		data$value<-round(data$value,0)
	return(data)
})

output$bar<-renderChart({
		a<-nPlot(value~type,group='variable',data=data(),type='multiBarChart',dom='bar')
		a$set(width = 1000, height = 500)
	return(a)
	
})
output$box<-renderChart({
	if(input$all){
		h3=Highcharts$new()
		h3$series(
			data=toJSONArray2(full_time()[,c('unix','diff')],names=F,json=F),
			type='line',
			color='blue',
			name='Time 2 Randomization')
		#h3$tooltip(useHTML = T, formatter = "#! function() { return this.name + this.y; } !#")
		h3$series(
		    data=toJSONArray2(full_time()[,c('unix','rands')],names=F,json=F),
		    type='column',
		    color =' rgba(255,0,0,0.10)',
		    name='Randomizations',
		    yAxis=1
    	)
    	h3$tooltip(useHTML = T, formatter = "#! function() { return this.series.name + ': ' + this.y; } !#")
		h3$xAxis(type='datetime', title = list(text = "Month"),labels=list(rotation = -90,align='right'))
		h3$yAxis(
		    list(
		        list(
		            title = list(text = 'Inq. to Randomization'),min=0
		        ),
		        list(
		            title = list(text = '# Randomizations'), min=0,
		            opposite =TRUE
		        )
		    )
		)
		h3$addParams(dom='box')
		h3$set(width = 1000, height = 600)
	return(h3)

}
else
{
	bwstats = setNames(as.data.frame(boxplot(diff ~ site_name, data = time2rand(), plot = F)$stats[,1:23]), nm = NULL)
	h2 = Highcharts$new()
	h2$set(series = list(list(name = "Days to randomization distribution", data = bwstats)))
	h2$series(
	    data=toJSONArray2(ddply(time2rand(),.(site_name),summarize,value=sum(rands)),names=F,json=F),
	    type='scatter',
	    color =' rgba(255,0,0,0.5)',
	    name='Randomizations',
	    yAxis=1
    )
    #h2$tooltip(useHTML=T,formatter="#!function(){ return 'Randomizations: ' + this.y}!#")
  
	h2$xAxis(categories = levels(factor(time2rand()$site_name)), title = list(text = "Site Name"),labels=list(rotation = -90,align='right'))
	h2$yAxis(
    list(
        list(
            title = list(text = 'Inq. to Randomization'),min=0
        ),
        list(
            title = list(text = '# Randomizations'), min=0,
            opposite =TRUE
        )
    )
)
	h2$chart(type = "boxplot")
	h2$addParams(dom='box')
	h2$set(width = 1000, height = 600)
return(h2)
}
	})
})






