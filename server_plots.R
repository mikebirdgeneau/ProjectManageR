
# Plot Functions ----------------------------------------------------------

generateWorkplanGantt <- function(){
  require(ggplot2)
  require(scales)
  
  # Get Data from Database
  dt_projects <- listProjects()
  dt_tasks <- listTasks()
  
  # Merge data to generate Project Start / End Dates
  temp <- merge(dt_projects,dt_tasks,by=c("project_id"))
  temp[,start:=as.Date(start),]
  temp[,finish:=as.Date(finish),]
  
  plot.data <- merge(dt_projects,temp[,list(start = min(start,na.rm=TRUE),finish = max(finish,na.rm=TRUE)),by=c("project_id")],by=c("project_id"))

  plot.data[,date.avg:=(as.numeric(start)*2+as.numeric(finish))/3,]
  setkeyv(plot.data,c("date.avg","finish"))
  plot.data<-plot.data[order(-rank(start),-rank(date.avg))]
  plot.data[,id:=seq_along(project),]
  plot.data[,desc:=factor(project,levels=unique(plot.data$project)),]
  plot.data[,max.date:=finish,by=c("id")]
  
  plot.data[,plot_priority := factor(priority,levels = c(1,2,3,4),labels = c("Business Critical","Important","Wait-List","On-Hold"))]
  
  dt.workdays<-data.table(dates=seq(as.Date("2015-12-01"),as.Date("2018-01-01"),by=1))
  dt.workdays[,dow:=wday(dates),]
  dt.workdays[,workday:=1,]
  dt.workdays[dow %in% c(1,7),workday:=0,]
  dt.workdays[dates %in% as.Date(c("2016-01-01","2016-02-15","2016-03-25","2016-03-28","2016-05-23","2016-07-01","2016-07-08","2016-08-01","2016-09-05","2016-10-10","2016-11-11","2016-12-26","2016-12-27")),workday:=0]
  
  str(input$wpGanttDateRange)
  
  temp <<- plot.data
  
  p.schedule<-ggplot(data=plot.data,aes(x=desc,fill=as.factor(category),alpha=plot_priority))+geom_rect(data=dt.workdays[workday==0,],aes(x=NULL,ymin=dates-0.5,ymax=dates+0.5),xmin=0,xmax=nrow(plot.data)+1,fill="darkgrey",alpha=0.3)+
    #geom_rect(aes(x=desc, ymin=original.plan.start-0.25,ymax=original.plan.Finish+0.25,xmin=id-0.4,xmax=id+0.4),fill="grey",colour=NA,alpha=0.6)+
    geom_rect(aes(x=desc, ymin=start-0.25,ymax=finish+0.25,xmin=id-0.4,xmax=id+0.4),colour="black")+
    #geom_rect(aes(x=desc, ymin=actual.start-0.25,ymax=actual.Finish+0.25,xmin=id-0.1,xmax=id+0.1),colour="black",fill="black",alpha=1.0)+
    coord_flip(ylim=input$wpGanttDateRange)+theme_bw()+scale_alpha_manual(name="Priority",values=c("Business Critical" = 1.0,"Important" = 0.75,"Wait-List" = 0.5,"On-Hold"=0.25))+geom_text(aes(y=start-0.25,x=desc,label=format(start,format="%m/%d"),hjust=1.1,size=10))+geom_text(aes(y=max.date,x=desc,label=paste0(format(max.date,format="%m/%d")),hjust=-0.1,size=10))+xlab("")+ylab("")+scale_size_continuous(guide=FALSE)+geom_hline(yintercept=as.numeric(Sys.Date()),lty=2)+geom_hline(yintercept=as.numeric(as.Date("2016-01-01")),lwd=0.8,lty=2)+scale_y_date(breaks=date_breaks(width="1 month"),minor_breaks=date_breaks(width="1 day"),labels=date_format("%m/%Y"))+theme(legend.position=c("bottom"))+ylab(NULL)+scale_fill_discrete(name="Category")
  
  return(list(plot = p.schedule,rows = nrow(plot.data)))
}
