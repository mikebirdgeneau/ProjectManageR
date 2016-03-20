
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)
library(ggplot2)
library(data.table)



shinyServer(function(input, output, session) {
  
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
    
    #str(input$wpGanttDateRange)
    
    p.schedule<-ggplot(data=plot.data,aes(x=desc,fill=as.factor(category),alpha=plot_priority))+geom_rect(data=dt.workdays[workday==0,],aes(x=NULL,ymin=dates-0.5,ymax=dates+0.5),xmin=0,xmax=nrow(plot.data)+1,fill="darkgrey",alpha=0.3)+
      #geom_rect(aes(x=desc, ymin=original.plan.start-0.25,ymax=original.plan.Finish+0.25,xmin=id-0.4,xmax=id+0.4),fill="grey",colour=NA,alpha=0.6)+
      geom_rect(aes(x=desc, ymin=start-0.25,ymax=finish+0.25,xmin=id-0.4,xmax=id+0.4),colour="black")+
      #geom_rect(aes(x=desc, ymin=actual.start-0.25,ymax=actual.Finish+0.25,xmin=id-0.1,xmax=id+0.1),colour="black",fill="black",alpha=1.0)+
      coord_flip(ylim=c(input$wpGanttDateRange[1]-31,input$wpGanttDateRange[2]+31))+theme_bw()+scale_alpha_manual(name="Priority",values=c("Business Critical" = 1.0,"Important" = 0.75,"Wait-List" = 0.5,"On-Hold"=0.25))+geom_text(aes(y=start-0.25,x=desc,label=format(start,format="%m/%d"),hjust=1.1,size=10))+geom_text(aes(y=max.date,x=desc,label=paste0(format(max.date,format="%m/%d")),hjust=-0.1,size=10))+xlab("")+ylab("")+scale_size_continuous(guide=FALSE)+geom_hline(yintercept=as.numeric(Sys.Date()),lty=2)+geom_hline(yintercept=as.numeric(as.Date("2016-01-01")),lwd=0.8,lty=2)+scale_y_date(breaks=date_breaks(width="1 month"),minor_breaks=date_breaks(width="1 day"),labels=date_format("%m/%Y"))+theme(legend.position=c("bottom"))+ylab(NULL)+scale_fill_discrete(name="Category")
    
    return(list(plot = p.schedule,rows = nrow(plot.data)))
  }
  
  output$workplanGantt <- renderPlot({
    listProjects()
    result <- generateWorkplanGantt()
    result$plot
  })
  
  output$wpGanttChart <- renderUI({
    result <- generateWorkplanGantt()
    isolate({
    list(plotOutput("workplanGantt", height = min(c(max(c(250,result$rows*50)),800))))
    })
  })
  
  generateProjectGantt <- function(this.project){
    require(ggplot2)
    require(scales)
    
    if(missing(this.project)){
      return(NULL)
    }
    
    # Get Data from Database
    thisProject_id <- listProjects()[project==this.project,]$project_id
    message(thisProject_id)
    dt_tasks <- listTasks()[project_id == thisProject_id,]
    
    # Merge data to generate Project Start / End Dates
    dt_tasks[,start:=as.Date(start),]
    dt_tasks[,finish:=as.Date(finish),]
    
    plot.data <- copy(dt_tasks)
    
    str(plot.data)
    
    plot.data[,date.avg:=(as.numeric(start)*2+as.numeric(finish))/3,]
    setkeyv(plot.data,c("date.avg","finish"))
    plot.data<-plot.data[order(-rank(start),-rank(date.avg))]
    plot.data[,id:=seq_along(title),]
    plot.data[,desc:=factor(title,levels=unique(plot.data$title)),]
    plot.data[,max.date:=finish,by=c("id")]
    
    plot.data[,plot_priority := factor(priority,levels = c(1,2,3,4),labels = c("Business Critical","Important","Wait-List","On-Hold"))]
    
    dt.workdays<-data.table(dates=seq(as.Date("2015-12-01"),as.Date("2018-01-01"),by=1))
    dt.workdays[,dow:=wday(dates),]
    dt.workdays[,workday:=1,]
    dt.workdays[dow %in% c(1,7),workday:=0,]
    dt.workdays[dates %in% as.Date(c("2016-01-01","2016-02-15","2016-03-25","2016-03-28","2016-05-23","2016-07-01","2016-07-08","2016-08-01","2016-09-05","2016-10-10","2016-11-11","2016-12-26","2016-12-27")),workday:=0]
    
    #str(input$wpGanttDateRange)
    
    p.schedule<-ggplot(data=plot.data,aes(x=desc,fill=as.factor(assigned),alpha=plot_priority))+geom_rect(data=dt.workdays[workday==0,],aes(x=NULL,ymin=dates-0.5,ymax=dates+0.5),xmin=0,xmax=nrow(plot.data)+1,fill="darkgrey",alpha=0.3)+
      #geom_rect(aes(x=desc, ymin=original.plan.start-0.25,ymax=original.plan.Finish+0.25,xmin=id-0.4,xmax=id+0.4),fill="grey",colour=NA,alpha=0.6)+
      geom_rect(aes(x=desc, ymin=start-0.25,ymax=finish+0.25,xmin=id-0.4,xmax=id+0.4),colour="black")+
      #geom_rect(aes(x=desc, ymin=actual.start-0.25,ymax=actual.Finish+0.25,xmin=id-0.1,xmax=id+0.1),colour="black",fill="black",alpha=1.0)+
      coord_flip(ylim=c(input$projectDateRange[1]-7,input$projectDateRange[2]+7))+theme_bw()+scale_alpha_manual(name="Priority",values=c("Business Critical" = 1.0,"Important" = 0.75,"Wait-List" = 0.5,"On-Hold"=0.25))+geom_text(aes(y=start-0.25,x=desc,label=format(start,format="%m/%d"),hjust=1.1,size=10))+geom_text(aes(y=max.date,x=desc,label=paste0(format(max.date,format="%m/%d")),hjust=-0.1,size=10))+xlab("")+ylab("")+scale_size_continuous(guide=FALSE)+geom_hline(yintercept=as.numeric(Sys.Date()),lty=2)+geom_hline(yintercept=as.numeric(as.Date("2016-01-01")),lwd=0.8,lty=2)+scale_y_date(breaks=date_breaks(width="1 month"),minor_breaks=date_breaks(width="1 day"),labels=date_format("%m/%Y"))+theme(legend.position=c("bottom"))+ylab(NULL)+scale_fill_discrete(name="Category")+ggtitle(input$selectedProject)
    
    return(list(plot = p.schedule,rows = nrow(plot.data)))
  }

  output$projectGantt <- renderPlot({
    listProjects()
    listTasks()
    result <- generateProjectGantt(this.project = input$selectedProject)
    result$plot
  })
  
  output$projGanttChart <- renderUI({
    result <- generateProjectGantt(this.project = input$selectedProject)
    isolate({
      list(plotOutput("projectGantt", height = min(c(max(c(250,result$rows*50)),800))))
    })
  })
  
  observeEvent(input$goAddProject, {
      addProject(project = input$addProject_project,
                 description = input$addProject_description,
                 category = input$addProject_category, 
                 subcategory = input$addProject_subcategory,
                 goal_id = (listTeamGoals()[title==input$addProject_goal]$goal_id),
                 priority = input$addProject_priority)
    toggleModal(session = session,modalId = "addProjectModal",toggle = "close")
    message("Done Adding Project")
  })
  
  observeEvent(input$goAddTask, {
    addTask(title = input$addTask_title,
            description = input$addTask_description,
            assigned = input$addTask_assigned, 
            start = input$addTask_range[1],
            finish = input$addTask_range[2], 
            progress = 0.0,
            url = input$addTask_url,
            project_id = listProjects()[project==input$selectedProject,]$project_id,
            priority = input$addTask_priority)
    toggleModal(session = session,modalId = "addTaskModal",toggle = "close")
    message("Done Adding Task")
  })
  
  observeEvent(input$goEditTask, {
    updateTask(id = listTasks()[project_id==listProjects()[project==input$selectedProject,]$project_id & title == input$selectedTask,]$task_id,
            title = input$editTask_title,
            description = input$editTask_description,
            assigned = input$editTask_assigned, 
            start = input$editTask_range[1],
            finish = input$editTask_range[2], 
            progress = 0.0,
            url = input$editTask_url,
            project_id = listProjects()[project==input$selectedProject,]$project_id,
            priority = input$editTask_priority)
    toggleModal(session = session,modalId = "editTaskModal",toggle = "close")
    message("Done Editing Task")
  })
  
  output$editTaskModelSelect <- renderUI({
    selectizeInput(inputId = "selectedTask",label="Select Task",choices = listTasks()[project_id==listProjects()[project==input$selectedProject,]$project_id,]$title)
  })
  
  output$addTaskModalUI <- renderUI({
    input$selectedTask
    list(
    p(paste0("Add Task to Project ",input$selectedProject,":")),
    textInput("addTask_title",label = "Task:",placeholder = "Task Name"),
    textInput("addTask_description",label = "Description:",placeholder = "Project Description"),
    textInput("addTask_assigned",label = "Assigned To:",placeholder = "Responsible Person"),
    dateRangeInput("addTask_range",start = Sys.Date(), end = Sys.Date()+31,label = "Start Date:"),
    textInput(inputId = "addTask_url",label="URL (if applicable):",placeholder = "Github, Jive, etc.",width = "100%"),
    selectInput("addTask_priority",label = "Priority:",choices = priorities)
    )
  })
  output$editTaskModalUI <- renderUI({
    input$selectedTask
    thisTask <- listTasks()[project_id==listProjects()[project==input$selectedProject,]$project_id & title == input$selectedTask,]
    
    list(
      hr(),
      p(paste0("Edit Task: ",input$selectedTask,":")),
      textInput("editTask_title",label = "Task:",value = thisTask$title),
      textInput("editTask_description",label = "Description:",value = thisTask$description),
      textInput("editTask_assigned",label = "Assigned To:", thisTask$assigned),
      dateRangeInput("editTask_range",start = thisTask$start, end = thisTask$finish,label = "Start Date:"),
      textInput(inputId = "editTask_url",label="URL (if applicable):",value = thisTask$url,width = "100%"),
      selectInput("editTask_priority",label = "Priority:",choices = priorities,selected = thisTask$priority)
    )
  })
  
  output$projectFilter <- renderUI({
    input$goAddProject
    listProjects()
    isolate({
      selectizeInput("selectedProject","Selected Project:",choices = listProjects()$project)
    })
  })
  
  output$projectDateRange <- renderUI({
    input$selectedProject
    isolate({
      dt_tasks <- listTasks()[project_id == listProjects()[project==input$selectedProject,]$project_id,]
      dt_tasks[,start:=as.Date(start),]
      dt_tasks[,finish:=as.Date(finish),]
      dt_tasks<-dt_tasks[,list(start=min(start),finish=max(finish)),by=c("project_id")]
      
      list(
        dateRangeInput("projectDateRange",label = "Date Range",start = dt_tasks$start,end = dt_tasks$finish,weekstart = 0)
      )
      
    })
  })
  
})
