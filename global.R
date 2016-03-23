
library(shiny)
library(shinyBS)
library(data.table)
library(ggplot2)
library(rhandsontable)

# Global Variables & Initialization ---------------------------------------

source("db.R")

# Branding
appTitle = "Project Manager"

categoryName <- "Division"
subCategoryName <- "Asset"

# Priorties
priorities <- list("Business Critical" = 1,"Important" = 2,"Wait-Listed" = 3,"On-Hold" = 4)

# Mission-critical: Most deserving of precious resources at this time.
# Important: Will pursue now but with less emphasis than initiatives deemed mission-critical.
# Wait-listed: Will tackle as soon as resources are freed up from initiatives in categories 1 and 2.
# On hold: Will not undertake or plan for at this time.

tasksWithStatus <- function(){
  temp <- listTasks()
  temp[,targetProgress:=as.numeric(Sys.Date()-as.Date(start))/as.numeric(as.Date(finish)-as.Date(start)),]
  temp[Sys.Date()>as.Date(finish),targetProgress:=1,]
  temp[Sys.Date()<as.Date(start),targetProgress:=0,]
  
  temp[progress==1,Status:="Complete",]
  temp[progress==0,Status:="Not Started",]
  temp[(progress-targetProgress)<(-0.1),Status:="Behind Schedule",]
  temp[(progress-targetProgress)>(0.1),Status:="Ahead of Schedule",]
  temp[abs(progress-targetProgress)<(0.1) & progress>0,Status:="On Track",]
  return(temp)
}