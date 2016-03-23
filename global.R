
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
