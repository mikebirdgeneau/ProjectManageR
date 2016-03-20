
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyBS)

shinyUI(
  navbarPage(title = "Project ManageR",
             theme = "css/lumen.min.css", 
             fluid = TRUE, collapsible = TRUE,
             tabPanel("Project ManageR",
                      sidebarPanel(
                        h4("Filters"),
                        dateRangeInput("wpGanttDateRange","Date Filter",start=Sys.Date()-31,end=Sys.Date()+365,weekstart = 0,separator = "to")
                        ,width=3),
                      mainPanel(
                        uiOutput("wpGanttChart")
                        ,width=9)
             ),
             tabPanel("Project Schedules",
                      fluidRow(
                        column(6,
                               h4("Project Schedules")
                        ),
                        column(6,
                               # Add Project
                               bsModal("addProjectModal", "Add New Project", "addProject", size = "large",list(
                                fluidRow(column(12,
                                                textInput("addProject_project",label = "Project Name:",placeholder = "Project Name"),
                                                textInput("addProject_description",label = "Description:",placeholder = "Project Description"),
                                                textInput("addProject_category",label = "Project Category:",placeholder = "Project Category"),
                                                textInput("addProject_subcategory",label = "Project Subcategory:",placeholder = "Project Subcategory"),
                                                selectizeInput(inputId = "addProject_goal",label="Supports Team Goal:",choices = listTeamGoals()$title,width = "100%"),
                                                selectInput("addProject_priority",label = "Priority:",choices = priorities),
                                                bsButton("goAddProject",type = "action", label = "Add Project",icon = icon("save"))
                                ))
                               )),
                               # Add Task
                               bsModal("addTaskModal", "Add Task to Project", "addTaskToProject", size = "large",list(
                                 fluidRow(column(12,
                                                 uiOutput("addTaskModalUI"),
                                                 bsButton("goAddTask",type = "action", label = "Add Task",icon = icon("save"))
                                 ))
                               )),
                               # Edit Task
                               bsModal("editTaskModal", "Edit Task", "editTask", size = "large",list(
                                 fluidRow(column(12,
                                                 uiOutput("editTaskModelSelect"),
                                                 uiOutput("editTaskModalUI"),
                                                 bsButton("goEditTask",type = "action", label = "Apply Changes",icon = icon("save"))
                                 ))
                               )),
                               fluidRow(
                                 column(3,
                                        actionLink("addProject",label = "Add Project",icon = icon("plus"))
                                 ),
                                 column(3,
                                        actionLink("addTaskToProject",label = "Add Task",icon = icon("plus"))
                                 ),
                                 column(3,
                                        actionLink("editTask",label = "Edit Task",icon = icon("pencil"))
                                 )
                               )
                                        
                        )),
                      sidebarPanel(
                        h4("Filters"),
                        uiOutput("projectFilter"),
                        uiOutput("projectDateRange")
                        ,width=3),
                      mainPanel(
                        uiOutput("projGanttChart")
                        ,width=9)
             ),
             tabPanel("Tasks",
                      sidebarPanel(
                        h4("Filters")
                      ,width=3),
                      mainPanel(
                        p("Task List")
                        ,width = 9)
             ),
             #tabPanel("Goals",
            #          p("Team Goals ... tasks to link to these!")),
             windowTitle = "Project ManageR"
  )
)
