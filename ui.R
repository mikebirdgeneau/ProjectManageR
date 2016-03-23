
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#library(shinyBS)

shinyUI(
  navbarPage(title = appTitle,
             theme = "css/lumen.min.css", 
             fluid = TRUE, collapsible = TRUE,
             tabPanel("Overview", icon = icon("star"),
                      sidebarPanel(
                        h4("Filters"),
                        dateRangeInput("wpGanttDateRange","Date Filter",start=Sys.Date()-31,end=Sys.Date()+365,weekstart = 0,separator = "to")
                        ,width=3),
                      mainPanel(
                        uiOutput("wpGanttChart")
                        ,width=9)
             ),
             tabPanel("Project Schedules", icon = icon("calendar"),
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
                               # Delete Task
                               bsModal("deleteTaskModal", "Delete Task", "deleteTask", size = "large",list(
                                 fluidRow(column(12,
                                                 uiOutput("deleteTaskModelSelect"),
                                                 bsButton("goDeleteTask",type = "action", label = "Delete Selected Task",icon = icon("trash"))
                                 ))
                               )),
                               # Confirm Delete Task
                               bsModal("confirmDeleteTaskModal","Confirm Task Deletion", "confirmDeleteTaskModalTrigger", size = "small", list(
                                 fluidRow(column(12,
                                                 bsButton("confirmDeleteTask", type = "action", label = "Confirm Deletion", icon=icon("trash"), style = "danger"),
                                                 bsButton("cancelDeleteTask", type = "action", label = "Cancel", icon = icon("undo"))
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
                                 ),
                                 column(3,
                                        actionLink("deleteTask", label = "Delete Task", icon("trash")))
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
             tabPanel("Tasks", icon = icon("tasks"),
                      sidebarPanel(
                        h4("Filters"),
                        uiOutput("taskFilters")
                        ,width=3),
                      mainPanel(
                        p("Task List")
                        ,width = 9)
                      
                      
             ),
             tabPanel("Editor", icon=icon("pencil"),
                      tabsetPanel(id="editor_tabs",
                                  tabPanel(title="Projects",
                                           h4("Project Editor"),
                                           rHandsontableOutput("hot_projects")
                                  ),
                                  tabPanel(title="Tasks",
                                           h4("Task Editor"),
                                           rHandsontableOutput("hot_tasks")
                                  ),
                                  tabPanel(title="Team Goals",
                                           h4("Team Goal Editor"),
                                           rHandsontableOutput("hot_goals")
                                  )
                      )
             ),
             #tabPanel("Goals",
             #          p("Team Goals ... tasks to link to these!")),
             windowTitle = "Project ManageR"
  )
)

