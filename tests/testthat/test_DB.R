# Test DB

setwd("../../")
source("db.R")

addTeamGoal("Test Team Goal","Testing Team Goal","KPI Name")
listTeamGoals()
updateTeamGoal(max(listTeamGoals()$goal_id),title = "Updated Team Goal")
listTeamGoals()
deleteTeamGoal(max(listTeamGoals()$goal_id))


addProject("Test Project","some description","Some Category","Some Subcategory",6,1)
listProjects()
updateProject(max(listProjects()$project_id),project = "Updated Test Project")
listProjects()
deleteProject(max(listProjects()$project_id))

addTask("Sample Task","Description","Mike B.",Sys.Date(),Sys.Date()+7,progress = 0.00,priority = 1,project_id = 1)
listTasks()
updateTask(max(listTasks()$task_id),title = "Sample Task Updated")
listTasks()
deleteTask(max(listTasks()$task_id))

