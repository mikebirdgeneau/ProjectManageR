
# Database Functions ------------------------------------------------------

library(RSQLite)

connectDB <- function(){
  con <- dbConnect(RSQLite::SQLite(shared.cache=TRUE),"db/workPlan.sqlite")
  return(con)
}

disconnectDB <- function(con){
  dbDisconnect(con)
}

addKeyResult <- function(key_result){
  con <- connectDB()
  dbGetQuery(con,"CREATE TABLE IF NOT EXISTS keyresults(keyresult_id INTEGER PRIMARY KEY AUTOINCREMENT, key_result TEXT)")
  dbGetQuery(con,paste0("INSERT INTO keyresults (`keyresult_id`,`key_result`) VALUES (NULL,'",key_result,"')"))
}

addTeamGoal <- function(title,description,key_result){
  con <- connectDB()
  dbGetQuery(con,"CREATE TABLE IF NOT EXISTS goals(goal_id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, description TEXT, key_result TEXT);")
  dbGetQuery(con,paste0("INSERT INTO goals (`goal_id`,`title`,`description`,`key_result`) VALUES (NULL,'",title,"','",description,"','",key_result,"')"))
  disconnectDB(con)
}

addProject <- function(project,description,category,subcategory,goal_id,priority){
  con <- connectDB()
  dbGetQuery(con,"CREATE TABLE IF NOT EXISTS projects(project_id INTEGER PRIMARY KEY AUTOINCREMENT, project TEXT, description TEXT, category TEXT, subcategory TEXT, goal_id INTEGER, priority INTEGER);")
  dbGetQuery(con,paste0("INSERT INTO projects(`project_id`,`project`,`description`,`category`,`subcategory`,`goal_id`,`priority`) VALUES (NULL,'",project,"','",description,"','",category,"','",subcategory,"','",goal_id,"','",priority,"')"))
  disconnectDB(con)
}

addTask <- function(title,description,assigned,start,finish,progress=0.00,url=NULL,priority,project_id){
  con <- connectDB()
  dbGetQuery(con,"CREATE TABLE IF NOT EXISTS tasks(task_id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, description TEXT, assigned TEXT, start TEXT, finish TEXT, progress REAL, url TEXT, priority INTEGER,  project_id INTEGER);")
  dbGetQuery(con,paste0("INSERT INTO tasks(`task_id`,`title`,`description`,`assigned`,`start`,`finish`,`progress`,`url`,`priority`,`project_id`) VALUES (NULL,'",title,"','",description,"','",assigned,"','",start,"','",finish,"','",progress,"','",url,"','",priority,"','",project_id,"')"))
  disconnectDB(con)
}

updateTeamGoal <- function(id,title,description,key_result){
  con <- connectDB()
  item <- dbGetQuery(con,paste0("SELECT * from goals WHERE goal_id = ",id))
  
  if(!missing(title)){item$title <- title}
  if(!missing(description)){item$description <- description}
  if(!missing(key_result)){item$key_result <- key_result}
  
  dbGetQuery(con,paste0("UPDATE goals SET `title` = '",item$title,"',`description` = '",item$description,"',`key_result` = '",item$key_result,"' WHERE `goal_id` =",id))
  disconnectDB(con)
}

updateProject <- function(id,project,description,category,subcategory,goal_id,priority){
  con <- connectDB()
  item <- dbGetQuery(con,paste0("SELECT * from projects WHERE project_id = ",id))
  
  if(!missing(project)){item$project <- project}
  if(!missing(description)){item$description <- description}
  if(!missing(category)){item$category <- category}
  if(!missing(subcategory)){item$subcategory <- subcategory}
  if(!missing(goal_id)){item$goal_id <- goal_id}
  if(!missing(priority)){item$priority <- priority}
  
  dbGetQuery(con,paste0("UPDATE projects SET `project` = '",item$project,"',`description` = '",item$description,"',`category` = '",item$category,"',`subcategory`= '",item$subcategory,"', `goal_id` = ",item$goal_id,",`priority`= ",item$priority," WHERE project_id = ",id))
  disconnectDB(con)
  rm(item)
}

updateTask <- function(id,title,description,assigned,start,finish,progress=0.00,url=NULL,priority,project_id){
  con <- connectDB()
  item <- dbGetQuery(con,paste0("SELECT * from tasks WHERE task_id = ",id))
  
  if(!missing(title)){item$title <- title}
  if(!missing(description)){item$description <- description}
  if(!missing(assigned)){item$assigned <- assigned}
  if(!missing(start)){item$start <- start}
  if(!missing(finish)){item$finish <- finish}
  if(!missing(progress)){item$progress <- progress}
  if(!missing(url)){item$url <- url}
  if(!missing(priority)){item$priority <- priority}
  if(!missing(project_id)){item$project_id <- project_id}
  
  dbGetQuery(con,paste0("UPDATE tasks SET `title`= '",item$title,"',`description`= '",item$description,"',`assigned`= '",item$assigned,"',`start`= '",item$start,"',`finish`= '",item$finish,"',`progress`= ",item$progress,",`url`= '",item$url,"',`priority`= ",item$priority,",`project_id`= ",item$project_id," WHERE task_id = ",id))
  
  disconnectDB(con)
  rm(item)
}

deleteTeamGoal <- function(id){
  con <- connectDB()
  dbGetQuery(con,paste0("DELETE FROM goals WHERE goal_id = ",id))
  disconnectDB(con)
}

deleteProject <- function(id){
  con <- connectDB()
  dbGetQuery(con,paste0("DELETE FROM projects WHERE project_id = ",id))
  disconnectDB(con)
}

deleteTask <- function(id){
  con <- connectDB()
  dbGetQuery(con,paste0("DELETE FROM tasks WHERE task_id = ",id))
  disconnectDB(con)
}

listKeyResults <- function(){
  con <- connectDB()
  result <- dbGetQuery(con,paste0("SELECT * FROM keyresults"))
  disconnectDB(con)
  return(result)
}

listTeamGoals <- function(){
  con <- connectDB()
  result <- data.table(dbGetQuery(con,paste0("SELECT * FROM goals")))
  disconnectDB(con)
  return(result)
}

listProjects <- function(){
  con <- connectDB()
  result <- data.table(dbGetQuery(con,paste0("SELECT * FROM projects")))
  disconnectDB(con)
  return(result)
  
}

listTasks <- function(){
  con <- connectDB()
  result <- data.table(dbGetQuery(con,paste0("SELECT * FROM tasks")))
  disconnectDB(con)
  return(result)
}

