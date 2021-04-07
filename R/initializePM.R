#' Initialize a PM server
#'
#' use the initializePM() fuction to initiate the Server
#' @export
initializePM<-function(){
#在本地路径下搭建数据库
  #setwd(path)
  Project_data=ProjectData
  User_data=userData
  #数据库路径
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
  #创建用户群
  user_list<-data.frame(row_id = character(),
                        UserName=character(),
                        Password=character(),
                        Group=character(),
                        Phone=character(),
                        Mail=character(),
                        stringsAsFactors = FALSE)
  #写到sql中
  DBI::dbWriteTable(pool, "userList", user_list, overwrite = T)
  DBI::dbWriteTable(pool, "userList", User_data, overwrite = F,append=T)

  #写入Project Data
  responses_df <- data.frame(row_id = character(),
                             ProjectID = character(),
                             Description = character(),
                             SourceFrom = character(),
                             Participant = character(),
                             Status=character(),
                             dateStart = as.Date(character()),
                             dateEnd = as.Date(character()),
                             stringsAsFactors = FALSE)
  DBI::dbWriteTable(pool, "responses_df", responses_df, overwrite =T)
  DBI::dbWriteTable(pool, "responses_df", Project_data, overwrite =F,append=T)
  pool::poolClose(pool)
}
