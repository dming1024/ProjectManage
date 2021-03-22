#' Initialize a PM server
#'
#' use the initializePM() fuction to initiate the example Server
#' @param path the work direction
#' @export
initializePM<-function(path){
#在本地路径下搭建数据库
  setwd(path)
  Project_data=ProjectData
  User_data=userData
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
  #创建用户群
  user_list<-data.frame(UserName=character(),
                        Password=character(),
                        Group=character(),
                        Phone=character(),
                        Mail=character())
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
