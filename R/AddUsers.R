#' Add users for PMshiny
#'
#' use the AddUsers() fuction to initiate the Server
#' @import pool RSQLite DBI sodium readxl
#' @param userlist EXCEL or NULL
#' @param initiate default is False. if you want to add users, the uerlist is EXCEL file and set this param to True.
#' @export
AddUsers<-function(userlist,initiate=F){
  #初始化
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
  if(initiate){
    #创建用户群
    user_list<-data.frame(UserName=character(),
                          Password=character(),
                          Group=character(),
                          Phone=character(),
                          Mail=character())
    #写到sql中
    DBI::dbWriteTable(pool, "userList", user_list, overwrite = T)

    #初始化
    credentials = data.frame(
      UserName = c("admin", "user01"),
      Password   = sapply(c("admin", "user01"),sodium::password_store),
      Group  = c("admin", "normal"),
      Phone=c("12345678","78612534"),
      Mail=c("qq@126.com","myuser@qq.com"),
      stringsAsFactors = F
    )

    DBI::dbWriteTable(pool, "userList", credentials, overwrite = F,append=T)

  }else{
    #增加新用户群
    userlist$Group="normal"
    userlist$Password=sapply(userlist$Password,sodium::password_store)
    #写到sql中
    pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
    DBI::dbWriteTable(pool, "userList", userlist, overwrite = F, append = T)
  }
  pool::poolClose(pool)
}
