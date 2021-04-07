#' Add users for PMshiny
#'
#' use the AddUsers() fuction to add users in bulk
#' @param userlist a excel file,including:UserName,Password,Group,Phone and Mail
#' @param initiate default is T if you want to add users, provide dataFrame and set this param to True.
#' @export
AddUsers<-function(initiate=T,userlist){
  #初始化
  #match.arg()
  if(initiate){
    #创建用户群
    user_list<-data.frame(row_id = UUIDgenerate(),
                          UserName=character(),
                          Password=character(),
                          Group=character(),
                          Phone=character(),
                          Mail=character())
    #写到sql中
    database=system.file("database", "db.sqlite", package = "ProjectManage")
    pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
    DBI::dbWriteTable(pool, "userList", user_list, overwrite = T)

    #初始化
    credentials = data.frame(
      UserName = c("admin", "user01","user02"),
      Password   = sapply(c("admin", "user01","user02"),sodium::password_store),
      Group  = c("admin", "normal","viewer"),
      Phone=c("12345678","78612534","0000000"),
      Mail=c("qq@126.com","myuser@qq.com","youremail@email.com"),
      stringsAsFactors = F
    )
    DBI::dbWriteTable(pool, "userList", credentials, overwrite = F,append=T)
    pool::poolClose(pool)

  }else{
    #match.arg(Group,
    #          choices = c("normal","viewer"))
    #增加新用户群
    userlist=readxl::read_excel(userlist)
    b=uuid::UUIDgenerate(n=nrow(userlist))
    b=data.frame(row_id=b)
    userlist=cbind(b,userlist)
    names(userlist)=c("row_id","UserName","Password","Group","Phone","Mail")
    userlist$Password=sapply(as.character(userlist$Password),sodium::password_store)
    #写到sql中
    database=system.file("database", "db.sqlite", package = "ProjectManage")
    pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
    DBI::dbWriteTable(pool, "userList", userlist, overwrite = F, append = T)
    pool::poolClose(pool)
  }

}
