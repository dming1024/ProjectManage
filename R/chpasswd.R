#' Change users password for PMshiny
#'
#' use the chpasswd() fuction to change user's password
#' @param username username for password to chage
#' @param newpasswd newpassword
#' @export
chpasswd<-function(username,newpasswd){
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
  newpasswd=sapply(newpasswd, sodium::password_store)
  DBI::dbExecute(pool, sprintf('UPDATE "userList" SET "Password" = ? WHERE "UserName" = ("%s")', username),
                 param = list(newpasswd)
                 )
  pool::poolClose(pool)


}


