#' Delete users for PMshiny
#'
#' use the deleteUser() fuction to initiate the Server
#' @param username user's name in database
#' @export
deleteUser<-function(username){
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
  DBI::dbExecute(pool, sprintf('DELETE FROM "userList" WHERE "UserName" = ("%s")', username))
  pool::poolClose(pool)
}
