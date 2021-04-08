#' Import Data List into PMshiny
#'
#' use the AddData() fuction to import the Project Data List
#' @import uuid
#' @param dataList dataFrame or NULL
#' @param overwrite default is False. if you want to overwite previous Data, set this param to True.
#' @export
AddData<-function(dataList,overwrite=F){
  if(overwrite){
    database=system.file("database", "db.sqlite", package = "ProjectManage")
    pool <<- pool::dbPool(RSQLite::SQLite(), dbname = database)
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
    df2=as.data.frame(dataList)
    df2$dateEnd=as.character(df2$dateEnd)
    df2$dateStart=as.character(df2$dateStart)
    b=uuid::UUIDgenerate(n=nrow(df2))
    b=data.frame(row_id=b)
    df3=cbind(b,df2)
    DBI::dbWriteTable(pool, "responses_df", df3, overwrite =F,append=T)
    pool::poolClose(pool)
  }else{
    database=system.file("database", "db.sqlite", package = "ProjectManage")
    pool <<- pool::dbPool(RSQLite::SQLite(), dbname = database)
    df2=as.data.frame(dataList)
    df2$dateEnd=as.character(df2$dateEnd)
    df2$dateStart=as.character(df2$dateStart)
    b=uuid::UUIDgenerate(n=nrow(df2))
    b=data.frame(row_id=b)
    df3=cbind(b,df2)
    pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
    DBI::dbWriteTable(pool, "responses_df", df3, overwrite =F,append=T)
    pool::poolClose(pool)
  }
}
