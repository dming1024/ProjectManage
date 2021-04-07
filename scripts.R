#编辑用户信息
entry_user <- function(button_id){

  shiny::showModal(
    shiny::modalDialog(
      shiny::div(id=("entry_user"),
                 shiny::tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
                 shiny::tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
                 shiny::fluidPage(
                   shiny::fluidRow(
                     shiny::splitLayout(
                       cellWidths = c("250px", "100px"),
                       cellArgs = list(style = "vertical-align: top"),
                       textInput("Phone", labelMandatory("Phone"), placeholder = "")
                     ),
                     #sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
                     shiny::textInput("Mail", "Mail"),
                     #shiny::splitLayout(
                     #   cellWidths = c("170px", "170px"),
                     #   cellArgs = list(style = "vertical-align: top"),
                     #   textInput("SourceFrom", labelMandatory("SourceFrom"), placeholder = ""),
                     #   textInput("Participant", labelMandatory("Participant"), placeholder = "")
                     # ),
                     # shiny::dateInput("dateEnd","End Date:"),
                     # shiny::helpText(labelMandatory(""), paste("Mandatory field.")),
                     shiny::actionButton(button_id, "SubmitUser")
                   ),
                   easyClose = TRUE
                 )
      )
    )
  )
}

#更新后台user
shiny::observeEvent(input$edit_user, priority = 20,{

  #展示新的修改结果
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <<- pool::dbPool(RSQLite::SQLite(), dbname = database)
  SQL_df <- DBI::dbReadTable(pool, "userList")
  pool::poolClose(pool)

  shiny::showModal(
    if(length(input$userinfo_list_rows_selected) > 1 ){
      shiny::modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$userinfo_list_rows_selected) < 1){
      shiny::modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })

  if(length(input$userinfo_list_rows_selected) == 1 ){
    entry_user("SubmitUser")
    shiny::updateTextInput(session, "Phone", value = SQL_df[input$userinfo_list_rows_selected, "Phone"])
    shiny::updateTextInput(session, "Mail", value = SQL_df[input$userinfo_list_rows_selected, "Mail"])

  }
})

#更新sql数据库
shiny::observeEvent(input$SubmitUser, priority = 20, {
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
  SQL_df <- DBI::dbReadTable(pool, "userList")

  row_selection <- SQL_df[input$userinfo_list_rows_selected, "row_id"]
  DBI::dbExecute(pool, sprintf('UPDATE "userList" SET "Phone" = ?, "Mail" = ? WHERE "row_id" = ("%s")', row_selection),
                 param = list(as.character(input$Phone),
                              as.character(input$Mail)))
  shiny::removeModal()
  pool::poolClose(pool)
})





#从数据库中删除数据，根据response_table_rows_selected
deleteUsers <- shiny::reactive({
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = database)
  SQL_df <- DBI::dbReadTable(pool, "userList")
  row_selection <- SQL_df[input$userinfo_list_rows_selected, "row_id"]

  quary <- lapply(row_selection, function(nr){
    DBI::dbExecute(pool, sprintf('DELETE FROM "userList" WHERE "row_id" == ("%s")', nr))
  })
  pool::poolClose(pool)
})

#将delet button事件与deleteData函数连接
shiny::observeEvent(input$delete_user, priority = 20,{

  if(length(input$userinfo_list_rows_selected)>=1 ){
    deleteUsers()
  }

  shiny::showModal(

    if(length(input$userinfo_list_rows_selected) < 1 ){
      shiny::modalDialog(
        title = "Warning",
        paste("Please select row(s)." ),easyClose = TRUE
      )
    })
})


#add
##########

#从数据库读取数据
appendUser <- function(data){
  database=system.file("database", "db.sqlite", package = "ProjectManage")
  pool <- pool::dbPool(RSQLite::SQLite(), dbname =database)
  quary <- DBI::sqlAppendTable(pool, "userList", data, row.names = FALSE)
  DBI::dbExecute(pool, quary)
  pool::poolClose(pool)
}

#如果有add_button时间，唤醒entry_form表格
shiny::observeEvent(input$add_user, priority = 20,{

  entry_add("submitAdd")

})


entry_add<-function(button_id){

  shiny::showModal(
    shiny::modalDialog(
      shiny::div(id=("entry_add"),
                 shiny::tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
                 shiny::tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
                 shiny::fluidPage(
                   shiny::fluidRow(
                     shiny::splitLayout(
                       cellWidths = c("250px", "100px"),
                       cellArgs = list(style = "vertical-align: top"),
                       textInput("UserName", labelMandatory("UserName"), placeholder = ""),
                       selectInput("Group", labelMandatory("Group"), multiple = FALSE, choices = c("normal", "viewer"),selected = "viewer")
                     ),
                     #sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
                     #shiny::textAreaInput("Description", "Description", placeholder = "项目描述", height = 100, width = "354px"),
                     shiny::splitLayout(
                       cellWidths = c("170px", "170px"),
                       cellArgs = list(style = "vertical-align: top"),
                       textInput("Password", labelMandatory("Password"), placeholder = ""),
                       textInput("Phone", labelMandatory("Phone"), placeholder = ""),
                       textInput("Mail", labelMandatory("Mail"), placeholder = "")
                     ),
                     #shiny::dateInput("dateEnd","End Date:"),
                     #shiny::helpText(labelMandatory(""), paste("Mandatory field.")),
                     shiny::actionButton(button_id, "Submit")
                   ),
                   easyClose = TRUE
                 )
      )
    )
  )
}


formUser <- shiny::reactive({

  formData <- data.frame(row_id = UUIDgenerate(),
                         UserName = input$UserName,
                         Password = input$Password,
                         Group = input$Group,
                         Phone = input$Phone,
                         Mail = input$Mail,
                         stringsAsFactors = FALSE)
  return(formData)
})

#若果有submit事件，调取appendData，同时remove/初始化 表格数据
shiny::observeEvent(input$submitAdd, priority = 20,{
  appendUser(formUser())
  shinyjs::reset("entry_add")
  shiny::removeModal()

})

