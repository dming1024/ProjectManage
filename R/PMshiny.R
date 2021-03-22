#' Project Manage Panle
#'
#' use the PMshiny() fuction to initiate the Server
#' @rawNamespace import(data.table,except=c(last,first,between))
#' @rawNamespace import(RSQLite,except=show)
#' @rawNamespace import(DT,except=c(dataTableOutput,renderDataTable))
#' @rawNamespace import(dqshiny,except=c(enable,toggle,hidden,disabled,hide,click,disable,show))
#' @rawNamespace import(shiny,except=runExample)
#' @rawNamespace import(pool,except=show)
#' @import shinydashboard shinyjs uuid dplyr ggplot2 scales sodium
#' @export
PMshiny<-function(){

  loginpage <<- shiny::div(
    id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    shiny::wellPanel(
      shiny::tags$h2("Welcome", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
      shiny::textInput("userName", placeholder="Username", label =  shiny::tagList(icon("user"), "Username")),
      shiny::passwordInput("passwd", placeholder="Password", label =  shiny::tagList(icon("unlock-alt"), "Password")),
      shiny::br(),
      shiny::div(
                       style = "text-align: center;",
                       shiny::actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                    padding: 10px 15px; width: 150px; cursor: pointer;
                                    font-size: 18px; font-weight: 600;"),
                       shinyjs::hidden(
                         shiny::div(id = "nomatch",
                                    shiny::tags$p("Oops! Incorrect username or password!",
                                    style = "color: red; font-weight: 600;
                                    padding-top: 5px;font-size:16px;",
                                    class = "text-center"))),
                       shiny::br(),
                       shiny::br()
      )))

  #必须要输入的项目
  labelMandatory <<- function(label) {
    shiny::tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  appCSS <<- ".mandatory_star { color: red; }"


  ui<-shiny::fluidPage(skin="green",
                #tags$header(header),
                shiny::tags$body(
                  # Note the wrapping of the string in HTML()
                  shiny::tags$style(HTML("
                                  @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
                                  body {
                                  background-color: #8DB6CD;
                                  color: #000000;
                                  }
                                  h2 {
                                  font-family: 'arial', sans-serif;
                                  }
                                  .main-header .logo {
                                  font-family: 'arial';
                                  font-weight: bold;
                                  font-size: 20px
                                  }

                                  .sidebar-menu li a {
                                  color:#FFFFFF;
                                  font-weight: bold;
                                  font-size:15px;
                                  font-family: 'arial';
                                  }

                                  .shiny-input-container {
                                  color: #474747;
                                  }"))
  ),
  shinyjs::useShinyjs(),
  shiny::uiOutput("Mainui"),
  #设置loging的快捷键
  shiny::tags$script(HTML("$(function(){
                   $(document).keyup(function(e) {
                   if (e.which == 13) {
                   $('#login').click()
                   }
                   });
})"))
)
  #pool <<- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
  UserInfo<-shiny::reactive({
    pool <<- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
    df=DBI::dbReadTable(pool,"userList")
    return(df)
    pool::poolClose(pool)
  })


  server<-function(input, output, session){

    login = FALSE
    group="normal"
    USER <- shiny::reactiveValues(login = login,group=group)
    Username <- shiny::reactive({
      if(USER$login == TRUE){
        #获取permission权限信息
        return(input$userName)
      }else{
        return("FALSE")
      }
    })

    shiny::observe({
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- shiny::isolate(input$userName)
            Password <- shiny::isolate(input$passwd)
            UserInfo=UserInfo()
            if(length(which(UserInfo$UserName==Username))==1) {
              pasmatch  <- UserInfo["Password"][which(UserInfo$UserName==Username),]
              pasverify <- sodium::password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
                USER$group<-UserInfo["Group"][which(UserInfo$UserName==Username),]
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
        }
      }
    })


    shiny::observe({
      if(USER$group=="admin"){
        output$Mainui<-shiny::renderUI({
          if (USER$login == TRUE ) {
            shiny::includeCSS(system.file("css", "packagestyle.css", package = "ProjectManage"))
            shinydashboard::dashboardPage(skin="green",
                                          shinydashboard::dashboardHeader(title = "Project Manage",
                                          tags$li(a(href ="javascript:window.location.reload(true)",
                                                    icon("unlock"),
                                                    style = "cursor: pointer;"),
                                                  class = "dropdown")
                          ),

                          shinydashboard::dashboardSidebar(
                            width = 175,
                            shinydashboard::sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              id = "tabs",
                              shinydashboard::menuItem("ProjectOverview", tabName = "dashboard", icon = icon("dashboard")),
                              shinydashboard::menuItem("ProjectDetail", icon = icon("th"), tabName = "widgets"),
                              shinydashboard::menuItem("UserInfo",icon = icon("address-book"), tabName = "userinfo")
                            )
                          ),
                          shinydashboard::dashboardBody(
                            # Boxes need to be put in a row (or column)
                            shinydashboard::tabItems(
                              shinydashboard::tabItem(tabName = "dashboard",
                                      shiny::fluidRow(
                                        #box(width = 12,actionButton("count","IncrementProcess")),
                                        shinydashboard::valueBoxOutput("progressBox",width = 6),
                                        shinydashboard::valueBoxOutput("approvalBox",width = 6),
                                        shiny::column(shiny::plotOutput("summary0"),width=10,offset = 1),
                                        #shiny::br(),
                                        shiny::column(shiny::plotOutput("summary1"),width=10,offset = 1),
                                        #shiny::br(),
                                        shiny::column(shiny::plotOutput("dailyProject"),width=10,offset = 1),
                                        #shiny::br(),
                                        shiny::column(shiny::plotOutput("feedback"),width=10,offset = 1)
                                        #shiny::br()
                                      )
                              ),
                              shinydashboard::tabItem(tabName = "widgets",
                                             shiny::fluidRow(
                                        shinyjs::useShinyjs(),
                                        shinyjs::inlineCSS(appCSS),
                                        shiny::column(DT::dataTableOutput("dataout"),width = 12,offset=0.5),
                                        shiny::column(shiny::actionButton("add_button", "Add", icon("plus")),
                                                      shiny::actionButton("edit_button", "Edit", icon("edit")),
                                                      shiny::actionButton("delete_button", "Delete", icon("trash-alt")),
                                                      shiny::actionButton("view_button","View",icon("info-circle")),
                                               width = 12,offset=0.5)
                                      )
                              ),
                              shinydashboard::tabItem(tabName = "userinfo",
                                             shiny::column(DT::dataTableOutput("userinfo_list"),width = 12,offset=0.5)
                              )
                            ))

            )
          } else {
            loginpage
          }
        })
      }
      })

    shiny::observe({
      if(USER$group=="normal"){
        output$Mainui<-shiny::renderUI({
          if (USER$login == TRUE ) {
            shiny::includeCSS(system.file("css", "packagestyle.css", package = "ProjectManage"))
            shinydashboard::dashboardPage(skin="green",
                                          shinydashboard::dashboardHeader(title = "Project Manage",
                                                                          tags$li(a(href ="javascript:window.location.reload(true)",
                                                                                    icon("unlock"),
                                                                                    style = "cursor: pointer;"),
                                                                                  class = "dropdown")
                                          ),

                                          shinydashboard::dashboardSidebar(
                                            width = 175,
                                            shinydashboard::sidebarMenu(
                                              # Setting id makes input$tabs give the tabName of currently-selected tab
                                              id = "tabs",
                                              shinydashboard::menuItem("ProjectOverview", tabName = "dashboard", icon = icon("dashboard")),
                                              shinydashboard::menuItem("ProjectDetail", icon = icon("th"), tabName = "widgets")
                                            )
                                          ),
                                          shinydashboard::dashboardBody(
                                            # Boxes need to be put in a row (or column)
                                            shinydashboard::tabItems(
                                              shinydashboard::tabItem(tabName = "dashboard",
                                                                      shiny::fluidRow(
                                                                        #box(width = 12,actionButton("count","IncrementProcess")),
                                                                        shinydashboard::valueBoxOutput("progressBox",width = 6),
                                                                        shinydashboard::valueBoxOutput("approvalBox",width = 6),
                                                                        shiny::column(shiny::plotOutput("summary0"),width=10,offset = 1),
                                                                        #shiny::br(),
                                                                        shiny::column(shiny::plotOutput("summary1"),width=10,offset = 1),
                                                                        #shiny::br(),
                                                                        shiny::column(shiny::plotOutput("dailyProject"),width=10,offset = 1),
                                                                        #shiny::br(),
                                                                        shiny::column(shiny::plotOutput("feedback"),width=10,offset = 1)
                                                                        #shiny::br()
                                                                      )
                                              ),
                                              shinydashboard::tabItem(tabName = "widgets",
                                                                      shiny::fluidRow(
                                                                        shinyjs::useShinyjs(),
                                                                        shinyjs::inlineCSS(appCSS),
                                                                        shiny::column(DT::dataTableOutput("dataout"),width = 12,offset=0.5),
                                                                        shiny::column(shiny::actionButton("add_button", "Add", icon("plus")),
                                                                                      shiny::actionButton("edit_button", "Edit", icon("edit")),
                                                                                      shiny::actionButton("view_button","View",icon("info-circle")),
                                                                                      width = 12,offset=0.5)
                                                                      )
                                              )
                                            ))

            )
          } else {
            loginpage
          }
        })
      }
    })

    #后台计算
    shiny::observe({
      if(USER$login == TRUE){

        responses_df <- shiny::reactive({
          input$submit
          input$submit_edit
          #input$copy_button
          input$delete_button
          #input$view_button
          #把数据responses_df读出来
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          df=RSQLite::dbReadTable(pool, "responses_df")
          return(df)
          pool::poolClose(pool)
        })

        output$summary0 <- shiny::renderPlot({
          table <- responses_df() %>% select(-row_id)
          p1=table %>% dplyr::filter(Status=="Processing") %>%  dplyr::count(SourceFrom) %>%
            dplyr::add_tally(n,name="total") %>%  dplyr::mutate(perc=n/total) %>%
            ggplot2::ggplot(aes(reorder(SourceFrom,-n),n,fill=SourceFrom))+
            ggplot2::geom_col()+ggplot2::geom_text(aes(label=sprintf("%s",n)),nudge_y = 0.02)+
            ggplot2::theme(legend.position = "none",axis.text.x=element_text(family="Arial",size=15,angle=30),
                                                                                  plot.title = element_text(family = "Arial",face="bold",size=20,hjust = 0.5))+
            ggplot2::labs(x="",y="",title="JOBS NEEDED TO DO")
          p1
        })

        output$summary1<-shiny::renderPlot({
          table <- responses_df() %>% dplyr::select(-row_id)
          p2=table %>% dplyr::filter(Status=="Done") %>% dplyr::count(SourceFrom) %>%
            dplyr::add_tally(n,name="total") %>%
            dplyr::mutate(perc=n/total) %>%
            ggplot2::ggplot(aes(reorder(SourceFrom,-n),n,fill=SourceFrom))+
            ggplot2::geom_col()+geom_text(aes(label=sprintf("%s",n)),nudge_y = 0.02)+
            ggplot2::theme(legend.position = "none",axis.text.x=element_text(family="Arial",size=15,angle=90,vjust = 0.2),
                                                                                  plot.title = element_text(family = "Arial",face="bold",size=20,hjust = 0.5))+
            ggplot2::labs(x="",y="",title="JOBS ARCHIVED")
          p2
        })

        output$dailyProject<-shiny::renderPlot({
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          df=RSQLite::dbReadTable(pool, "responses_df")
          pool::poolClose(pool)
          starDate_cut=cut.Date(as.Date(df$dateStart),breaks = "week")
          p= data.frame(week=starDate_cut)%>% dplyr::count(week) %>%
            dplyr::mutate(month=gsub("[0-9]{4}\\-([0-9]{2})\\-[0-9]{2}","\\1",week)) %>%
            dplyr::mutate(weeks=round(as.numeric(gsub("[0-9]{4}\\-[0-9]{2}\\-([0-9]{2})","\\1",week))/7)+1)  %>%
            dplyr::mutate(years=gsub("([0-9]{4})\\-.*","\\1",week)) %>%
            ggplot2::ggplot(aes(month, weeks, fill= n))+
            ggplot2::geom_tile(color= "white",size=3)+
            ggplot2::scale_fill_gradient(low="green", high="red") +
            ggplot2::facet_wrap(~years)+
            ggplot2::theme(panel.background = element_blank())+
            ggplot2:: theme(legend.position = "bottom",
                  plot.title = element_text(family = "Arial",face="bold",size=20,hjust = 0.5))+
            ggplot2::labs(x=" ", y=" ",fill="Project (N)",title="Capacity")+
            ggplot2::scale_x_discrete(labels = c("01"="January","02"="February","03"="March","04"="April",
                                        "05"="May","06"="June","07"="July","08"="August","09"="September",
                                        "10"="October","11"="November","12"="December"))+
            ggplot2::theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1,family="Times"))+
            ggplot2::scale_y_continuous(
              breaks = c(1:5),
              labels = c("1"="week 1","2"="week 2","3"="week 3",
                         "4"="week 4","5"="week 5")) +
            ggplot2::theme(
              text = element_text(size=15),
              plot.margin=unit(c(1.2,1,1.2,1),"cm")
            )
          p
        })

        output$feedback<-shiny::renderPlot({
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          df=RSQLite::dbReadTable(pool, "responses_df")
          pool::poolClose(pool)

          p=df %>% dplyr::select(c(dateStart,dateEnd)) %>% dplyr::mutate(dateStart=as.Date(dateStart)) %>%
            dplyr::mutate(dateEnd=as.Date(dateEnd)) %>%
            dplyr::mutate(FeedBack=dateEnd-dateStart)%>% dplyr::count(FeedBack) %>%
            ggplot2::ggplot(aes(x=FeedBack,y=n)) +
            ggplot2::geom_bar(stat = "identity",fill="lightblue")+
            ggplot2::xlim(c(-0.5,20))+ggplot2::labs(x="FeedBack (Days)",y="Projects (N)",title = "FeedBack Time")+
            ggplot2::theme(
              text = element_text(size=15),
              plot.margin=unit(c(1.2,1,1.2,1),"cm"),
              plot.title = element_text(family = "Arial",face="bold",size=20,hjust = 0.5),
              panel.background = element_blank()
            )
          p
        })


        #展示用户信息
        output$userinfo_list<- DT::renderDataTable({
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          user_list=RSQLite::dbReadTable(pool, "userList")[,c(1,3,4,5)]
          pool::poolClose(pool)
          DT::datatable(user_list,
                    callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                    rownames = FALSE,
                    selection = "none",
                    options = list(
                      class = "compact",
                      #colnames = c("",""),
                      #caption = tags$caption(myTitle, style = "color:black"),
                      dom = 't',
                      ordering = FALSE,
                      paging = FALSE,
                      searching = FALSE
                      #headerCallback = JS(headerCallback)

                    ))
        })

        output$progressBox<-shinydashboard::renderValueBox({
          table <- responses_df() %>% dplyr::select(-row_id)
          todo_jobs=nrow(table[table$Status=="Processing",])
          #total_jobs=nrow(table)
          shinydashboard::valueBox(
            paste0(todo_jobs," needed to do"),"Progressing",icon=icon("list"),
            color="purple"
          )
        })

        output$approvalBox<-shinydashboard::renderValueBox({
          table <- responses_df() %>% dplyr::select(-row_id)
          done_jobs=nrow(table[table$Status=="Done",])
          total_jobs=nrow(table)
          #finshedRates=round((done_jobs/total_jobs)*100,2)
          shinydashboard::valueBox(
            paste0(done_jobs," jobs Completed!"),'Archived',icon=icon("thumbs-up",lib="glyphicon"),
            color="yellow"
          )
        })

        fieldsMandatory <- c("name", "sex")

        shiny::observe({
          mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                     !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
          mandatoryFilled <- all(mandatoryFilled)

          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })

        #录入数据的对话框
        entry_form <- function(button_id){

          shiny::showModal(
            shiny::modalDialog(
              shiny::div(id=("entry_form"),
                         shiny::tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
                         shiny::tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
                         shiny::fluidPage(
                           shiny::fluidRow(
                             shiny::splitLayout(
                               cellWidths = c("250px", "100px"),
                               cellArgs = list(style = "vertical-align: top"),
                               textInput("ProjectID", labelMandatory("ProjectID"), placeholder = ""),
                               selectInput("Status", labelMandatory("Status"), multiple = FALSE, choices = c("Processing", "Done"),selected = "Processing")
                      ),
                      #sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
                      shiny::textAreaInput("Description", "Description", placeholder = "项目描述", height = 100, width = "354px"),
                      shiny::splitLayout(
                        cellWidths = c("170px", "170px"),
                        cellArgs = list(style = "vertical-align: top"),
                        textInput("SourceFrom", labelMandatory("SourceFrom"), placeholder = ""),
                        textInput("Participant", labelMandatory("Participant"), placeholder = "")
                      ),
                      shiny::dateInput("dateEnd","End Date:"),
                      shiny::helpText(labelMandatory(""), paste("Mandatory field.")),
                      shiny::actionButton(button_id, "Submit")
                    ),
                    easyClose = TRUE
                  )
              )
            )
          )
        }

        #获取输入的数据，同时自动产生row_id，universal unique identifier
        formData <- shiny::reactive({

          #如果还是Processing的状态，就不给出dateEnd，以...代替
          dateEnd<-shiny::reactive({
            if(input$Status=="Processing"){
              dateEnd="..."
            }else{
              dateEnd=as.character(input$dateEnd)
            }
          })

          formData <- data.frame(row_id = UUIDgenerate(),
                                 ProjectID = input$ProjectID,
                                 Description = input$Description,
                                 SourceFrom = input$SourceFrom,
                                 Participant = input$Participant,
                                 Status = input$Status,
                                 dateStart = as.character(Sys.Date()),
                                 dateEnd= dateEnd(),
                                 stringsAsFactors = FALSE)
          return(formData)
        })

        #从数据库读取数据
        appendData <- function(data){
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          quary <- DBI::sqlAppendTable(pool, "responses_df", data, row.names = FALSE)
          DBI::dbExecute(pool, quary)
          pool::poolClose(pool)
        }

        #如果有add_button时间，唤醒entry_form表格
        shiny::observeEvent(input$add_button, priority = 20,{

          entry_form("submit")

          inputStatus=shiny::reactive({
            if(shiny::isTruthy(input$Status)){
              if(input$Status=="Done")
              {
                inputStatus="T"
              }else{inputStatus="F"}
            }else{inputStatus="F"}
            return(inputStatus)
          })

          shiny::observe({
            shinyjs::hide("dateEnd")
            if((inputStatus())){
              shinyjs::show("dateEnd")
            }
          })


        })


        #若果有submit事件，调取appendData，同时remove/初始化 表格数据
        shiny::observeEvent(input$submit, priority = 20,{

          appendData(formData())
          shinyjs::reset("entry_form")
          shiny::removeModal()

        })


        #从数据库中删除数据，根据response_table_rows_selected
        deleteData <- shiny::reactive({
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          SQL_df <- DBI::dbReadTable(pool, "responses_df")%>% dplyr::arrange(by_group=Status) %>% dplyr::arrange(desc(Status))
          row_selection <- SQL_df[input$dataout_rows_selected, "row_id"]

          quary <- lapply(row_selection, function(nr){
            DBI::dbExecute(pool, sprintf('DELETE FROM "responses_df" WHERE "row_id" == ("%s")', nr))
          })
          pool::poolClose(pool)
        })

        #将delet button事件与deleteData函数连接
        shiny::observeEvent(input$delete_button, priority = 20,{

          if(length(input$dataout_rows_selected)>=1 ){
            deleteData()
          }

          shiny::showModal(

            if(length(input$dataout_rows_selected) < 1 ){
              shiny::modalDialog(
                title = "Warning",
                paste("Please select row(s)." ),easyClose = TRUE
              )
            })
        })


        #在ui界面进行数据编辑,done显示dateEnd，Processing无dateEnd
        shiny::observeEvent(input$edit_button, priority = 20,{
          inputStatus=shiny::reactive({
            if(shiny::isTruthy(input$Status)){
              if(input$Status=="Done")
              {
                inputStatus="T"
              }else{inputStatus="F"}
            }else{inputStatus="F"}
            return(inputStatus)
          })

          shiny::observe({
            shinyjs::hide("dateEnd")
            if((inputStatus())){
              shinyjs::show("dateEnd")
            }
          })

          pool <<- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          SQL_df <- DBI::dbReadTable(pool, "responses_df") %>% dplyr::arrange(by_group=Status) %>% dplyr::arrange(desc(Status))
          pool::poolClose(pool)
          shiny::showModal(
            if(length(input$dataout_rows_selected) > 1 ){
              shiny::modalDialog(
                title = "Warning",
                paste("Please select only one row." ),easyClose = TRUE)
            } else if(length(input$dataout_rows_selected) < 1){
              shiny::modalDialog(
                title = "Warning",
                paste("Please select a row." ),easyClose = TRUE)
            })

          if(length(input$dataout_rows_selected) == 1 ){

            entry_form("submit_edit")

            shiny::updateTextInput(session, "ProjectID", value = SQL_df[input$dataout_rows_selected, "ProjectID"])
            shiny::updateTextAreaInput(session, "Description", value = SQL_df[input$dataout_rows_selected, "Description"])
            shiny::updateTextInput(session, "SourceFrom", value = SQL_df[input$dataout_rows_selected, "SourceFrom"])
            shiny::updateSelectInput(session, "Participant", selected = SQL_df[input$dataout_rows_selected, "Participant"])

            shiny::updateSelectInput(session, "Status", selected = SQL_df[input$dataout_rows_selected, "Status"])
            #updateSliderInput(session, "age", value = SQL_df[input$dataout_rows_selected, "age"])
            #updateDateInput(session,"dateEnd",value = SQL_df[input$dataout_rows_selected, "dateEnd"])
            #updateDateInput(session,"dateEnd",value ="...")

          }
        })

        #使用view_button，对记录的信息进行view
        shiny::observeEvent(input$view_button,priority = 20,{
          pool <<- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          SQL_df <- DBI::dbReadTable(pool, "responses_df") %>% shiny::arrange(by_group=Status) %>% shiny::arrange(desc(Status))
          pool::poolClose(pool)

          shiny::showModal(
            if(length(input$dataout_rows_selected) > 1 ){
              shiny::modalDialog(
                title = "Warning",
                paste("Please select only one row." ),easyClose = TRUE)
            } else if(length(input$dataout_rows_selected) < 1){
              shiny::modalDialog(
                title = "Warning",
                paste("Please select a row." ),easyClose = TRUE)
            })


          if(length(input$dataout_rows_selected) == 1 ){

            shiny::showModal(
              shiny::modalDialog(
                shiny::div(id="view_form",
                    #tags$body(tags$style(HTML("view_form{ background-color: blue;}"))),

                    shiny::tags$style(HTML(".modal-dialog{ width:400px;}")), #Modify the width of the dialog
                    #tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
                    shiny::fluidPage(
                      #tags$style(HTML(".container-fluid{background-color:#8DB6CD;}")),
                      shiny::fluidRow(
                        shiny::column(
                          shiny::tags$style(HTML("#ProjectID_view{background-color:#8DB6CD;font-family:'arial';font-weight:bold;font-size:15px;text-align:center;}")),
                          shiny::textOutput("ProjectID_view"),
                          width = 12),
                        shiny::column(
                          shiny::tags$br(),
                          width = 12
                        ),
                        shiny::column(
                          shiny::textOutput("summary_view"),
                          width=12
                        )
                      ),
                      easyClose = TRUE
                    )
                )
              )
            )

            output$ProjectID_view<-shiny::renderText({
              as.character(SQL_df[input$dataout_rows_selected, "ProjectID"])
            })

            output$summary_view<-shiny::renderText({

              project_status<-shiny::reactive({
                as.character(SQL_df[input$dataout_rows_selected, "Status"])
              })

              if(project_status()=="Done"){
                sprintf("该项目是由%s在%s提出的，主要目的是进行%s，%s已在%s完成！",
                        SQL_df[input$dataout_rows_selected, "SourceFrom"],
                        SQL_df[input$dataout_rows_selected, "dateStart"],
                        SQL_df[input$dataout_rows_selected, "Description"],
                        SQL_df[input$dataout_rows_selected, "Participant"],
                        SQL_df[input$dataout_rows_selected, "dateEnd"])
              }
              else{
                sprintf("该项目是由%s在%s提出的，主要目的是进行%s，%s正在努力分析中~",
                        SQL_df[input$dataout_rows_selected, "SourceFrom"],
                        SQL_df[input$dataout_rows_selected, "dateStart"],
                        SQL_df[input$dataout_rows_selected, "Description"],
                        SQL_df[input$dataout_rows_selected, "Participant"])
              }

            })


          }

        })


        #更新后台数据库
        shiny::observeEvent(input$submit_edit, priority = 20, {
          pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
          SQL_df <- DBI::dbReadTable(pool, "responses_df") %>% dplyr::arrange(by_group=Status) %>% dplyr::arrange(desc(Status))
          row_selection <- SQL_df[input$dataout_row_last_clicked, "row_id"]
          DBI::dbExecute(pool, sprintf('UPDATE "responses_df" SET "ProjectID" = ?, "Description" = ?, "SourceFrom" = ?,
                                  "Participant" = ?, "Status" = ?, "dateEnd" = ? WHERE "row_id" = ("%s")', row_selection),
                    param = list(input$ProjectID,
                                 input$Description,
                                 input$SourceFrom,
                                 input$Participant,
                                 input$Status,
                                 as.character(input$dateEnd)))
          shiny::removeModal()
          pool::poolClose(pool)

        })



        output$dataout <- DT::renderDataTable({
          table <- responses_df() %>% dplyr::select(-row_id)
          table <- DT::datatable(table[order(table$Status,decreasing = T),],
                             extensions = 'RowGroup',
                             selection = 'single',
                             rownames = FALSE,
                             options = list(
                               rowGroup=list(dataSrc=4)
                             )
          )
        })
      }
    })
  }
  #pool::poolClose(pool)
  shiny::runApp(shiny::shinyApp(ui, server),launch.browser = T)
      }

