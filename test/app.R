library(shinydashboard)

library(shintoanalytics)
library(shintohttpheaders)


.secret <- config::get("httpheaders", file = "c:/repos/wbm3.0/conf/config.yml")$secret


read_app_settings <- function(file = "this_version.yml"){
  
  thisv <- yaml::read_yaml(file)
  
  c(thisv, 
    list(
      logout_link = switch(thisv$platform, 
                            local = "",
                            shinyserver = "__logout__",
                            shinyproxy = "https://login.microsoftonline.com/common/oauth2/logout"
      )
    ))
  
}


.app <- read_app_settings("this_version.yml")



li_klant <- function(app){
  tags$li(tags$span(app$klant), class = "dropdown",
          style = glue::glue("font-weight: 500;",
                       "height: 50px;",
                       "font-size: 1.1em;",
                       "color: white;",
                       "padding-top: 15px;",
                       "padding-bottom: 15px;",
                       "padding-right: 30px;",
                       "display: block;"))
}





header <- dashboardHeader(
  
  li_klant(.app)
  
  
)

sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            div(p("Dashboard tab content"))
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output) { 
    
    
    
  }
)




