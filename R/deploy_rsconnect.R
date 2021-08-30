



check_appname <- function(appname = ""){
  
  if(appname =="")return("")
  
  have_tv <- file.exists("this_version.yml")
  
  if(!have_tv){
    "Geen this_version.yml gevonden"
  } else {
    
    tv <- yaml::read_yaml("this_version.yml")
    klant <- unname(unlist(tv[c("klant","gemeente")]))
    
    if(!grepl(klant, appname, ignore.case = TRUE)){
      paste0("Klant is '", klant, "' maar appName is '", appname, "', klopt dit?")
    } else {
      paste0("Klant naam (", klant, ") komt overeen met appName (", appname, ")")
    }
    
  }
  
}


#' Deploy app op Rstudio Connect
deploy_rsconnect <- function(){
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Deploy App op Rstudio Connect - Shinto Labs",
                           left = miniUI::miniTitleBarCancelButton(label = "Annuleren", primary = TRUE),
                           right = NULL),
    miniUI::miniContentPanel(
      
      shiny::textInput("txt_appname", "Applicatie naam", value = ""),
      shiny::uiOutput("ui_appname_check"),
      shiny::textInput("txt_appid", "Applicatie ID (optioneel)", value = ""),
      shiny::textInput("txt_account", "Account (rsconnect user)", value = ""),
      shiny::selectInput("sel_server", "Server", choices = "connect.shintolabs.net"),
      
      shiny::uiOutput("ui_manifest_check"),
      
      shiny::tags$p(style = "font-size: 1.2em;",
          "Controleer en zo nodig pas de informatie aan."
      ),
      
      shiny::actionButton("btn_deploy", "Deploy", icon = icon("paper-plane"),
                          class = "btn-success btn-lg")
      
    )
  )
  
  server <- function(input, output, session){
    
    shiny::updateTextInput(session, "txt_appname", value = basename(getwd()))
    shiny::updateTextInput(session, "txt_account", value = rsconnect::accounts()$name[1])
    
    output$ui_appname_check <- shiny::renderUI({
      shiny::req(input$txt_appname)
      shiny::tags$p(check_appname(input$txt_appname),
                    style = "font-size: 1.1em; color: red;")
    })
    
    output$ui_manifest_check <- shiny::renderUI({
      
      if(!file.exists("shintoconnect_manifest.yml")){
        shiny::tags$p(style = "font-size: 1.1em; color: red;",
               
               "Dit project is niet aangemaakt met 'shintoshiny::make_deploy_project',",
               "Annuleer en maak eerst een deploy project!"
               )
      } else NULL
      
    })
    
    
    observeEvent(input$cancel, {
      shiny::stopApp("Deploy geannulleerd.")
    })
    
    shiny::observeEvent(input$btn_deploy, {
      
      showModal(
        modalDialog(title = tagList(tags$span(icon("exclamation-triangle"), style = "color:red;"), 
                                    "Deploy - laatste check"), 
                    size = "m",
                    
                    tags$p(paste("Deploy naar", input$txt_appname, 
                                 "op Rstudio Connect (", 
                                 input$sel_server, ")- weet je het zeker?")),
                    tags$p("Als je 'Ja' klikt, wacht tot dit window sluit (dit kan enkele minuten duren)"),
                    actionButton("btn_confirm","Ja!", icon = icon("check"), class = "btn-success")
        )
      )
      
      
    })
    
    
    observeEvent(input$btn_confirm, {
      
      rsconnect::deployApp(
        appTitle = input$txt_appname,
        appId = if(input$txt_appid == "")NULL else as.integer(input$txt_appid),
        account = input$txt_account,
        server = input$sel_server,
        launch.browser = TRUE,
        lint = TRUE,
        forceUpdate = TRUE
      )
      
      shiny::stopApp(paste("Applicatie gedeployed naar",input$txt_appname))
      
    })
    
    
    
  }
  
  shiny::runGadget(ui, server, 
                   viewer = shiny::dialogViewer(dialogName = "Deploy naar Rstudio Connect - Shinto Labs"),
                   stopOnCancel = FALSE)
  
}



