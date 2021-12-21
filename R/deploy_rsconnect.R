



check_appname <- function(appname = ""){
  
  if(appname =="")return("")
  
  have_tv <- file.exists("this_version.yml")
  
  if(!have_tv){
    tags$p("Geen this_version.yml gevonden", style = "color: blue; font-size: 1.1em;")
  } else {
    
    tv <- yaml::read_yaml("this_version.yml")
    klant <- unname(unlist(tv[c("klant","gemeente")]))
    
    if(!grepl(klant, appname, ignore.case = TRUE)){
      tags$p(paste0("Klant is '", klant, "' maar appName is '", appname, "', klopt dit?"),
             style = "color: red; font-size: 1.1em;")
    } else {
      tags$p(paste0("Klant naam (", klant, ") komt overeen met appName (", appname, ")"),
             style = "color: green; font-size: 1.1em;")
    }
    
  }
  
}


#' Deploy app op Rstudio Connect
#' Rstudio gadget om applicatie te deployen.
#' (export niet nodig, wordt geladen onder Add-ins in Rstudio, zie inst/rstudio/addins.dcf)
deploy_rsconnect <- function(){
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Deploy App op Rstudio Connect - Shinto Labs",
                           left = miniUI::miniTitleBarCancelButton(label = "Annuleren", primary = TRUE),
                           right = NULL),
    miniUI::miniContentPanel(
      
      shiny::textInput("txt_appname", "Applicatie naam", value = ""),
      shiny::uiOutput("ui_appname_check"),
      
      shiny::selectInput("sel_where", 
                         "Waar?",
                         choices = c("Oude connect server" = "oud",
                                     "Development - Infra21" = "development",
                                     "Production - Infra21" = "production")),
      
      shiny::textInput("txt_appid", "Applicatie ID (optioneel)", value = ""),
      shiny::selectInput("txt_account", "Account (rsconnect user)", choices = NULL),

      shiny::uiOutput("ui_manifest_check"),
      
      shiny::tags$p(style = "font-size: 1.2em;",
          "Controleer en zo nodig pas de informatie aan."
      ),
      
      shiny::actionButton("btn_deploy", "Deploy", icon = shiny::icon("paper-plane"),
                          class = "btn-success btn-lg")
      
    )
  )
  
  server <- function(input, output, session){
    
    shiny::updateTextInput(session, "txt_appname", value = basename(getwd()))
    
    
    acc <- rsconnect::accounts()
    
    shiny::updateSelectInput(session, "txt_account", choices = acc$name)
    
    output$ui_appname_check <- shiny::renderUI({
      shiny::req(input$txt_appname)
      check_appname(input$txt_appname)
    })
    
    output$ui_manifest_check <- shiny::renderUI({
      
      if(!file.exists("shintoconnect_manifest.yml")){
        shiny::tags$p(style = "font-size: 1.1em; color: red;",
               
               "Dit project is niet aangemaakt met 'shintoshiny::make_deploy_project',",
               "Annuleer en maak eerst een deploy project!"
               )
      } else NULL
      
    })
    
    selected_server <- reactive({
      
      switch(input$sel_where, 
             oud = "connect.shintolabs.net",
             development = "devapp.shintolabs.net",
             production = "app.shintolabs.net")
      
    })
    
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp("Deploy geannulleerd.")
    })
    
    shiny::observeEvent(input$btn_deploy, {
      
      showModal(
        modalDialog(title = shiny::tagList(shiny::tags$span(shiny::icon("exclamation-triangle"), style = "color:red;"), 
                                    "Deploy - laatste check"), 
                    size = "m",
                    
                    if(input$sel_where == "production"){
                      tags$p(HTML("Je gaat deployen naar de <strong>Productieomgeving</strong>!!"),
                             style = "font-size: 1.2em;")
                    } else NULL,
                    
                    shiny::tags$p(paste("Deploy naar", input$txt_appname, 
                                 "op Rstudio Connect (", 
                                 selected_server(), ")- weet je het zeker?")),
                    shiny::tags$p("Als je 'Ja' klikt, wacht tot dit window sluit (dit kan enkele minuten duren)"),
                    shiny::actionButton("btn_confirm","Ja!", icon = shiny::icon("check"), class = "btn-success")
        )
      )
      
      
    })
    
    
    shiny::observeEvent(input$btn_confirm, {
      
      rsconnect::deployApp(
        appTitle = input$txt_appname,
        appId = if(input$txt_appid == "")NULL else input$txt_appid,
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



