



check_appname <- function(appname = ""){
  
  if(appname =="")return("")
  
  have_tv <- file.exists("this_version.yml")
  
  if(!have_tv){
    tags$p("Geen this_version.yml gevonden", style = "color: blue; font-size: 1.1em;")
  } else {
    
    tv <- yaml::read_yaml("this_version.yml")
    klant <- unname(unlist(tv[c("klant","gemeente","tenant")]))
    
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
#' @importFrom DBI dbDisconnect
#' @importFrom rsconnect deployApp
#' @importFrom shintodb connect config_is_encrypted decrypt_config_file
deploy_rsconnect <- function(){
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Deploy App op Rstudio Connect - Shinto Labs",
                           left = miniUI::miniTitleBarCancelButton(label = "Annuleren", primary = TRUE),
                           right = NULL),
    
    miniUI::miniContentPanel(
      
      shiny::textInput("txt_appname", "Applicatie naam", value = ""),
      shiny::uiOutput("ui_appname_check"),
      
      shiny::selectInput("sel_where", "Waar?", choices = NULL),
      
      shiny::textInput("txt_appid", "Applicatie ID (optioneel)", value = ""),
      
      shiny::uiOutput("ui_manifest_check"),
      
      shiny::tags$p(style = "font-size: 1.2em;",
          "Controleer en zo nodig pas de informatie aan."
      ),
      
      shiny::radioButtons("rad_log_deploy", "Log deployment (rsconnect_deployments)", 
                          choices = c("Ja","Nee"), selected = "Ja", inline = TRUE),
    
      
      shiny::actionButton("btn_deploy", "Deploy", icon = shiny::icon("paper-plane"),
                          class = "btn-success btn-lg")
      
    )
  )
  
  server <- function(input, output, session){
    
    db_config_file <- "conf/config.yml"  # shinto hardcoded
    
    acc <- rsconnect::accounts()
    
    shiny::updateSelectInput(session, "sel_where", choices = acc$server, selected = character(0))
    
    shiny::updateTextInput(session, "txt_appname", value = basename(getwd()))
    
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
    
    selected_server <- shiny::reactive({
      input$sel_where
    })
    
    selected_account <- reactive({
      req(input$sel_where)
      acc$name[acc$server == input$sel_where]
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp("Deploy geannulleerd.")
    })
    

    shiny::observeEvent(input$btn_deploy, {
      
      shiny::showModal(
        shiny::modalDialog(title = shiny::tagList(shiny::tags$span(
                  shiny::icon("triangle-exclamation"), style = "color:red;"), 
                                    "Deploy - laatste check"), 
                    size = "m",
                  
                    shiny::uiOutput("ui_package_sources"),
                  
                  
                    if(input$sel_where == "app.shintolabs.net"){
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
      
      # 
      if(input$rad_log_deploy == "Ja" & file.exists(db_config_file)){
        con <- connect_db_rsconnect_deployments(db_config_file)
        
        if(!is.null(con)){
          on.exit({
            DBI::dbDisconnect(con)
          })  
        }
          
      }

      #if(shintodb::config_is_encrypted(db_config_file)){
      # unencrypted wordt toch geskipt
      if(file.exists(db_config_file)){
        shintodb::decrypt_config_file(db_config_file,db_config_file)
        on.exit({shintodb::encrypt_config_file(db_config_file)}, add = TRUE)  
      }
      
      
      # Deploy de app
      
      appid <- input$txt_appid
      
      if(appid == ""){
        resp <- rsconnect::deployApp(appName = input$txt_appname,
                                     appTitle = input$txt_appname,
                                     account = selected_account(),
                                     server = selected_server(),
                                     launch.browser = TRUE,
                                     forceUpdate = TRUE)
        
      } else {
        resp <- rsconnect::deployApp(appId = appid,
                                     account = selected_account(),
                                     server = selected_server(),
                                     launch.browser = TRUE,
                                     forceUpdate = TRUE)  
      }
      
      
      
      if(isTRUE(resp) && input$rad_log_deploy == "Ja"){
        # Schrijf deployment info naar rsconnect_deployments database
        log_rsconnect_deployments(con, 
                                  appname = input$txt_appname, 
                                  environment = selected_server(), 
                                  userid = selected_account())
      }
      
      
      
      shiny::stopApp(paste("Applicatie gedeployed naar",input$txt_appname))
      
    })
    
    
    
  }
  
  shiny::runGadget(ui, server, 
                   viewer = shiny::dialogViewer(dialogName = "Deploy naar Rstudio Connect - Shinto Labs"),
                   stopOnCancel = FALSE)
  
}



