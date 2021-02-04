#' Connect to shinto devpostgres02
#'@export
#'@importFrom config get
#'@importFrom DBI dbConnect dbWriteTable dbDisconnect
shinto_db_connection <- function(what = c("CBS","BRKdata","BAGdata","shintoanalytics"), 
                                 file = "conf/config.yml"){
  
  what <- match.arg(what)
  conf <- config::get(what, file = file)
  
  DBI::dbConnect(RPostgres::Postgres(),
            dbname = conf$dbname,
            host = conf$dbhost,
            port = 5432,
            user = conf$dbuser,
            password = conf$dbpassword)  
}


#' Schema for logins table in shintoanalytics db
#'@export
logins_schema <- function(){
  tibble(
    # (1) general
    id = character(),
    timestamp = integer(),
    application = character(),
    application_version = character(),
    user = character(),
    # (2) session$clientData
    url_protocol = character(), 
    url_hostname = character(), 
    url_port = character(), 
    url_pathname = character(), 
    url_search = character(), 
    url_hash_initial = character(),
    url_hash = character(),
    pixelratio = character(),
    # (3) navigator / window info (JS)
    language = character(),
    cookieEnabled = logical(),
    windowWidth = character(),
    windowHeight = character(),
    screenWidth = character(),
    screenHeight = character(),
    # (4) bowser (JS)
    browserName = character(),
    browserVersion = character(),
    osName = character(),
    osVersion = character()
  )
}

#' WATCH OUT
clear_login_database <- function(){
  
  db <- shinto_db_connection("shintoanalytics")
  on.exit(DBI::dbDisconnect(db))
  
  so_schema <- logins_schema()
  
  # manual override :)
  if(FALSE){
    DBI::dbWriteTable(db, "logins", so_schema, overwrite = TRUE)  
  }
  
}


# geen export
#' @importFrom tibble as_tibble
add_logins_row <- function(object, db = NULL){
  
  if(is.null(db)){
    db <- shinto_db_connection("shintoanalytics")
    on.exit(dbDisconnect(db))
  }
  
  
  # Type convert: precies zoals logins_schema.
  scheme <- logins_schema()
  cls <- sapply(scheme, class)
  
  for(nm in names(object)){
    clas <- cls[nm]
    
    if(!is.na(clas)){
      fun <- base::get(paste0("as.", clas))
      object[[nm]] <- fun(object[[nm]])  
    } else {
      warning(paste("Variable",nm,"not in class definition - skipped"))
    }
    
    
  }
  
  row <- tibble::as_tibble(object)
  
  response <- DBI::dbWriteTable(db, "logins", row, append = TRUE)
  
  return(response)
}


#' Schrijf user login data
#' Voor gebruik in een shiny app. Zet eerst 'nav' met shintobrowserinfo, vind user (via lokale get_user, bv.),
#' en geef naam van applicatie door.
#' @export
shinto_write_user_login <- function(user = "unknown", 
                                    application = "package_test", 
                                    version = "",
                                    db = NULL, 
                                    nav = NULL,
                                    session = shiny::getDefaultReactiveDomain()){
  
  # (1) general
  obj <- list(
    id = uuid::UUIDgenerate(),
    timestamp = as.integer(Sys.time()),
    application = application,
    application_version = version,
    user = user  
  )
  
  # (2) sessio$clientData
  clientdat <- shiny::reactiveValuesToList(session$clientData)
  if(!is.null(clientdat)){
    
    sessiondat <- clientdat[c("url_protocol",
                              "url_hostname",
                              "url_port",
                              "url_pathname",
                              "url_search",
                              "url_hash_initial",
                              "url_hash",
                              "pixelratio")]
    
    obj <- c(obj, sessiondat)
  }
  
  # (3) and (4)
  if(!is.null(nav)){
    nav <- list(
      
      language = nav$language,
      cookieEnabled = nav$cookieEnabled,
      windowWidth = nav$windowWidth,
      windowHeight = nav$windowHeight,
      screenWidth = nav$screenWidth,
      screenHeight = nav$screenHeight,
      browserName = nav$name,
      browserVersion = nav$version,
      osName = nav$osname,
      osVersion = nav$osversion
      
    )  
  }
  
  object <- c(obj, nav)
  
  response <- add_logins_row(object, db)
  
  return(response)
}

#' Read (n) last login(s)
#'@export
last_logins <- function(n = 1, db = NULL){
  
  if(is.null(db)){
    db <- shinto_db_connection("shintoanalytics")
    on.exit(dbDisconnect(db))
  }
  
  tbl(db, "logins") %>%
    dplyr::arrange(dplyr::desc(timestamp)) %>%
    head(n) %>%
    collect
  
}


