
#' Write deployment info to rsconnect_deployments database
#' @export
#' @rdname rsconnect_deployments
log_rsconnect_deployments <- function(con, appname, environment, userid){
  
  tv <- yaml::read_yaml("this_version.yml")
  
  fn_scm <- "shintoconnect_manifest.yml"
  if(!file.exists(fn_scm)){
    message("Dit kan alleen uit een 'deploy project' gedaan worden.")
    return(NULL)
  } 
  scm <- yaml::read_yaml(fn_scm)
  if(is.null(scm$git$branch))scm$git$branch <- ""
  if(is.null(scm$git$remote))scm$git$remote <- ""
  
  # current software version, when using git flow (correctly)
  if(file.exists("VERSION")){
    cur_ver <- readLines("VERSION")[1]    
  } else {
    cur_ver <- ""
  }
  
  # of heel ingewikkeld via git logs
  #get_current_version()
  #if(is.na(cur_ver))cur_ver <- ""
  
  tab <- data.frame(
    timestamp = format(Sys.time()),
    environment = environment,
    appname = appname,
    userid = userid,
    git_sha = scm$git$sha,
    git_branch = scm$git$branch,
    git_remote = scm$git$remote,
    version = cur_ver
  )
  
  DBI::dbWriteTable(con, DBI::Id(schema = "rsconnect", table = "deployments"),
               tab, append = TRUE)
  
}



#' @export
#' @rdname rsconnect_deployments
connect_db_rsconnect_deployments <- function(config_file){
  
  # Gebruik standaard config (default entry)
  con <- try({
    shintodb::connect("rsconnect_deployments", 
                      file = config_file,
                      config_entry = "default")
  }, silent = TRUE)
  
  # zo niet, vul password in
  if(is.null(con) | inherits(con, "try-error")){
    
    port <- Sys.getenv("SHINTO_DEV2_LOCAL_PORT")
    if(port == ""){
      stop("Set environment variable SHINTO_DEV2_LOCAL_PORT with your local forwarding port (e.g. 2222)")
    }
    
    con <- DBI::dbConnect(RPostgres::Postgres(), 
                          dbname = "rsconnect_deployments",
                          host = "localhost", 
                          port = port, 
                          user = "rsconnect_deployments@postgres-dev2", 
                          password = rstudioapi::askForPassword(
                            prompt = "Password user 'rsconnect_deployments' (dev2, 1Password)")
    )
  }
  
  con
}
