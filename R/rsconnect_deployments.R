
#' Write deployment info to rsconnect_deployments database
#' @param con Database connection to `rsconnect_deployments` 
#' @param appname app name on posit connect
#' @param environment Environment field in db (filled elsewhere)
#' @param userid userid
#' @export
#' @rdname rsconnect_deployments
log_rsconnect_deployments <- function(con, appname, environment, userid,
                                      shintoconnect_manifest = NULL, 
                                      version = NULL
                                      ){
  
  if(is.null(con)){
    cli::cli_alert_danger("log_rsconnect_deployments : provide db connection to rsconnect_deployments")  
    return(invisible(NULL))
  }
  
  scm <- shintoconnect_manifest
  if(is.null(scm)){
    fn_scm <- "shintoconnect_manifest.yml"
    if(!file.exists(fn_scm)){
      message("Dit kan alleen uit een 'deploy project' gedaan worden.")
      return(NULL)
    } 
    scm <- yaml::read_yaml(fn_scm)  
  }
  
  
  if(is.null(scm$git$branch))scm$git$branch <- ""
  if(is.null(scm$git$remote))scm$git$remote <- ""
  
  # current software version, when using git flow (correctly)
  if(is.null(version)){
    if(file.exists("VERSION")){
      version <- readLines("VERSION")[1]    
    } else {
      version <- ""
    }  
  }
  
  tab <- data.frame(
    timestamp = format(Sys.time()),
    environment = environment,
    appname = appname,
    userid = userid,
    git_sha = scm$git$sha,
    git_branch = scm$git$branch,
    git_remote = scm$git$remote,
    version = version
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
    
    cli::cli_alert_warning("Deployment not logged to rsconnect_deployments database, add password to 'development' section only")
    return(invisible(NULL))
  }
  
  con
}
