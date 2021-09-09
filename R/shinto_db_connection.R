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




