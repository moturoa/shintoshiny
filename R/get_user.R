#' Get user in this session
#' @export
get_user <- function(default = "demo", session = shiny::getDefaultReactiveDomain()){
  
  if(is.null(session$user)){
    "demo"
  } else {
    session$user
  }

}
