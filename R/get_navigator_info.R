
#' Read navigator information and make available as a Shiny input
#' @description When run, input$navigatorInfo is created, a list with lots of info on your system, including
#' Shiny session clientdata (user, url, port, etc.), navigator info (window size, screen size, resolution), and browser
#' info (using bowser.js), version, and platform (win/mac/mobile etc.).
#' @param session The session object.
#' @export
get_navigator_info <- function(session = getDefaultReactiveDomain()){
  session$sendCustomMessage("navigatorInfo", list(id = session$ns("navigatorInfo")))
}

