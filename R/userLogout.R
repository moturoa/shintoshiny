#' Shiny module for logout menu
#' @param id Input id
#' @param username Use \code{\link{get_user}} to get the current user.
#' @rdname userLogoutModule
#' @importFrom shinydashboard dropdownMenuOutput
#' @export
userLogoutUI <- function(id){
  
  ns <- NS(id)

  shinydashboard::dropdownMenuOutput(ns("profile"))
  
}

#' @rdname userLogoutModule
#' @importFrom shinydashboard dropdownMenu renderMenu notificationItem
#' @export
userLogoutModule <- function(input, output, session, username = "unknown"){
  
  output$profile <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(type = "notifications", 
                 icon =  shiny::icon("user"), 
                 headerText = "Profiel", 
                 badgeStatus = NULL,
                 shinydashboard::notificationItem(
                   text = glue::glue("Ingelogd als {username}"),
                   icon("user")
                 ),
                 shinydashboard::notificationItem(
                   text = "Log uit",
                   shiny::icon("sign-out"),
                   href="__logout__"
                 )
    )
  })
  
  
}
