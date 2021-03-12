#' Browser/navigator info dependencies
#' @description In the UI, place the command \code{shintoshiny::browser_info_dependencies()}.
#' @export
shintoshiny_dependencies <- function() {
  
  list(
    htmltools::htmlDependency(name = "browserInfo-assets", version = "0.1",
                 package = "shintoshiny",
                 src = "assets",
                 script = "navigatorInfo/navigatorInfo.js"
    ),
    htmltools::htmlDependency(name = "bowser", version = "0.1",
                   package = "shintoshiny",
                   src = "assets",
                   script = "bowser/bowser.min.js"
    ),
    htmltools::htmlDependency(name = "buttons", version = "0.1",
                   package = "shintoshiny",
                   src = "assets",
                   stylesheet = "css/buttons.css"
    ),
    htmltools::htmlDependency(name = "tooltip",  
      version = "0.1",
      src = "assets",
      script = "tooltip/tooltip.js",
      stylesheet = "tooltip/tooltip.css"
    )
  )
}


