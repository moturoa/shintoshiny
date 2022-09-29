
#' Info for all required packages for this application
#' @export
package_dependencies_info <- function(){
  app_deps <- suppressWarnings({
    rsconnect::appDependencies()
  })  
  
  sessioninfo::package_info(pkgs = app_deps$package)
}





