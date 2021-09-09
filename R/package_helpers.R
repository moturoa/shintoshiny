#' Check for package version
#' @export
#' @rdname package_helpers
check_for_version <- function(package, version){
  if(packageVersion(package) < version){
    stop(paste0("Install newest '",package,"' (see preload/load_packages)"))
  }
  
}

#' Load modules in a Shinto Shiny app
#' @rdname package_helpers
#' @export
load_modules <- function(){
  for(z in dir("modules/", full.names = TRUE, recursive = TRUE, pattern = "[.]R$")){
    source(z)
  }
}
