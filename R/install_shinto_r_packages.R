#' Install (all) Shinto Labs R packages
#' @param what Which packages to install. Defaults to most common ones.
#' @export
install_shinto_rpackages <- function(what = c("shintobag","shintoshiny",
                                              "shintoui","shinyfilterset","shintodashboard",
                                              "shintousers","shinysavedom","shintopostcode")){
  
  
  for(p in what){
    remotes::install_github(paste0("moturoa/",p), upgrade = "never", quiet = TRUE)
    print(paste(p, ": done installing / no updates found."))
  }
  
}
