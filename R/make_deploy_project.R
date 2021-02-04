#' Make a project to deploy / update an application
#' @description Opens a copy of this project including only relevant files,
#' from which you can deploy the application to Shinto Labs' Rstudio Connect server.
#' @param name Name of the project, normally: client_appname ("eindhoven_kamerverhuur", "riec_rro").
#' @param path Where to make the new project, the project will be created in a subdirectory,
#' given by its \code{name}.
#' @param  ignore_dirs Directories to ignore, these contain content that should **not** be uploaded
#' to Rstudio Connect.
#' @param ignore_files Files to ignore (usually these are files in the root, otherwise use
#' \code{ignore_dirs}).
#' @examples
#' \dontrun{
#'
#' # Opens a new project "nederweert_buitengebied", a copy of the current repository
#' # from which you can deploy to rsconnect (via the deploy button), after removing
#' # unwanted files (data for other clients, etc.), and setting client-specific
#' # parameters (for example, in "this_version.yml")
#' shintoconnect::deploy("nederweert_buitengebied")
#' }
#' @export
make_deploy_project <- function(name,
                   path = "c:/repos_deploy",
                   ignore_dirs = c("scripts","stubs","test","tests",
                                   "rsconnect", ".git", ".Rproj.user"),
                   ignore_files = c(".gitignore",".Rhistory", "[.]Rproj$")
){
  
  
  requireNamespace("rstudioapi")
  requireNamespace("R.utils")
  requireNamespace("uuid")
  
  dirs <- list.dirs(full.names=FALSE, recursive=FALSE)
  dirs <- dirs[!dirs %in% ignore_dirs]
  
  out_path <- file.path(path, name)
  dir.create(out_path, showWarnings = FALSE)
  
  here <- rstudioapi::getActiveProject()
  
  lapply(dirs, function(p){
    R.utils::copyDirectory(p, to = file.path(out_path, p), overwrite = TRUE)
  })
  
  fn_root <- setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
  i_del <- unlist(sapply(ignore_files, function(x)grep(x, fn_root)))
  fn_root <- fn_root[-i_del]
  
  file.copy(fn_root, out_path, overwrite = TRUE)
  
  
  checklist <- c(
    "CHECKLIST voor Deploy naar Rstudio Connect",
    "",
    "- Aanpassen gemeente/klant in this_version.yml",
    "- Handmatig verwijderen data/ folders die niet nodig zijn voor deze klant",
    "- Run de app vanuit dit project als laatste lokale test"
  )
  writeLines(checklist, file.path(out_path, "CHECKLIST.md"))
  
  
  # manifest
  manif <- list(
    timestamp = format(Sys.time()),
    uuid = uuid::UUIDgenerate(),
    git = read_git_version()
  )
  yaml::write_yaml(manif, file.path(out_path, "shintoconnect_manifest.yml"))
  
  
  rstudioapi::initializeProject(out_path)
  rstudioapi::openProject(out_path, newSession = TRUE)
  rstudioapi::navigateToFile(file.path(out_path, "CHECKLIST.md"))
}




