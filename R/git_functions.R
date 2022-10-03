
# Find out if current commit is a tag, and if not, how recent

#' @importFrom gert git_tag_list

# like '3.5.2.1' TRUE else FALSE
is_version_nr <- function(x){
  suppressWarnings({
    all(sapply(strsplit(x, "[.]"), function(val)!is.na(as.integer(val))))  
  })
}


num_versions <- function(x){
  .make_numeric_version(x, TRUE, .standard_regexps()$valid_package_version)  
}


versions <- function(){
  
  all_tags <- gert::git_tag_list()
  
  is_version <- mapply(is_version_nr, all_tags$name)
  if(length(is_version) == 0)return(all_tags[0,])
  
  all_tags <- all_tags[is_version,]
  
  vnrs <- num_versions(all_tags$name)
  all_tags$latest_version <- vnrs == max(vnrs)
  
  all_tags
}


get_latest_version <- function(){
  vnrs <- versions()
  if(nrow(vnrs) == 0)return(NA_character_)
  vnrs$name[vnrs$latest_version]
}



get_release_commits <- function(){
  g <- gert::git_log()
  out <- subset(g, grepl("Merge branch 'release/([0-9][.]){1,4}", message))  
  out$version <- stringr::str_extract(out$message, "'.*'") %>%
    stringr::str_replace_all("[']", "") %>%
    stringr::str_replace_all("release/", "")
  
  latest_v <- get_latest_version()
  if(is.na(latest_v))return(out[0,])
  
  out$latest <- out$version == latest_v
  out
}

get_current_version <- function(){
  
  cur_commit <- gert::git_info()$commit
  releases <- get_release_commits()
  if(nrow(releases) == 0)return(NA_character_)
  
  comm <- subset(releases, commit == cur_commit)
  
  if(nrow(comm) == 0){
    return(NA_character_)
  } else {
    comm$version
  }
  
}

