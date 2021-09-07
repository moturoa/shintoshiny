#' Common shiny inputs with Shinto defaults
#' 
#' @rdname shintoinputs
#' @export
select_input <- function(...){
  
  shiny::selectizeInput(...,
                 options = list(
                   'plugins' = list('remove_button'))
  )
  
}

#' @rdname shintoinputs
#' @param dropdown_align_right If TRUE, aligns the dropdown on the right side
#' @param live_search If TRUE, adds a search field 
#' @param dropup_auto If TRUE, dropdown 'drops' up if no room on the bottom
#' @importFrom shinyWidgets pickerInput
#' @export
picker_input <- function(...,
                         dropdown_align_right = TRUE,
                         dropup_auto = TRUE,
                         live_search = FALSE
                         ){
  
  shinyWidgets::pickerInput(..., 
                            options = list(`actions-box` = TRUE,
                                           `deselect-all-text` = "Alles uit",
                                           `dropdown-align-right` = dropdown_align_right,
                                           `selected-text-format` = "count > 3",
                                           `none-selected-text` = "Geen selectie",
                                           `select-all-text` = "Alles aan",
                                           `none-results-text` = "Geen selectie",
                                           `count-selected-text` = ">3 Geselecteerd",
                                           `dropup-auto` = dropup_auto,
                                           `live-search` = live_search,
                                           `live-search-normalize` = live_search
                            ))
  
}


#' @rdname shintoinputs
#' @importFrom shinyWidgets materialSwitch
#' @export
material_switch <- function(...,
                            status = "primary",
                            inline = TRUE){
  
  shinyWidgets::materialSwitch(...,
                               inline = inline,
                               status = status)
  
}





