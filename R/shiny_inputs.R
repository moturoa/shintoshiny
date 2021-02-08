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
#' @importFrom shinyWidgets pickerInput
#' @export
picker_input <- function(...,
                         dropdown_align_right = TRUE,
                         dropup_auto = TRUE
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
                                           `dropup-auto` = dropup_auto
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





