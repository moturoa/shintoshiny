#' Stop application after idle timeout 
#' @details jQuery implementation
#' @param time Time after which 'stopApp' is called
#' @param units Either 'minutes' or 'seconds' of time.
#' @export
idle_timeout <- function(time = 10, units = c("minutes","seconds")){
  
  units <- match.arg(units)
  
  out <- callModule(idle_timeout_module, "idle_timeout", time = time, units = units)
  
return(invisible(out))
}


idle_timeout_module <- function(input, output, session, time, units){
  
  observe({
    
    req(input$app_idle_time)
    
    chk_time <- ifelse(units == "minutes", time * 60, time)
    
    if(input$app_idle_time >= chk_time){
      shiny::stopApp()
    }
    
  })
  
return(reactive(input$app_idle_time))
}
