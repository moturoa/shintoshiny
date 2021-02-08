



datatableUI <- function(id, width = "100%", 
                        height = "auto",
                        export_button = TRUE,
                        export_button_class = "btn-secondary"
){
  
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(ns("dt_table"), width = width, height = height),
    tags$br(),
    if(export_button){
      exportButtonUI(ns("export"), export_button_class = export_button_class)
    } else NULL
  )
  
  
}

datatableModule <- function(input, output, session,
                            data,  
                            filename_prefix = "data_download",
                            formats = c("Excel","CSV","JSON"),
                            ...  # extra argumenten naar datatable
){
  
  output$dt_table <- DT::renderDT({
    
    datatafel(data(), ...)
    
  })
  
  callModule(exportButton, id = "export", 
             data = data, 
             formats = formats,
             filename_prefix = filename_prefix)
  
}



exportButtonUI <- function(id, export_button_class = "btn-secondary"){
  
  ns <- NS(id)
  
  actionButton(ns("btn_export"), "Export...", class = export_button_class)
  
}

exportButton <- function(input, output, session, 
                         data, 
                         formats = c("Excel", "CSV","JSON"),
                         filename_prefix = "data_download"){
  
  
  observeEvent(input$btn_export, {
    
    showModal(
      # zie modal.R
      data_export_modal(ns = session$ns, 
                        formats = formats)  
    )
    
  })
  
  output$btn_excel <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data(), file)
    }
  )
  
  output$btn_csv <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  output$btn_json <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".json")
    },
    content = function(file) {
      writeLines(jsonlite::toJSON(data(), pretty = TRUE), file)
    }
  )
  
  
}




data_export_modal <- function(ns = NS(NULL),
                              title = "Export data", 
                              info_text = "Kies het gewenste formaat:",
                              formats = c("Excel","CSV","JSON")){
  
  modalDialog(
    
    tags$h3(title),
    tags$hr(),
    
    tags$p(info_text),
    
    
    if("Excel" %in% formats){
      downloadButton(ns("btn_excel"), "Excel", icon = icon("file-excel"),
                     class = "btn-lg btn-light")
    },
    
    if("CSV" %in% formats){
      downloadButton(ns("btn_csv"), "CSV", icon = icon("table"),
                     class = "btn-lg btn-light")  
    },
    
    if("JSON" %in% formats){
      downloadButton(ns("btn_json"), "JSON", icon = icon("file-code"),
                     class = "btn-lg btn-light")  
    },
    
    
    # ID van deze button maakt niet uit, 'close' gaat via JS (`data-dismiss`) 
    footer = actionButton(ns("xyz"), "Sluiten", 
                          icon = icon("remove"), 
                          class= "btn-danger",
                          `data-dismiss` = "modal")
  )
  
}


