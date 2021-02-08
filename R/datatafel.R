#' Datatable, Nederlandse versie
#' @importFrom DT datatable
#' @export
datatafel <- function(..., 
                      dom  = "frtip",
                      pageLength = 10, 
                      escape = FALSE,
                      rownames = FALSE,
                      ordering = TRUE,
                      extensions = "Responsive",
                      selection = "single"){
  
  DT::datatable(...,
            escape = escape, 
            rownames = rownames,
            selection = selection,
            extensions = extensions,
            options = list(dom = dom,
                           ordering = ordering,
                           lengthChange = FALSE,
                           pageLength = pageLength,
                           language = list(search = "Zoek",
                                           emptyTable = "Geen resultaten.",
                                           zeroRecords = "Geen resultaten.",
                                           info = "Tonen _START_ tot _END_ van _TOTAL_ rijen.",
                                           infoFiltered = "(gefilterd uit _MAX_ rijen)",
                                           infoEmpty = "Geen resultaten.",
                                           paginate = list(
                                             first = "Eerste",
                                             last = "Laatste",
                                             `next` = "Volgende",
                                             previous = "Vorige"
                                           )
                           )
            )
  )
}


