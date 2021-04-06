
library(shiny)
library(shintoshiny)

ui <- fluidPage(
  shintoshiny::shintoshiny_dependencies(),
  shintoshiny::disconnect_message("Uw sessie is verlopen!"),
  
  tags$h2("TEST"),
  tags$hr(),
  verbatimTextOutput("txt_out")
)

server <- function(input, output, session) {
  
  app_idle_time <- shintoshiny::idle_timeout(1, "min")
  
  output$txt_out <- renderPrint({
    app_idle_time()
  })
  
}

shinyApp(ui, server)

