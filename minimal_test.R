library(shiny)

ui <- fluidPage(
  h1("Test App"),
  p("If you see this, Shiny is working!")
)

server <- function(input, output) {}

shinyApp(ui, server)
