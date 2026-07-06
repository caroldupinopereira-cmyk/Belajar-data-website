# Simple test app
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Test App"),
  dashboardSidebar(
    fileInput("file1", "Choose CSV", accept = ".csv")
  ),
  dashboardBody(
    h1("Hello World!"),
    verbatimTextOutput("txt")
  )
)

server <- function(input, output) {
  output$txt <- renderPrint({ "App is working!" })
}

shinyApp(ui, server)
