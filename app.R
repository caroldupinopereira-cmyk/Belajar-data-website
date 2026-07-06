# ============================================================
# Intelligent Data Analytics Dashboard
# Using shinydashboard for stable UI
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)

# Supported file types
SUPPORTED_FORMATS <- c("csv", "txt", "tsv", "xls", "xlsx")

# Source modules
source("modules/data_processor.R")
source("modules/statistics.R")
source("modules/visualizations.R")
source("modules/time_series.R")
source("modules/report.R")

# Global variables
UPLOAD_DIR <- "uploads"
REPORT_DIR <- "reports"
dir.create(UPLOAD_DIR, showWarnings = FALSE)
dir.create(REPORT_DIR, showWarnings = FALSE)

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Intelligent Data Analytics Dashboard"),
  dashboardSidebar(
    fileInput("file_upload", "Upload Data", accept = SUPPORTED_FORMATS),
    tags$hr(),
    tags$div(style = "padding: 10px;",
      tags$strong("Supported Formats:"),
      tags$br(),
      tags$span(class = "label label-info", "CSV"), " ",
      tags$span(class = "label label-info", "TXT"), " ",
      tags$span(class = "label label-info", "XLS"), " ",
      tags$span(class = "label label-info", "XLSX")
    ),
    tags$hr(),
    tags$div(id = "data-info-sidebar", style = "padding: 10px; display:none;",
      tags$strong("Dataset Info:"),
      tags$div(id = "sidebar-info")
    ),
    tags$div(id = "column-sidebar", style = "padding: 10px; display:none;",
      tags$strong("Columns:"),
      tags$div(id = "sidebar-columns")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .stat-box { background: white; padding: 15px; border-radius: 5px; margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
        .stat-box h4 { margin-top: 0; color: #3498db; }
        .stat-value { font-size: 18px; font-weight: bold; }
        .stat-label { color: #7f8c8d; font-size: 12px; }
        .chart-box { background: white; padding: 15px; border-radius: 5px; margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
      "))
    ),
    tabItems(
      # Tab 1: Home / Upload
      tabItem(tabName = "home",
        fluidRow(
          box(title = "Welcome", width = 12, status = "primary", solidHeader = TRUE,
            tags$h3("Intelligent Data Analytics Dashboard"),
            tags$p("Upload your data file to start analyzing."),
            tags$hr(),
            tags$h4("Features:"),
            tags$ul(
              tags$li("Data Exploration & Summary Statistics"),
              tags$li("Interactive Visualizations (Scatter, Bar, Histogram, Box Plot, Line, Pie, Heatmap)"),
              tags$li("Time Series Analysis & Forecasting"),
              tags$li("Automatic Report Generation")
            )
          )
        )
      ),

      # Tab 2: Explore
      tabItem(tabName = "explore",
        fluidRow(
          box(title = "Data Preview", width = 12, status = "primary", solidHeader = TRUE,
            DT::dataTableOutput("data_table")
          )
        ),
        fluidRow(
          box(title = "Data Summary", width = 6, status = "info", solidHeader = TRUE,
            verbatimTextOutput("data_summary")
          ),
          box(title = "Missing Values", width = 6, status = "warning", solidHeader = TRUE,
            plotlyOutput("missing_plot")
          )
        )
      ),

      # Tab 3: Statistics
      tabItem(tabName = "statistics",
        fluidRow(
          box(title = "Descriptive Statistics", width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("stats_cards")
          )
        ),
        fluidRow(
          box(title = "Correlation Heatmap", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("correlation_plot", height = "500px")
          )
        )
      ),

      # Tab 4: Visualize
      tabItem(tabName = "visualize",
        fluidRow(
          box(title = "Chart Settings", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("chart_type", "Chart Type", 
                choices = c("Scatter", "Bar", "Histogram", "Box Plot", "Line", "Pie"))),
              column(3, selectInput("x_col", "X Axis", choices = NULL)),
              column(3, selectInput("y_col", "Y Axis", choices = NULL)),
              column(3, selectInput("color_col", "Color By", choices = NULL))
            )
          )
        ),
        fluidRow(
          box(title = "Chart", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("main_chart", height = "500px")
          )
        )
      ),

      # Tab 5: Time Series
      tabItem(tabName = "timeseries",
        fluidRow(
          box(title = "Time Series Settings", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3, selectInput("ts_date_col", "Date Column", choices = NULL)),
              column(3, selectInput("ts_value_col", "Value Column", choices = NULL)),
              column(3, numericInput("forecast_n", "Forecast Periods", value = 12, min = 1, max = 60)),
              column(3, tags$br(), actionButton("run_forecast", "Run Forecast", class = "btn-primary"))
            )
          )
        ),
        fluidRow(
          box(title = "Forecast Plot", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("ts_plot", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Time Series Summary", width = 12, status = "info", solidHeader = TRUE,
            uiOutput("ts_summary")
          )
        )
      ),

      # Tab 6: Reports
      tabItem(tabName = "reports",
        fluidRow(
          box(title = "Generate Report", width = 12, status = "primary", solidHeader = TRUE,
            tags$p("Generate an automatic report with all your analysis results."),
            tags$hr(),
            fluidRow(
              column(6, 
                tags$h4("HTML Report"),
                tags$p("Interactive HTML report with charts and tables."),
                actionButton("generate_html", "Generate HTML Report", class = "btn-primary btn-lg")
              ),
              column(6,
                tags$h4("PDF Report"),
                tags$p("PDF document for printing."),
                actionButton("generate_pdf", "Generate PDF Report", class = "btn-success btn-lg")
              )
            ),
            tags$hr(),
            uiOutput("report_status")
          )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  rv <- reactiveValues(
    data = NULL,
    stats = NULL,
    filename = NULL
  )

  # File upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    result <- load_data(input$file_upload$datapath)

    if (result$success) {
      rv$data <- result$data
      rv$filename <- input$file_upload$name
      rv$stats <- calculate_statistics(rv$data)

      # Update sidebar
      show("data-info-sidebar")
      show("column-sidebar")

      # Update all selectInputs
      updateSelectInput(session, "x_col", choices = names(rv$data))
      updateSelectInput(session, "y_col", choices = names(rv$data))
      updateSelectInput(session, "color_col", choices = c("None", names(rv$data)))
      updateSelectInput(session, "ts_date_col", choices = names(rv$data))
      updateSelectInput(session, "ts_value_col", choices = names(rv$data)[sapply(rv$data, is.numeric)])

      showNotification(paste("File", rv$filename, "loaded successfully!"), type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # Sidebar info
  output$sidebar_info <- renderUI({
    req(rv$data)
    info <- get_data_summary(rv$data)
    tagList(
      tags$p(paste("File:", rv$filename)),
      tags$p(paste("Rows:", info$rows)),
      tags$p(paste("Columns:", info$cols)),
      tags$p(paste("Missing:", info$missing_total))
    )
  })

  output$sidebar_columns <- renderUI({
    req(rv$data)
    tagList(
      lapply(names(rv$data), function(col) {
        type <- class(rv$data[[col]])[1]
        tags$p(paste(col, "-", type))
      })
    )
  })

  # Data table
  output$data_table <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data, options = list(scrollX = TRUE, pageLength = 10))
  })

  # Data summary
  output$data_summary <- renderPrint({
    req(rv$data)
    summary(rv$data)
  })

  # Missing plot
  output$missing_plot <- renderPlotly({
    req(rv$data)
    missing <- colSums(is.na(rv$data))
    df <- data.frame(Column = names(missing), Missing = as.numeric(missing))
    p <- ggplot(df, aes(x = Column, y = Missing, fill = Column)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Missing Values per Column")
    ggplotly(p)
  })

  # Stats cards
  output$stats_cards <- renderUI({
    req(rv$stats, rv$data)
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    if (length(numeric_cols) == 0) return(tags$p("No numeric columns found"))

    cards <- lapply(numeric_cols[1:min(6, length(numeric_cols))], function(col) {
      s <- rv$stats[[col]]
      if (is.null(s)) return(NULL)
      tags$div(class = "col-md-4",
        tags$div(class = "stat-box",
          tags$h4(col),
          fluidRow(
            column(6, tags$div(tags$span(class = "stat-label", "Mean: "), tags$span(class = "stat-value", round(s$mean, 2)))),
            column(6, tags$div(tags$span(class = "stat-label", "Median: "), tags$span(class = "stat-value", round(s$median, 2))))
          ),
          fluidRow(
            column(6, tags$div(tags$span(class = "stat-label", "SD: "), tags$span(class = "stat-value", round(s$sd, 2)))),
            column(6, tags$div(tags$span(class = "stat-label", "Range: "), tags$span(class = "stat-value", round(s$range, 2))))
          )
        )
      )
    })
    do.call(tagList, cards)
  })

  # Correlation
  output$correlation_plot <- renderPlotly({
    req(rv$stats)
    if (!is.null(rv$stats$correlation)) {
      create_heatmap(rv$stats$correlation)
    }
  })

  # Main chart
  output$main_chart <- renderPlotly({
    req(rv$data, input$chart_type, input$x_col)
    color <- if (is.null(input$color_col) || input$color_col == "None") NULL else input$color_col

    tryCatch({
      switch(input$chart_type,
        "Scatter" = create_scatter_plot(rv$data, input$x_col, input$y_col, color),
        "Bar" = create_bar_plot(rv$data, input$x_col, input$y_col),
        "Histogram" = create_histogram(rv$data, input$x_col),
        "Box Plot" = create_box_plot(rv$data, input$y_col, color),
        "Line" = create_line_plot(rv$data, input$x_col, input$y_col, color),
        "Pie" = create_pie_chart(rv$data, input$x_col)
      )
    }, error = function(e) {
      plotly_empty() %>% layout(title = "Select columns to visualize")
    })
  })

  # Time Series
  observeEvent(input$run_forecast, {
    req(rv$data, input$ts_date_col, input$ts_value_col)

    result <- analyze_time_series(rv$data, input$ts_date_col, input$ts_value_col, input$forecast_n)

    if (!is.null(result$forecast_plot)) {
      output$ts_plot <- renderPlotly(result$forecast_plot)
    }

    output$ts_summary <- renderUI({
      tagList(
        tags$p(paste("Data points:", result$summary$n_points)),
        tags$p(paste("Range:", result$summary$start, "to", result$summary$end)),
        tags$p(paste("CV:", round(result$trend$cv, 2), "%"))
      )
    })
  })

  # Report
  observeEvent(input$generate_html, {
    req(rv$data, rv$stats)
    output$report_status <- renderUI({
      tags$div(class = "alert alert-info", "Generating report...")
    })
    result <- generate_report(rv$data, rv$stats, "html")
    if (result$success) {
      output$report_status <- renderUI({
        tags$div(class = "alert alert-success", 
          tags$strong("Report generated! "), 
          tags$a(href = result$filepath, "Download Report", target = "_blank"))
      })
    } else {
      output$report_status <- renderUI({
        tags$div(class = "alert alert-danger", result$message)
      })
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
