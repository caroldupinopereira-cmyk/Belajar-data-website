# ============================================================
# Intelligent Data Analytics Dashboard
# Main Application (Shiny Server)
# ============================================================

library(shiny)
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
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "css/style.css"),
    tags$script(src = "js/main.js")
  ),

  # Navigation
  tags$nav(class = "navbar",
    tags$div(class = "nav-brand", "Intelligent Data Analytics Dashboard"),
    tags$div(class = "nav-links",
      tags$a(href = "#upload", class = "nav-link active", "Upload"),
      tags$a(href = "#explore", class = "nav-link", "Explore"),
      tags$a(href = "#statistics", class = "nav-link", "Statistics"),
      tags$a(href = "#visualize", class = "nav-link", "Visualize"),
      tags$a(href = "#timeseries", class = "nav-link", "Time Series"),
      tags$a(href = "#reports", class = "nav-link", "Reports")
    )
  ),

  # Sidebar
  tags$div(class = "app-container",
    tags$div(class = "sidebar",
      tags$div(class = "sidebar-section",
        tags$h4("Data Source"),
        fileInput("file_upload", NULL, accept = SUPPORTED_FORMATS),
        tags$div(class = "supported-formats",
          tags$span(class = "format-badge", "CSV"),
          tags$span(class = "format-badge", "TXT"),
          tags$span(class = "format-badge", "XLS"),
          tags$span(class = "format-badge", "XLSX")
        )
      ),
      tags$div(class = "sidebar-section", id = "data-info-panel", style = "display:none;",
        tags$h4("Dataset Info"),
        tags$div(id = "data-info-content")
      ),
      tags$div(class = "sidebar-section", id = "column-panel", style = "display:none;",
        tags$h4("Columns"),
        tags$div(id = "column-list")
      )
    ),

    # Main Content
    tags$div(class = "main-content",

      # Upload Section
      tags$div(id = "upload", class = "section active",
        tags$div(class = "upload-area", id = "drop-zone",
          tags$div(class = "upload-icon", "📁"),
          tags$h3("Drag & Drop File Here"),
          tags$p("or click to browse"),
          tags$p(class = "upload-hint", "Supports: CSV, TXT, XLS, XLSX")
        ),
        tags$div(id = "upload-status", style = "display:none;")
      ),

      # Explore Section
      tags$div(id = "explore", class = "section",
        tags$h2("Data Exploration"),
        tags$div(class = "explore-tabs",
          tags$button(class = "tab-btn active", "Preview"),
          tags$button(class = "tab-btn", "Summary"),
          tags$button(class = "tab-btn", "Missing Data")
        ),
        tags$div(id = "explore-content")
      ),

      # Statistics Section
      tags$div(id = "statistics", class = "section",
        tags$h2("Statistical Analysis"),
        tags$div(class = "stats-grid", id = "stats-grid"),
        tags$div(id = "correlation-section", style = "display:none;",
          tags$h3("Correlation Matrix"),
          plotlyOutput("correlation_plot", height = "400px")
        )
      ),

      # Visualize Section
      tags$div(id = "visualize", class = "section",
        tags$h2("Data Visualization"),
        tags$div(class = "viz-controls",
          tags$div(class = "control-group",
            tags$label("Chart Type"),
            selectInput("chart_type", NULL,
              choices = c("Scatter", "Bar", "Histogram", "Box Plot", "Line", "Heatmap", "Pie"))
          ),
          tags$div(class = "control-group",
            tags$label("X Axis"),
            selectInput("x_col", NULL, choices = NULL)
          ),
          tags$div(class = "control-group",
            tags$label("Y Axis"),
            selectInput("y_col", NULL, choices = NULL)
          ),
          tags$div(class = "control-group",
            tags$label("Color By"),
            selectInput("color_col", NULL, choices = NULL)
          )
        ),
        tags$div(class = "chart-container",
          plotlyOutput("main_chart", height = "500px")
        )
      ),

      # Time Series Section
      tags$div(id = "timeseries", class = "section",
        tags$h2("Time Series Analysis"),
        tags$div(class = "ts-controls",
          tags$div(class = "control-group",
            tags$label("Date Column"),
            selectInput("ts_date_col", NULL, choices = NULL)
          ),
          tags$div(class = "control-group",
            tags$label("Value Column"),
            selectInput("ts_value_col", NULL, choices = NULL)
          ),
          tags$div(class = "control-group",
            tags$label("Forecast Periods"),
            numericInput("forecast_n", NULL, value = 12, min = 1, max = 60)
          ),
          tags$button(class = "btn btn-primary", id = "run_forecast", "Run Forecast")
        ),
        tags$div(id = "ts-results", style = "display:none;",
          plotlyOutput("ts_plot", height = "400px"),
          tags$div(id = "ts-summary")
        )
      ),

      # Reports Section
      tags$div(id = "reports", class = "section",
        tags$h2("Report Generation"),
        tags$div(class = "report-options",
          tags$div(class = "report-option",
            tags$h4("Quick Report"),
            tags$p("Generate HTML report with all analyses"),
            tags$button(class = "btn btn-primary", id = "generate_html", "Generate HTML")
          ),
          tags$div(class = "report-option",
            tags$h4("PDF Report"),
            tags$p("Export as PDF document"),
            tags$button(class = "btn btn-secondary", id = "generate_pdf", "Generate PDF")
          )
        ),
        tags$div(id = "report-status")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    stats = NULL,
    filename = NULL
  )

  # File upload handler
  observeEvent(input$file_upload, {
    req(input$file_upload)
    filepath <- input$file_upload$datapath
    result <- load_data(filepath)

    if (result$success) {
      rv$data <- result$data
      rv$filename <- input$file_upload$name

      # Update UI
      showElement("data-info-panel")
      showElement("column-panel")

      # Update inputs
      updateSelectInput(session, "x_col", choices = names(rv$data))
      updateSelectInput(session, "y_col", choices = names(rv$data))
      updateSelectInput(session, "color_col", choices = c("None", names(rv$data)))
      updateSelectInput(session, "ts_date_col", choices = names(rv$data))
      updateSelectInput(session, "ts_value_col", choices = names(rv$data)[sapply(rv$data, is.numeric)])

      # Calculate stats
      rv$stats <- calculate_statistics(rv$data)

      showNotification(paste("File", rv$filename, "uploaded successfully!"), type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })

  # Data info output
  output$data_info_content <- renderUI({
    req(rv$data)
    info <- get_data_summary(rv$data)
    HTML(paste0(
      "<div class='info-item'><strong>File:</strong> ", rv$filename, "</div>",
      "<div class='info-item'><strong>Rows:</strong> ", info$rows, "</div>",
      "<div class='info-item'><strong>Columns:</strong> ", info$cols, "</div>",
      "<div class='info-item'><strong>Missing:</strong> ", info$missing_total, "</div>"
    ))
  })

  # Column list output
  output$column_list <- renderUI({
    req(rv$data)
    col_html <- paste(
      sapply(names(rv$data), function(col) {
        type <- class(rv$data[[col]])[1]
        paste0("<div class='col-item'>", col, " <span class='col-type'>", type, "</span></div>")
      }),
      collapse = ""
    )
    HTML(col_html)
  })

  # Explore content
  output$explore_content <- renderUI({
    req(rv$data)
    tagList(
      DT::datatable(head(rv$data, 20), options = list(scrollX = TRUE, pageLength = 10))
    )
  })

  # Stats grid
  output$stats_grid <- renderUI({
    req(rv$stats, rv$data)
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    if (length(numeric_cols) == 0) return(tags$p("No numeric columns found"))

    cards <- lapply(numeric_cols[1:min(6, length(numeric_cols))], function(col) {
      s <- rv$stats[[col]]
      if (is.null(s)) return(NULL)
      tags$div(class = "stat-card",
        tags$h4(col),
        tags$div(class = "stat-values",
          tags$div(class = "stat-item", tags$span(class = "stat-label", "Mean"), tags$span(class = "stat-value", round(s$mean, 2))),
          tags$div(class = "stat-item", tags$span(class = "stat-label", "Median"), tags$span(class = "stat-value", round(s$median, 2))),
          tags$div(class = "stat-item", tags$span(class = "stat-label", "SD"), tags$span(class = "stat-value", round(s$sd, 2))),
          tags$div(class = "stat-item", tags$span(class = "stat-label", "Min"), tags$span(class = "stat-value", round(s$min, 2))),
          tags$div(class = "stat-item", tags$span(class = "stat-label", "Max"), tags$span(class = "stat-value", round(s$max, 2)))
        )
      )
    })
    do.call(tagList, cards)
  })

  # Correlation plot
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
        "Heatmap" = create_heatmap(rv$stats$correlation),
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

    showElement("ts-results")

    if (!is.null(result$forecast_plot)) {
      output$ts_plot <- renderPlotly(result$forecast_plot)
    }

    output$ts_summary <- renderUI({
      tags$div(class = "ts-info",
        tags$p(paste("Data points:", result$summary$n_points)),
        tags$p(paste("Range:", result$summary$start, "to", result$summary$end)),
        tags$p(paste("CV:", round(result$trend$cv, 2), "%"))
      )
    })
  })

  # Report generation
  observeEvent(input$generate_html, {
    req(rv$data, rv$stats)
    showNotification("Generating report...", type = "message")
    result <- generate_report(rv$data, rv$stats, "html")
    if (result$success) {
      showNotification("Report generated!", type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
