# ============================================================
# Intelligent Data Analytics Dashboard
# Pemrograman Sains Data - Proyek Akhir
# ============================================================

# Install packages if not installed
required_packages <- c(
  "shiny", "shinydashboard", "DT", "dplyr", "tidyr", "ggplot2",
  "plotly", "readxl", "writexl", "jsonlite", "rmarkdown",
  "forecast", "tseries", "corrplot", "scales", "knitr",
  "base64enc", "htmltools", "shinyjs"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

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

# Supported file types
SUPPORTED_FORMATS <- c(
  "csv", "txt", "tsv",
  "xls", "xlsx"
)
