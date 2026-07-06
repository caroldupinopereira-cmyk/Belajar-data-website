# Intelligent Data Analytics Dashboard

Platform analisis data cerdas untuk eksplorasi data otomatis (Auto EDA).

## Fitur

- **Upload Data** - CSV, TXT, XLS, XLSX
- **Data Exploration** - Preview, summary, missing data
- **Statistical Analysis** - Mean, median, std dev, skewness, kurtosis, correlation
- **Interactive Visualization** - Scatter, Bar, Histogram, Box Plot, Line, Heatmap, Pie
- **Time Series Analysis** - ARIMA forecasting, decomposition
- **Report Generation** - Auto HTML reports

## Tech Stack

- **Backend:** R (Shiny)
- **Frontend:** HTML, CSS, JavaScript
- **Libraries:** ggplot2, plotly, forecast, DT, dplyr

## How to Run

1. Install R and RStudio
2. Install required packages:
   ```r
   install.packages(c("shiny", "shinyjs", "DT", "plotly", "dplyr", "forecast"))
   ```
3. Open `app.R` in RStudio
4. Click **Run App**

## Project Structure

```
├── app.R              # Main Shiny application
├── global.R           # Global settings
├── modules/           # R modules
│   ├── data_processor.R
│   ├── statistics.R
│   ├── visualizations.R
│   ├── time_series.R
│   └── report.R
├── www/               # Frontend assets
│   ├── css/style.css
│   └── js/main.js
├── uploads/           # Uploaded files
└── reports/           # Generated reports
```

## Author

Carol Dupinopereira - Pemrograman Sains Data

## Acknowledgments

Bakti Siregar, M.Sc. - Course Instructor
