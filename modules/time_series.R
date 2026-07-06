# ============================================================
# Module: Time Series Analysis
# Forecasting and trend analysis
# ============================================================

analyze_time_series <- function(data, date_col, value_col, forecast_periods = 12) {
  # Prepare time series data
  ts_data <- data.frame(
    date = as.Date(data[[date_col]]),
    value = as.numeric(data[[value_col]])
  )
  ts_data <- ts_data[order(ts_data$date), ]
  ts_data <- na.omit(ts_data)

  # Create ts object
  ts_obj <- ts(ts_data$value, frequency = 12)

  results <- list()

  # Basic info
  results$summary <- list(
    start = min(ts_data$date),
    end = max(ts_data$date),
    n_points = nrow(ts_data),
    frequency = 12
  )

  # Decomposition
  if (length(ts_data$value) >= 24) {
    decomp <- stl(ts_obj, s.window = "periodic")
    results$decomposition <- decomp
  }

  # ARIMA Forecast
  tryCatch({
    fit <- auto.arima(ts_obj)
    forecast_result <- forecast(fit, h = forecast_periods)

    results$arima <- list(
      model = fit,
      forecast = forecast_result,
      summary = summary(fit)
    )

    # Create forecast plot
    p <- autoplot(forecast_result) +
      theme_minimal() +
      labs(title = "ARIMA Forecast",
           x = "Time", y = "Value")
    results$forecast_plot <- ggplotly(p)
  }, error = function(e) {
    results$arima <- list(error = e$message)
  })

  # Trend analysis
  results$trend <- list(
    mean = mean(ts_data$value),
    sd = sd(ts_data$value),
    cv = sd(ts_data$value) / mean(ts_data$value) * 100
  )

  return(results)
}

detect_seasonality <- function(ts_obj, frequency = 12) {
  decomp <- stl(ts_obj, s.window = "periodic")
  seasonal_strength <- 1 - var(decomp$time.series[, "remainder"]) /
    var(decomp$time.series[, "remainder"] + decomp$time.series[, "seasonal"])

  list(
    has_seasonality = seasonal_strength > 0.1,
    strength = seasonal_strength,
    decomposition = decomp
  )
}
