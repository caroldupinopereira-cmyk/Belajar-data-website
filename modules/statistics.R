# ============================================================
# Module: Statistics Calculator
# Advanced descriptive statistics
# ============================================================

calculate_statistics <- function(data) {
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]

  if (ncol(numeric_data) == 0) {
    return(list(error = "Tidak ada kolom numerik ditemukan"))
  }

  stats <- list()

  for (col in names(numeric_data)) {
    x <- na.omit(numeric_data[[col]])

    stats[[col]] <- list(
      # Central Tendency
      mean = mean(x),
      median = median(x),
      mode = as.numeric(names(sort(table(x), decreasing = TRUE)[1])),

      # Dispersion
      sd = sd(x),
      variance = var(x),
      range = max(x) - min(x),
      iqr = IQR(x),

      # Position
      min = min(x),
      max = max(x),
      q1 = quantile(x, 0.25),
      q3 = quantile(x, 0.75),

      # Shape
      skewness = calculate_skewness(x),
      kurtosis = calculate_kurtosis(x),

      # Count
      n = length(x),
      n_missing = sum(is.na(numeric_data[[col]]))
    )
  }

  # Correlation matrix
  if (ncol(numeric_data) >= 2) {
    stats$correlation <- cor(numeric_data, use = "pairwise.complete.obs")
  }

  return(stats)
}

calculate_skewness <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

calculate_kurtosis <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  ((n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - m) / s)^4) -
    (3 * (n - 1)^2) / ((n - 2) * (n - 3))
}

frequency_table <- function(x, bins = 10) {
  if (is.numeric(x)) {
    breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = bins + 1)
    table(cut(x, breaks = breaks, include.lowest = TRUE))
  } else {
    table(x)
  }
}
