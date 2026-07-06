# ============================================================
# Module: Visualizations
# Interactive charts using plotly + ggplot2
# ============================================================

create_scatter_plot <- function(data, x_col, y_col, color_col = NULL) {
  p <- ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(aes_string(color = color_col), size = 3, alpha = 0.7) +
    theme_minimal() +
    labs(title = paste(y_col, "vs", x_col))

  if (!is.null(color_col)) {
    p <- p + scale_color_brewer(palette = "Set2")
  }

  ggplotly(p)
}

create_bar_plot <- function(data, x_col, y_col = NULL) {
  if (is.null(y_col)) {
    p <- ggplot(data, aes_string(x = x_col)) +
      geom_bar(fill = "#3498db", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Distribusi", x_col))
  } else {
    p <- ggplot(data, aes_string(x = x_col, y = y_col)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste(y_col, "per", x_col))
  }
  ggplotly(p)
}

create_histogram <- function(data, col, bins = 30) {
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(bins = bins, fill = "#2ecc71", alpha = 0.8, color = "white") +
    theme_minimal() +
    labs(title = paste("Histogram", col))
  ggplotly(p)
}

create_box_plot <- function(data, y_col, group_col = NULL) {
  p <- ggplot(data, aes_string(y = y_col, fill = group_col)) +
    geom_boxplot(alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Box Plot", y_col))

  if (!is.null(group_col)) {
    p <- p + scale_fill_brewer(palette = "Set2")
  }

  ggplotly(p)
}

create_line_plot <- function(data, x_col, y_col, color_col = NULL) {
  p <- ggplot(data, aes_string(x = x_col, y = y_col, group = color_col, color = color_col)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = paste(y_col, "over", x_col))
  ggplotly(p)
}

create_heatmap <- function(cor_matrix) {
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Var1", "Var2", "Correlation")

  p <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = round(Correlation, 2)), size = 3) +
    scale_fill_gradient2(low = "#e74c3c", high = "#3498db", mid = "white", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Heatmap")

  ggplotly(p)
}

create_pie_chart <- function(data, col) {
  freq <- as.data.frame(table(data[[col]]))
  names(freq) <- c("Category", "Count")

  p <- ggplot(freq, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    labs(title = paste("Pie Chart", col))
  ggplotly(p)
}
