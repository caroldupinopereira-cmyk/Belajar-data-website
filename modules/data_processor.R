# ============================================================
# Module: Data Processor
# Handles file upload and data loading
# ============================================================

load_data <- function(file_path) {
  ext <- tools::file_ext(file_path)

  tryCatch({
    data <- switch(ext,
      "csv" = read.csv(file_path, stringsAsFactors = FALSE),
      "txt" = read.csv(file_path, sep = "\t", stringsAsFactors = FALSE),
      "tsv" = read.csv(file_path, sep = "\t", stringsAsFactors = FALSE),
      "xls" = readxl::read_excel(file_path),
      "xlsx" = readxl::read_excel(file_path),
      stop("Format file tidak didukung!")
    )
    return(list(success = TRUE, data = data, message = "Data berhasil dimuat"))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Error:", e$message)))
  })
}

get_data_summary <- function(data) {
  list(
    rows = nrow(data),
    cols = ncol(data),
    columns = names(data),
    types = sapply(data, class),
    head = head(data, 10),
    missing = colSums(is.na(data)),
    missing_total = sum(is.na(data))
  )
}

clean_data <- function(data, action = "remove_na") {
  switch(action,
    "remove_na" = na.omit(data),
    "fill_mean" = {
      data[] <- lapply(data, function(x) {
        if (is.numeric(x)) x[is.na(x)] <- mean(x, na.rm = TRUE)
        x
      })
      data
    },
    "fill_median" = {
      data[] <- lapply(data, function(x) {
        if (is.numeric(x)) x[is.na(x)] <- median(x, na.rm = TRUE)
        x
      })
      data
    },
    data
  )
}
