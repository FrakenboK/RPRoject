#' Helper functions for Shiny application
#'
#' Functions for data processing and reactive logic in Shiny app
#'
#' @param df_reactive Reactive value containing data frame
#' @param metrics_reactive Reactive value containing metrics
#' @param data_dir Data directory path
#'
#' @return Helper functions for Shiny
#' @export
load_cached_data_helper <- function(data_dir) {
  function() {
    ipinfo_path <- file.path(data_dir, "ipinfo_latest.rds")
    metrics_path <- file.path(data_dir, "metrics_latest.rds")
    
    if (!file.exists(ipinfo_path)) {
      return(list(success = FALSE, message = paste("Cached data not found at:", ipinfo_path)))
    }
    
    tryCatch({
      df <- readRDS(ipinfo_path)
      
      if (file.exists(metrics_path)) {
        metrics <- readRDS(metrics_path)
      } else {
        metrics <- yisasnxlooukup::calc_ip_metrics(df)
      }
      
      return(list(success = TRUE, df = df, metrics = metrics))
    }, error = function(e) {
      return(list(success = FALSE, message = paste("Error loading cache:", conditionMessage(e))))
    })
  }
}

parse_ips_helper <- function(text) {
  if (is.null(text) || text == "") {
    return(character(0))
  }
  
  ips <- unlist(strsplit(text, "\\s+"))
  ips <- ips[ips != ""]
  ips <- unique(ips)
  
  return(ips)
}

process_fetched_data <- function(df_raw) {
  df <- yisasnxlooukup::prepare_ipinfo(df_raw)
  metrics <- yisasnxlooukup::calc_ip_metrics(df)
  return(list(df = df, metrics = metrics))
}

save_data_cache <- function(df, metrics, data_dir) {
  tryCatch({
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    ipinfo_path <- file.path(data_dir, "ipinfo_latest.rds")
    metrics_path <- file.path(data_dir, "metrics_latest.rds")
    
    saveRDS(df, ipinfo_path)
    saveRDS(metrics, metrics_path)
    
    return(list(success = TRUE, message = paste("Data cached successfully at:", data_dir)))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Cache save failed:", conditionMessage(e))))
  })
}

process_csv_upload <- function(file_path) {
  tryCatch({
    file_content <- readLines(file_path, warn = FALSE)
    
    ips_from_file <- unlist(strsplit(file_content, "\\s+|,"))
    ips_from_file <- ips_from_file[ips_from_file != ""]
    ips_from_file <- trimws(ips_from_file)
    ips_from_file <- ips_from_file[ips_from_file != ""]
    
    ips_from_file <- unique(ips_from_file)
    
    if (length(ips_from_file) == 0) {
      return(list(success = FALSE, message = "CSV файл не содержит IP адресов", ips = character(0)))
    }
    
    return(list(success = TRUE, message = sprintf("Загружено %d IP адресов", length(ips_from_file)), ips = ips_from_file))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Ошибка при чтении CSV файла:", conditionMessage(e)), ips = character(0)))
  })
}

