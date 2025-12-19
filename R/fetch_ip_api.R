#' Fetch IP address information from ip-api.com
#'
#' @param ips Character vector of IP addresses (IPv4/IPv6)
#' @param fields Character string of comma-separated fields to request from API
#' @param base_url Base URL for the API
#' @param timeout_sec Timeout in seconds for HTTP requests
#' @param sleep_sec Sleep time in seconds between requests (to respect rate limits)
#'
#' @return A tibble with one row per IP address, containing API response data
#' @export
#'
#' @examples
#' \dontrun{
#' ips <- c("8.8.8.8", "1.1.1.1")
#' result <- fetch_ip_api(ips)
#' }
fetch_ip_api <- function(ips,
                         fields = "status,as,lat,lon,timezone,asname,city,regionName,org,country,isp,message",
                         base_url = "http://ip-api.com/json",
                         timeout_sec = 10,
                         sleep_sec = 0) {
  
  if (!requireNamespace("ipaddress", quietly = TRUE)) {
    stop("Package 'ipaddress' is required for IP validation")
  }
  
  if (!is.character(ips) || length(ips) == 0) {
    stop("ips must be a non-empty character vector")
  }
  
  ips <- unique(ips[ips != ""])
  
  if (length(ips) == 0) {
    stop("No valid IP addresses provided")
  }
  
  find_project_root <- function() {
    current <- getwd()
    max_depth <- 10
    depth <- 0
    
    while (depth < max_depth) {
      if (file.exists(file.path(current, "DESCRIPTION"))) {
        return(current)
      }
      parent <- dirname(current)
      if (parent == current) break
      current <- parent
      depth <- depth + 1
    }
    
    return(getwd())
  }
  
  project_root <- find_project_root()
  log_file <- file.path(project_root, "yisasnxlooukup_api.log")
  
  log_msg <- function(msg) {
    cat(msg, "\n")
    flush.console()
    
    tryCatch({
      cat(msg, "\n", file = log_file, append = TRUE)
    }, error = function(e) {
      tryCatch({
        if (!file.exists(log_file)) {
          file.create(log_file)
        }
        cat(msg, "\n", file = log_file, append = TRUE)
      }, error = function(e2) {
        cat(sprintf("Warning: Could not write to log: %s\n", conditionMessage(e2)))
      })
    })
  }
  
  cat(sprintf("=== Log file: %s ===\n", log_file))
  cat(sprintf("=== Log file exists: %s ===\n", file.exists(log_file)))
  cat(sprintf("=== Log file writable: %s ===\n", file.access(log_file, mode = 2) == 0))
  log_msg(sprintf("=== Log started at %s ===", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  results <- list()
  lookup_ts <- lubridate::now(tzone = "UTC")
  
  for (i in seq_along(ips)) {
    ip <- ips[i]
    
    is_valid <- tryCatch({
      parsed <- ipaddress::ip_address(ip)
      if (length(parsed) > 0 && !is.na(parsed[1])) {
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) {
      ipv4_pattern <- "^([0-9]{1,3}\\.){3}[0-9]{1,3}$"
      ipv6_pattern <- "^([0-9a-fA-F]{0,4}:){2,7}[0-9a-fA-F]{0,4}$"
      grepl(ipv4_pattern, ip) || grepl(ipv6_pattern, ip)
    })
    
    if (!is_valid) {
      log_msg(sprintf("Invalid IP: %s", ip))
      results[[i]] <- list(
        ip = ip,
        status = "fail",
        message = "invalid ip",
        lookup_ts = lookup_ts
      )
      next
    }
    
    result <- tryCatch({
      url <- paste0(base_url, "/", ip, "?fields=", fields)
      url <- gsub("^https://", "http://", url)
      
      log_msg(url)
      
      req <- httr2::request(url) |>
        httr2::req_timeout(timeout_sec) |>
        httr2::req_options(followlocation = TRUE)
      
      resp <- httr2::req_perform(req)
      
      log_msg(sprintf("HTTP Status: %d", httr2::resp_status(resp)))
      
      json_data <- httr2::resp_body_json(resp)
      
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        json_str <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
        log_msg(sprintf("Response:\n%s", json_str))
      } else {
        log_msg(sprintf("Response: %s", paste(names(json_data), collapse = ", ")))
      }
      
      if (!is.null(json_data$status)) {
        log_msg(sprintf("API Status: %s", json_data$status))
      }
      
      json_data$ip <- ip
      json_data$lookup_ts <- lookup_ts
      
      json_data
    }, error = function(e) {
      error_msg <- conditionMessage(e)
      log_msg(sprintf("ERROR: %s", error_msg))
      log_msg(sprintf("Error class: %s", class(e)[1]))
      if (inherits(e, "http_error")) {
        log_msg(sprintf("HTTP Error details: %s", as.character(e)))
      }
      
      list(
        ip = ip,
        status = "fail",
        message = paste0("request failed: ", conditionMessage(e)),
        lookup_ts = lookup_ts
      )
    })
    
    results[[i]] <- result
    
    if (sleep_sec > 0 && i < length(ips)) {
      Sys.sleep(sleep_sec)
    }
  }
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    df <- dplyr::bind_rows(results)
  } else {
    all_cols <- unique(unlist(lapply(results, names)))
    results_df <- lapply(results, function(x) {
      row <- list()
      for (col in all_cols) {
        row[[col]] <- if (col %in% names(x)) x[[col]] else NA
      }
      row
    })
    df <- do.call(rbind, lapply(results_df, function(x) {
      data.frame(x, stringsAsFactors = FALSE)
    }))
  }
  
  return(df)
}
