#' API client configuration and helpers
#'
#' Functions for managing API requests and configuration
#'
#' @param base_url Base URL for the API
#' @param timeout_sec Timeout in seconds
#'
#' @return API client configuration
#' @export
create_api_client <- function(base_url = "http://ip-api.com/json", timeout_sec = 10) {
  list(
    base_url = base_url,
    timeout_sec = timeout_sec,
    fields = "status,as,lat,lon,timezone,asname,city,regionName,org,country,isp,message"
  )
}

build_api_url <- function(client, ip) {
  url <- paste0(client$base_url, "/", ip, "?fields=", client$fields)
  gsub("^https://", "http://", url)
}

make_api_request <- function(url, timeout_sec = 10) {
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_options(followlocation = TRUE)
  
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

handle_api_error <- function(error, ip) {
  error_msg <- conditionMessage(error)
  
  list(
    ip = ip,
    status = "fail",
    message = paste0("request failed: ", error_msg),
    lookup_ts = lubridate::now(tzone = "UTC")
  )
}

format_api_response <- function(json_data, ip, lookup_ts) {
  json_data$ip <- ip
  json_data$lookup_ts <- lookup_ts
  json_data
}

get_default_fields <- function() {
  "status,as,lat,lon,timezone,asname,city,regionName,org,country,isp,message"
}

validate_api_response <- function(response) {
  if (is.null(response)) {
    return(FALSE)
  }
  
  if (!is.list(response)) {
    return(FALSE)
  }
  
  if (!"status" %in% names(response)) {
    return(FALSE)
  }
  
  TRUE
}

extract_error_message <- function(response) {
  if (is.null(response)) {
    return("Unknown error")
  }
  
  if ("message" %in% names(response)) {
    return(response$message)
  }
  
  if ("error" %in% names(response)) {
    return(response$error)
  }
  
  "Unknown error"
}

prepare_batch_request <- function(ips, client, sleep_sec = 0) {
  results <- list()
  lookup_ts <- lubridate::now(tzone = "UTC")
  
  for (i in seq_along(ips)) {
    ip <- ips[i]
    
    result <- tryCatch({
      url <- build_api_url(client, ip)
      json_data <- make_api_request(url, client$timeout_sec)
      
      if (validate_api_response(json_data)) {
        format_api_response(json_data, ip, lookup_ts)
      } else {
        handle_api_error(list(message = extract_error_message(json_data)), ip)
      }
    }, error = function(e) {
      handle_api_error(e, ip)
    })
    
    results[[i]] <- result
    
    if (sleep_sec > 0 && i < length(ips)) {
      Sys.sleep(sleep_sec)
    }
  }
  
  results
}

