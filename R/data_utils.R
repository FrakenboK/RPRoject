#' Utility functions for IP data processing
#'
#' Additional helper functions for data validation and processing
#'
#' @param df Data frame with IP information
#' @param ip IP address string
#'
#' @return Processed data or validation result
#' @export
validate_ip_format <- function(ip) {
  if (is.null(ip) || is.na(ip) || ip == "") {
    return(FALSE)
  }
  
  ipv4_pattern <- "^([0-9]{1,3}\\.){3}[0-9]{1,3}$"
  ipv6_pattern <- "^([0-9a-fA-F]{0,4}:){2,7}[0-9a-fA-F]{0,4}$"
  
  grepl(ipv4_pattern, ip) || grepl(ipv6_pattern, ip)
}

filter_valid_ips <- function(ips) {
  if (length(ips) == 0) {
    return(character(0))
  }
  
  valid_ips <- character(0)
  for (ip in ips) {
    if (validate_ip_format(ip)) {
      valid_ips <- c(valid_ips, ip)
    }
  }
  
  unique(valid_ips)
}

normalize_ip_list <- function(ips) {
  if (is.null(ips) || length(ips) == 0) {
    return(character(0))
  }
  
  ips <- unlist(strsplit(ips, "\\s+|,"))
  ips <- ips[ips != ""]
  ips <- trimws(ips)
  ips <- ips[ips != ""]
  unique(ips)
}

extract_country_info <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  
  if (!"country" %in% names(df)) {
    return(data.frame())
  }
  
  country_info <- df |>
    dplyr::filter(!is.na(country), status == "success") |>
    dplyr::select(ip, country, regionName, city) |>
    dplyr::distinct()
  
  return(country_info)
}

extract_asn_info <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  
  if (!"asn" %in% names(df)) {
    return(data.frame())
  }
  
  asn_info <- df |>
    dplyr::filter(!is.na(asn), status == "success") |>
    dplyr::select(ip, asn, asname, org) |>
    dplyr::distinct()
  
  return(asn_info)
}

format_lookup_timestamp <- function(timestamp) {
  if (is.null(timestamp)) {
    return(NA_character_)
  }
  
  if (inherits(timestamp, "POSIXct")) {
    return(format(timestamp, "%Y-%m-%d %H:%M:%S %Z"))
  }
  
  as.character(timestamp)
}

get_api_response_summary <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      total = 0,
      success = 0,
      failed = 0,
      success_rate = 0
    ))
  }
  
  total <- nrow(df)
  success <- sum(df$status == "success", na.rm = TRUE)
  failed <- total - success
  success_rate <- if (total > 0) round(success / total * 100, 2) else 0
  
  return(list(
    total = total,
    success = success,
    failed = failed,
    success_rate = success_rate
  ))
}

prepare_export_data <- function(df, format = "csv") {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }
  
  export_cols <- c("ip", "status", "asn", "asname", "country", "regionName",
                   "city", "org", "isp", "timezone", "lat", "lon")
  
  export_cols <- intersect(export_cols, names(df))
  
  if (length(export_cols) == 0) {
    return(data.frame())
  }
  
  df_export <- df[, export_cols, drop = FALSE]
  
  if (inherits(df_export, "tbl_df") || inherits(df_export, "tbl")) {
    df_export <- as.data.frame(df_export)
  }
  
  for (col in names(df_export)) {
    if (is.list(df_export[[col]])) {
      df_export[[col]] <- unlist(df_export[[col]])
    }
  }
  
  return(df_export)
}

validate_data_structure <- function(df) {
  required_cols <- c("ip", "status")
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  if (nrow(df) == 0) {
    return(list(
      valid = FALSE,
      message = "Data frame is empty"
    ))
  }
  
  return(list(
    valid = TRUE,
    message = "Data structure is valid"
  ))
}

