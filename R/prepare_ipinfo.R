#' Prepare and normalize IP information data
#'
#' @param df_raw Raw data frame from fetch_ip_api()
#'
#' @return A normalized tibble with standardized columns and types
#' @export
#'
#' @examples
#' \dontrun{
#' raw <- fetch_ip_api(c("8.8.8.8"))
#' df <- prepare_ipinfo(raw)
#' }
prepare_ipinfo <- function(df_raw) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required")
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required")
  }
  
  df <- dplyr::as_tibble(df_raw)
  
  expected_cols <- c(
    "ip", "status", "as", "asn", "as_text", "asname", "country",
    "regionName", "city", "org", "isp", "timezone", "lat", "lon",
    "error", "message", "lookup_ts"
  )
  
  for (col in expected_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  if ("as" %in% names(df)) {
    asn_match <- stringr::str_match(df$as, "^AS(\\d+)")
    if (is.matrix(asn_match) && ncol(asn_match) >= 2) {
      df$asn <- ifelse(!is.na(asn_match[, 2]), 
                       as.integer(asn_match[, 2]), NA_integer_)
    } else {
      df$asn <- NA_integer_
    }
    df$as_text <- df$as
  } else {
    df$asn <- NA_integer_
    df$as_text <- NA_character_
  }
  
  if ("lat" %in% names(df)) {
    df$lat <- as.numeric(df$lat)
  }
  if ("lon" %in% names(df)) {
    df$lon <- as.numeric(df$lon)
  }
  
  char_cols <- c("status", "as", "as_text", "asname", "country", "regionName",
                 "city", "org", "isp", "timezone", "error", "message")
  
  for (col in char_cols) {
    if (col %in% names(df)) {
      df[[col]] <- stringr::str_trim(df[[col]])
      df[[col]] <- ifelse(df[[col]] == "", NA_character_, df[[col]])
    }
  }
  
  if ("ip" %in% names(df)) {
    df$ip <- as.character(df$ip)
  }
  
  if ("lookup_ts" %in% names(df)) {
    if (!inherits(df$lookup_ts, "POSIXct")) {
      df$lookup_ts <- lubridate::as_datetime(df$lookup_ts)
    }
  }
  
  df <- df[, intersect(expected_cols, names(df))]
  
  df <- df |>
    dplyr::distinct(ip, .keep_all = TRUE)
  
  return(df)
}
