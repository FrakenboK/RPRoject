#' Calculate metrics from cleaned IP information data
#'
#' @param df_clean Cleaned data frame from prepare_ipinfo()
#'
#' @return A list containing:
#'   - summary: tibble with general metrics
#'   - by_country: count of IPs by country
#'   - by_asn: count of IPs by ASN/asname
#'   - geo_ok: rows with valid lat/lon for mapping
#' @export
#'
#' @examples
#' \dontrun{
#' df <- prepare_ipinfo(fetch_ip_api(c("8.8.8.8", "1.1.1.1")))
#' metrics <- calc_ip_metrics(df)
#' metrics$summary
#' }
calc_ip_metrics <- function(df_clean) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required")
  }
  
  df <- dplyr::as_tibble(df_clean)
  
  summary <- dplyr::tibble(
    n_ips_total = nrow(df),
    n_ok = sum(df$status == "success", na.rm = TRUE),
    n_fail = sum(df$status != "success", na.rm = TRUE),
    n_unique_asn = dplyr::n_distinct(df$asn[df$status == "success" & !is.na(df$asn)]),
    n_countries = dplyr::n_distinct(df$country[df$status == "success" & !is.na(df$country)])
  )
  
  by_country <- df |>
    dplyr::filter(status == "success") |>
    dplyr::count(country, sort = TRUE, name = "n")
  
  by_asn <- df |>
    dplyr::filter(status == "success") |>
    dplyr::count(asn, asname, sort = TRUE, name = "n")
  
  geo_ok <- df |>
    dplyr::filter(status == "success", !is.na(lat), !is.na(lon))
  
  return(list(
    summary = summary,
    by_country = by_country,
    by_asn = by_asn,
    geo_ok = geo_ok
  ))
}
