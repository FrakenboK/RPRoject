test_that("prepare_ipinfo нормализует данные корректно", {
  df_raw <- data.frame(
    ip = c("8.8.8.8", "1.1.1.1"),
    status = c("success", "success"),
    as = c("AS15169", "AS13335"),
    asname = c("Google LLC", "Cloudflare, Inc."),
    country = c("United States", "United States"),
    regionName = c("California", "California"),
    city = c("Mountain View", "San Francisco"),
    org = c("Google LLC", "Cloudflare, Inc."),
    isp = c("Google LLC", "Cloudflare, Inc."),
    timezone = c("America/Los_Angeles", "America/Los_Angeles"),
    lat = c(37.4056, 37.7749),
    lon = c(-122.0775, -122.4194),
    stringsAsFactors = FALSE
  )
  
  result <- prepare_ipinfo(df_raw)
  
  expect_s3_class(result, "tbl_df")
  expect_true("ip" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true("asn" %in% names(result))
  expect_true("as_text" %in% names(result))
  expect_equal(result$asn[1], 15169L)
  expect_equal(result$asn[2], 13335L)
  expect_type(result$lat, "double")
  expect_type(result$lon, "double")
  expect_equal(nrow(result), length(unique(result$ip)))
})

test_that("prepare_ipinfo обрабатывает отсутствующие колонки", {
  df_raw <- data.frame(
    ip = "8.8.8.8",
    status = "success",
    stringsAsFactors = FALSE
  )
  
  result <- prepare_ipinfo(df_raw)
  
  expected_cols <- c("ip", "status", "asn", "as_text", "asname", "country",
                     "regionName", "city", "org", "isp", "timezone", "lat", "lon")
  
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("prepare_ipinfo обрабатывает ASN в разных форматах", {
  df_raw1 <- data.frame(
    ip = "8.8.8.8",
    status = "success",
    as = "AS15169",
    stringsAsFactors = FALSE
  )
  
  result1 <- prepare_ipinfo(df_raw1)
  expect_equal(result1$asn[1], 15169L)
  expect_equal(result1$as_text[1], "AS15169")
  
  df_raw2 <- data.frame(
    ip = "8.8.8.8",
    status = "success",
    stringsAsFactors = FALSE
  )
  
  result2 <- prepare_ipinfo(df_raw2)
  expect_true(is.na(result2$asn[1]))
  expect_true(is.na(result2$as_text[1]))
})

test_that("prepare_ipinfo обрабатывает неуспешные статусы", {
  df_raw <- data.frame(
    ip = c("8.8.8.8", "invalid.ip"),
    status = c("success", "fail"),
    message = c(NA, "invalid query"),
    stringsAsFactors = FALSE
  )
  
  result <- prepare_ipinfo(df_raw)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$status[1], "success")
  expect_equal(result$status[2], "fail")
})

test_that("prepare_ipinfo удаляет дубликаты IP", {
  df_raw <- data.frame(
    ip = c("8.8.8.8", "8.8.8.8", "1.1.1.1"),
    status = c("success", "success", "success"),
    stringsAsFactors = FALSE
  )
  
  result <- prepare_ipinfo(df_raw)
  
  expect_equal(nrow(result), 2)
  expect_equal(length(unique(result$ip)), 2)
})

test_that("prepare_ipinfo обрабатывает пустые строки", {
  df_raw <- data.frame(
    ip = "8.8.8.8",
    status = "success",
    city = "",
    country = "  ",
    stringsAsFactors = FALSE
  )
  
  result <- prepare_ipinfo(df_raw)
  
  expect_true(is.na(result$city[1]))
  expect_true(is.na(result$country[1]))
})

test_that("calc_ip_metrics рассчитывает метрики корректно", {
  df_clean <- data.frame(
    ip = c("8.8.8.8", "1.1.1.1", "208.67.222.222"),
    status = c("success", "success", "fail"),
    asn = c(15169L, 13335L, NA_integer_),
    asname = c("Google LLC", "Cloudflare, Inc.", NA_character_),
    country = c("United States", "United States", NA_character_),
    regionName = c("California", "California", NA_character_),
    city = c("Mountain View", "San Francisco", NA_character_),
    org = c("Google LLC", "Cloudflare, Inc.", NA_character_),
    isp = c("Google LLC", "Cloudflare, Inc.", NA_character_),
    timezone = c("America/Los_Angeles", "America/Los_Angeles", NA_character_),
    lat = c(37.4056, 37.7749, NA_real_),
    lon = c(-122.0775, -122.4194, NA_real_),
    stringsAsFactors = FALSE
  )
  
  metrics <- calc_ip_metrics(df_clean)
  
  expect_type(metrics, "list")
  expect_true("summary" %in% names(metrics))
  expect_true("by_country" %in% names(metrics))
  expect_true("by_asn" %in% names(metrics))
  expect_true("geo_ok" %in% names(metrics))
  expect_equal(metrics$summary$n_ips_total, 3)
  expect_equal(metrics$summary$n_ok, 2)
  expect_equal(metrics$summary$n_fail, 1)
  expect_equal(metrics$summary$n_unique_asn, 2)
  expect_equal(metrics$summary$n_countries, 1)
})

test_that("calc_ip_metrics корректно группирует по странам", {
  df_clean <- data.frame(
    ip = c("8.8.8.8", "1.1.1.1", "9.9.9.9"),
    status = c("success", "success", "success"),
    country = c("United States", "United States", "Germany"),
    asn = c(15169L, 13335L, 192L),
    asname = c("Google", "Cloudflare", "Quad9"),
    stringsAsFactors = FALSE
  )
  
  metrics <- calc_ip_metrics(df_clean)
  
  expect_s3_class(metrics$by_country, "tbl_df")
  expect_equal(nrow(metrics$by_country), 2)
  expect_true("United States" %in% metrics$by_country$country)
  expect_true("Germany" %in% metrics$by_country$country)
  us_count <- metrics$by_country$n[metrics$by_country$country == "United States"]
  expect_equal(us_count, 2)
})

test_that("calc_ip_metrics корректно группирует по ASN", {
  df_clean <- data.frame(
    ip = c("8.8.8.8", "8.8.4.4", "1.1.1.1"),
    status = c("success", "success", "success"),
    asn = c(15169L, 15169L, 13335L),
    asname = c("Google LLC", "Google LLC", "Cloudflare, Inc."),
    country = c("United States", "United States", "United States"),
    stringsAsFactors = FALSE
  )
  
  metrics <- calc_ip_metrics(df_clean)
  
  expect_s3_class(metrics$by_asn, "tbl_df")
  expect_equal(nrow(metrics$by_asn), 2)
  google_count <- metrics$by_asn$n[metrics$by_asn$asn == 15169L]
  expect_equal(google_count, 2)
})

test_that("calc_ip_metrics фильтрует только успешные записи", {
  df_clean <- data.frame(
    ip = c("8.8.8.8", "invalid.ip", "1.1.1.1"),
    status = c("success", "fail", "success"),
    country = c("United States", NA_character_, "United States"),
    asn = c(15169L, NA_integer_, 13335L),
    asname = c("Google", NA_character_, "Cloudflare"),
    stringsAsFactors = FALSE
  )
  
  metrics <- calc_ip_metrics(df_clean)
  
  expect_equal(sum(metrics$by_country$n), 2)
  expect_equal(sum(metrics$by_asn$n), 2)
})

test_that("calc_ip_metrics корректно фильтрует geo_ok", {
  df_clean <- data.frame(
    ip = c("8.8.8.8", "1.1.1.1", "9.9.9.9"),
    status = c("success", "success", "success"),
    lat = c(37.4056, 37.7749, NA_real_),
    lon = c(-122.0775, -122.4194, NA_real_),
    country = c("United States", "United States", "Germany"),
    stringsAsFactors = FALSE
  )
  
  metrics <- calc_ip_metrics(df_clean)
  
  expect_equal(nrow(metrics$geo_ok), 2)
  expect_true(all(!is.na(metrics$geo_ok$lat)))
  expect_true(all(!is.na(metrics$geo_ok$lon)))
})


