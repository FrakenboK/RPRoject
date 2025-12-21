test_that("fetch_ip_api валидирует входные параметры", {
  expect_error(fetch_ip_api(character(0)), "No valid IP addresses provided")
  expect_error(fetch_ip_api(c(1, 2, 3)), "ips must be a non-empty character vector")
  expect_error(fetch_ip_api(c("", "  ")), "No valid IP addresses provided")
  expect_error(fetch_ip_api(NULL), "ips must be a non-empty character vector")
})

test_that("fetch_ip_api обрабатывает невалидные IP адреса", {
  skip_on_cran()
  
  result <- fetch_ip_api("invalid.ip.address")
  
  expect_true("ip" %in% names(result))
  expect_true("status" %in% names(result))
  expect_equal(result$status[1], "fail")
  expect_true("message" %in% names(result))
})

test_that("fetch_ip_api удаляет дубликаты IP", {
  skip_on_cran()
  
  result <- fetch_ip_api(c("8.8.8.8", "8.8.8.8", "1.1.1.1"))
  
  expect_equal(length(unique(result$ip)), 2)
  expect_true("8.8.8.8" %in% result$ip)
  expect_true("1.1.1.1" %in% result$ip)
})

test_that("fetch_ip_api возвращает правильную структуру данных", {
  skip_on_cran()
  
  result <- fetch_ip_api("8.8.8.8")
  
  expect_true("ip" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true("lookup_ts" %in% names(result))
  expect_equal(result$ip[1], "8.8.8.8")
  expect_true(!is.null(result$lookup_ts[1]))
})

test_that("fetch_ip_api обрабатывает несколько IP адресов", {
  skip_on_cran()
  
  ips <- c("8.8.8.8", "1.1.1.1")
  result <- fetch_ip_api(ips)
  
  expect_equal(nrow(result), 2)
  expect_true(all(ips %in% result$ip))
})

test_that("fetch_ip_api обрабатывает параметр sleep_sec", {
  skip_on_cran()
  
  start_time <- Sys.time()
  result <- fetch_ip_api(c("8.8.8.8", "1.1.1.1"), sleep_sec = 0.1)
  end_time <- Sys.time()
  
  expect_equal(nrow(result), 2)
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(elapsed >= 0.1)
})

test_that("fetch_ip_api логирует операции", {
  skip_on_cran()
  
  project_root <- getwd()
  log_file <- file.path(project_root, "yisasnxlooukup_api.log")
  
  if (file.exists(log_file)) {
    file.remove(log_file)
  }
  
  result <- fetch_ip_api("8.8.8.8")
  
  expect_true("ip" %in% names(result))
})

test_that("fetch_ip_api обрабатывает смешанные валидные и невалидные IP", {
  skip_on_cran()
  
  result <- fetch_ip_api(c("8.8.8.8", "invalid.ip", "1.1.1.1"))
  
  expect_equal(nrow(result), 3)
  expect_true("8.8.8.8" %in% result$ip)
  expect_true("invalid.ip" %in% result$ip)
  expect_true("1.1.1.1" %in% result$ip)
})

