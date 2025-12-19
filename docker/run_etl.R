#!/usr/bin/env Rscript
# ETL script for yisasnxlooukup: fetch IP data and save to data directory

library(yisasnxlooukup)

# Get data directory from environment variable
out_dir <- Sys.getenv("OURPACK_DATA_DIR", "/srv/ourpack/data")

# Create directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Get IP list from environment variable
ip_list_env <- Sys.getenv("IP_LIST", "")

if (ip_list_env == "") {
  # Default demo IP list if IP_LIST is not set
  ips <- c(
    "8.8.8.8",
    "1.1.1.1",
    "208.67.222.222",
    "208.67.220.220",
    "9.9.9.9",
    "149.112.112.112",
    "76.76.19.19",
    "76.223.122.150",
    "94.140.14.14",
    "94.140.15.15"
  )
  cat("Using default demo IP list (10 IPs)\n")
} else {
  # Parse IP list from environment variable (comma or space separated)
  ips <- unlist(strsplit(ip_list_env, "[,\\s]+"))
  ips <- ips[ips != ""]
  cat(sprintf("Using IP list from IP_LIST environment variable (%d IPs)\n", length(ips)))
}

if (length(ips) == 0) {
  stop("No IP addresses provided")
}

cat(sprintf("Starting ETL for %d IP addresses...\n", length(ips)))

# Step 1: Fetch data from API
cat("Fetching data from API...\n")
raw <- yisasnxlooukup::fetch_ip_api(ips)

# Step 2: Prepare and normalize data
cat("Preparing and normalizing data...\n")
df <- yisasnxlooukup::prepare_ipinfo(raw)

# Step 3: Calculate metrics
cat("Calculating metrics...\n")
metrics <- yisasnxlooukup::calc_ip_metrics(df)

# Step 4: Save results
ipinfo_path <- file.path(out_dir, "ipinfo_latest.rds")
metrics_path <- file.path(out_dir, "metrics_latest.rds")

cat(sprintf("Saving results to %s...\n", out_dir))
saveRDS(df, ipinfo_path)
saveRDS(metrics, metrics_path)

# Print summary
cat("\n=== ETL Summary ===\n")
cat(sprintf("Total IPs: %d\n", metrics$summary$n_ips_total))
cat(sprintf("Successful: %d\n", metrics$summary$n_ok))
cat(sprintf("Failed: %d\n", metrics$summary$n_fail))
cat(sprintf("Unique ASN: %d\n", metrics$summary$n_unique_asn))
cat(sprintf("Countries: %d\n", metrics$summary$n_countries))
cat(sprintf("\nFiles saved:\n"))
cat(sprintf("  - %s\n", ipinfo_path))
cat(sprintf("  - %s\n", metrics_path))
cat("ETL completed successfully!\n")

