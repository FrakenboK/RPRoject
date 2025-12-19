#!/usr/bin/env Rscript
# Быстрый запуск через R (без Docker)
# Использование: Rscript quick_start.R

cat("=== Проверка и установка зависимостей ===\n")
required_packages <- c(
  "shiny", "httr2", "jsonlite", "dplyr", "tidyr", 
  "stringr", "ipaddress", "ggplot2", "DT", "lubridate",
  "devtools"
)

missing_packages <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat(sprintf("Требуется установка: %s\n", pkg))
  } else {
    cat(sprintf("✓ %s установлен\n", pkg))
  }
}

if (length(missing_packages) > 0) {
  cat(sprintf("\nУстановка %d пакетов...\n", length(missing_packages)))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
  cat("Установка завершена!\n\n")
}

cat("\n=== Загрузка пакета yisasnxlooukup ===\n")
# Загрузка пакета
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools не установлен. Установите: install.packages('devtools')")
}

# Генерируем NAMESPACE если нужно
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2", repos = "https://cloud.r-project.org")
}

cat("Генерация документации...\n")
devtools::document(quiet = TRUE)

cat("Загрузка пакета...\n")
devtools::load_all()

cat("\n=== Запуск приложения ===\n")
cat("Приложение откроется в браузере автоматически.\n")
cat("Для остановки нажмите Ctrl+C\n\n")

# Запуск приложения (используем функцию напрямую, так как она загружена)
run_app()
