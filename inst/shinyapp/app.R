library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(yisasnxlooukup)
library(RColorBrewer)

ui <- fluidPage(
  titlePanel("IP Address Information Lookup"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "ips",
        "IP Addresses (one per line):",
        rows = 10,
        placeholder = "8.8.8.8\n1.1.1.1\n..."
      ),
      actionButton("fetch", "Fetch", class = "btn-primary"),
      br(), br(),
      actionButton("load_cached", "Load Cached", class = "btn-info"),
      br(), br(),
      h5("Или загрузите из CSV файла:"),
      fileInput(
        "csv_file",
        "Выберите CSV файл с IP адресами",
        accept = c(".csv", ".txt"),
        buttonLabel = "Выбрать файл",
        placeholder = "Файл не выбран"
      ),
      br(),
      checkboxInput("sleep", "Делать паузу между запросами", value = FALSE),
      conditionalPanel(
        condition = "input.sleep == true",
        numericInput("sleep_sec", "Пауза (секунды):", value = 1, min = 0.1, max = 10, step = 0.1)
      )
    ),
    
    mainPanel(
      h3("Summary"),
      verbatimTextOutput("summary"),
      br(),
      h3("IP Address Details"),
      downloadButton("download_csv", "Export to CSV", class = "btn-success"),
      br(), br(),
      DT::dataTableOutput("table"),
      br(),
      h3("Top Countries"),
      plotOutput("plot_countries"),
      br(),
      h3("Top ASN"),
      plotOutput("plot_asn")
    )
  )
)

server <- function(input, output, session) {
  
  data_dir <- Sys.getenv("OURPACK_DATA_DIR", "")
  if (data_dir == "" || (data_dir == "/srv/ourpack/data" && !dir.exists(data_dir))) {
    current_wd <- getwd()
    project_root <- current_wd
    
    if (!file.exists(file.path(current_wd, "DESCRIPTION"))) {
      parent_dir <- dirname(current_wd)
      if (file.exists(file.path(parent_dir, "DESCRIPTION"))) {
        project_root <- parent_dir
      }
    }
    
    data_dir <- file.path(project_root, "data")
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  df_reactive <- reactiveVal(NULL)
  metrics_reactive <- reactiveVal(NULL)
  
  load_cached_data <- function() {
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
  
  observe({
    cached <- load_cached_data()
    if (cached$success) {
      df_reactive(cached$df)
      metrics_reactive(cached$metrics)
      showNotification("Loaded cached data on startup", type = "message", duration = 3)
    }
  })
  
  observeEvent(input$load_cached, {
    cached <- load_cached_data()
    if (cached$success) {
      df_reactive(cached$df)
      metrics_reactive(cached$metrics)
      showNotification("Cached data loaded successfully!", type = "message", duration = 3)
    } else {
      showNotification(
        paste("Failed to load cached data:", cached$message),
        type = "error",
        duration = 10
      )
    }
  })
  
  observeEvent(input$csv_file, {
    req(input$csv_file)
    
    tryCatch({
      file_content <- readLines(input$csv_file$datapath, warn = FALSE)
      
      ips_from_file <- unlist(strsplit(file_content, "\\s+|,"))
      ips_from_file <- ips_from_file[ips_from_file != ""]
      ips_from_file <- trimws(ips_from_file)
      ips_from_file <- ips_from_file[ips_from_file != ""]
      
      ips_from_file <- unique(ips_from_file)
      
      if (length(ips_from_file) == 0) {
        showNotification("CSV файл не содержит IP адресов", type = "error", duration = 5)
        return()
      }
      
      updateTextAreaInput(
        session,
        "ips",
        value = paste(ips_from_file, collapse = "\n")
      )
      
      showNotification(
        sprintf("Загружено %d IP адресов из файла %s", length(ips_from_file), input$csv_file$name),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste("Ошибка при чтении CSV файла:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })
  
  parse_ips <- function(text) {
    if (is.null(text) || text == "") {
      return(character(0))
    }
    
    ips <- unlist(strsplit(text, "\\s+"))
    ips <- ips[ips != ""]
    ips <- unique(ips)
    
    return(ips)
  }
  
  observeEvent(input$fetch, {
    
    ips <- parse_ips(input$ips)
    
    validate(
      need(length(ips) > 0, "Please enter at least one IP address")
    )
    
    validate(
      need(length(ips) <= 200, paste("Too many IP addresses. Maximum is 200, you provided", length(ips)))
    )
    
    showNotification("Fetching IP information...", type = "message", duration = NULL, id = "fetch_progress")
    
    tryCatch({
      sleep_sec <- if (isTRUE(input$sleep)) {
        if (is.null(input$sleep_sec) || is.na(input$sleep_sec)) 0 else input$sleep_sec
      } else {
        0
      }
      
      df_raw <- yisasnxlooukup::fetch_ip_api(ips, sleep_sec = sleep_sec)
      
      df <- yisasnxlooukup::prepare_ipinfo(df_raw)
      
      metrics <- yisasnxlooukup::calc_ip_metrics(df)
      
      df_reactive(df)
      metrics_reactive(metrics)
      
      tryCatch({
        if (!dir.exists(data_dir)) {
          dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
        }
        
        ipinfo_path <- file.path(data_dir, "ipinfo_latest.rds")
        metrics_path <- file.path(data_dir, "metrics_latest.rds")
        
        saveRDS(df, ipinfo_path)
        saveRDS(metrics, metrics_path)
        
        showNotification(
          paste("Data fetched and cached successfully!", sprintf("Cache: %s", data_dir)),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Data fetched successfully, but cache save failed:", conditionMessage(e)),
          type = "warning",
          duration = 5
        )
      })
      
      removeNotification("fetch_progress")
      
    }, error = function(e) {
      removeNotification("fetch_progress")
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10)
    })
  })
  
  output$summary <- renderText({
    metrics <- metrics_reactive()
    if (is.null(metrics)) {
      return("No data yet. Enter IP addresses and click Fetch.")
    }
    
    s <- metrics$summary
    paste0(
      "Total IPs: ", s$n_ips_total, "\n",
      "Successful: ", s$n_ok, "\n",
      "Failed: ", s$n_fail, "\n",
      "Unique ASN: ", s$n_unique_asn, "\n",
      "Countries: ", s$n_countries
    )
  })
  
  output$table <- DT::renderDataTable({
    df <- df_reactive()
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame())
    }
    
    if (inherits(df, "tbl_df") || inherits(df, "tbl")) {
      df <- as.data.frame(df)
    }
    
    cols_to_show <- c("ip", "status", "asn", "asname", "country", "regionName",
                      "city", "org", "isp", "timezone")
    
    cols_to_show <- intersect(cols_to_show, names(df))
    
    if (length(cols_to_show) == 0) {
      return(data.frame())
    }
    
    df_display <- df[, cols_to_show, drop = FALSE]
    
    for (col in names(df_display)) {
      if (is.list(df_display[[col]])) {
        df_display[[col]] <- unlist(df_display[[col]])
      }
    }
    
    DT::datatable(
      df_display,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("ip_addresses_", timestamp, ".csv")
    },
    content = function(file) {
      df <- df_reactive()
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      if (inherits(df, "tbl_df") || inherits(df, "tbl")) {
        df <- as.data.frame(df)
      }
      
      cols_to_export <- c("ip", "status", "asn", "asname", "country", "regionName",
                          "city", "org", "isp", "timezone", "lat", "lon")
      
      cols_to_export <- intersect(cols_to_export, names(df))
      
      df_export <- df[, cols_to_export, drop = FALSE]
      
      write.csv(df_export, file, row.names = FALSE, na = "")
    }
  )
  
  output$plot_countries <- renderPlot({
    metrics <- metrics_reactive()
    if (is.null(metrics) || nrow(metrics$by_country) == 0) {
      return(NULL)
    }
    
    top_countries <- metrics$by_country |>
      dplyr::filter(!is.na(country)) |>
      head(10) |>
      dplyr::mutate(country_ordered = reorder(country, n))
    
    if (nrow(top_countries) == 0) {
      return(NULL)
    }
    
    n_colors <- nrow(top_countries)
    colors <- RColorBrewer::brewer.pal(min(n_colors, 11), "Set3")[1:n_colors]
    if (n_colors > 11) {
      colors <- rainbow(n_colors)
    }
    
    ggplot(top_countries, aes(x = country_ordered, y = n, fill = country_ordered)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.1, size = 3.5, color = "black") +
      scale_fill_manual(values = colors, guide = "none") +
      coord_flip() +
      labs(
        x = "Country",
        y = "Number of IPs",
        title = "Top Countries"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$plot_asn <- renderPlot({
    metrics <- metrics_reactive()
    if (is.null(metrics) || nrow(metrics$by_asn) == 0) {
      return(NULL)
    }
    
    top_asn <- metrics$by_asn |>
      dplyr::filter(!is.na(asn)) |>
      head(10) |>
      dplyr::mutate(
        asn_label = paste0("AS", asn),
        asn_label_full = ifelse(!is.na(asname) & asname != "", 
                                paste0("AS", asn, " - ", asname),
                                paste0("AS", asn)),
        asn_ordered = reorder(asn_label, n)
      )
    
    if (nrow(top_asn) == 0) {
      return(NULL)
    }
    
    n_colors <- nrow(top_asn)
    colors <- RColorBrewer::brewer.pal(min(n_colors, 11), "Pastel1")[1:n_colors]
    if (n_colors > 11) {
      colors <- terrain.colors(n_colors)
    }
    
    ggplot(top_asn, aes(x = asn_ordered, y = n, fill = asn_ordered)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.1, size = 3.5, color = "black") +
      scale_fill_manual(values = colors, guide = "none") +
      coord_flip() +
      labs(
        x = "ASN",
        y = "Number of IPs",
        title = "Top ASN"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
}

shinyApp(ui = ui, server = server)

