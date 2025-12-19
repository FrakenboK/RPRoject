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
