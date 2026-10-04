# =============================================================================
# Mapping Reported Racism in Canada (2018-2020) — Shiny app
#
# Data are prepared by R/01_clean.R and read from app/data/.
# Run locally from the repository root:  shiny::runApp("app")
# Deploy:                                 rsconnect::deployApp("app")
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr)
  library(lubridate); library(leaflet); library(sf); library(plotly); library(DT)
})

incidents   <- readRDS("data/incidents.rds")
labels_long <- readRDS("data/labels_long.rds")
provinces   <- readRDS("data/provinces.rds")

# ---- palette (one hue for magnitude; neutral ink for text) -------------------
blue <- "#2a78d6"; ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#8a8984"
seq_blues <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

date_min <- min(incidents$date, na.rm = TRUE)
date_max <- max(incidents$date, na.rm = TRUE)
communities <- sort(unique(labels_long$label[labels_long$field == "Targeted community"]))
categories  <- sort(unique(incidents$category))
breakdown_fields <- c("Type of incident", "Context", "Identity-based", "Targeted community")

key_events <- tibble(
  date  = ymd(c("2019-10-21", "2020-03-11")),
  label = c("Federal election", "WHO declares COVID-19 pandemic")
)

plotly_clean <- function(p) {
  p |>
    config(displayModeBar = FALSE) |>
    layout(font = list(family = "Inter, system-ui, sans-serif", color = ink2),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           margin = list(l = 10, r = 20, t = 10, b = 10))
}

# =============================================================================
# UI
# =============================================================================
filters <- list(
  dateRangeInput("dates", "Incident date", start = date_min, end = date_max,
                 min = date_min, max = date_max),
  selectizeInput("community", "Targeted community", choices = communities,
                 multiple = TRUE, options = list(placeholder = "All communities")),
  checkboxGroupInput("category", "Incident category", choices = categories,
                     selected = categories),
  actionLink("reset", "Reset filters"),
  hr(),
  p(class = "small text-muted",
    "Counts are media-reported incidents, not official crime statistics. See About for limitations.")
)

ui <- page_navbar(
  title = "Mapping Reported Racism in Canada",
  theme = bs_theme(version = 5, primary = blue, base_font = font_google("Inter", local = FALSE),
                   bg = "#fcfcfb", fg = ink),
  sidebar = sidebar(width = 290, filters),
  fillable = FALSE,

  # ---- Overview ---------------------------------------------------------------
  nav_panel(
    "Overview",
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box("Incidents", textOutput("vb_n"), p(textOutput("vb_span", inline = TRUE))),
      value_box("Non-criminal", textOutput("vb_noncrim"), p("of incidents in selection")),
      value_box("Led to charges", textOutput("vb_charged"), p("of incidents in selection")),
      value_box("Busiest month", textOutput("vb_peak"), p(textOutput("vb_peak_n", inline = TRUE)))
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        span("Where incidents were reported"),
        radioButtons("map_metric", NULL, inline = TRUE,
                     choices = c("Per 100,000 residents" = "rate", "Incident count" = "count"))
      ),
      leafletOutput("map", height = 520),
      card_footer(class = "small text-muted",
        "Rates use Statistics Canada 2021 Census population. In the rate view, areas with fewer than",
        "10 incidents are shown in grey because their rates are unstable. Click a province for details.")
    ),
    card(
      card_header("Top cities"),
      plotlyOutput("city_bar", height = 380),
      card_footer(class = "small text-muted", textOutput("city_missing", inline = TRUE))
    )
  ),

  # ---- Trends -------------------------------------------------------------------
  nav_panel(
    "Trends",
    card(
      full_screen = TRUE,
      card_header("Incidents per month"),
      plotlyOutput("timeline", height = 380),
      card_footer(class = "small text-muted",
        "Data end in June 2020, so 2020 is a half year: compare months, not calendar years.")
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        span("What kind of incidents?"),
        selectInput("breakdown", NULL, choices = breakdown_fields, width = "220px")
      ),
      plotlyOutput("breakdown_bar", height = 420),
      card_footer(class = "small text-muted",
        "An incident can carry several labels (e.g. hate speech and vandalism), so bars can sum to more than the incident total. Incidents with no label in this field are not shown.")
    )
  ),

  # ---- Data -----------------------------------------------------------------------
  nav_panel(
    "Data",
    card(
      card_header("Incident records (filtered)"),
      DTOutput("table", fill = FALSE),
      card_footer(downloadButton("download", "Download filtered CSV", class = "btn-sm"))
    )
  ),

  # ---- About -----------------------------------------------------------------------
  nav_panel(
    "About",
    card(
      card_body(
        h4("About this project"),
        p("An interactive exploration of 1,000 racist and identity-based incidents in Canada",
          "reported in news media between January 2018 and June 2020. Each record links to its source article."),
        h5("Methods"),
        tags$ul(
          tags$li("Fields holding several labels in one cell (incident type, context, identity, community) are split so each label is counted once per incident."),
          tags$li("Provincial rates divide incident counts by 2021 Census population; provinces and territories with fewer than 10 incidents are flagged as unstable."),
          tags$li("Missing province, city and gender values are kept as explicit categories rather than dropped.")
        ),
        h5("Limitations"),
        tags$ul(
          tags$li(strong("Media-reported, not prevalence. "),
                  "Incidents enter the data only if a news outlet covered them, so spikes can reflect news attention as well as events."),
          tags$li(strong("English-language sources. "),
                  "None of the source articles come from French-language outlets, so Quebec is very likely under-counted."),
          tags$li(strong("Partial 2020. "), "Coverage ends 26 June 2020."),
          tags$li(strong("Sparse fields. "),
                  "Victim gender is missing for about 65% of incidents; province is missing for about 6%.")
        ),
        h5("Code"),
        p("Cleaning, analysis and app code: ",
          a("github.com/SallyQahl/canada-racism-incidents",
            href = "https://github.com/SallyQahl/canada-racism-incidents", target = "_blank"))
      )
    )
  ),
  nav_spacer(),
  nav_item(a("GitHub", href = "https://github.com/SallyQahl/canada-racism-incidents",
             target = "_blank"))
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start = date_min, end = date_max)
    updateSelectizeInput(session, "community", selected = character(0))
    updateCheckboxGroupInput(session, "category", selected = categories)
  })

  filtered <- reactive({
    req(input$dates)
    d <- incidents |>
      filter(is.na(date) | (date >= input$dates[1] & date <= input$dates[2]),
             category %in% input$category)
    if (length(input$community)) {
      ids <- labels_long |>
        filter(field == "Targeted community", label %in% input$community) |>
        pull(incident_id)
      d <- d |> filter(incident_id %in% ids)
    }
    d
  })

  filtered_labels <- reactive(labels_long |> filter(incident_id %in% filtered()$incident_id))

  pct <- function(x) if (length(x) == 0 || is.nan(x)) "—" else sprintf("%.0f%%", x * 100)

  # ---- value boxes -------------------------------------------------------------
  output$vb_n       <- renderText(format(nrow(filtered()), big.mark = ","))
  output$vb_span    <- renderText(sprintf("%s to %s", format(input$dates[1], "%b %Y"),
                                          format(input$dates[2], "%b %Y")))
  output$vb_noncrim <- renderText(pct(mean(filtered()$category == "Non-Criminal")))
  output$vb_charged <- renderText(pct(mean(filtered()$category == "Criminal: Charged")))
  peak <- reactive({
    filtered() |> filter(!is.na(month)) |> count(month) |> slice_max(n, n = 1, with_ties = FALSE)
  })
  output$vb_peak   <- renderText(if (nrow(peak())) format(peak()$month, "%b %Y") else "—")
  output$vb_peak_n <- renderText(if (nrow(peak())) sprintf("%d incidents", peak()$n) else "")

  # ---- map ----------------------------------------------------------------------
  prov_data <- reactive({
    counts <- filtered() |> filter(province != "Not specified") |> count(province, name = "n")
    provinces |>
      select(-incidents, -per_100k) |>
      left_join(counts, by = "province") |>
      mutate(n = coalesce(n, 0L),
             rate = n / population_2021 * 1e5,
             value = if (input$map_metric == "rate") rate else n,
             unstable = n < 10)
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -96, lat = 58, zoom = 3)
  })

  observe({
    p <- prov_data()
    is_rate <- input$map_metric == "rate"
    # Tiny counts over tiny populations (the territories, PEI) produce extreme,
    # meaningless rates. In rate view they are greyed out instead of coloured.
    hide <- is_rate & p$unstable
    shown <- p$value[!hide]
    pal <- colorNumeric(seq_blues, domain = c(0, max(c(shown, 1e-9))))
    fill <- ifelse(hide, "#d9d8d3", pal(p$value))
    metric_label <- if (is_rate) "Incidents per 100k" else "Incidents"
    hover <- ifelse(
      hide, sprintf("%s: %d incidents (too few for a stable rate)", p$province, p$n),
      if (is_rate) sprintf("%s: %.1f per 100k (%d incidents)", p$province, p$rate, p$n)
      else sprintf("%s: %d incidents", p$province, p$n)
    )
    popup <- sprintf(
      "<strong>%s</strong><br>%d incidents<br>%.1f per 100,000 residents%s",
      p$province, p$n, p$rate,
      ifelse(p$unstable, "<br><em>Fewer than 10 incidents: rate is unstable</em>", "")
    )
    proxy <- leafletProxy("map", data = p) |>
      clearShapes() |> clearControls() |>
      addPolygons(fillColor = fill, fillOpacity = 0.85,
                  color = "#ffffff", weight = 1.2,
                  dashArray = ifelse(p$unstable, "4", ""),
                  label = hover, popup = popup,
                  highlightOptions = highlightOptions(weight = 2.5, color = ink, bringToFront = TRUE)) |>
      addLegend("bottomright", pal = pal, values = shown, title = metric_label, opacity = 0.85)
    if (any(hide)) {
      proxy |> addLegend("bottomleft", colors = "#d9d8d3", opacity = 0.85,
                         labels = "Fewer than 10 incidents")
    }
  })

  # ---- top cities -------------------------------------------------------------
  output$city_bar <- renderPlotly({
    d <- filtered() |> filter(city != "Not specified") |>
      count(city, province, sort = TRUE) |> slice_head(n = 15) |>
      mutate(city = factor(city, levels = rev(city)))
    validate(need(nrow(d) > 0, "No incidents match these filters."))
    plot_ly(d, x = ~n, y = ~city, type = "bar", orientation = "h",
            marker = list(color = blue),
            text = ~n, textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y} (%{customdata})<br>%{x} incidents<extra></extra>",
            customdata = ~province) |>
      layout(xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
             yaxis = list(title = ""), bargap = 0.35) |>
      plotly_clean()
  })
  output$city_missing <- renderText({
    k <- sum(filtered()$city == "Not specified")
    sprintf("%d incidents in this selection have no city recorded and are not shown.", k)
  })

  # ---- timeline ----------------------------------------------------------------------
  output$timeline <- renderPlotly({
    d <- filtered() |> filter(!is.na(month)) |> count(month) |>
      complete(month = seq(floor_date(input$dates[1], "month"),
                           floor_date(input$dates[2], "month"), by = "month"),
               fill = list(n = 0))
    validate(need(sum(d$n) > 0, "No incidents match these filters."))
    ev <- key_events |> filter(date >= input$dates[1], date <= input$dates[2])
    p <- plot_ly(d, x = ~month, y = ~n, type = "scatter", mode = "lines+markers",
                 line = list(color = blue, width = 2), marker = list(color = blue, size = 6),
                 hovertemplate = "%{x|%B %Y}<br>%{y} incidents<extra></extra>") |>
      layout(xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "", rangemode = "tozero", gridcolor = "#e6e5e0"),
             hovermode = "x unified")
    if (nrow(ev)) {
      p <- p |> layout(
        shapes = lapply(ev$date, function(x) list(
          type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
          line = list(color = muted, width = 1, dash = "dot"))),
        annotations = lapply(seq_len(nrow(ev)), function(i) list(
          x = ev$date[i], y = 1 - 0.08 * (i - 1), yref = "paper", text = ev$label[i],
          showarrow = FALSE, xanchor = "left", xshift = 4, yanchor = "top",
          font = list(size = 11, color = ink2)))
      )
    }
    plotly_clean(p)
  })

  # ---- breakdown -------------------------------------------------------------------
  output$breakdown_bar <- renderPlotly({
    d <- filtered_labels() |>
      filter(field == input$breakdown) |>
      count(short, sort = TRUE) |>
      mutate(short = factor(short, levels = rev(short)))
    validate(need(nrow(d) > 0, "No labelled incidents for this field in the selection."))
    plot_ly(d, x = ~n, y = ~short, type = "bar", orientation = "h",
            marker = list(color = blue), text = ~n, textposition = "outside",
            cliponaxis = FALSE,
            hovertemplate = "%{y}<br>%{x} incidents<extra></extra>") |>
      layout(xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
             yaxis = list(title = ""), bargap = 0.45) |>
      plotly_clean()
  })

  # ---- data table ---------------------------------------------------------------------
  table_data <- reactive({
    filtered() |>
      arrange(desc(date)) |>
      transmute(Date = date, Province = province, City = city,
                Description = sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>',
                                      url, htmltools::htmlEscape(description)),
                Category = category, `Targeted community` = community,
                Context = context, Type = incident_type)
  })
  output$table <- renderDT({
    datatable(table_data(), escape = FALSE, rownames = FALSE, filter = "top",
              class = "compact stripe",
              options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
                             columnDefs = list(list(width = "34%", targets = 3),
                                               list(className = "dt-nowrap", targets = 0))))
  })
  output$download <- downloadHandler(
    filename = function() sprintf("racism-incidents-canada-%s.csv", Sys.Date()),
    content  = function(file) readr::write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
