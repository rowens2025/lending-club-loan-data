
# Libraries
library(shiny)
library(bslib)
library(arrow)
library(dplyr)
library(janitor)
library(ggplot2)
library(here)
library(lubridate)
library(shinyWidgets)
library(scales)

# theme - mostly from css
app_theme <- bs_theme(
  version = 5,
  bg = "#0b1220",
  fg = "#eaf1fb",
  primary = "#49c5aa",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

# load
load_model_table <- function() {
  arrow::read_parquet(here::here("data", "processed", "loans_model_table.parquet")) |>
    janitor::clean_names()
}

load_survival_table <- function() {
  arrow::read_parquet(here::here("data", "processed", "loans_survival_table.parquet")) |>
    janitor::clean_names()
}

# formatting
fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)

fmt_pct <- function(x, digits = 1) paste0(round(100 * x, digits), "%")

fmt_usd <- function(x, digits = 2) {
  if (is.na(x)) return("N/A")
  ax <- abs(x)
  if (ax >= 1e9) return(paste0("$", round(x / 1e9, digits), "B"))
  if (ax >= 1e6) return(paste0("$", round(x / 1e6, 0), "M"))
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}


ui_kpi <- function(label, value_id, sub = NULL, style = c("default","teal","lime","orange")) {
  style <- match.arg(style)
  div(
    class = paste("tw-kpi", if (style != "default") style else ""),
    div(class = "label", label),
    div(class = "value", textOutput(value_id, inline = TRUE)),
    if (!is.null(sub)) div(class = "sub", sub)
  )
}

page_shell <- function(sidebar_ui, hero_ui, body_ui) {
  div(
    class = "tw-shell",
    div(class = "tw-sidebar", sidebar_ui),
    div(
      class = "tw-main",
      div(class = "tw-hero", hero_ui),
      body_ui
    )
  )
}

# plot theme
theme_tw_plot <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      axis.text  = element_text(color = "#0b1220"),
      axis.title = element_text(color = "#0b1220"),
      axis.ticks = element_line(color = "grey60"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# one for each page, cannot find a way to make reusable
filters_ui_overview <- function() {
  tagList(
    h5("Filters", style="font-size:14px;margin-bottom:10px;"),
    helpText("Filter the portfolio and charts. Tip: exclude recent vintages that haven’t matured."),
    uiOutput("filter_issue_range_ov"),
    uiOutput("filter_grade_ov"),
    uiOutput("filter_term_ov"),
    uiOutput("filter_purpose_ov"),
    br(),
    actionButton("reset_filters_ov", "Reset Filters", class = "btn btn-sm")
  )
}

filters_ui_drivers <- function() {
  tagList(
    h5("Filters", style="font-size:14px;margin-bottom:10px;"),
    helpText("Same idea as Overview, but scoped to this tab."),
    uiOutput("filter_issue_range_dr"),
    uiOutput("filter_grade_dr"),
    uiOutput("filter_term_dr"),
    uiOutput("filter_purpose_dr"),
    br(),
    actionButton("reset_filters_dr", "Reset Filters", class = "btn btn-sm")
  )
}

# dashboard
ui <- page_navbar(
  title = "Financial Credit Risk Lab",
  theme = app_theme,
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
# Tab 1 - Explore
  nav_panel(
    "Explore — Overview",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title",
            div(class = "tw-logo", "TW"),
            div("TRACKWISE • Risk Console")
        ),
        div(class = "tw-side-pill",
            strong("Explore"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "Portfolio overview + defaults over time."
            )
        ),
        div(class = "tw-side-group", filters_ui_overview())
      ),
      
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Portfolio Snapshot • LendingClub (2007–2018)"),
        div(class = "tw-kpis",
            ui_kpi("Total loans", "kpi_total_loans_ov"),
            ui_kpi("Defaults (count)", "kpi_default_count_ov", style = "orange"),
            ui_kpi("Defaulted balance", "kpi_defaulted_balance_ov", style = "teal"),
            ui_kpi("Default rate", "kpi_default_rate_ov", style = "teal"),
            ui_kpi("Median FICO", "kpi_median_fico_ov", style = "lime")
        )
      ),
      
      body_ui = div(
        class = "tw-cards",
        div(
          class = "tw-card tw-plot",
          h4("Default rate over time (issue month)"),
          plotOutput("plot_default_trend_ov", height = 320)
        ),
        div(
          class = "tw-card tw-plot",
          h4("Default rate by grade (quick view)"),
          plotOutput("plot_by_grade_ov", height = 320)
        )
      )
    )
  ),
  
# Tab 2 drivers
  nav_panel(
    "Explore — Drivers",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title",
            div(class = "tw-logo", "TW"),
            div("TRACKWISE • Risk Console")
        ),
        div(class = "tw-side-pill",
            strong("Drivers"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "Default drivers under filters."
            )
        ),
        div(class = "tw-side-group", filters_ui_drivers())
      ),
      
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Risk Drivers • Exploratory View"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "Default rates by FICO band and purpose.")
      ),
      
      body_ui = div(
        class = "tw-cards",
        div(
          class = "tw-card tw-plot",
          h4("Default rate vs FICO (binned)"),
          plotOutput("plot_fico_bins_dr", height = 320)
        ),
        div(
          class = "tw-card tw-plot",
          h4("Default rate by purpose (top 12 within filter)"),
          plotOutput("plot_purpose_dr", height = 320)
        )
      )
    )
  ),
  
# Tab 3 - model
  nav_panel(
    "Model — Default PD",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title", div(class = "tw-logo", "TW"), div("TRACKWISE • Risk Console")),
        div(class = "tw-side-pill",
            strong("PD Model"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "ROC/AUC, lift, and feature signals."
            )
        )
      ),
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Origination PD • Model Results"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "place holder for description")
      ),
      body_ui = div(class="tw-cards",
                    div(class="tw-card", h4("Coming next"), p("ROC/AUC + decile lift table + importance.")),
                    div(class="tw-card", h4("Coming next"), p("Calibration + threshold view.")))
    )
  ),
  
# Tab 4 - survivability model
  nav_panel(
    "Model — Survivability",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title", div(class = "tw-logo", "TW"), div("TRACKWISE • Risk Console")),
        div(class = "tw-side-pill",
            strong("Survival Model"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "Time-to-default & early-default capture."
            )
        )
      ),
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Survivability • Model Results"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "Cox artifacts place holder")
      ),
      body_ui = div(class="tw-cards",
                    div(class="tw-card", h4("Coming next"), p("Survival curves by risk bucket.")),
                    div(class="tw-card", h4("Coming next"), p("6/9/12 month capture tables.")))
    )
  ),
  
# Tab 5 test
  nav_panel(
    "Try It — Loan Input",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title", div(class = "tw-logo", "TW"), div("TRACKWISE • Risk Console")),
        div(class = "tw-side-pill",
            strong("Loan Scoring"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "Input loan details → PD + survivability."
            )
        )
      ),
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Single Loan • Scoring Demo"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "place holder for form")
      ),
      body_ui = div(class="tw-cards",
                    div(class="tw-card", h4("Coming next"), p("Loan input form.")),
                    div(class="tw-card", h4("Coming next"), p("Predicted PD + survival output.")))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load once
  dt_model <- load_model_table()
  
  # Normalize once
  dt_model <- dt_model %>%
    mutate(
      issue_dt = as.Date(paste0(issue_ym, "-01")),
      grade    = as.character(grade),
      purpose  = trimws(as.character(purpose)),
      term     = as.integer(term)
    )
  
  observe({
    min_dt <- min(dt_model$issue_dt, na.rm = TRUE)
    max_dt <- max(dt_model$issue_dt, na.rm = TRUE)
    
    default_end <- max_dt %m-% months(12)
    if (is.na(default_end) || default_end < min_dt) default_end <- max_dt
    
    # issue range
    output$filter_issue_range_ov <- renderUI({
      dateRangeInput("issue_range_ov", "Issue month range",
                     start = min_dt, end = default_end, min = min_dt, max = max_dt,
                     format = "yyyy-mm")
    })
    
    # grade
    output$filter_grade_ov <- renderUI({
      choices <- sort(unique(dt_model$grade))
      pickerInput("grade_sel_ov", "Grade",
                  choices = choices, selected = choices, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"))
    })
    
    # term
    output$filter_term_ov <- renderUI({
      choices <- sort(unique(dt_model$term))
      pickerInput("term_sel_ov", "Term",
                  choices = as.character(choices),
                  selected = as.character(choices),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE))
    })
    
    # purpose
    output$filter_purpose_ov <- renderUI({
      top_purpose <- dt_model %>%
        count(purpose, sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull(purpose) %>%
        as.character()
      
      pickerInput("purpose_sel_ov", "Purpose (top 10)",
                  choices = top_purpose, selected = top_purpose, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2"))
    })
  })
  
# drivers ui
  observe({
    min_dt <- min(dt_model$issue_dt, na.rm = TRUE)
    max_dt <- max(dt_model$issue_dt, na.rm = TRUE)
    
    default_end <- max_dt %m-% months(12)
    if (is.na(default_end) || default_end < min_dt) default_end <- max_dt
    
    output$filter_issue_range_dr <- renderUI({
      dateRangeInput("issue_range_dr", "Issue month range",
                     start = min_dt, end = default_end, min = min_dt, max = max_dt,
                     format = "yyyy-mm")
    })
    
    output$filter_grade_dr <- renderUI({
      choices <- sort(unique(dt_model$grade))
      pickerInput("grade_sel_dr", "Grade",
                  choices = choices, selected = choices, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"))
    })
    
    output$filter_term_dr <- renderUI({
      choices <- sort(unique(dt_model$term))
      pickerInput("term_sel_dr", "Term",
                  choices = as.character(choices),
                  selected = as.character(choices),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE))
    })
    
    output$filter_purpose_dr <- renderUI({
      top_purpose <- dt_model %>%
        count(purpose, sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull(purpose) %>%
        as.character()
      
      pickerInput("purpose_sel_dr", "Purpose (top 10)",
                  choices = top_purpose, selected = top_purpose, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2"))
    })
  })
  

  observeEvent(input$reset_filters_ov, {
    min_dt <- min(dt_model$issue_dt, na.rm = TRUE)
    max_dt <- max(dt_model$issue_dt, na.rm = TRUE)
    default_end <- max_dt %m-% months(12)
    if (is.na(default_end) || default_end < min_dt) default_end <- max_dt
    
    updateDateRangeInput(session, "issue_range_ov", start = min_dt, end = default_end)
    
    grades <- sort(unique(dt_model$grade))
    updatePickerInput(session, "grade_sel_ov", selected = grades)
    
    terms <- sort(unique(dt_model$term))
    updatePickerInput(session, "term_sel_ov", selected = as.character(terms))
    
    top_purpose <- dt_model %>%
      count(purpose, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(purpose) %>%
      as.character()
    updatePickerInput(session, "purpose_sel_ov", choices = top_purpose, selected = top_purpose)
  })
  

  observeEvent(input$reset_filters_dr, {
    min_dt <- min(dt_model$issue_dt, na.rm = TRUE)
    max_dt <- max(dt_model$issue_dt, na.rm = TRUE)
    default_end <- max_dt %m-% months(12)
    if (is.na(default_end) || default_end < min_dt) default_end <- max_dt
    
    updateDateRangeInput(session, "issue_range_dr", start = min_dt, end = default_end)
    
    grades <- sort(unique(dt_model$grade))
    updatePickerInput(session, "grade_sel_dr", selected = grades)
    
    terms <- sort(unique(dt_model$term))
    updatePickerInput(session, "term_sel_dr", selected = as.character(terms))
    
    top_purpose <- dt_model %>%
      count(purpose, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(purpose) %>%
      as.character()
    updatePickerInput(session, "purpose_sel_dr", choices = top_purpose, selected = top_purpose)
  })
  

  filtered_overview <- reactive({
    dt <- dt_model
    
    if (!is.null(input$issue_range_ov) && length(input$issue_range_ov) == 2) {
      dt <- dt %>% filter(issue_dt >= input$issue_range_ov[1], issue_dt <= input$issue_range_ov[2])
    }
    if (!is.null(input$grade_sel_ov) && length(input$grade_sel_ov) > 0) {
      dt <- dt %>% filter(grade %in% input$grade_sel_ov)
    }
    if (!is.null(input$term_sel_ov) && length(input$term_sel_ov) > 0) {
      term_int <- suppressWarnings(as.integer(input$term_sel_ov))
      term_int <- term_int[!is.na(term_int)]
      if (length(term_int) > 0) dt <- dt %>% filter(term %in% term_int)
    }
    if (!is.null(input$purpose_sel_ov) && length(input$purpose_sel_ov) > 0) {
      dt <- dt %>% filter(purpose %in% input$purpose_sel_ov)
    }
    
    dt
  })
  
  filtered_drivers <- reactive({
    dt <- dt_model
    
    if (!is.null(input$issue_range_dr) && length(input$issue_range_dr) == 2) {
      dt <- dt %>% filter(issue_dt >= input$issue_range_dr[1], issue_dt <= input$issue_range_dr[2])
    }
    if (!is.null(input$grade_sel_dr) && length(input$grade_sel_dr) > 0) {
      dt <- dt %>% filter(grade %in% input$grade_sel_dr)
    }
    if (!is.null(input$term_sel_dr) && length(input$term_sel_dr) > 0) {
      term_int <- suppressWarnings(as.integer(input$term_sel_dr))
      term_int <- term_int[!is.na(term_int)]
      if (length(term_int) > 0) dt <- dt %>% filter(term %in% term_int)
    }
    if (!is.null(input$purpose_sel_dr) && length(input$purpose_sel_dr) > 0) {
      dt <- dt %>% filter(purpose %in% input$purpose_sel_dr)
    }
    
    dt
  })
  
 # KPIs
  output$kpi_total_loans_ov <- renderText({
    dt <- filtered_overview()
    fmt_int(nrow(dt))
  })
  
  output$kpi_default_count_ov <- renderText({
    dt <- filtered_overview()
    fmt_int(sum(dt$default_flag == 1, na.rm = TRUE))
  })
  
  output$kpi_defaulted_balance_ov <- renderText({
    dt <- filtered_overview()
    bal <- sum(dt$loan_amnt[dt$default_flag == 1], na.rm = TRUE)
    fmt_usd(bal, digits = 2)
  })
  
  output$kpi_default_rate_ov <- renderText({
    dt <- filtered_overview()
    fmt_pct(mean(dt$default_flag, na.rm = TRUE), 1)
  })
  
  output$kpi_median_fico_ov <- renderText({
    dt <- filtered_overview()
    round(median(dt$fico_mid, na.rm = TRUE))
  })
  
# plots
  output$plot_default_trend_ov <- renderPlot({
    dt <- filtered_overview()
    req(nrow(dt) > 0)
    
    df <- dt %>%
      group_by(issue_dt) %>%
      summarise(default_rate = mean(default_flag, na.rm = TRUE), .groups = "drop") %>%
      arrange(issue_dt)
    
    ggplot(df, aes(x = issue_dt, y = default_rate)) +
      geom_line(linewidth = 1.1, color = "#49c5aa") +
      geom_point(size = 1.6, color = "#b7d84b") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      labs(x = "Issue month", y = "Default rate") +
      theme_tw_plot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_by_grade_ov <- renderPlot({
    dt <- filtered_overview()
    req(nrow(dt) > 0)
    
    dt <- dt %>% mutate(grade = factor(grade, levels = c("A","B","C","D","E","F","G")))
    
    df <- dt %>%
      group_by(grade) %>%
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups = "drop")
    
    ggplot(df, aes(x = grade, y = default_rate)) +
      geom_col(fill = "#1f3850") +
      geom_text(
        aes(label = percent(default_rate, accuracy = 0.1)),
        color = "white",
        size = 3.4,
        vjust = 1.6
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Grade", y = "Default rate") +
      theme_tw_plot()
  })
  
# plot drivers
  output$plot_fico_bins_dr <- renderPlot({
    dt <- filtered_drivers()
    req(nrow(dt) > 0)
    
    df <- dt %>%
      filter(!is.na(fico_mid), !is.na(default_flag)) %>%
      mutate(fico_bin = cut(fico_mid, breaks = seq(580, 860, by = 20), include.lowest = TRUE)) %>%
      group_by(fico_bin) %>%
      summarise(default_rate = mean(default_flag), n = n(), .groups = "drop")
    
    ggplot(df, aes(x = fico_bin, y = default_rate)) +
      geom_col(fill = "#1f3850") +
      geom_line(aes(group = 1), linewidth = 1, color = "#49c5aa") +
      geom_point(color = "#b7d84b", size = 1.6) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "FICO band", y = "Default rate") +
      theme_tw_plot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  })
  
  output$plot_purpose_dr <- renderPlot({
    dt <- filtered_drivers()
    req(nrow(dt) > 0)
    
    df <- dt %>%
      group_by(purpose) %>%
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      slice_head(n = 12) %>%
      arrange(default_rate)
    
    ggplot(df, aes(x = reorder(purpose, default_rate), y = default_rate)) +
      geom_col(fill = "#1f3850") +
      geom_point(color = "#b7d84b", size = 2) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Default rate") +
      theme_tw_plot()
  })
  
}

shinyApp(ui, server)

