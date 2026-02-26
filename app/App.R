# app.R
# ------------------------------------------------------------
# Financial Credit Risk Lab — Trackwise Risk Console (Shiny)
# Tabs: Explore Overview, Explore Drivers, Model Default PD, Try It Loan Input
# ------------------------------------------------------------

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
library(stringr)
library(pROC)
library(fmsb)

# ----------------------------
# Theme
# ----------------------------
app_theme <- bs_theme(
  version = 5,
  bg = "#0b1220",
  fg = "#eaf1fb",
  primary = "#49c5aa",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

# ----------------------------
# Load + normalize + ensure realized economics
# ----------------------------
load_model_table <- function() {
  dt <- arrow::read_parquet(here::here("data", "processed", "loans_model_table.parquet")) |>
    janitor::clean_names()
  
  # Ensure issue_ym and issue_dt
  dt <- dt |>
    mutate(
      issue_ym = as.character(issue_ym),
      issue_dt = as.Date(paste0(issue_ym, "-01"))
    )
  
  # Ensure realized economics exist, else compute if base econ exists.
  if (!all(c("realized_net_loss", "realized_net_profit") %in% names(dt))) {
    base_needed <- c("loan_amnt","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee")
    if (all(base_needed %in% names(dt))) {
      dt <- dt |>
        mutate(
          realized_net_loss   = loan_amnt - total_rec_prncp - recoveries + collection_recovery_fee,
          realized_net_profit = total_rec_prncp + total_rec_int + total_rec_late_fee + recoveries - collection_recovery_fee - loan_amnt
        )
    } else {
      dt <- dt |>
        mutate(realized_net_loss = NA_real_, realized_net_profit = NA_real_)
    }
  }
  
  dt
}

# ----------------------------
# Formatting helpers
# ----------------------------
fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x, digits = 1) paste0(round(100 * x, digits), "%")

fmt_usd <- function(x, digits = 2) {
  if (is.na(x)) return("N/A")
  ax <- abs(x)
  if (ax >= 1e9) return(paste0("$", round(x / 1e9, digits), "B"))
  if (ax >= 1e6) return(paste0("$", round(x / 1e6, 0), "M"))
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

# KPI UI component
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

# Plot theme (ggplot)
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

# ----------------------------
# Shared filter UI builder
# ----------------------------
filters_ui <- function(prefix, include_fico_band = FALSE) {
  tagList(
    h5("Filters", style="font-size:14px;margin-bottom:10px;"),
    helpText("Filters apply to this tab's KPIs + charts."),
    uiOutput(paste0("filter_issue_range_", prefix)),
    uiOutput(paste0("filter_grade_", prefix)),
    uiOutput(paste0("filter_term_", prefix)),
    uiOutput(paste0("filter_purpose_", prefix)),
    if (include_fico_band) uiOutput(paste0("filter_fico_band_", prefix)),
    br(),
    actionButton(paste0("reset_filters_", prefix), "Reset Filters", class = "btn btn-sm")
  )
}

# ----------------------------
# Compute PD model (train <=2015) and score 2016
# ----------------------------
feature_cols <- c(
  "fico_mid","dti","int_rate","loan_amnt","installment",
  "revol_util","open_acc","total_acc","inq_last_6mths","delinq_2yrs","pub_rec",
  "term","home_ownership","verification_status","purpose","addr_state"
)

# policy curve optimized via cumulative sums
compute_policy_curve_fast <- function(df_scored, grid = seq(0, 1, by = 0.01)) {
  df <- df_scored |>
    arrange(desc(pd)) |>
    mutate(
      realized_loss_pos = pmax(realized_net_loss, 0),
      realized_profit   = realized_net_profit
    )
  
  n <- nrow(df)
  if (n == 0) return(tibble())
  
  cum_defaults <- cumsum(df$default_flag == 1)
  cum_loss     <- cumsum(df$realized_loss_pos)
  cum_profit   <- cumsum(df$realized_profit)
  
  tot_defaults <- sum(df$default_flag == 1, na.rm = TRUE)
  tot_loss     <- sum(df$realized_loss_pos, na.rm = TRUE)
  tot_profit   <- sum(df$realized_profit, na.rm = TRUE)
  
  out <- lapply(grid, function(p) {
    k <- floor(n * p)
    if (k <= 0) {
      kept_defaults <- tot_defaults
      kept_loss <- tot_loss
      kept_profit <- tot_profit
      decline_n <- 0
    } else if (k >= n) {
      kept_defaults <- 0
      kept_loss <- 0
      kept_profit <- 0
      decline_n <- n
    } else {
      kept_defaults <- tot_defaults - cum_defaults[k]
      kept_loss     <- tot_loss     - cum_loss[k]
      kept_profit   <- tot_profit   - cum_profit[k]
      decline_n <- k
    }
    
    tibble(
      decline_pct = p,
      decline_n   = decline_n,
      kept_n      = n - decline_n,
      defaults_baseline = tot_defaults,
      defaults_kept     = kept_defaults,
      defaults_avoided  = tot_defaults - kept_defaults,
      loss_baseline     = tot_loss,
      loss_kept         = kept_loss,
      loss_avoided      = tot_loss - kept_loss,
      profit_baseline   = tot_profit,
      profit_kept       = kept_profit,
      delta_profit      = kept_profit - tot_profit
    )
  })
  
  bind_rows(out)
}

pd_to_decile <- function(pd_value, pd_ref) {
  pd_ref <- pd_ref[is.finite(pd_ref)]
  if (length(pd_ref) < 50 || !is.finite(pd_value)) return(NA_integer_)
  qs <- quantile(pd_ref, probs = seq(0, 1, by = 0.1), na.rm = TRUE, type = 7)
  d <- findInterval(pd_value, vec = qs, rightmost.closed = TRUE, all.inside = TRUE)
  as.integer(d)
}

# ----------------------------
# UI
# ----------------------------
ui <- page_navbar(
  title = "Financial Credit Risk Lab",
  theme = app_theme,
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # ---- Fix Tab 4 KPI width (and keep layout stable) ----
    tags$style(HTML("
      .tw-kpis.tw-kpis-wide .tw-kpi { min-width: 240px; }
      .tw-kpis.tw-kpis-wide { gap: 18px; flex-wrap: wrap; }
      .tw-kpis.tw-kpis-wide .tw-kpi .value { white-space: nowrap; }
    "))
  ),
  
  # ------------------------
  # Tab 1 — Explore Overview
  # ------------------------
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
        div(class = "tw-side-group", filters_ui("ov", include_fico_band = FALSE))
      ),
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Portfolio Snapshot • LendingClub (completed loans)"),
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
        div(class = "tw-card tw-plot",
            h4("Default rate over time (issue month)"),
            plotOutput("plot_default_trend_ov", height = 300)
        ),
        div(class = "tw-card tw-plot",
            h4("Default rate by grade (quick view)"),
            plotOutput("plot_by_grade_ov", height = 300)
        )
      )
    )
  ),
  
  # ------------------------
  # Tab 2 — Explore Drivers
  # ------------------------
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
        div(class = "tw-side-group", filters_ui("dr", include_fico_band = TRUE))
      ),
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Risk Drivers • Exploratory View"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "Default rates by FICO band and purpose (within current filters).")
      ),
      body_ui = div(
        class = "tw-cards",
        div(class="tw-card tw-plot",
            h4("Default rate vs FICO (binned)"),
            plotOutput("plot_fico_bins_dr", height = 300)
        ),
        div(class="tw-card tw-plot",
            h4("Purpose profile — radar view"),
            plotOutput("plot_purpose_radar_dr", height = 300)
        )
      )
    )
  ),
  
  # ------------------------
  # Tab 3 — Model Default PD
  # ------------------------
  nav_panel(
    "Model — Default PD",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title",
            div(class = "tw-logo", "TW"),
            div("TRACKWISE • Risk Console")
        ),
        div(class = "tw-side-pill",
            strong("PD Model"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "2016 holdout: PD deciles + realized profit policy curve."
            )
        ),
        div(class="tw-side-group",
            h5("Policy Controls", style="font-size:14px;margin-bottom:10px;"),
            helpText("Decline top X% of loans by predicted PD (within filters)."),
            sliderInput("decline_pct_md", "Decline top X% by PD", min = 0, max = 1, value = 0.30, step = 0.01),
            br(),
            div(style="color:rgba(234,241,251,0.75);font-size:12px;",
                "0% = approve all. 100% = decline everything. Uses realized economics."
            )
        ),
        div(class="tw-side-group", filters_ui("md", include_fico_band = TRUE))
      ),
      
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Origination PD • Model Results"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "Train ≤ 2015; evaluate + simulate on 2016 (holdout). Filters apply to KPIs + charts below."),
        div(class="tw-kpis",
            ui_kpi("Defaults avoided", "kpi_defaults_avoided_md", style="orange"),
            ui_kpi("Loss avoided", "kpi_loss_avoided_md", style="teal"),
            ui_kpi("Total profit (kept)", "kpi_profit_kept_md", style="lime"),
            ui_kpi("Δ Profit vs baseline", "kpi_delta_profit_md", style="teal")
        )
      ),
      
      body_ui = div(
        class="tw-cards",
        div(class="tw-card tw-plot",
            h4("Observed default rate by PD decile (2016 holdout)"),
            plotOutput("plot_default_by_decile_md", height = 280)
        ),
        div(class="tw-card tw-plot",
            h4("Profit impact curve (decline top X% by PD) — 0% to 100%"),
            plotOutput("plot_profit_curve_md", height = 280)
        ),
        div(class="tw-card",
            h4("Policy snapshot (selected decline %)"),
            textOutput("txt_policy_snapshot_md")
        )
      )
    )
  ),
  
  # ------------------------
  # Tab 4 — Try It Loan Input
  # ------------------------
  nav_panel(
    "Try It — Loan Input",
    page_shell(
      sidebar_ui = tagList(
        div(class = "tw-side-title",
            div(class = "tw-logo", "TW"),
            div("TRACKWISE • Risk Console")
        ),
        div(class = "tw-side-pill",
            strong("Loan Scoring"),
            div(style="margin-top:6px;color:rgba(234,241,251,0.7);font-size:12px;",
                "Enter loan inputs → PD + decile + decline decision."
            )
        ),
        div(class="tw-side-group",
            h5("Inputs", style="font-size:14px;margin-bottom:10px;"),
            numericInput("in_fico", "FICO (mid)", value = 680, min = 300, max = 850),
            numericInput("in_dti", "DTI", value = 15, min = 0, max = 60),
            numericInput("in_int_rate", "Interest rate (%)", value = 14, min = 0, max = 60),
            numericInput("in_loan_amnt", "Loan amount", value = 10000, min = 500, max = 50000),
            numericInput("in_installment", "Monthly installment", value = 350, min = 10, max = 2000),
            sliderInput("in_revol_util", "Revolving util (%)", min = 0, max = 100, value = 30),
            numericInput("in_open_acc", "Open accounts", value = 8, min = 0, max = 99),
            numericInput("in_total_acc", "Total accounts", value = 20, min = 0, max = 99),
            numericInput("in_inq", "Inquiries last 6m", value = 1, min = 0, max = 20),
            numericInput("in_delinq", "Delinquencies (2y)", value = 0, min = 0, max = 20),
            numericInput("in_pub_rec", "Public records", value = 0, min = 0, max = 20),
            uiOutput("in_term_ui"),
            uiOutput("in_home_ui"),
            uiOutput("in_ver_ui"),
            uiOutput("in_purpose_ui"),
            uiOutput("in_state_ui"),
            br(),
            sliderInput("decline_pct_try", "Decline top X% (decision threshold)", min = 0, max = 1, value = 0.30, step = 0.01)
        )
      ),
      
      hero_ui = tagList(
        div(style="font-weight:700;letter-spacing:0.3px;opacity:0.9;",
            "Single Loan • Scoring Demo"),
        div(style="margin-top:10px;color:rgba(234,241,251,0.7);font-size:12px;",
            "Scores with the same logistic PD model (train ≤ 2015) and compares against 2016 PD distribution.")
      ),
      
      body_ui = div(
        class="tw-cards",
        div(class="tw-card",
            h4("Score result"),
            # ---- wider KPI row so APPROVE/DECLINE fits ----
            div(class="tw-kpis tw-kpis-wide",
                ui_kpi("Predicted PD", "kpi_pd_try", style="teal"),
                ui_kpi("Risk decile", "kpi_decile_try", style="orange"),
                ui_kpi("Decision", "kpi_decision_try", style="lime")
            ),
            uiOutput("warn_oob_try")
        ),
        div(class="tw-card tw-plot",
            h4("Where your loan sits vs 2016 PD distribution"),
            plotOutput("plot_pd_hist_try", height = 320)
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  # Load once
  dt_model <- load_model_table()
  
  # Normalize types for modeling
  dt_model <- dt_model |>
    mutate(
      grade = as.character(grade),
      purpose = trimws(as.character(purpose)),
      term = suppressWarnings(as.integer(term)),
      home_ownership = as.factor(home_ownership),
      verification_status = as.factor(verification_status),
      purpose = as.factor(purpose),
      addr_state = as.factor(addr_state)
    )
  
  # Define FICO bins for filters/plots
  fico_breaks <- seq(580, 860, by = 20)
  dt_model <- dt_model |>
    mutate(fico_band = cut(fico_mid, breaks = fico_breaks, include.lowest = TRUE))
  
  # ----------------------------
  # Build train/test for PD model
  # ----------------------------
  train_end <- "2015-12"
  dt_train <- dt_model |>
    filter(issue_ym <= train_end) |>
    select(all_of(c("default_flag", feature_cols))) |>
    tidyr::drop_na()
  
  # Train model once
  model_glm <- glm(default_flag ~ ., data = dt_train, family = binomial(link = "logit"))
  
  # Store factor levels from training (critical for Tab 4 scoring)
  train_levels <- list(
    term = sort(unique(dt_train$term)),
    home_ownership = levels(dt_train$home_ownership),
    verification_status = levels(dt_train$verification_status),
    purpose = levels(dt_train$purpose),
    addr_state = levels(dt_train$addr_state)
  )
  
  # Numeric training ranges for OOD warnings (1%–99%)
  num_cols <- c("fico_mid","dti","int_rate","loan_amnt","installment","revol_util","open_acc","total_acc","inq_last_6mths","delinq_2yrs","pub_rec")
  train_ranges <- lapply(num_cols, function(cn) {
    x <- dt_train[[cn]]
    x <- x[is.finite(x)]
    if (length(x) < 100) return(c(lo = NA_real_, hi = NA_real_))
    qs <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE, type = 7)
    c(lo = as.numeric(qs[1]), hi = as.numeric(qs[2]))
  })
  names(train_ranges) <- num_cols
  
  # UI for Tab 4 selects based on training levels
  output$in_term_ui <- renderUI({
    selectInput("in_term", "Term (months)", choices = train_levels$term, selected = train_levels$term[1])
  })
  
  output$in_home_ui <- renderUI({
    choices <- train_levels$home_ownership
    choices <- choices[choices != "ANY"]
    
    default_choice <- if ("RENT" %in% choices) "RENT" else if ("MORTGAGE" %in% choices) "MORTGAGE" else choices[1]
    
    selectInput(
      "in_home",
      "Home Ownership",
      choices = choices,
      selected = default_choice
    )
  })
  
  output$in_ver_ui <- renderUI({
    selectInput("in_ver", "Verification status", choices = train_levels$verification_status, selected = train_levels$verification_status[1])
  })
  output$in_purpose_ui <- renderUI({
    selectInput("in_purpose", "Purpose", choices = train_levels$purpose, selected = train_levels$purpose[1])
  })
  output$in_state_ui <- renderUI({
    selectInput("in_state", "State", choices = train_levels$addr_state, selected = train_levels$addr_state[1])
  })
  
  # ----------------------------
  # Filter wiring (OV, DR, MD)
  # ----------------------------
  init_filter_ui <- function(prefix, date_min, date_max) {
    
    output[[paste0("filter_issue_range_", prefix)]] <- renderUI({
      dateRangeInput(
        paste0("issue_range_", prefix),
        "Issue month range",
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max,
        format = "yyyy-mm"
      )
    })
    
    output[[paste0("filter_grade_", prefix)]] <- renderUI({
      choices <- sort(unique(dt_model$grade))
      pickerInput(
        paste0("grade_sel_", prefix),
        "Grade",
        choices = choices, selected = choices, multiple = TRUE,
        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
      )
    })
    
    output[[paste0("filter_term_", prefix)]] <- renderUI({
      choices <- sort(unique(dt_model$term))
      pickerInput(
        paste0("term_sel_", prefix),
        "Term",
        choices = as.character(choices),
        selected = as.character(choices),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    })
    
    output[[paste0("filter_purpose_", prefix)]] <- renderUI({
      top_purpose <- dt_model |>
        count(purpose, sort = TRUE) |>
        slice_head(n = 10) |>
        pull(purpose) |>
        as.character()
      pickerInput(
        paste0("purpose_sel_", prefix),
        "Purpose (top 10)",
        choices = top_purpose, selected = top_purpose, multiple = TRUE,
        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")
      )
    })
    
    output[[paste0("filter_fico_band_", prefix)]] <- renderUI({
      bands <- dt_model |>
        filter(!is.na(fico_band)) |>
        distinct(fico_band) |>
        arrange(fico_band) |>
        pull(fico_band) |>
        as.character()
      pickerInput(
        paste0("fico_sel_", prefix),
        "FICO band",
        choices = bands, selected = bands, multiple = TRUE,
        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3")
      )
    })
  }
  
  # Tab 1 defaults: mature end = max - 12 months
  min_dt_all <- min(dt_model$issue_dt, na.rm = TRUE)
  max_dt_all <- max(dt_model$issue_dt, na.rm = TRUE)
  default_end_all <- max_dt_all %m-% months(12)
  if (is.na(default_end_all) || default_end_all < min_dt_all) default_end_all <- max_dt_all
  
  observe({
    init_filter_ui("ov", min_dt_all, default_end_all)
    init_filter_ui("dr", min_dt_all, default_end_all)
    
    dt_2016 <- dt_model |> filter(issue_ym >= "2016-01", issue_ym <= "2016-12")
    min_2016 <- min(dt_2016$issue_dt, na.rm = TRUE)
    max_2016 <- max(dt_2016$issue_dt, na.rm = TRUE)
    init_filter_ui("md", min_2016, max_2016)
  })
  
  reset_filters <- function(prefix, date_min, date_max) {
    observeEvent(input[[paste0("reset_filters_", prefix)]], {
      updateDateRangeInput(session, paste0("issue_range_", prefix), start = date_min, end = date_max)
      
      grades <- sort(unique(dt_model$grade))
      updatePickerInput(session, paste0("grade_sel_", prefix), selected = grades)
      
      terms <- sort(unique(dt_model$term))
      updatePickerInput(session, paste0("term_sel_", prefix), selected = as.character(terms))
      
      top_purpose <- dt_model |>
        count(purpose, sort = TRUE) |>
        slice_head(n = 10) |>
        pull(purpose) |>
        as.character()
      updatePickerInput(session, paste0("purpose_sel_", prefix), choices = top_purpose, selected = top_purpose)
      
      if (!is.null(input[[paste0("fico_sel_", prefix)]])) {
        bands <- dt_model |>
          filter(!is.na(fico_band)) |>
          distinct(fico_band) |>
          arrange(fico_band) |>
          pull(fico_band) |>
          as.character()
        updatePickerInput(session, paste0("fico_sel_", prefix), selected = bands)
      }
    }, ignoreInit = TRUE)
  }
  
  reset_filters("ov", min_dt_all, default_end_all)
  reset_filters("dr", min_dt_all, default_end_all)
  
  dt_2016 <- dt_model |> filter(issue_ym >= "2016-01", issue_ym <= "2016-12")
  reset_filters("md", min(dt_2016$issue_dt, na.rm = TRUE), max(dt_2016$issue_dt, na.rm = TRUE))
  
  filtered_by_prefix <- function(prefix, base_df) {
    reactive({
      dt <- base_df
      
      r <- input[[paste0("issue_range_", prefix)]]
      if (!is.null(r) && length(r) == 2) {
        dt <- dt |> filter(issue_dt >= r[1], issue_dt <= r[2])
      }
      
      g <- input[[paste0("grade_sel_", prefix)]]
      if (!is.null(g) && length(g) > 0) dt <- dt |> filter(grade %in% g)
      
      tsel <- input[[paste0("term_sel_", prefix)]]
      if (!is.null(tsel) && length(tsel) > 0) {
        term_int <- suppressWarnings(as.integer(tsel))
        term_int <- term_int[!is.na(term_int)]
        if (length(term_int) > 0) dt <- dt |> filter(term %in% term_int)
      }
      
      psel <- input[[paste0("purpose_sel_", prefix)]]
      if (!is.null(psel) && length(psel) > 0) {
        dt <- dt |> filter(as.character(purpose) %in% as.character(psel))
      }
      
      fsel <- input[[paste0("fico_sel_", prefix)]]
      if (!is.null(fsel) && length(fsel) > 0) {
        dt <- dt |> filter(as.character(fico_band) %in% as.character(fsel))
      }
      
      dt
    })
  }
  
  filtered_overview <- filtered_by_prefix("ov", dt_model)
  filtered_drivers  <- filtered_by_prefix("dr", dt_model)
  
  dt_model_2016 <- dt_model |> filter(issue_ym >= "2016-01", issue_ym <= "2016-12")
  filtered_model <- filtered_by_prefix("md", dt_model_2016)
  
  # ----------------------------
  # TAB 1 — KPIs + plots
  # ----------------------------
  output$kpi_total_loans_ov <- renderText({ fmt_int(nrow(filtered_overview())) })
  
  output$kpi_default_count_ov <- renderText({
    dt <- filtered_overview()
    fmt_int(sum(dt$default_flag == 1, na.rm = TRUE))
  })
  
  output$kpi_defaulted_balance_ov <- renderText({
    dt <- filtered_overview()
    bal <- sum(dt$loan_amnt[dt$default_flag == 1], na.rm = TRUE)
    fmt_usd(bal)
  })
  
  output$kpi_default_rate_ov <- renderText({
    dt <- filtered_overview()
    fmt_pct(mean(dt$default_flag, na.rm = TRUE), 1)
  })
  
  output$kpi_median_fico_ov <- renderText({
    dt <- filtered_overview()
    round(median(dt$fico_mid, na.rm = TRUE))
  })
  
  output$plot_default_trend_ov <- renderPlot({
    dt <- filtered_overview()
    req(nrow(dt) > 0)
    
    df <- dt |>
      group_by(issue_dt) |>
      summarise(default_rate = mean(default_flag, na.rm = TRUE), .groups = "drop") |>
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
    
    dt <- dt |> mutate(grade = factor(grade, levels = c("A","B","C","D","E","F","G")))
    
    df <- dt |>
      group_by(grade) |>
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups = "drop")
    
    ggplot(df, aes(x = grade, y = default_rate)) +
      geom_col(fill = "#1f3850") +
      geom_text(aes(label = percent(default_rate, accuracy = 0.1)),
                color = "white", size = 3.4, vjust = 1.6) +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Grade", y = "Default rate") +
      theme_tw_plot()
  })
  
  # ----------------------------
  # TAB 2 — drivers plots
  # ----------------------------
  output$plot_fico_bins_dr <- renderPlot({
    dt <- filtered_drivers()
    req(nrow(dt) > 0)
    
    df <- dt |>
      filter(!is.na(fico_mid), !is.na(default_flag)) |>
      mutate(fico_bin = cut(fico_mid, breaks = fico_breaks, include.lowest = TRUE)) |>
      group_by(fico_bin) |>
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups = "drop")
    
    ggplot(df, aes(x = fico_bin, y = default_rate)) +
      geom_col(fill = "#1f3850") +
      geom_line(aes(group = 1), linewidth = 1, color = "#49c5aa") +
      geom_point(color = "#b7d84b", size = 1.6) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "FICO band", y = "Default rate") +
      theme_tw_plot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  })
  
  output$plot_purpose_radar_dr <- renderPlot({
    dt <- filtered_drivers()
    req(nrow(dt) > 0)
    
    top10 <- dt |>
      count(purpose, sort = TRUE) |>
      slice_head(n = 10) |>
      pull(purpose) |>
      as.character()
    
    df <- dt |>
      filter(as.character(purpose) %in% top10) |>
      group_by(purpose) |>
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups="drop") |>
      arrange(desc(n))
    
    vals <- df$default_rate
    names(vals) <- as.character(df$purpose)
    
    purpose_names <- names(vals)
    v <- as.numeric(vals)
    
    maxv <- max(v, na.rm = TRUE)
    if (!is.finite(maxv) || maxv <= 0) maxv <- 0.30
    
    radar_df <- rbind(
      rep(maxv, length(v)),
      rep(0, length(v)),
      v
    )
    radar_df <- as.data.frame(radar_df)
    colnames(radar_df) <- purpose_names
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    
    # IMPORTANT: your radar panel is WHITE, so labels/grid must be DARK/GRAY (not white)
    par(mar = c(1.2, 1.2, 2.2, 1.2), bg = "white", fg = "#0b1220")
    
    poly_col <- adjustcolor("#49c5aa", alpha.f = 0.18)
    line_col <- "#49c5aa"
    
    # ring labels (percent)
    ring_vals <- seq(0, maxv, length.out = 5)
    ring_labs <- paste0(round(ring_vals * 100), "%")
    
    fmsb::radarchart(
      radar_df,
      axistype = 1,
      pcol = line_col,
      pfcol = poly_col,
      plwd = 2,
      plty = 1,
      cglcol = adjustcolor("grey40", alpha.f = 0.45),
      cglty = 1,
      cglwd = 1,
      axislabcol = "#0b1220",
      caxislabels = ring_labs,
      vlabcol = "#0b1220",
      vlcex = 0.9
    )
    
    # small note: title is handled by your card header; keep plot clean
  })
  
  # ----------------------------
  # TAB 3 — model scoring + policy simulation (FILTERED!)
  # ----------------------------
  scored_model_2016 <- reactive({
    dt <- filtered_model()
    req(nrow(dt) > 0)
    
    needed <- c("default_flag", feature_cols, "realized_net_loss", "realized_net_profit")
    miss <- setdiff(needed, names(dt))
    if (length(miss) > 0) stop("Missing columns in model table: ", paste(miss, collapse = ", "))
    
    dt_cc <- dt |>
      select(all_of(needed)) |>
      tidyr::drop_na()
    
    dt_cc <- dt_cc |>
      mutate(pd = predict(model_glm, newdata = dt_cc, type = "response"))
    
    dt_cc
  })
  
  policy_tbl_md <- reactive({
    df <- scored_model_2016()
    req(nrow(df) > 0)
    compute_policy_curve_fast(df, grid = seq(0, 1, by = 0.01))
  })
  
  selected_policy_md <- reactive({
    pt <- policy_tbl_md()
    req(nrow(pt) > 0)
    p <- input$decline_pct_md
    pt[which.min(abs(pt$decline_pct - p)), ]
  })
  
  output$kpi_defaults_avoided_md <- renderText({
    sp <- selected_policy_md()
    fmt_int(sp$defaults_avoided)
  })
  
  output$kpi_loss_avoided_md <- renderText({
    sp <- selected_policy_md()
    fmt_usd(sp$loss_avoided)
  })
  
  output$kpi_profit_kept_md <- renderText({
    sp <- selected_policy_md()
    fmt_usd(sp$profit_kept)
  })
  
  output$kpi_delta_profit_md <- renderText({
    sp <- selected_policy_md()
    fmt_usd(sp$delta_profit)
  })
  
  output$txt_policy_snapshot_md <- renderText({
    sp <- selected_policy_md()
    paste0(
      "Decline ", percent(sp$decline_pct, accuracy = 1),
      " (", fmt_int(sp$decline_n), " loans) — keep ", fmt_int(sp$kept_n), " loans."
    )
  })
  
  output$plot_default_by_decile_md <- renderPlot({
    df <- scored_model_2016()
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(risk_decile = ntile(pd, 10))
    
    dec <- df |>
      group_by(risk_decile) |>
      summarise(default_rate = mean(default_flag, na.rm = TRUE), n = n(), .groups="drop") |>
      arrange(risk_decile)
    
    ggplot(dec, aes(x = factor(risk_decile), y = default_rate)) +
      geom_col(fill = "#1f3850") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = "PD decile (10 = highest PD)", y = "Observed default rate") +
      theme_tw_plot()
  })
  
  output$plot_profit_curve_md <- renderPlot({
    pt <- policy_tbl_md()
    req(nrow(pt) > 0)
    
    ggplot(pt, aes(x = decline_pct, y = delta_profit)) +
      geom_line(linewidth = 1.1, color = "#49c5aa") +
      geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Decline top X% by PD", y = "Δ Profit vs baseline") +
      theme_tw_plot()
  })
  
  # ----------------------------
  # TAB 4 — loan input scoring
  # ----------------------------
  pd_reference_try <- reactive({
    df <- scored_model_2016()
    if (nrow(df) >= 200) return(df$pd)
    
    base <- dt_model_2016 |>
      select(all_of(c("default_flag", feature_cols, "realized_net_loss", "realized_net_profit"))) |>
      tidyr::drop_na() |>
      mutate(pd = predict(model_glm, newdata = ., type = "response"))
    base$pd
  })
  
  input_row_try <- reactive({
    data.frame(
      fico_mid = as.numeric(input$in_fico),
      dti = as.numeric(input$in_dti),
      int_rate = as.numeric(input$in_int_rate),
      loan_amnt = as.numeric(input$in_loan_amnt),
      installment = as.numeric(input$in_installment),
      revol_util = as.numeric(input$in_revol_util),
      open_acc = as.integer(input$in_open_acc),
      total_acc = as.integer(input$in_total_acc),
      inq_last_6mths = as.integer(input$in_inq),
      delinq_2yrs = as.integer(input$in_delinq),
      pub_rec = as.integer(input$in_pub_rec),
      term = as.integer(input$in_term),
      home_ownership = factor(input$in_home, levels = train_levels$home_ownership),
      verification_status = factor(input$in_ver, levels = train_levels$verification_status),
      purpose = factor(input$in_purpose, levels = train_levels$purpose),
      addr_state = factor(input$in_state, levels = train_levels$addr_state)
    )
  })
  
  pd_try <- reactive({
    row <- input_row_try()
    as.numeric(predict(model_glm, newdata = row, type = "response"))
  })
  
  output$warn_oob_try <- renderUI({
    row <- input_row_try()
    warns <- c()
    
    for (cn in num_cols) {
      r <- train_ranges[[cn]]
      x <- as.numeric(row[[cn]])
      if (is.finite(r["lo"]) && is.finite(r["hi"]) && is.finite(x)) {
        if (x < r["lo"] || x > r["hi"]) {
          warns <- c(warns, paste0(cn, " outside training 1–99% range (", round(r["lo"],2), "–", round(r["hi"],2), ")"))
        }
      }
    }
    
    if (length(warns) == 0) return(NULL)
    
    div(
      style="margin-top:10px;color:#ffb86b;font-size:12px;line-height:1.4;",
      strong("Note: "), "Some inputs are outside the model's training range. Predictions may be unreliable.",
      tags$ul(lapply(warns, tags$li))
    )
  })
  
  output$kpi_pd_try <- renderText({
    p <- pd_try()
    percent(p, accuracy = 0.1)
  })
  
  output$kpi_decile_try <- renderText({
    p <- pd_try()
    d <- pd_to_decile(p, pd_reference_try())
    if (is.na(d)) "N/A" else as.character(d)
  })
  
  
  output$kpi_decision_try <- renderText({
    pd_ref <- pd_reference_try()
    req(length(pd_ref) >= 50)
    thr <- as.numeric(quantile(pd_ref, probs = 1 - input$decline_pct_try, na.rm = TRUE, type = 7))
    p <- pd_try()
    if (!is.finite(p) || !is.finite(thr)) return("N/A")
    if (input$decline_pct_try <= 0) return("APPROVE")
    if (input$decline_pct_try >= 1) return("DECLINE")
    if (p >= thr) "DECLINE" else "APPROVE"
  })
  
  output$plot_pd_hist_try <- renderPlot({
    pd_ref <- pd_reference_try()
    req(length(pd_ref) >= 50)
    
    thr <- as.numeric(quantile(pd_ref, probs = 1 - input$decline_pct_try, na.rm = TRUE, type = 7))
    p   <- pd_try()
    
    df <- tibble(pd = pd_ref)
    
    ggplot(df, aes(x = pd)) +
      geom_histogram(bins = 50, fill = "#1f3850", color = NA) +
      geom_vline(xintercept = thr, linetype = 2, linewidth = 1, color = "#49c5aa") +
      geom_vline(xintercept = p, linewidth = 1.1, color = "#b7d84b") +
      scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(x = "Predicted PD", y = "Count") +
      theme_tw_plot()
  })
}

shinyApp(ui, server)