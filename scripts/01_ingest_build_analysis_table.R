library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(janitor)
library(arrow)


# --- Initial Config ---
RAW_PATH  <- "data/raw/accepted_2007_to_2018Q4.csv"
OUT_PATH  <- "data/processed/loans_analysis_ready.parquet"

# Read
dt <- fread(RAW_PATH, na.strings = c("", "NA", "N/A"))
dt <- janitor::clean_names(dt)


# Parse issue_d to year/month 

dt <- dt %>%
  mutate(
    issue_date = suppressWarnings(parse_date_time(issue_d, orders = c("b-y","b-Y"))),
    issue_year = year(issue_date),
    issue_month = month(issue_date),
    issue_ym = sprintf("%04d-%02d", issue_year, issue_month)
  )

# default flag and filtering to only completed loans
dt <- dt %>%
  mutate(
    loan_status = str_squish(as.character(loan_status)),
    default_flag = case_when(
      loan_status %in% c("Charged Off", "Default") ~ 1L,
      loan_status == "Fully Paid" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(default_flag)) %>%
  filter(!is.na(issue_year), !is.na(issue_month))

# enhance data by adding fico_mid
if (all(c("fico_range_low","fico_range_high") %in% names(dt))) {
  dt <- dt %>%
    mutate(
      fico_mid = (as.numeric(fico_range_low) + as.numeric(fico_range_high)) / 2
    )
}

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
write_parquet(dt, OUT_PATH)

# sanity checks and summary
cat("Saved:", OUT_PATH)
cat("Rows:", nrow(dt))
cat("Issue date range:", min(dt$issue_ym), "→", max(dt$issue_ym))
cat("Default rate:", round(mean(dt$default_flag), 4))
print(table(dt$loan_status))
