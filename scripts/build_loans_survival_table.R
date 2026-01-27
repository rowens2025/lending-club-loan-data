library(arrow)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

raw_path <- "data/raw/accepted_2007_to_2018Q4.csv"
dt <- readr::read_csv(raw_path, show_col_types = FALSE)

parse_lc_month <- function(x) {
  suppressWarnings(lubridate::my(x))
}

dt <- dt %>%
  mutate(
    loan_status = str_squish(as.character(loan_status)),
    issue_d_dt = parse_lc_month(issue_d),
    last_pymnt_d_dt = parse_lc_month(last_pymnt_d),

    event_default = case_when(
      loan_status %in% c("Charged Off", "Default") ~ 1L,
      loan_status == "Fully Paid" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(event_default)) %>%
  filter(!is.na(issue_d_dt), !is.na(last_pymnt_d_dt)) %>%
  mutate(
    duration_months = interval(issue_d_dt, last_pymnt_d_dt) %/% months(1)
  ) %>%
  filter(duration_months >= 0, duration_months <= 120)

keep_cols <- c(
  # time & outcome
  "issue_d_dt","last_pymnt_d_dt","duration_months","event_default","loan_status",

  # core
  "term","int_rate","loan_amnt","installment","grade","sub_grade",
  "fico_range_low","fico_range_high","dti","revol_util","annual_inc",
  "emp_length","home_ownership","verification_status","purpose","addr_state",

  # richer bureau snapshot fields
  "num_tl_30dpd","num_tl_90g_dpd_24m","pct_tl_nvr_dlq","percent_bc_gt_75",
  "tot_hi_cred_lim","total_bal_ex_mort","total_bc_limit","total_il_high_credit_limit",
  "acc_open_past_24mths","mths_since_recent_inq","mths_since_last_delinq",
  "pub_rec_bankruptcies","tax_liens",
  "hardship_flag","hardship_status","hardship_dpd","debt_settlement_flag"
)

keep_cols <- keep_cols[keep_cols %in% names(dt)] 

dt_surv <- dt %>%
  select(all_of(keep_cols)) %>%
  mutate(
    fico_mid = ifelse(!is.na(fico_range_low) & !is.na(fico_range_high),
                      (fico_range_low + fico_range_high)/2, NA_real_)
  )

out_path <- "data/processed/loans_survival_table.parquet"
arrow::write_parquet(dt_surv, out_path)

cat("Saved:", out_path)
cat("Rows:", nrow(dt_surv))
cat("Default event rate:", mean(dt_surv$event_default))
cat("Duration range:", min(dt_surv$duration_months))
cat(" - ", max(dt_surv$duration_months))
