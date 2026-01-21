library(arrow)
library(dplyr)
library(stringr)
library(janitor)

IN_PATH  <- "data/processed/loans_analysis_ready.parquet"
OUT_PATH <- "data/processed/loans_model_table.parquet"

dt <- read_parquet(IN_PATH) %>% as.data.frame() %>% clean_names()

# ---- helpers ----
pct_to_num <- function(x) {
  # converts "13.5%" -> 13.5 ; leaves numeric as-is
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, "%", "")
  suppressWarnings(as.numeric(x))
}

term_to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_extract(x, "\\d+")
  suppressWarnings(as.integer(x))
}

emp_to_num <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  x[x %in% c("", "na", "n/a", "null")] <- NA
  x <- str_replace_all(x, "years|year", "")
  x <- str_replace_all(x, "\\+", "")
  x <- str_replace_all(x, "<\\s*1", "0")
  x <- str_squish(x)
  suppressWarnings(as.integer(x))
}



# select feature contract
keep <- c(
  "issue_year","issue_month","issue_ym","default_flag",
  "loan_amnt","term","int_rate","installment","grade","sub_grade",
  "fico_mid","dti","revol_util","delinq_2yrs","inq_last_6mths",
  "open_acc","pub_rec","revol_bal","total_acc",
  "annual_inc","emp_length","home_ownership","verification_status",
  "purpose","addr_state"
)



m <- dt %>% select(all_of(keep))
# picking specific columns to achieve parsimony, based on core numeric drivers, fields with 
# economic meaning, and fields known before default
m <- m %>%
  mutate(
    term       = term_to_num(term),
    int_rate   = pct_to_num(int_rate),
    revol_util = pct_to_num(revol_util),
    emp_length = emp_to_num(emp_length),

    # numerics
    loan_amnt   = as.numeric(loan_amnt),
    installment = as.numeric(installment),
    fico_mid    = as.numeric(fico_mid),
    dti         = as.numeric(dti),
    delinq_2yrs = as.integer(delinq_2yrs),
    inq_last_6mths = as.integer(inq_last_6mths),
    open_acc    = as.integer(open_acc),
    pub_rec     = as.integer(pub_rec),
    revol_bal   = as.numeric(revol_bal),
    total_acc   = as.integer(total_acc),
    annual_inc  = as.numeric(annual_inc),

    # categoricals
    grade = as.factor(grade),
    sub_grade = as.factor(sub_grade),
    home_ownership = as.factor(home_ownership),
    verification_status = as.factor(verification_status),
    purpose = as.factor(purpose),
    addr_state = as.factor(addr_state)
  )

# drop rows with missing values 
m <- m %>%
  filter(
    !is.na(default_flag),
    !is.na(issue_ym),
    !is.na(loan_amnt),
    !is.na(term),
    !is.na(int_rate),
    !is.na(fico_mid)
  )

write_parquet(m, OUT_PATH)

cat("Saved:", OUT_PATH)
cat("Rows:", nrow(m))
cat("Default rate:", round(mean(m$default_flag), 4))
cat("Issue range:", min(m$issue_ym), "→", max(m$issue_ym))
