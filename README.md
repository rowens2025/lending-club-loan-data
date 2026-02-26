# Lending Club Loan Data — Financial Credit Risk Lab (Shiny)

This repo builds a small “risk console” on top of LendingClub loan performance data, ending in a Shiny app that explores portfolio risk drivers and simulates underwriting policy using a simple, interpretable Probability of Default (PD) model.

## Why the default rate looks “high”
You’ll notice default percentages that can look higher than what a bank/credit union might quote. That’s expected for two reasons:

1) **Completed-loans lens (closed book):**  
For modeling and evaluation, we focus on loans that have reached an outcome (paid off vs. charged off/defaulted). That creates a *matured cohort* where every loan has “finished the story.” In the real world, lenders usually talk about a **live, revolving book** that includes many active loans that haven’t had time to default yet. Mixing active and unresolved loans into the denominator will typically reduce the apparent default rate.

2) **ML-ready target definition:**  
A supervised ML model needs a clear target label. Using completed loans avoids “unknown outcomes” and leakage from partial performance. It’s not meant to represent an institution’s current portfolio mix; it’s meant to create a clean, learnable signal for PD modeling and policy simulation.

## What the app does
- **Explore:** portfolio snapshot + driver views (e.g., default rate by FICO bands and purpose) under interactive filters.
- **Model (Default PD):** trains a logistic regression PD model on mature vintages and evaluates on a held-out period.
- **Policy simulation:** tests “decline top X% by predicted PD” and measures impact using realized cashflow economics (profit/loss based on repayments and recoveries).
- **Try It:** enter a hypothetical loan to see its predicted PD, relative risk decile, and whether it would be declined under the selected policy.
