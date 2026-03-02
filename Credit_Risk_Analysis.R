# ============================================================
# Loan Approval Risk Analysis
# Author: Samidha Basrur
# Tools: R, tidyverse, ggplot2
# Dataset: 2,000 loan applications
# ============================================================

#  1. SETUP

library(tidyverse)
library(ggplot2)
library(scales)

#  2. LOAD DATA

loan_data <- read.csv("loan_approval.csv", stringsAsFactors = FALSE)

# Initial inspection
head(loan_data)
str(loan_data)
summary(loan_data)

#  3. DATA VALIDATION

# Check for missing values
colSums(is.na(loan_data))

# Check for duplicate rows
cat("Duplicate rows:", sum(duplicated(loan_data)), "\n")

# Validate credit score range (FICO: 300–850)
cat("Credit Score Range:", range(loan_data$credit_score), "\n")

# Check for negative values in key numeric columns
cat("Income range:", range(loan_data$income), "\n")
cat("Loan Amount range:", range(loan_data$loan_amount), "\n")
cat("Years Employed range:", range(loan_data$years_employed), "\n")

# Check loan_approved values before conversion
table(loan_data$loan_approved)

#  4. DATA CLEANING 

# Convert loan_approved from "True"/"False" string to logical
loan_data <- loan_data %>%
  mutate(loan_approved = loan_approved == "True")

# Verify conversion
cat("Loan Approved Distribution:\n")
table(loan_data$loan_approved)

# 5. FEATURE ENGINEERING

loan_data <- loan_data %>%
  mutate(
    # FICO-based credit score categories
    credit_category = factor(
      case_when(
        credit_score < 580 ~ "Poor",
        credit_score < 670 ~ "Fair",
        credit_score < 740 ~ "Good",
        credit_score >= 740 ~ "Excellent"
      ),
      levels = c("Poor", "Fair", "Good", "Excellent")
    ),
    
    # Employment tenure categories
    employment_level = factor(
      case_when(
        years_employed <= 10 ~ "Entry (0-10 yrs)",
        years_employed <= 25 ~ "Mid (11-25 yrs)",
        years_employed > 25  ~ "Senior (26+ yrs)"
      ),
      levels = c("Entry (0-10 yrs)", "Mid (11-25 yrs)", "Senior (26+ yrs)")
    ),
    
    # Debt-to-income ratio
    loan_to_income = loan_amount / income,
    
    # Debt burden category
    debt_burden_category = factor(
      case_when(
        loan_to_income < 0.20 ~ "Low (<20%)",
        loan_to_income < 0.40 ~ "Moderate (20-40%)",
        loan_to_income >= 0.40 ~ "High (>=40%)"
      ),
      levels = c("Low (<20%)", "Moderate (20-40%)", "High (>=40%)")
    ),
    
    # High-value loan flag
    high_loan = loan_amount >= 40000
  )

# Verify all engineered features
cat("\nCredit Category Distribution:\n")
table(loan_data$credit_category)

cat("\nEmployment Level Distribution:\n")
table(loan_data$employment_level)

cat("\nDebt Burden Distribution:\n")
table(loan_data$debt_burden_category)

cat("\nHigh-Value Loan Distribution:\n")
table(loan_data$high_loan)

# 6. ANALYSIS

# Q1: Does employment tenure compensate for low credit scores?

q1_summary <- loan_data %>%
  group_by(credit_category, employment_level) %>%
  summarise(
    applications = n(),
    approved = sum(loan_approved),
    approval_rate = mean(loan_approved) * 100,
    .groups = "drop"
  )

print(q1_summary)

# Focus on poor credit applicants specifically
poor_credit_summary <- loan_data %>%
  filter(credit_category == "Poor") %>%
  group_by(employment_level) %>%
  summarise(
    applications = n(),
    approved = sum(loan_approved),
    approval_rate = mean(loan_approved) * 100,
    .groups = "drop"
  )

cat("\n=== POOR CREDIT: EMPLOYMENT TENURE IMPACT ===\n")
print(poor_credit_summary)

# Improvement from Entry to Senior for poor credit
improvement <- poor_credit_summary$approval_rate[3] - poor_credit_summary$approval_rate[1]
cat("\nApproval rate improvement (Entry to Senior, Poor Credit):",
    round(improvement, 2), "percentage points\n")

# Q2: Best applicant profiles for high-value loans ($40k+)

high_loan_data <- loan_data %>%
  filter(high_loan == TRUE)

cat("\nHigh-Value Loan Applications ($40k+):", nrow(high_loan_data))
cat("\nOverall Approval Rate for High-Value Loans:",
    sprintf("%.1f%%\n", mean(high_loan_data$loan_approved) * 100))

q2_summary <- high_loan_data %>%
  group_by(credit_category, employment_level) %>%
  summarise(
    applications = n(),
    approved = sum(loan_approved),
    approval_rate = mean(loan_approved) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(approval_rate))

print(q2_summary)

# 7. VISUALIZATIONS 

# Plot 1: Heatmap — Credit Score vs Employment Tenure

q1_plot <- ggplot(q1_summary,
                  aes(x = employment_level,
                      y = credit_category,
                      fill = approval_rate)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%.1f%%", approval_rate)),
            color = "white",
            fontface = "bold",
            size = 6) +
  scale_fill_gradient2(
    low = "#d32f2f",
    mid = "#ff9800",
    high = "#388e3c",
    midpoint = 50,
    limits = c(0, 100),
    name = "Approval\nRate (%)"
  ) +
  labs(
    title = "Does Employment Tenure Compensate for Low Credit Scores?",
    subtitle = "Approval rates by Credit Category and Employment Level",
    x = "Employment Level",
    y = "Credit Score Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.title = element_text(size = 10),
    panel.grid = element_blank()
  )

print(q1_plot)

# Plot 2: Horizontal Bar Chart — Best Profiles for High-Value Loans

q2_plot_data <- q2_summary %>%
  filter(applications >= 5)  # Exclude combinations with small sample sizes

q2_plot <- ggplot(q2_plot_data,
                  aes(x = reorder(paste(credit_category, "+", employment_level),
                                  approval_rate),
                      y = approval_rate,
                      fill = credit_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%% (n=%d)", approval_rate, applications)),
            hjust = -0.1,
            size = 3.5,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "Poor"      = "#d32f2f",
    "Fair"      = "#ff9800",
    "Good"      = "#4caf50",
    "Excellent" = "#2196f3"
  )) +
  coord_flip() +
  ylim(0, 115) +
  labs(
    title = "Best Applicant Profiles for High-Value Loans ($40k+)",
    x = NULL,
    y = "Approval Rate (%)",
    fill = "Credit Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(q2_plot)