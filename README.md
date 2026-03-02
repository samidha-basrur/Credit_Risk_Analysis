# Loan Approval Risk Analysis
**Tools:** R, Flexdashboard, ggplot2, tidyverse
**Dataset:** 2,000 loan applications
**Focus:** Credit risk segmentation, debt burden analysis, approval pattern identification


## Overview

Lending decisions carry real consequences - for institutions managing risk exposure and for applicants whose financial futures depend on fair, consistent evaluation criteria. This project takes a business analytics lens to 2,000 loan applications, asking a straightforward question: **what actually drives approval?**

Using industry-standard FICO credit scoring frameworks, I engineered key risk features - including debt-to-income ratio, credit risk tiers, and employment tenure categories — to move beyond surface-level approval rates and uncover the patterns underneath.

The findings are sharper than expected. Most notably, **not a single applicant with a poor credit score was approved for a high-value loan ($40k+)** - a hard risk boundary that has clear implications for lending policy, compliance documentation, and risk-based decision-making.


## Business Questions

This analysis was structured around three practical questions a risk or compliance team might actually ask:

1. Can strong employment tenure compensate for a weak credit score?
2. Which applicant profile is most likely to be approved for a high-value loan?
3. How does debt burden interact with credit quality to shape approval outcomes?


## What the Data Revealed

**Credit score is the dominant gatekeeper.**
Applicants with excellent credit (740+) were approved at 93–100% regardless of how long they had been employed. Employment tenure, while meaningful at the margins, could not override a poor credit history.

**Employment tenure has limited compensatory power.**
Poor-credit applicants with 26+ years of employment were still approved at just 9% - suggesting the institution applies a firm floor on credit risk regardless of other signals.

**Debt burden compounds the risk picture.**
As debt-to-income ratios climbed, approval rates fell sharply - and for poor-credit applicants carrying high debt loads, the approval rate dropped to zero.

**High-value loans follow a strict risk threshold.**
Of 195 poor-credit applicants who applied for loans of $40,000 or more, none were approved. Meanwhile, excellent-credit applicants achieved a 98.8% approval rate for the same loan tier.


## Dashboard

The interactive dashboard was built in R Flexdashboard and presents findings across five panels:

- **KPI Summary** - total applications, overall approval rate, average credit score, and high-value loan risk flag
- **Credit Score vs Employment Heatmap** - approval rates across all credit and tenure combinations
- **Debt Burden Impact Chart** - how debt-to-income ratio shifts approval likelihood by credit category
- **High-Value Loan Rankings** - best and worst performing applicant profiles for $40k+ loans
- **Key Metrics Table** - approval breakdowns by credit score tier, employment level, and debt burden segment


## Files

| File | Description |
|---|---|
| `Credit_Risk_Analysis.R` | Full R script - data cleaning, validation, feature engineering, analysis, and visualizations |
| `loan_dashboard.Rmd` | R Markdown flexdashboard source file |
| `loan_approval.csv` | Dataset - 2,000 loan applications |


## Skills Applied

- Data validation and quality assessment
- Feature engineering - FICO credit tiers, debt-to-income ratio, employment segmentation
- Exploratory data analysis and trend identification
- Risk-based segmentation and pattern recognition
- Interactive dashboard development
- Business-oriented findings documentation
