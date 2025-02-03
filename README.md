
# Financial Management App

## Overview
This project is a financial management application designed to help users allocate income efficiently, track spending, and manage financial goals. The app follows a customizable hierarchical structure, allowing users to define how their income is distributed among various accounts.

## Why Use This App?
- Highly flexible—users can customize the hierarchy to match their financial situation:
  - If highly indebted, structure the hierarchy to focus on debt repayment.
  - If financially stable, design a hierarchy to prioritize savings and investments.
- Monitors money utilization, helping users adjust allocations to improve financial management.
- Adapts to changing financial situations:
  - Fully funded accounts can be deactivated, directing new income toward deficit accounts.
  - Income is redistributed dynamically based on priorities.

## Features
- **Customizable Account Hierarchy**: Users define a structure that suits their financial goals.
- **Automated Allocations**: Income is automatically distributed based on preset percentages.
- **Transaction Tracking**: Logs deposits, withdrawals, and inter-account transfers.
- **Financial Insights & Visualizations**:
  - Highcharter-based charts to track balance trends and due amounts.
  - Alerts for missed and upcoming payments.
- **Smart Financial Adaptation**:
  - Fully funded accounts stop receiving allocations.
  - Deficient accounts receive additional funding.

## Technologies Used
- **Backend**: R (Shiny)
- **Database**: SQLite, MongoDB, Google Cloud Platform (GCP)
- **Frontend**: Shiny UI, HTML, CSS, JavaScript
- **Visualization**: Highcharter, DT, ggplot2

## Getting Started
To clone the repository, run:
```bash
git clone https://github.com/statisticsguru1/personal-finance-manager.git
cd personal-finance-manager
```

Alternatively, launch directly in R:
```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, bslib, highcharter, tidyverse, bsicons, knitr, shinylogs, DT)
runGitHub(
  'personal-finance-manager',
  username = 'statisticsguru1',
  ref = "main",
  subdir="NULL"
)
```

## Contribution
Contributions are welcome! Check the Issues tab for open tasks.

## License
This project is licensed under the MIT License. 

