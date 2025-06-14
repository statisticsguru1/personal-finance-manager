
# 📊 Income Allocation & Financial Adaptation App

**Purpose**: This application is designed to empower individuals to take control of their finances by ensuring that **every unit of income is allocated to a specific goal or need**. Through a system of virtual accounts and adaptive redistribution, users can prioritize debts, cover essential needs, and grow their savings—all in one seamless workflow.

---

## 🛠️ Key Features

- **Custom Virtual Accounts**  
  Create a financial structure tailored to your life—e.g., Needs (50%), Debt Repayment (30%), Savings (20%).

- **Automated Income Allocation**  
  Every income deposit is automatically split according to user-defined percentages.

- **Adaptive Reallocation Engine**  
  Once an account (e.g., a specific loan) is fully funded, it is deactivated and its share is redistributed among other goals.

- **Emergency Preparedness**  
  Dedicated allocation for unexpected expenses via the Miscellaneous Needs account.

- **Interactive Visual Insights**  
  Real-time charts and dashboards using **Highcharter**, **ggplot2**, and **DT** to help users track progress, get alerts, and analyze trends.

- **Real-Life Simulations**  
  Dynamic testing across multiple user scenarios (e.g., high debt, low income) ensures the system adapts to a wide range of financial realities.

---

## 🔧 Built With

- **Frontend**: HTML, CSS, JavaScript (via Shiny UI)
- **Backend**: R & Shiny
- **Databases**: SQLite & MongoDB (for robust data persistence)
- **Visualization**: Highcharter, DT, ggplot2
- **Deployment**: Google Cloud Platform (GCP)

---

## 🚀 Future Roadmap

- 📱 Mobile app integration  
- 🧠 AI-driven budgeting suggestions  
- 📊 Spending behavior analytics using ML  
- 🧾 Smart bill tracking & reminders  

---

## 👥 Who It’s For

Whether you're:
- Trying to pay off multiple debts
- Saving for an emergency fund
- Managing fluctuating freelance income  

This app helps you stay financially organized, no matter your starting point.

---

**Every coin has a purpose.** This app makes sure of it.

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

