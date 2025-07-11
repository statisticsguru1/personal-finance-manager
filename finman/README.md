# Finman <img src="https://img.shields.io/badge/status-active-brightgreen" align="right"/>

![R-CMD-check](https://github.com/statisticsguru1/personal-finance-manager/actions/workflows/r-ci.yml/badge.svg?branch=refactor-main-account-docs)
[![codecov](https://codecov.io/github/statisticsguru1/personal-finance-manager/branch/refactor-main-account-docs/graph/badge.svg?token=O02A92ODYD)](https://codecov.io/github/statisticsguru1/personal-finance-manager)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: Proprietary](https://img.shields.io/badge/license-Proprietary-red.svg)](LICENSE)
![Last Commit](https://img.shields.io/github/last-commit/statisticsguru1/personal-finance-manager)


---

> The **Finman** package provides a robust, extensible framework for modeling personal
and group financial systems using hierarchical accounts. Built with R6,
it supports object-oriented definitions for accounts at multiple levels — from
 general-purpose wallets to specialized savings and debt accounts — with
 flexible rules for allocation, due tracking, and status management.

---

## 📦 Overview

**Finman** provides a robust object-oriented structure (via R6) for managing a 
hierarchy of financial accounts

### 🔧 What It Does
📁 **Hierarchical Account Structure:**

`MainAccount:` Root account that holds and distributes funds.

`ChildAccount:` Intermediate accounts with customizable allocation and prioritization.

`GrandchildAccount:` Specialized accounts for fixed dues (e.g., rent, internet) with due dates and periods.

🔄 **Automatic Allocation & Balancing:**

Deposits flow down through the account hierarchy based on allocation percentages and account status.

Fully funded accounts become inactive, allowing surplus to remain or be redirected.

🔒 **Concurrency-Safe Modifications:**

Built-in file locking to avoid race conditions when working in shared or API environments.

📈 **Stateful Tracking:**

Balances, dues, fixed amounts, priorities, and transaction history are persistently stored per user.

🧱 **Composable Architecture:**


The package is designed to power the backend of a broader financial management system
including REST APIs (via Plumber) and dashboards (Shiny/React).

---

## 🛠️ Installation

```r
# Install devtools if not already installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("festusnzuma/personal-finance-manager/finman")
```

---

## 🚀 Getting Started

```r
library(finman)

main <- MainAccount$new(name = "Wallet")
child <- ChildAccount$new(name = "Goals", allocation = 0.5)

main$add_child(child)
main$deposit(1000)

main$get_balance()   # 1000
child$get_balance()  # 500 (50% allocation)
```

---


## 📚 Documentation

Visit the full documentation site at:

➡️ [https://festusnzuma.github.io/personal-finance-manager](https://festusnzuma.github.io/personal-finance-manager)

## Developer Notes
- Modular R6 class definitions allow you to extend or override behavior.

- All logic is pure R and does not depend on external DBs or systems.

---

---

## 🧪 Development Metrics

| Metric               | Status        |
|----------------------|---------------|
| R CMD Check          | ![R-CMD-check](https://github.com/statisticsguru1/personal-finance-manager/actions/workflows/r-ci.yml/badge.svg?branch=refactor-main-account-docs)
| Test Coverage        | [![codecov](https://codecov.io/github/statisticsguru1/personal-finance-manager/branch/refactor-main-account-docs/graph/badge.svg?token=O02A92ODYD)](https://codecov.io/github/statisticsguru1/personal-finance-manager)|
| License              | [![License: Proprietary](https://img.shields.io/badge/license-Proprietary-red.svg)](LICENSE) |
| Lifecycle            | ![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg) |

---

## 🔒 License

This software is licensed on a **proprietary, source-available** basis.

You **may not**:
- Copy, redistribute, or modify the code
- Use it in public or commercial settings

You **may**:
- View and study the code for learning or inspection

📧 To request licensing or use permissions, contact:  
**mutindafestus27@gmail.com**

---

## 👤 Author

**Festus Nzuma**  
📬 [GitHub Profile](https://github.com/statisticsguru1)  
📧 mutindafestus27@gmail.com  

---

## 🤝 Contributing

This package is not currently accepting public contributions. 
For private collaboration inquiries, contact the author.

---

## 📁 Repository Structure

```text
personal-finance-manager/
├── finman/           # R package logic
├── api/              # REST API endpoints (plumber)
├── tests/            # Unit tests
├── shinyapp/         # (future) Shiny frontend
├── reactapp/         # (future) React mobile frontend
├── .github/          # CI workflows
├── README.md
└── LICENSE
```

---

## 🏁 Roadmap

- [x] Core account logic.
- [x] API development.
- [ ] Load testing.
- [ ] Shiny frontend integration.
- [ ] React frontend integration.

---

Built with ❤️ using R6, testthat, and tidyverse.
