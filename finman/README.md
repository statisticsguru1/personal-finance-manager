# Finman <img src="https://img.shields.io/badge/status-active-brightgreen" align="right"/>

[![R-CMD-check](https://github.com/statisticsguru1/personal-finance-manager/actions/workflows/r-ci.yml/badge.svg?branch=refactor-main-account-docs)
[![codecov](https://codecov.io/github/statisticsguru1/personal-finance-manager/branch/refactor-main-account-docs/graph/badge.svg?token=O02A92ODYD)](https://codecov.io/github/statisticsguru1/personal-finance-manager)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: Proprietary](https://img.shields.io/badge/license-Proprietary-red.svg)](LICENSE)

---

> **Finman**: A recursive financial account management engine in R.  
> Built for investment groups, cooperative systems, and personal finance automation.

---

## 📦 Overview

**Finman** provides a robust object-oriented structure (via R6) for managing a hierarchy of financial accounts, including:

- ✅ Master / main accounts
- ✅ Child and grandchild accounts
- ✅ Transaction & balance tracking
- ✅ Due amounts and priority logic
- ✅ Configurable allocation rules
- ✅ API-ready & UI-ready logic layer

Finman powers the backend of a broader financial management system including REST APIs (via Plumber) and dashboards (Shiny/React).

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

---

## ✅ Features

| Feature | Status |
|--------|--------|
| Recursive account hierarchy | ✔️ |
| Balance + due tracking | ✔️ |
| Allocation control | ✔️ |
| Secure credential helpers | ✔️ |
| API integration support | ✔️ |
| CI-tested and coverage-tracked | ✔️ |

---

## 🧪 Development Metrics

| Metric               | Status        |
|----------------------|---------------|
| R CMD Check          | [![R-CMD-check](https://github.com/statisticsguru1/personal-finance-manager/actions/workflows/r-ci.yml/badge.svg?branch=refactor-main-account-docs)
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

This package is not currently accepting public contributions. For private collaboration inquiries, contact the author.

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

- [x] Core account logic
- [x] API testing
- [ ] Loan + fixed account extension
- [ ] Shiny frontend integration
- [ ] React client support

---

Built with ❤️ using R6, testthat, and tidyverse.
