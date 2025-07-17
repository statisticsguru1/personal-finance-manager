# Finman <img src="https://img.shields.io/badge/status-active-brightgreen" align="right"/>

![R-CMD-check](https://github.com/statisticsguru1/personal-finance-manager/actions/workflows/r-ci.yml/badge.svg?branch=refactor-main-account-docs)
[![codecov](https://codecov.io/github/statisticsguru1/personal-finance-manager/branch/refactor-main-account-docs/graph/badge.svg?token=O02A92ODYD)](https://codecov.io/github/statisticsguru1/personal-finance-manager)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-Proprietary-red.svg)](LICENSE)
![Last Commit](https://img.shields.io/github/last-commit/statisticsguru1/personal-finance-manager)
[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)


Welcome to **Personal Finance Manager** an open-source, modular platform designed to help individuals and investment groups manage money more intentionally. It provides a structured, rule-based approach to budgeting, saving, borrowing, and allocating income using a dynamic account system.


## 🚀 Vision

To build a **transparent**, **accountable**, and **modular** finance management system that empowers individuals to take control of their money with clarity and intention.

Currently, the focus is on **personal finance**, helping users to:

- Align **income with expenses** through structured allocation strategies including sinking funds for upcoming bills and goal-based saving.
- **Reduce debt** with disciplined repayment workflows sinking funds for upcoming bills and goal-based saving. 
- Track and grow **savings goals** with rule-based automation  
- Encourage intentional spending and budgeting habits

Support for **group-based finance** — including shared accounts, rotating savings (merry-go-rounds), and member management — is planned for future releases.


## 🔍 Why This Project?

Traditional budgeting tools often fail to capture the complexity of real-world personal income - expediture balance or enforce discipline in individual saving behaviors. This project was built to:

- Bring **automation and control** to personal/group finance tracking.
- Offer **flexible allocation systems** (like 50/30/20 budgeting).
- Support **goal-based** saving, debt tracking, and scheduled obligations such as **sinking funds**.
- Enable **modular customization** through APIs, Shiny dashboards, and React apps.
- Ensure **privacy and security** even when integrating with partner services—authentication uses **JWT tokens**, sharing only user UUID and roles, maintaining anonymity and avoiding de-identification.


## 🧩 Key Components

| Module      | Technology     | Purpose |
|-------------|----------------|---------|
| **R Package** | R + R6         | Core business logic, classes for account structure, data serialization |
| **API**     | Plumber (R)     | REST API for account operations, transactions, automation |
| **Shiny App**| Shiny (R)      | Admin and user interface for managing groups/accounts |
| **React App**| React.js       | Mobile-friendly UI for day-to-day use, especially for group members |


## 🧠 Core Concepts

- **Main, Child, and Grandchild Accounts**: Funds flow hierarchically with automatic allocation rules, mimicking real-world budgeting structures.
- **Dynamic Setup**: Users can configure accounts based on their financial goals—whether it's debt mitigation, savings, or managing irregular or low income. The system adapts to your strategy.
- **Fixed Obligations**: Define recurring payments with due dates and frequency (e.g., rent, subscriptions).
- **Sinking Funds**: Support for gradual saving toward future large expenses (e.g., annual insurance, school fees).
- **Greedy Account Logic**: Once an account is fully funded, it is automatically deactivated, and excess funds are redistributed intelligently to remaining active siblings.
- **Account Reactivation**: Inactive periodic accounts (e.g., monthly, yearly) are automatically reactivated after their due dates to resume funding.
- **Recursive Account Nesting**: Users can attach multiple child and grandchild accounts to mirror their actual expense structure with deep nesting flexibility.

- **Custom Fields**: Admins can define custom registration or account creation fields, supporting diverse setups.


## 🌐 Use Cases

*Current focus:*

- ✅ Personal budgeting and savings

- ✅ Goal-oriented saving (e.g., rent, subscriptions, emergency funds)

- ✅ Debt tracking and repayment strategies

- ✅ Sinking funds and structured allocation planning

*Planned for future releases:*

- 🔄 Group-based savings (e.g., chamas, merry-go-rounds)

- 💸 Micro-loans and internal lending frameworks

- 🏢 Custom financial apps for organizations or cooperatives

---

## 📚 Project Modules

To dive deeper into individual modules and their documentation:

👉 See [Modules](modules.qmd)

Or explore full API docs:

- [API (Plumber)](./api/index.html)
- [R Package (`finman`)](./finman/index.html)

## 👨‍💻 Authors & Contributors

- **Festus Nzuma** (Author, Maintainer)  
  📧 mutindafestus27@gmail.com  
  🐙 [GitHub](https://github.com/statisticsguru1)


## 📜 License

MIT. See [LICENSE](https://github.com/statisticsguru1/finance-manager/LICENSE.md) for details.

--- 
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!


[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)