# 📡 Personal Finance Manager API

Welcome to the documentation for the **Personal Finance Manager API** — a secure backend built with [Plumber](https://www.rplumber.io/) in R for managing user account trees, transactions, and personal finances.

---

## 📘 API Reference Index

- [Auth Filter](auth-filter.md)
- [POST /register](register.md)
- [POST /login](login.md)
- [POST /deposit](deposit.md)
- [POST /withdraw](withdraw.md)
- [GET /accounts](accounts.md)

---

## 🔐 Authentication

Most endpoints require a valid Bearer JWT token in the `Authorization` header. Auth and rate limiting are enforced globally via a request filter.

```
Authorization: Bearer <your-token>
```

---

## 📦 Core Features

- Account creation and persistent storage (per user)
- Deposits, withdrawals, transfers, and account hierarchy logic
- Fully modular backend logic powered by the [`finman`](https://github.com/statisticsguru1/personal-finance-manager/finman) R package
- Works with Shiny, React, or any frontend via HTTP

---

## 🧭 How to Use the API

1. Register a user (`/register`)
2. Login to receive token (`/login`)
3. Use your token to make authenticated requests (e.g., `/deposit`, `/accounts`)
4. Each request is validated and tracked for rate limits


Need help? Contact: [mutindafestus27@gmail.com](mailto:mutindafestus27@gmail.com)
