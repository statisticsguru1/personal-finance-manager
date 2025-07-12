# 📡 Personal Finance Manager API

Welcome to the documentation for the **Personal Finance Manager API** — a secure backend built with [Plumber](https://www.rplumber.io/) in R for managing user account trees, transactions, and personal finances.

---

## 📘 API Reference Index

### Filters

- [Auth Filter](auth-filter.md)

### POST

- [POST /register](register.md)
- [POST /login](login.md)
- [POST /deposit](deposit.md)
- [POST /withdraw](withdraw.md)
- [POST /distribute](distribute.md)
- [POST /set_child_allocations](set_child_allocations.md)
- [POST /change_account_status](change_account_status.md)
- [POST /set_priority](set_priority.md)
- [POST /set_due_date](set_due_date.md)
- [POST /set_fixed_amount](set_fixed_amount.md)
- [POST /set_account_type](set_account_type.md)
- [POST /set_account_freq](set_account_freq.md)
- [POST /set_account_periods](set_account_periods.md)


### GET

- [GET /get_balance](get_balance.md)
- [GET /get_transactions](get_transactions.md)
- [GET /list_child_accounts](list_child_accounts.md)
- [GET /list_all_accounts](list_all_accounts.md)
- [GET /find_account_by_name](find_account_by_name.md)
- [GET /find_account_by_uuid](find_account_by_uuid.md)
- [GET /compute_total_balance](compute_total_balance.md)
- [GET /move_balance](move_balance.md)
- [GET /compute_total_due](compute_total_due.md)
- [GET /compute_total_due_within_days](compute_total_due_within_days.md)
- [GET /spending](spending.md)
- [GET /total_income](total_income.md)
- [GET /allocated_amount](allocated_amount.md)
- [GET /income_utilization](income_utilization.md)
- [GET /walking_amount](walking_amount.md)
- [GET /get_account_status](get_account_status.md)
- [GET /get_priority](get_priority.md)
- [GET /get_due_date](get_due_date.md)
- [GET /get_fixed_amount](get_fixed_amount.md)
- [GET /get_account_type](get_account_type.md)
- [GET /get_account_freq](get_account_freq.md)
- [GET /get_account_periods](get_account_periods.md)


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
