# 💸 POST /withdraw

Withdraws funds from a user’s account. Requires a valid JWT token and a UUID identifying the account. Updates balances and persists changes securely.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/withdraw`
- **Auth**: Required
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📤 Request Parameters

| Name                 | Type     | Required | Description                                |
|----------------------|----------|----------|--------------------------------------------|
| `uuid`               | string   | ✅       | UUID of the account to withdraw from       |
| `amount`             | float    | ✅       | Amount to withdraw                         |
| `channel`            | string   | ✅       | Withdrawal channel (e.g., mpesa, bank)     |
| `transaction_number` | string   | ❌       | Optional reference number                  |
| `initiated_by`       | string   | ❌       | Who initiated the withdrawal (default = "User") |
| `transaction_date`   | string   | ❌       | Timestamp of withdrawal (default = now)    |

---

## 📥 Example Request Body

```json
{
  "uuid": "acc-2023-savings-abc123",
  "amount": 1000,
  "channel": "bank",
  "transaction_number": "BANK54321",
  "initiated_by": "User"
}
```

---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "account_uuid": "acc-2023-savings-abc123",
  "balance": 2300,
  "start_time": "2025-07-10T09:30:00.123Z",
  "end_time": "2025-07-10T09:30:00.180Z",
  "execution_time": 0.057
}
```

---

## ❌ Failure Responses

| Status | Reason                         |
|--------|--------------------------------|
| `400`  | Invalid withdrawal amount      |
| `403`  | Not authorized for this account|
| `404`  | Account not found              |
| `500`  | Server error / internal failure|

---

## 🧠 Notes

- Withdrawals use `with_account_lock()` to ensure safe concurrency.
- Errors are structured with timestamps for traceability.
- You must have sufficient permissions for the account.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)

