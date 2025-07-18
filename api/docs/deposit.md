# 💰 POST /deposit

Deposits funds into a user’s account. Requires a valid JWT token and a UUID identifying the account. Updates balances and persists changes securely.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/deposit`
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

| Name                | Type     | Required | Description                              |
|---------------------|----------|----------|------------------------------------------|
| `uuid`              | string   | ✅       | UUID of the account to deposit into      |
| `amount`            | float    | ✅       | Amount to deposit                        |
| `channel`           | string   | ✅       | Deposit channel (e.g., M-Pesa, Bank)     |
| `transaction_number`| string   | ❌       | Optional reference number                |
| `by`                | string   | ❌       | Who performed the deposit (default = User) |
| `date`              | string   | ❌       | Deposit timestamp (default = now)        |

---

## 📥 Example Request Body (JSON)

```json
{
  "uuid": "acc-2023-savings-abc123",
  "amount": 1500,
  "channel": "mpesa",
  "transaction_number": "MPESA123456",
  "by": "User"
}
```

---

## 📤 Success Response

```json
{
  "success": true,
  "status": 200,
  "account_uuid": "acc-2023-savings-abc123",
  "amount": 1500,
  "balance": 3300,
  "start_time": "2025-07-10T08:50:15.123Z",
  "end_time": "2025-07-10T08:50:15.200Z",
  "execution_time": 0.077
}
```

---

## ❌ Failure Response Examples

| Status | Reason                        |
|--------|-------------------------------|
| `403`  | Unauthorized for this account |
| `404`  | Account not found             |
| `500`  | Internal server error         |

---

## 🧠 Notes

- Uses a lock mechanism to ensure safe concurrent writes.
- Returns structured timing for performance tracking.
- Must be called **after auth** (JWT must be valid).

---

## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)

