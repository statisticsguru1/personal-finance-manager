# 📈 GET /walking_amount

Returns the "walking amount" (cumulative total) of either `amount_due` or `balance` for a given account and its children across a date range.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/walking_amount`
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📤 Query Parameters

| Name        | Type   | Required | Description                                                                    |
|-------------|--------|----------|--------------------------------------------------------------------------------|
| `uuid`      | string | ✅       | UUID of the account                                                            |
| `amt_type`  | string | ❌       | Type of amount to walk: `"amount_due"` or `"balance"` (default: `"amount_due"`) |
| `from`      | string | ❌       | Start date in `YYYY-MM-DD` format (default: very early date)                  |
| `to`        | string | ❌       | End date in `YYYY-MM-DD` format (default: today)                              |

---

## 📥 Example Request

```
GET /walking_amount?uuid=acc-2024-xyz&amt_type=balance&from=2024-01-01&to=2024-12-31
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-xyz",
  "amt_type": "balance",
  "from": "2024-01-01",
  "to": "2024-12-31",
  "walking_amount": [
    {
      "date": "2024-01-01",
      "value": 0
    },
    {
      "date": "2024-01-15",
      "value": 1200
    },
    {
      "date": "2024-02-01",
      "value": 2000
    }
    // ...
  ],
  "start_time": "2025-07-10T07:00:01.003Z",
  "end_time": "2025-07-10T07:00:01.023Z",
  "execution_time": 0.02
}
```
## ❌ Error Responses

| Status | Cause                                   |
| ------ | --------------------------------------- |
| `400`  | UUID or date params are missing/invalid |
| `403`  | Account not found or unauthorized       |
| `404`  | Tree or user not found                  |
| `500`  | Internal processing error               |


## 🧠 Notes
- Use amt_type = "amount_due" to get cumulative liabilities (default).

- Use amt_type = "balance" to visualize asset accumulation.

- Under the hood, uses account$walking_amount(amt_type, daterange).

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
