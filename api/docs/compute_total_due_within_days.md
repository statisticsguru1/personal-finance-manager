# ⏳ GET /compute_total_due_within_days

Returns the total amount due for a given account (and its children) within the next specified number of days.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/compute_total_due_within_days`
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

| Name    | Type   | Required | Description                                              |
|---------|--------|----------|----------------------------------------------------------|
| `uuid`  | string | ✅       | UUID of the account to evaluate                         |
| `days`  | number | ✅       | Number of days to look ahead when summing due amounts   |

---

## 📥 Example Request

```
GET /compute_total_due_within_days?uuid=acc-2024-456&days=30
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-456",
  "total_due": 1700.25,
  "within_days": 30,
  "start_time": "2025-07-10T13:25:14.882Z",
  "end_time": "2025-07-10T13:25:14.944Z",
  "execution_time": 0.06
}
```

## ❌ Failure Response Examples

| Status | Reason                                   |
| ------ | ---------------------------------------- |
| `400`  | UUID or days parameter missing           |
| `400`  | Invalid value for `days` (e.g. negative) |
| `403`  | Unauthorized                             |
| `404`  | Account not found                        |
| `500`  | Internal server error                    |

🧠 Notes
- Internally uses account$compute_total_due_within_n_days(days).
- days = 0 means today only.
- Useful for dashboards and forecasting panels showing "Amount Due in Next 7, 14, or 30 Days".

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
