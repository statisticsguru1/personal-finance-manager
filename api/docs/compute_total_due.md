# 💰 GET /compute_total_due

Returns the total amount due for a given account including its children and descendants.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/compute_total_due`
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

| Name   | Type   | Required | Description                        |
|--------|--------|----------|------------------------------------|
| `uuid` | string | ✅       | UUID of the account to evaluate due for |

---

## 📥 Example Request


```
GET /compute_total_due?uuid=acc-2024-456
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-456",
  "total_due": 3400.50,
  "start_time": "2025-07-10T13:11:05.123Z",
  "end_time": "2025-07-10T13:11:05.189Z",
  "execution_time": 0.07
}

```
## ❌ Failure Response Examples

| Status | Reason                             |
| ------ | ---------------------------------- |
| `400`  | Missing or invalid UUID            |
| `403`  | Unauthorized to access the account |
| `404`  | Account not found                  |
| `500`  | Internal server error              |


## 🧠 Notes
- Internally calls account$compute_total_due(), which traverses down the account hierarchy.
- This endpoint is useful for summarizing all upcoming obligations related to a specific category like “Utilities” or “Loans”.
- It performs a read-only operation — no locking needed.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
