# 🔁 POST /move_balance

Transfers a specified amount from one account to another.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/move_balance`
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

| Name        | Type   | Required | Description                              |
|-------------|--------|----------|------------------------------------------|
| `from_uuid` | string | ✅       | UUID of the source account               |
| `to_uuid`   | string | ✅       | UUID of the destination account          |
| `amount`    | number | ✅       | Amount to transfer (must be > 0)         |

---

## 📥 Example Request

```json
{
  "from_uuid": "acc-2024-abc",
  "to_uuid": "acc-2024-def",
  "amount": 2500
}
```

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Moved 2500 from Emergency to Farming",
  "from_uuid": "acc-2024-abc",
  "to_uuid": "acc-2024-def",
  "amount": 2500,
  "start_time": "2025-07-10T12:42:15.155Z",
  "end_time": "2025-07-10T12:42:15.190Z",
  "execution_time": 0.035
}
```

## ❌ Failure Response Examples 

| Status | Reason                             |
| ------ | ---------------------------------- |
| `400`  | Missing or invalid amount          |
| `400`  | Missing `from_uuid` or `to_uuid`   |
| `404`  | Source or target account not found |
| `500`  | Internal server error              |


## 🧠 Notes
- This action internally calls from_account$move_balance().
- The operation is atomic and wrapped in a file lock for consistency.
- Will fail if source or target account doesn’t exist, or if balance is insufficient.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
