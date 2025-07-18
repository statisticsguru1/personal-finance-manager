# 🪙 GET /allocated_amount

Returns the total allocated amount (deposits) for an account and all its child accounts in a specified date range.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/allocated_amount`
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

| Name    | Type   | Required | Description                                            |
|---------|--------|----------|--------------------------------------------------------|
| `uuid`  | string | ✅       | UUID of the account to compute allocation for         |
| `from`  | string | ❌       | Start date (`YYYY-MM-DD`). Defaults to earliest date  |
| `to`    | string | ❌       | End date (`YYYY-MM-DD`). Defaults to today            |

---

## 📥 Example Request

```
GET /allocated_amount?uuid=acc-2023-001&from=2024-01-01&to=2024-12-31
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2023-001",
  "from": "2024-01-01",
  "to": "2024-12-31",
  "allocated_amount": 21500,
  "start_time": "2025-07-10T15:42:31.115Z",
  "end_time": "2025-07-10T15:42:31.162Z",
  "execution_time": 0.047
}
```
## ❌ Failure Response Examples

| Status | Reason                                |
| ------ | ------------------------------------- |
| `400`  | UUID missing or invalid               |
| `403`  | Unauthorized access                   |
| `404`  | Account not found                     |
| `500`  | Internal server error or invalid date |


## 🧠 Notes
- Internally uses account$allocated_amount(date_range).

- Includes all deposits allocated to the account and its descendants.

- If no from or to is provided, defaults to full history.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
