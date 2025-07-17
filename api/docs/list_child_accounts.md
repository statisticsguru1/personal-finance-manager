# 🧾 GET /list_child_accounts

Retrieves all child account names under a specified parent account.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/list_child_accounts`  
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

| Name   | Type   | Required | Description                   |
|--------|--------|----------|-------------------------------|
| `uuid` | string | ✅       | UUID of the parent account    |

---

## 📥 Example Request

```
GET /list_child_accounts?uuid=parent-acc-2024-001
```

---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "parent-acc-2024-001",
  "account_name": "Goals",
  "child_account_names": ["Farming", "Education"],
  "child_count": 2,
  "start_time": "2025-07-10T10:40:12.004Z",
  "end_time": "2025-07-10T10:40:12.029Z",
  "execution_time": 0.025
}
```

## ❌ Failure Response Examples

| Status | Reason                             |
| ------ | ---------------------------------- |
| `403`  | Unauthorized to access the account |
| `404`  | Parent account not found           |
| `500`  | Internal error                     |


## 🧠 Notes
- This is a read-only operation — no lock is acquired.
- The list only includes direct children, not grandchildren.
- Used for account navigation, visualization, and hierarchy management.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
