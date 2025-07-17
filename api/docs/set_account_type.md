# 🏷️ POST /set_account_type

Updates the **account type** of a grandchild account. Account types can represent categories such as "utility", "subscription", "loan", etc.

---

## 📌 Summary

- **Method**: `POST`  
- **URL**: `/set_account_type`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/x-www-form-urlencoded
```

---

## 📥 Form Parameters

| Name         | Type   | Required | Description                                         |
|--------------|--------|----------|-----------------------------------------------------|
| uuid         | string | ✅       | UUID of the grandchild account                      |
| account_type | string | ✅       | New account type label or identifier (e.g. "rent")  |

---

## 💡 Example Request

```json
{
  "uuid": "acc-3245",
  "account_type": "loan"
}
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Account type updated to loan",
  "uuid": "acc-3245",
  "account_type": "loan",
  "start_time": "2025-07-10T12:44:22.300Z",
  "end_time": "2025-07-10T12:44:22.314Z",
  "execution_time": 0.014
}
```

---

## ❌ Error Responses

| Status | Description                                                   |
|--------|---------------------------------------------------------------|
| `400`  | Missing parameters or invalid format                          |
| `403`  | Not a grandchild account or unauthorized access               |
| `404`  | Account tree or account not found                             |
| `500`  | Internal server error during update                           |

---

## 🧠 Notes

- Only valid for accounts that inherit from `GrandchildAccount`.
- You can use `account_type` for filtering, reporting, or logic grouping.


---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
