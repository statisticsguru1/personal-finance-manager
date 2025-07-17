# 🌲 GET /list_all_accounts

Lists all accounts in the tree rooted at a specified account, including both children and parents (if implemented to allow upward tracing).

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/list_all_accounts`
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

| Name   | Type   | Required | Description                                 |
|--------|--------|----------|---------------------------------------------|
| `uuid` | string | ✅       | UUID of the account to start traversal from |

---

## 📥 Example Request

```
GET /list_all_accounts?uuid=goals-acc-2023-02
```

---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "goals-acc-2023-02",
  "total_accounts": 4,
  "account_names": [
    "Goals",
    "Farming",
    "Business",
    "Emergency Fund"
  ],
  "start_time": "2025-07-10T10:51:22.015Z",
  "end_time": "2025-07-10T10:51:22.030Z",
  "execution_time": 0.015
}
```
## ❌ Failure Response Examples

| Status | Reason                             |
| ------ | ---------------------------------- |
| `403`  | Unauthorized to access the account |
| `404`  | Account not found                  |
| `500`  | Internal error                     |


## 🧠 Notes
- This is a read-only endpoint.
- Internally calls account$list_all_accounts() which traverses children recursively.

- Does not include sibling accounts unless connected via hierarchy.

- Can be used for diagnostics, audit, or hierarchical views.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
