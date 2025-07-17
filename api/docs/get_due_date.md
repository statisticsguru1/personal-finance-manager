# 📅 GET /get_due_date

Retrieves the **due date** set for a grandchild account. This is useful for determining deadlines such as rent, bills, or recurring payments.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/get_due_date`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
```

---

## 📥 Query Parameters

| Name | Type   | Required | Description                          |
|------|--------|----------|--------------------------------------|
| uuid | string | ✅       | UUID of the grandchild account       |

---

## 💡 Example Request

```http
GET /get_due_date?uuid=acc-5678
Authorization: Bearer <your-token>
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-5678",
  "due_date": "2025-08-15 00:00:00",
  "start_time": "2025-07-10T11:00:00.012Z",
  "end_time": "2025-07-10T11:00:00.020Z",
  "execution_time": 0.008
}
```

---

## ❌ Error Responses

| Status | Description                                               |
|--------|-----------------------------------------------------------|
| `400`  | UUID is missing                                           |
| `403`  | Account is not a grandchild or unauthorized access        |
| `404`  | Account tree or account not found                         |
| `500`  | Internal server error                                     |

---

## 🧠 Notes

- Only accounts that inherit from `GrandchildAccount` will return a valid due date.
- The due date is returned as a string in standard POSIX format.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
