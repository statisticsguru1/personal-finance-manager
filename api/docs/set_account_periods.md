# 📅 POST /set_account_periods

Sets the **number of periods** for a grandchild account. Useful for budgeting logic that depends on period breakdown (e.g., 12 months, 4 quarters).

---

## 📌 Summary

- **Method**: `POST`  
- **URL**: `/set_account_periods`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 JSON Body Parameters

| Name    | Type    | Required | Description                                     |
|---------|---------|----------|-------------------------------------------------|
| uuid    | string  | ✅       | UUID of the grandchild account                  |
| periods | integer | ✅       | Number of periods (must be a positive integer)  |

---

## 💡 Example Request

```json
{
  "uuid": "acc-3487",
  "periods": 12
}
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-3487",
  "periods": 12,
  "message": "Number of periods set to 12",
  "start_time": "2025-07-10T16:03:01.120Z",
  "end_time": "2025-07-10T16:03:01.136Z",
  "execution_time": 0.016
}
```

---

## ❌ Error Responses

| Status | Description                                                    |
|--------|----------------------------------------------------------------|
| `400`  | Missing or invalid parameters (e.g., `uuid`, `periods`)        |
| `403`  | Account not found or not a grandchild account                  |
| `404`  | User not found or unauthorized access                          |
| `500`  | Internal server error                                          |

---

## 🧠 Notes

- This endpoint only works for accounts that inherit from `GrandchildAccount`.
- The `periods` value should be a positive whole number.


---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
