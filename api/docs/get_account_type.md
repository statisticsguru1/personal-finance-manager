# 🔍 GET /get_account_type

Retrieves the **account type** (label or identifier) of a grandchild account.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/get_account_type`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 Query Parameters

| Name | Type   | Required | Description                      |
|------|--------|----------|----------------------------------|
| uuid | string | ✅       | UUID of the grandchild account   |

---

## 💡 Example Request

```http
GET /get_account_type?uuid=acc-3245
Authorization: Bearer <your-token>
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-3245",
  "account_type": "loan",
  "start_time": "2025-07-10T13:12:45.210Z",
  "end_time": "2025-07-10T13:12:45.226Z",
  "execution_time": 0.016
}
```

---

## ❌ Error Responses

| Status | Description                                                   |
|--------|---------------------------------------------------------------|
| `400`  | Missing or invalid UUID                                       |
| `403`  | Account not found or not a grandchild account                 |
| `404`  | User tree not found or unauthorized access                    |
| `500`  | Internal server error                                         |

---

## 🧠 Notes

- This endpoint is restricted to accounts that inherit from `GrandchildAccount`.
- The returned `account_type` may be used for UI grouping, sorting, or business rules.
