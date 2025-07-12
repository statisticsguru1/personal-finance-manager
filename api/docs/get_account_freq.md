# 🔁 GET /get_account_freq

Returns the **frequency** (such as `Monthly`, `Weekly`, etc.) currently assigned to a grandchild account.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/get_account_freq`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 Query Parameters

| Name  | Type   | Required | Description                           |
|-------|--------|----------|---------------------------------------|
| uuid  | string | ✅       | UUID of the grandchild account        |

---

## 💡 Example Request

```http
GET /get_account_freq?uuid=acc-7890 HTTP/1.1
Authorization: Bearer <your-token>
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-7890",
  "freq": 30,
  "start_time": "2025-07-10T16:05:12.003Z",
  "end_time": "2025-07-10T16:05:12.020Z",
  "execution_time": 0.017
}
```

---

## ❌ Error Responses

| Status | Description                                                    |
|--------|----------------------------------------------------------------|
| `400`  | Missing `uuid`                                                 |
| `403`  | Account not found or not a grandchild account                  |
| `404`  | Account tree not found or unauthorized access                  |
| `500`  | Internal server error                                          |

---

## 🧠 Notes

- This endpoint is limited to accounts inheriting from `GrandchildAccount`.
- The returned frequency is expected to be a predefined label (e.g., 7, 30, 14) based on your internal logic.
