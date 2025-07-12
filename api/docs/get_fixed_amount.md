# 💰 GET /get_fixed_amount

Retrieves the **fixed amount** currently assigned to a grandchild account. This amount is used to compute the expected `amount_due`.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/get_fixed_amount`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 Query Parameters

| Name | Type   | Required | Description                                |
|------|--------|----------|--------------------------------------------|
| uuid | string | ✅       | UUID of the grandchild account to retrieve |

---

## 💡 Example Request

```http
GET /get_fixed_amount?uuid=acc-12345 HTTP/1.1
Authorization: Bearer your-token
Content-Type: application/json
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-12345",
  "fixed_amount": 1500,
  "start_time": "2025-07-10T12:15:42.101Z",
  "end_time": "2025-07-10T12:15:42.116Z",
  "execution_time": 0.015
}
```

---

## ❌ Error Responses

| Status | Description                                                   |
|--------|---------------------------------------------------------------|
| `400`  | Missing UUID or invalid query parameter                       |
| `403`  | Not a grandchild account or unauthorized access               |
| `404`  | Account tree or account not found                             |
| `500`  | Internal server error                                         |

---

## 🧠 Notes

- This endpoint is strictly for accounts inheriting from `GrandchildAccount`.
- It does **not** set or change the value — use `/set_fixed_amount` to update it.
