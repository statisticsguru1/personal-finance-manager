# 📅 GET /get_account_periods

Fetches the configured **number of periods** for a grandchild account. This is useful for understanding budgeting cycles such as monthly, quarterly, or custom-defined durations.

---

## 📌 Summary

- **Method**: `GET`  
- **URL**: `/get_account_periods`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 Query Parameters

| Name  | Type   | Required | Description                             |
|-------|--------|----------|-----------------------------------------|
| uuid  | string | ✅       | UUID of the grandchild account          |

---

## 💡 Example Request

```
GET /get_account_periods?uuid=acc-12345
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-12345",
  "periods": 12,
  "start_time": "2025-07-10T17:10:15.422Z",
  "end_time": "2025-07-10T17:10:15.438Z",
  "execution_time": 0.016
}
```

---

## ❌ Error Responses

| Status | Description                                                    |
|--------|----------------------------------------------------------------|
| `400`  | Missing or invalid `uuid` parameter                            |
| `403`  | Account not found or not a `GrandchildAccount`                 |
| `404`  | User not found or unauthorized access                          |
| `500`  | Internal server error                                          |

---

## 🧠 Notes

- This endpoint is strictly applicable to grandchild accounts.
- Periods may correspond to budgeting periods or billing cycles, depending on your app logic.
