# 🔁 POST /set_account_freq

Sets or updates the **frequency** of a grandchild account, which controls the budgeting or due cycle of the account.

---

## 📌 Summary

- **Method**: `POST`  
- **URL**: `/set_account_freq`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/x-www-form-urlencoded
```

---

## 📥 Form Parameters

| Name         | Type   | Required | Description                                 |
|--------------|--------|----------|---------------------------------------------|
| uuid         | string | ✅       | UUID of the grandchild account              |
| account_freq | string | ✅       | Frequency to assign (e.g., "Monthly", "Biweekly") |

---

## 💡 Example Request

```json
{
  "uuid": "acc-7890",
  "account_freq": 30
}
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Frequency for Internet set to Monthly",
  "uuid": "acc-7890",
  "freq": 30,
  "start_time": "2025-07-10T15:21:12.003Z",
  "end_time": "2025-07-10T15:21:12.020Z",
  "execution_time": 0.017
}
```

---

## ❌ Error Responses

| Status | Description                                                    |
|--------|----------------------------------------------------------------|
| `400`  | Missing `uuid` or `account_freq`                               |
| `403`  | Account not found or not a grandchild account                  |
| `404`  | User not found or unauthorized access                          |
| `500`  | Internal server error or failed to update account              |

---

## 🧠 Notes

- This endpoint only applies to accounts that inherit from `GrandchildAccount`.
- Frequency is useful when calculating periodic due amounts or expected contributions.
- Valid values depend on how your logic is implemented: e.g., `30`, `7`, `1`, etc.
