# 💵 POST /set_fixed_amount

Sets a **fixed amount** for a grandchild account, which is used to calculate the `amount_due` for the current or future periods.

---

## 📌 Summary

- **Method**: `POST`  
- **URL**: `/set_fixed_amount`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 JSON Body Parameters

| Name         | Type   | Required | Description                                           |
|--------------|--------|----------|-------------------------------------------------------|
| uuid         | string | ✅       | UUID of the grandchild account                        |
| fixed_amount | number | ✅       | Fixed amount per period (must be non-negative number) |

---

## 💡 Example Request

```json
{
  "uuid": "acc-9999",
  "fixed_amount": 1500
}
```

---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-9999",
  "fixed_amount": 1500,
  "amount_due": 1500,
  "start_time": "2025-07-10T11:42:11.112Z",
  "end_time": "2025-07-10T11:42:11.130Z",
  "execution_time": 0.018
}
```

---

## ❌ Error Responses

| Status | Description                                                  |
|--------|--------------------------------------------------------------|
| `400`  | Missing or invalid parameters (e.g., empty UUID or amount)   |
| `403`  | Account is not a grandchild or access not permitted          |
| `404`  | Account tree or specific account not found                   |
| `500`  | Internal server error                                        |

---

## 🧠 Notes

- This endpoint applies only to accounts inheriting from `GrandchildAccount`.
- `fixed_amount` must be a numeric value ≥ 0.
- Once set, `amount_due` will be automatically updated based on the new amount.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
