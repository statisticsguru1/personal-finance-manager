# 📊 GET /compute_total_balance

Returns the total balance of a specified account including all its children and descendants.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/compute_total_balance`
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
| `uuid` | string | ✅       | UUID of the account to compute total balance for |

---

## 📥 Example Request

```
GET /compute_total_balance?uuid=acc-2024-456
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-456",
  "total_balance": 18450.75,
  "start_time": "2025-07-10T13:02:15.120Z",
  "end_time": "2025-07-10T13:02:15.180Z",
  "execution_time": 0.06
}
```

## ❌ Failure Response Examples

| Status | Reason                             |
| ------ | ---------------------------------- |
| `400`  | Missing or invalid UUID            |
| `403`  | Unauthorized to access the account |
| `404`  | Account not found                  |
| `500`  | Internal server error              |


## 🧠 Notes
- Internally calls account$compute_total_balance().
- This is a read-only operation (no locking needed).
- Ideal for getting an overview of full funds under a category like "Needs" or "Investments".