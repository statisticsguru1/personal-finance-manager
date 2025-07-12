# 📄 GET /get_transactions

Retrieves the full transaction history (deposits, withdrawals, distributions, etc.) for a given account. Requires authentication and the user must be either the account owner or an admin.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/get_transactions`
- **Auth**: Required
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 🔍 Query Parameters

| Name   | Type   | Required | Description                        |
|--------|--------|----------|------------------------------------|
| `uuid` | string | ✅       | UUID of the account to inspect     |

---

## 📥 Example Request

```
GET /get_transactions?uuid=acc-2024-123
```
---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-123",
  "transaction_count": 3,
  "transactions": [
    {
      "type": "deposit",
      "amount": 1000,
      "channel": "mpesa",
      "by": "User",
      "timestamp": "2025-07-10 08:00:00"
    },
    {
      "type": "withdraw",
      "amount": 400,
      "channel": "bank",
      "by": "User",
      "timestamp": "2025-07-11 10:15:00"
    },
    {
      "type": "distribute",
      "amount": 600,
      "by": "System",
      "timestamp": "2025-07-12 09:00:00"
    }
  ],
  "start_time": "2025-07-10T10:01:15.120Z",
  "end_time": "2025-07-10T10:01:15.150Z",
  "execution_time": 0.03
}
```

## ❌ Failure Response Examples

| Status | Reason                             |
| ------ | ---------------------------------- |
| `403`  | Unauthorized to access the account |
| `404`  | Account not found                  |
| `500`  | Server-side or internal error      |


## 🧠 Notes
- Returns an array of transactions sorted chronologically (if implemented in your backend).
- You must include a valid Bearer token to access this endpoint.
- Returns an appropriate error if the account does not exist or is not accessible by the caller.
- Account locking is not required since this is a read-only operation.