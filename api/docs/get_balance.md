# 💰 GET /get_balance

Retrieve the current balance of a specified account by UUID.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/get_balance`
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

| Name    | Type   | Required | Description                   |
|---------|--------|----------|-------------------------------|
| `uuid`  | string | ✅       | UUID of the account to check  |

---

## 📥 Example Request


```
GET /get_balance?uuid=main-2023-001
Authorization: Bearer eyJhbGciOiJIUzI1...
```


## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "main-2023-001",
  "balance": 5120,
  "start_time": "2025-07-10T11:25:14.110Z",
  "end_time": "2025-07-10T11:25:14.118Z",
  "execution_time": 0.008
}
```

## ❌ Failure Responses

| Status | Error Message                     |
| ------ | --------------------------------- |
| 403    | Account not found or unauthorized |
| 404    | Account not found (admin access)  |
| 500    | Server-side error                 |


## 🧠 Notes
- This is a read-only operation and doesn't require account locking.
- If the account is not found or the user lacks permissions, appropriate error responses are returned.
- Internally uses account$get_balance().