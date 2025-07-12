# 🆔 GET /find_account_by_uuid

Returns detailed information about a specific account using its UUID.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/find_account_by_uuid`
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

| Name    | Type   | Required | Description                    |
|---------|--------|----------|--------------------------------|
| `uuid`  | string | ✅       | UUID of the account to lookup  |

---

## 📥 Example Request

```
GET /find_account_by_uuid?uuid=acc-2024-001
```

---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-001",
  "name": "Internet",
  "path": "Main > Needs > Internet",
  "type": "GrandchildAccount",
  "balance": 4200,
  "total_balance": 4200,
  "amount_due": 0,
  "total_amount_due": 0,
  "allocation": 0.15,
  "parent_uuid": "acc-2024-000",
  "start_time": "2025-07-10T11:55:10.215Z",
  "end_time": "2025-07-10T11:55:10.228Z",
  "execution_time": 0.013
}
```

## ❌ Failure Response Examples

| Status | Reason                                   |
| ------ | ---------------------------------------- |
| `400`  | UUID is missing in the request           |
| `403`  | Unauthorized access to the account       |
| `404`  | Account not found                        |
| `500`  | Account tree file not available or error |


## 🧠 Notes
- The endpoint retrieves all core metadata for an account.
- If the account has no parent, parent_uuid will be null.
- This endpoint can help render full account detail views or support  downstream actions.