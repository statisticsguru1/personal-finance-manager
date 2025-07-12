# 🟢 GET /get_account_status

Retrieves the current status (`active`, `inactive`, or `closed`) of a **child or grandchild** account.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/get_account_status`
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```

---

## 📥 Query Parameters

| Name   | Type   | Required | Description                        |
|--------|--------|----------|------------------------------------|
| uuid   | string | ✅       | UUID of the child/grandchild account |

---

## 💡 Example Request

```
GET /get_account_status?uuid=a3d5-xyz-7890
```

---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "a3d5-xyz-7890",
  "account_status": "inactive",
  "start_time": "2025-07-10T07:32:10.145Z",
  "end_time": "2025-07-10T07:32:10.160Z",
  "execution_time": 0.015
}
```
## ❌ Error Responses

| Status | Description                                                   |
| ------ | ------------------------------------------------------------- |
| `400`  | Missing `uuid` parameter                                      |
| `403`  | Account not found or not a child/grandchild account           |
| `404`  | Account tree not found (if user unauthorized or file missing) |
| `500`  | Unexpected internal server error                              |

## 🧠 Notes
- This endpoint strictly checks that the account is a subclass of ChildAccount.

- This check prevents status queries for Main or Parent accounts.