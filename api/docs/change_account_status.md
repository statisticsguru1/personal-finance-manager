# 🔄 POST /change_account_status

Changes the status of a **child or grandchild** account. Main or parent accounts are not eligible for this operation.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/change_account_status`
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```


---

## 📥 Request Body Parameters

| Name     | Type   | Required | Description                                                             |
|----------|--------|----------|-------------------------------------------------------------------------|
| `uuid`   | string | ✅       | UUID of the account to update                                           |
| `status` | string | ✅       | New status to apply: `"active"`, `"inactive"`, or `"closed"`            |

---

## 💡 Example Request

```json
{
  "uuid": "abc123-child-account",
  "status": "inactive"
}
```

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "abc123-child-account",
  "new_status": "inactive",
  "message": "Account Rent status updated to inactive",
  "start_time": "2025-07-10T07:10:10.003Z",
  "end_time": "2025-07-10T07:10:10.022Z",
  "execution_time": 0.019
}
```

## ❌ Error Responses
| Status | Description                                                  |
| ------ | ------------------------------------------------------------ |
| `400`  | Missing parameters or invalid status value                   |
| `403`  | Attempt to change a non-child account or unauthorized access |
| `404`  | Account not found                                            |
| `500`  | Unexpected server error                                      |

## 🧠 Notes
- Status transitions may impact how funds are allocated (e.g., inactive sub-accounts stop drawing allocations).

- Only accounts inheriting from ChildAccount class are eligible.

- This endpoint is locked via with_account_lock() to prevent race conditions during status changes.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
