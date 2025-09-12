# 🗑️ DELETE /delete
Deletes a user account/sub accounts. If the target account is the root account its associated data file is removed, if a sub-account, only the account node is removed. Requires a valid uuid for target account parameter. Returns success message or appropriate error if the user doesn't exist or deletion fails.

---

## 📌 Summary

**Method:** `DELETE`
**URL:** `/delete`
**Auth:** Not Required
**Tag:** `accounts`
---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```
---

## 📤 Request Parameters

| Name      | Type   | Required | Description                         |
| --------- | ------ | -------- | ----------------------------------- |
| `uuid` | string | ✅        | Unique account identifier uuid        |


## 📥 Example Request (Query Parameter)

```
DELETE /delete?uuid=4a76343a-e971-4bd7-b8c4-5fa9fccee433
```
---

## 📤 Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Account for user_id '4a76343a-e971-4bd7-b8c4-5fa9fccee433' deleted successfully.",
  "start_time": "2025-07-17T08:34:02.123Z",
  "end_time": "2025-07-17T08:34:02.160Z",
  "execution_time": 0.037
}
```
---

## ❌ Failure Response Examples

| Status | Reason                                |
| ------ | ------------------------------------- |
| `400`  | Missing required `user_id`            |
| `404`  | Account for given `user_id` not found |
| `500`  | Internal server error during deletion |


```json
{
  "success": false,
  "status": 404,
  "error": "Account for user_id 'xyz' does not exist.",
  "start_time": "2025-07-17T08:34:02.123Z",
  "end_time": "2025-07-17T08:34:02.150Z",
  "execution_time": 0.027
}
```

---

## 🧠 Notes
- The user ID is used to identify the user's account file (e.g., account_tree.Rds) in persistent storage.
- The operation is wrapped in a file lock to ensure safe concurrent access.
- No JWT token required, but the endpoint is still protected by rate limiting and exponential backoff to prevent abuse.
- This is a destructive operation and should be called with caution.

---

## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
