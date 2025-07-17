# 🧩 POST /set_child_allocation

Update the allocation assigned to a child account under a parent.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/set_child_allocation`
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

| Name          | Type    | Required | Description                                      |
|---------------|---------|----------|--------------------------------------------------|
| `parent_uuid` | string  | ✅       | UUID of the parent account                       |
| `child_name`  | string  | ✅       | Name of the child account                        |
| `allocation`  | float   | ✅       | New allocation value (must be between 0 and 1)   |

---

## 📥 Example Request

```json
{
  "parent_uuid": "main-2023-001",
  "child_name": "Goals",
  "allocation": 0.35
}
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Allocation for Goals updated to 0.35",
  "total_allocation": 1,
  "start_time": "2025-07-10T11:01:14.120Z",
  "end_time": "2025-07-10T11:01:14.215Z",
  "execution_time": 0.095
}
```

## ❌ Failure Responses

| Status | Error Reason                      |
| ------ | --------------------------------- |
| 400    | Allocation missing or invalid     |
| 404    | Parent or child account not found |
| 500    | Server-side exception occurred    |


## 🧠 Notes
- The parent account must already exist and contain the named child.
- The total allocation across all children may be validated depending on internal logic.
- Safely locked and persisted to account_tree.Rds.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
