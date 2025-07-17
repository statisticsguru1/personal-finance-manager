# 🧩 POST /add_sub_account

Creates a new child or grandchild account under a specified parent account.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/add_sub_account`
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

| Name            | Type   | Required | Description                                                              |
|-----------------|--------|----------|--------------------------------------------------------------------------|
| `parent_uuid`   | string | ✅       | UUID of the parent account                                               |
| `name`          | string | ✅       | Name of the new account (must be unique under this parent)              |
| `allocation`    | float  | ✅       | Allocation percentage (between 0 and 1)                                  |
| `priority`      | int    | ❌       | Priority weight for ordering (default: 0)                                |
| `fixed_amount`  | float  | ❌       | Fixed allocation amount (only used for grandchild accounts)              |
| `due_date`      | string | ❌       | Optional due date (ISO-8601 format)                                      |
| `account_type`  | string | ❌       | Type of grandchild account (e.g., "loan", "expense")                     |
| `freq`          | int    | ❌       | Frequency of recurring charges (used only for grandchild accounts)       |

---

## 📥 Example Request

```json
{
  "parent_uuid": "acc-main-2023-001",
  "name": "Rent",
  "allocation": 0.3,
  "priority": 1,
  "fixed_amount": 2000,
  "due_date": "2025-08-01T00:00:00Z",
  "account_type": "fixed",
  "freq": 30
}
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Rent added under Needs",
  "child_type": "GrandchildAccount",
  "allocation": 0.3,
  "start_time": "2025-07-10T10:01:35.120Z",
  "end_time": "2025-07-10T10:01:35.210Z",
  "execution_time": 0.09
}
```

## ❌ Failure Responses

| Status | Error Reason                              |
| ------ | ----------------------------------------- |
| 404    | Parent account not found                  |
| 400    | Invalid allocation or name already exists |
| 500    | Unexpected system or conversion errors    |

## 🧠 Notes

- Determines account type by checking the class of the parent account.
- Locks the user's account tree during modification.
- All accounts are persisted to account_tree.Rds.
