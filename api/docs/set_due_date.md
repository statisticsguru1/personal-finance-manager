# 📅 POST /set_due_date

Assigns or updates the **due date** of a grandchild account. Useful for managing financial obligations such as rent, loan payments, or bill deadlines.

---

## 📌 Summary

- **Method**: `POST`  
- **URL**: `/set_due_date`  
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/x-www-form-urlencoded
```

---

## 📥 Form Parameters

| Name      | Type   | Required | Description                                                 |
|-----------|--------|----------|-------------------------------------------------------------|
| uuid      | string | ✅       | UUID of the grandchild account                              |
| due_date  | string | ✅       | Due date in ISO format (`YYYY-MM-DD` or `YYYY-MM-DDTHH:MM:SSZ`) |

---

## 💡 Example Request


```json
{
  "uuid": "acc-5678",
  "due_date": "2025-08-15T00:00:00Z"
}
```
---

### ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "message": "Due date set for Internet",
  "uuid": "acc-5678",
  "due_date": "2025-08-15 00:00:00",
  "start_time": "2025-07-10T10:45:15.001Z",
  "end_time": "2025-07-10T10:45:15.020Z",
  "execution_time": 0.019
}
```

---

## ❌ Error Responses

| Status | Description                                               |
|--------|-----------------------------------------------------------|
| `400`  | UUID or due date missing, or due date has invalid format |
| `403`  | Not a grandchild account or unauthorized access           |
| `404`  | Account tree not found                                    |
| `500`  | Internal server error                                     |

---

## 🧠 Notes

- This endpoint **only works** on accounts inheriting from `GrandchildAccount`.  
- The due date must be a valid date string, preferably in ISO format (e.g., `"2025-08-01"` or `"2025-08-01T00:00:00Z"`).  
- Use in conjunction with scheduled alerts, arrears tracking, or automated reminders.
