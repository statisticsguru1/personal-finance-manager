# 🔧 POST /set_priority

Sets or updates the **priority** of a child or grandchild account. This affects ordering and potentially allocation rules if implemented.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/set_priority`
- **Tag**: `accounts`

---

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/x-www-form-urlencoded
```

---

## 📥 Form Parameters

| Name     | Type   | Required | Description                                   |
|----------|--------|----------|-----------------------------------------------|
| uuid     | string | ✅       | UUID of the child or grandchild account       |
| priority | string/number | ✅ | New priority value (can be numeric or labeled) |

---

## 💡 Example Request

```json

{
  "uuid": "abc",
  "priority": 1
}


```
### ✅ Success Response

``` json
{
  "success": true,
  "status": 200,
  "message": "Priority set to 1",
  "uuid": "acc-12345",
  "priority": "1",
  "start_time": "2025-07-10T08:02:01.473Z",
  "end_time": "2025-07-10T08:02:01.490Z",
  "execution_time": 0.017
}
```
## ❌ Error Responses
| Status | Description                                              |
| ------ | -------------------------------------------------------- |
| `400`  | Missing parameters or invalid priority format            |
| `403`  | Account is not a child/grandchild or unauthorized access |
| `404`  | Account tree or account not found                        |
| `500`  | Internal server error   
                                 
## 🧠 Notes
- This endpoint will only work for accounts that inherit from ChildAccount.

- Priority can be a number (e.g., 1, 2) or a string (e.g., "high", "low"), depending on your implementation logic.

- Used alongside account auto-allocation or sorting behavior.