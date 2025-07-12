# 💸 GET /spending

Calculates the total amount withdrawn (spent) from a given account (and its sub-accounts) within a specified date range.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/spending`
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

| Name    | Type   | Required | Description                                         |
|---------|--------|----------|-----------------------------------------------------|
| `uuid`  | string | ✅       | UUID of the account to evaluate                    |
| `from`  | string | ❌       | Start date (`YYYY-MM-DD`). Defaults to oldest date |
| `to`    | string | ❌       | End date (`YYYY-MM-DD`). Defaults to today         |

---

## 📥 Example Request

```
GET /spending?uuid=acc-2024-001&from=2025-01-01&to=2025-07-01
```

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-001",
  "from": "2025-01-01",
  "to": "2025-07-01",
  "total_spending": 24950,
  "start_time": "2025-07-10T14:01:02.830Z",
  "end_time": "2025-07-10T14:01:02.884Z",
  "execution_time": 0.054
}
```

## ❌ Failure Response Examples

| Status | Reason                       |
| ------ | ---------------------------- |
| `400`  | Missing UUID or invalid date |
| `403`  | Unauthorized access          |
| `404`  | Account not found            |
| `500`  | Internal server error        |

## 🧠 Notes
- Internally uses account$spending(daterange) method.
- If no from or to is specified, defaults to entire available history.
- Spending only includes withdrawal-type transactions.