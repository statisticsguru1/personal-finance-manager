# 💰 GET /total_income

Returns the total income (deposits/credits) received by an account and its sub-accounts over a specified date range.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/total_income`
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

| Name    | Type   | Required | Description                                       |
|---------|--------|----------|---------------------------------------------------|
| `uuid`  | string | ✅       | UUID of the account to compute income for        |
| `from`  | string | ❌       | Start date (`YYYY-MM-DD`). Defaults to earliest  |
| `to`    | string | ❌       | End date (`YYYY-MM-DD`). Defaults to today       |

---

## 📥 Example Request

```
GET /total_income?uuid=acc-2024-001&from=2025-01-01&to=2025-07-01
```
## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-001",
  "from": "2025-01-01",
  "to": "2025-07-01",
  "total_income": 37500,
  "start_time": "2025-07-10T14:17:11.125Z",
  "end_time": "2025-07-10T14:17:11.174Z",
  "execution_time": 0.049
}
```

## ❌ Failure Response Examples

| Status | Reason                            |
| ------ | --------------------------------- |
| `400`  | UUID not provided or invalid date |
| `403`  | Unauthorized access               |
| `404`  | Account not found                 |
| `500`  | Internal server error             |

## 🧠 Notes
- Internally uses account$total_income(date_range) method.

- If from or to are not provided, defaults to entire history.
- Income includes deposit-type transactions across the entire subtree.