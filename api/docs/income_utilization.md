# 📊 GET /income_utilization

Computes how much of the income was spent within a given date range for a specific account and its children.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/income_utilization`
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

| Name   | Type   | Required | Description                                           |
|--------|--------|----------|-------------------------------------------------------|
| `uuid` | string | ✅       | UUID of the account                                   |
| `from` | string | ❌       | Start date (`YYYY-MM-DD`). Defaults to earliest date |
| `to`   | string | ❌       | End date (`YYYY-MM-DD`). Defaults to today           |

---

## 📥 Example Request

```
GET /income_utilization?uuid=acc-2024-001&from=2024-01-01&to=2024-12-31
```

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "uuid": "acc-2024-001",
  "from": "2024-01-01",
  "to": "2024-12-31",
  "utilization": 0.82,
  "start_time": "2025-07-10T16:00:15.012Z",
  "end_time": "2025-07-10T16:00:15.061Z",
  "execution_time": 0.049
}
```

## ❌ Failure Response Examples

| Status | Reason                                |
| ------ | ------------------------------------- |
| `400`  | UUID is missing or invalid            |
| `403`  | Unauthorized access                   |
| `404`  | Account or tree not found             |
| `500`  | Internal server error or date parsing |


## 🧠 Notes
- Internally uses account$income_utilization(date_range).

- Utilization = spending / income, typically between 0 and 1.

- Returns 0 if no income is recorded during the range.

- Accepts optional from and to parameters for flexible filtering.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
