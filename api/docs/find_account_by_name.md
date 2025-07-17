# 🔍 GET /find_account_by_name

Searches for accounts in the user's account tree that match a given name. Useful for quickly locating accounts without needing their UUID.

---

## 📌 Summary

- **Method**: `GET`
- **URL**: `/find_account_by_name`
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

| Name   | Type   | Required | Description                      |
|--------|--------|----------|----------------------------------|
| `name` | string | ✅       | Name of the account to look up  |

---

## 📥 Example Request

```
GET /find_account_by_name?name=Rent
```


---

## ✅ Success Response

```json
{
  "success": true,
  "status": 200,
  "search_name": "Rent",
  "total_matches": 2,
  "matches": [
    {
      "uuid": "acc-234",
      "name": "Rent",
      "path": "Main > Needs > Rent"
    },
    {
      "uuid": "acc-987",
      "name": "Rent",
      "path": "Main > Rentals > Rent"
    }
  ],
  "start_time": "2025-07-10T11:20:10.050Z",
  "end_time": "2025-07-10T11:20:10.061Z",
  "execution_time": 0.011
}
```

## ❌ Failure Response Examples

| Status | Reason                                  |
| ------ | --------------------------------------- |
| `403`  | Unauthorized or tree not found          |
| `500`  | Missing argument `name` or server error |

---

## 🧠 Notes
- Matching is case-sensitive unless your backend handles normalization.
- Returns all matches if the name appears more than once.
- Use uuid from results for further operations (e.g., get balance, transactions).

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
