

# 👤 POST /register

Creates a new user account tree. Requires a valid `user_id`. Returns confirmation of successful user creation or appropriate error message.

---

## 📌 Summary

- **Method**: `POST`
- **URL**: `/register`
- **Auth**: Not Required
- **Tag**: `admin`

---

## 🔐 Headers

```
Content-Type: application/json
```

---

## 📤 Request Parameters

| Name      | Type   | Required | Description                          |
|-----------|--------|----------|--------------------------------------|
| `user_id` | string | ✅       | Unique ID for the user (e.g., UUID)  |

---

## 📥 Example Request Body (JSON)

```json
{
  "user_id": "4a76343a-e971-4bd7-b8c4-5fa9fccee433"
}
```

---

## 📤 Success Response

```json
{
  "success": true,
  "status": 201,
  "message": "User directory created",
  "user_id": "4a76343a-e971-4bd7-b8c4-5fa9fccee433",
  "start_time": "2025-07-17T08:32:00.511Z",
  "end_time": "2025-07-17T08:32:00.543Z",
  "execution_time": 0.032
}
```

---

## ❌ Failure Response Examples

| Status | Reason                      |
|--------|-----------------------------|
| `400`  | Missing required user_id    |
| `400`  | Invalid user ID format      |
| `409`  | User already exists         |
| `500`  | Internal server error       |

---

## 🧠 Notes

- `user_id` must be a valid string identifier (UUIDs recommended).
- User ID is sanitized and used to generate a secure directory structure.
- No JWT token required, but rate limiting and exponential backoff are applied to prevent abuse.
- The request is fast and designed for one-time setup per user.

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
