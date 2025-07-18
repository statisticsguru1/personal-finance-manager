# 🔐 Auth Filter

**Scope:** All protected endpoints  
**Route Type:** Global filter (`@filter auth`)  
**Tag:** `auth`

---

## 📌 Description

The `auth` filter is applied to all incoming requests **While**  `/login`
and  `/__ping__`, are exempt from JWT authentication, they are still subject to rate limiting and exponential backoff to prevent brute-force attacks.

It performs these responsibilities:

- 🔑 Verifies Bearer token from the `Authorization` header
- 🧾 Decodes token and attaches `user_id` and `role` to the request
- 📊 Enforces rate limiting using `MAX_REQUESTS` per `WINDOW_SIZE`
- ⏳ Applies **exponential backoff** for repeated failed logins

---

## 📫 Header Example

```
Authorization: Bearer eyJhbGciOiJIUzI1...
```

---

## 🔁 Response Summary

| Condition                   | Status | Example Error                                      |
|----------------------------|--------|----------------------------------------------------|
| Missing or malformed token | 401    | `{ "error": "Missing or invalid token" }`         |
| Invalid or expired token   | 401    | `{ "error": "Invalid or expired token" }`         |
| Too many failed attempts   | 429    | `{ "error": "Too many failed attempts. Wait 8s." }`|
| Rate limit exceeded        | 429    | `{ "error": "Rate limit exceeded. Try later." }`  |
| Misconfigured secret       | 500    | `{ "error": "Server misconfigured (missing JWT_SECRET)" }` |

---

## 🧠 Internal Notes

If the token is valid, your endpoint functions will have:

```r
req$user_id  # Unique user ID
req$role     # Role, e.g., "user", "admin"
```

The filter logic is defined in `plumber.R` using `@filter auth`.

---

## 🛠️ Environment Configuration

| Env Variable   | Purpose                                        | Default |
|----------------|------------------------------------------------|---------|
| `JWT_SECRET`   | Secret key for decoding JWT                    | _(required)_ |
| `MAX_REQUESTS` | Max allowed requests in the window             | `1000`  |
| `WINDOW_SIZE`  | Rate limit window duration in seconds          | `3600`  |

---
## 💖 Sponsors

Support my work through [GitHub Sponsors](https://github.com/sponsors/statisticsguru1)!

[![GitHub Sponsors](https://img.shields.io/github/sponsors/statisticsguru1?style=flat-square)](https://github.com/sponsors/statisticsguru1)
