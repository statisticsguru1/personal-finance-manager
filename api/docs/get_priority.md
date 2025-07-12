# 🔍 GET /get_priority
Returns the priority value of a child or grandchild account.

## 📌 Summary
- **Method**: GET
- **URL**: /get_priority
- **Auth**: Required
- **Tag**: accounts

## 🔐 Headers

```
Authorization: Bearer <your-token>
Content-Type: application/json
```
| Name | Type   | Required | Description                             |
| ---- | ------ | -------- | --------------------------------------- |
| uuid | string | ✅        | UUID of the child or grandchild account |

## 💡 Example Request
```
GET /get_priority?uuid=acc-12345
Authorization: Bearer <your-token>
```
## ❌ Error Responses
| Status | Description                                         |
| ------ | --------------------------------------------------- |
| `400`  | UUID not provided                                   |
| `403`  | Account not found or not a child/grandchild account |
| `404`  | Account tree not found for the user                 |
| `500`  | Internal server error                               |


## 🧠 Notes
- This endpoint is valid only for accounts that inherit from ChildAccount.

- The returned priority can be numeric or a string (e.g., "high", "low"), depending on your implementation.

- To set or change a priority, use the POST /set_priority endpoint.