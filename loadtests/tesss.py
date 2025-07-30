import time, uuid
import jwt as pyjwt
import requests
import random
JWT_SECRET = "test-secret"

def generate_token(user_id):
    payload = {
        "user_id": user_id,
        "role": "user",
        "iat": int(time.time())
    }
    return pyjwt.encode(payload, JWT_SECRET, algorithm="HS256")

user_id = f"user_{uuid.uuid4().hex[:8]}"
token = generate_token(user_id)

r=requests.post(
    url="http://127.0.0.1:8000/register",
    #data=dict(user_id=user_id),
    json={"user_id": user_id},
    headers={
                "Authorization": f"Bearer {token}"
            }
)

out = r.json().get('uuid')
print(out)

r1=requests.post(
    url="http://127.0.0.1:8000//add_sub_account",
    #data=dict(user_id=user_id),
    json={
        "parent_uuid": out,
        "name": f"acct_{uuid.uuid4().hex[:6]}",
        "allocation": round(random.uniform(0.1, 1.0), 2),
        "priority": random.randint(0, 3),
        "fixed_amount": random.choice([0, 100, 250]),
        "status": "active",
        "due_date": "2025-12-31T00:00:00Z",
        "account_type": random.choice(["FixedSaving", "NonFixedSaving", "Debt", "Bill","Expense"]),
        "freq": random.choice([1, 2, 4])
        },
    headers={
                "Authorization": f"Bearer {token}"
            }
)

print(r1.json())

r11=requests.get(
    url="http://127.0.0.1:8000/get_account_freq",
    #data=dict(user_id=user_id),
    json={
        "uuid": r1.json().get("uuid"),
        },
    headers={
                "Authorization": f"Bearer {token}"
            }
)

print(r11.json())


r21=requests.post(
    url="http://127.0.0.1:8000/add_sub_account",
    #data=dict(user_id=user_id),
    json={
        "parent_uuid": r1.json().get("uuid"),
        "name": f"acct_{uuid.uuid4().hex[:6]}",
        "allocation": round(random.uniform(0.1, 1.0), 2),
        "priority": random.randint(0, 3),
        "fixed_amount": random.choice([0, 100, 250]),
        "status": "active",
        "due_date": "2025-12-31T00:00:00Z",
        "account_type": random.choice(["FixedSaving", "NonFixedSaving", "Debt", "Bill","Expense"]),
        "freq": random.choice([1, 2, 4])
        },
    headers={
                "Authorization": f"Bearer {token}"
            }
)

print(r21.json())