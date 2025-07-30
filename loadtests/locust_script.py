from datetime import datetime, timedelta
from locust import HttpUser, task, between, LoadTestShape
from locust.exception import StopUser
import time, uuid, random
import jwt as pyjwt


JWT_SECRET = "test-secret"

def generate_token(user_id):
    payload = {
        "user_id": user_id,
        "role": "user",
        "iat": int(time.time())
    }
    return pyjwt.encode(payload, JWT_SECRET, algorithm="HS256")

class APIUser(HttpUser):
    wait_time = between(1, 3)

    def on_start(self):
        try:
            self.user_id = f"user_{uuid.uuid4().hex[:8]}"
            self.token = generate_token(self.user_id)
            self.headers = {
                "Authorization": f"Bearer {self.token}"
            }

            response = self.client.post(
                "/register",
                data={"user_id": self.user_id, "initial_balance": 1000},
                headers=self.headers
            )

            if response.status_code != 200:
                print(f"[{self.user_id}] Registration failed: {response.status_code} - {response.json().get('error')}")
                raise StopUser()
            else:
                cont = response.json()
                self.uuids_by_level = {
                    "main": cont.get("uuid"),
                    "children": [],
                    "grandchildren": []
                }
        except Exception as e:
            print(f"[{getattr(self, 'user_id', 'unknown')}] Exception during on_start: {e}")
            raise StopUser()

    def get_eligible_uuid(self, level_probs={"main": 0.5, "children": 0.3, "grandchildren": 0.2}):
        pool = []
        for level, prob in level_probs.items():
            pool.extend([uuid] * int(prob * 100) for uuid in (self.uuids_by_level[level] if isinstance(self.uuids_by_level[level], list) else [self.uuids_by_level[level]]))
        flat_pool = [uuid for sublist in pool for uuid in sublist]
        return random.choice(flat_pool) if flat_pool else None

    @task(4)
    def create_child_account(self):
        #eligible_parents = self.uuids_by_level["main"] + self.uuids_by_level["children"]
        parent_uuid = self.get_eligible_uuid(level_probs={"main": 0.3,"children": 0.7})
        if not parent_uuid:
            return

        #parent_uuid = uuids

        total_children = len(self.uuids_by_level["children"])
        total_grandchildren = len(self.uuids_by_level["grandchildren"])

        if parent_uuid == self.uuids_by_level["main"] and total_children >= 10:
            return
        
        if parent_uuid in self.uuids_by_level["children"] and total_grandchildren >= 50:
            return

        payload = {
            "parent_uuid": parent_uuid,
            "name": f"acct_{uuid.uuid4().hex[:6]}",
            "allocation": round(random.uniform(0.1, 1.0), 2),
            "priority": random.randint(0, 3),
            "fixed_amount": random.choice([0, 100, 250]),
            "status": "active",
            "due_date": "2025-12-31T00:00:00Z",
            "account_type": random.choice(["FixedSaving", "NonFixedSaving", "Debt", "Bill","Expense"]),
            "freq": random.choice([1, 2, 4])
        }
        

        response = self.client.post("/add_sub_account", json=payload, headers=self.headers)
        child_type = response.json().get('child_type')
        
        if response.status_code == 200:
            new_uuid = response.json().get("uuid")
            if child_type=='GrandchildAccount':
                self.uuids_by_level["grandchildren"].append(new_uuid)
            else:
                self.uuids_by_level["children"].append(new_uuid)

            self.uuids_by_level.setdefault("children_info", []).append({
                "parent_uuid": payload.get("parent_uuid"),
                "child_name": payload.get("name"),
            })
        else:
            print(f"[{self.user_id}] add_sub_account failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def ping(self):
        self.client.get("/__ping__")

    @task(4)
    def deposit(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        #amount = round(random.uniform(500, 10000), 2)
        amount = 500
        response = self.client.post(
            "/deposit",
            json={"uuid": uuid_to_use, "amount": amount, "channel": "ABSA"},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] Deposit failed: {response.status_code} - {response.json().get("error")}")

    @task(4)
    def withdraw(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 0.5, "children": 1, "grandchildren": 4})
        response = self.client.post(
            "/withdraw",
            json={"uuid": uuid_to_use, "amount": 200, "channel": "Barclays"},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] Withdraw failed: {response.status_code} - {response.json().get("error")}")

    @task(1)
    def distribute_funds(self):
        # Choose only accounts that are eligible to distribute (i.e., parents)
        eligible_accounts = self.uuids_by_level["main"] + self.uuids_by_level["children"]
        if not eligible_accounts:
            return

        uuid_ = random.choice(eligible_accounts)
        amount = round(random.uniform(10, 1000), 2)  # You can tweak this range
        transaction_id = f"txn_{uuid.uuid4().hex[:6]}"

        payload = {
            "uuid": uuid_,
            "amount": amount,
            "transaction": transaction_id,
            "by": "LocustBot"
        }

        response = self.client.post(
            "/distribute",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] Distribute failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_child_allocation(self):
        # Check if we have any children_info data
        children_info = self.uuids_by_level.get("children_info", [])
        if not children_info:
            return

        # Randomly choose a (parent_uuid, child_name) pair
        info = random.choice(children_info)
        parent_uuid = info["parent_uuid"]
        child_name = info["child_name"]

        # Generate a new allocation between 0.05 and 1.0
        allocation = round(random.uniform(0.05, 1.0), 2)

        payload = {
            "parent_uuid": parent_uuid,
            "child_name": child_name,
            "allocation": allocation
        }

        response = self.client.post("/set_child_allocation", json=payload, headers=self.headers)
        if response.status_code != 200:
            print(f"[{self.user_id}] set_child_allocation failed: {response.status_code} - {response.json().get("error")}")

    @task(4)
    def get_balance(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 1, "children": 1, "grandchildren": 1})
        response = self.client.get(
            "/get_balance",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_balance failed: {response.status_code} - {response.json().get("error")}")

    @task(4)
    def get_transactions(self):
        uuid_to_use = self.get_eligible_uuid( level_probs={"main": 1, "children": 1, "grandchildren": 1})
        response = self.client.get(
            "/get_transactions",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_transactions failed: {response.status_code} - {response.json().get("error")}")

    @task(3)
    def list_child_accounts(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 3, "children": 3, "grandchildren": 1})
        response = self.client.get(
            "/list_child_accounts",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] list_child_accounts failed: {response.status_code} - {response.json().get("error")}")

    @task(3)
    def list_all_accounts(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 3, "children": 3, "grandchildren": 3})
        response = self.client.get(
            "/list_all_accounts",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] list_all_accounts failed: {response.status_code} - {response.json().get("error")}")

    @task(3)
    def find_account_by_name(self):
        children_info = self.uuids_by_level.get("children_info", [])
        if not children_info:
            return
        
        accounts = ['main']

        for info in children_info:
            child_name = info["child_name"]
            accounts.append(child_name)
        
        acc_name = random.choice(accounts)

        response = self.client.get(
            "/find_account_by_name",
            json={"name": acc_name},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] find_account_by_name failed: {response.status_code} - {response.json().get("error")}")

    @task(3)
    def find_account_by_uuid(self):
        uuid_to_use = self.get_eligible_uuid( level_probs={"main": 3, "children": 3, "grandchildren": 3})
        response = self.client.get(
            "/find_account_by_uuid",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] find_account_by_uuid failed: {response.status_code} - {response.json().get("error")}")

    @task(3)
    def move_balance(self):
        from_uuid = self.get_eligible_uuid(level_probs={"main": 0.5, "children": 1, "grandchildren": 4})
        to_uuid = self.get_eligible_uuid(level_probs={"main": 1, "children": 1, "grandchildren": 1})

        #while from_uuid == to_uuid:
        #    to_uuid = self.get_eligible_uuid(level_probs={"main": 1, "children": 1, "grandchildren": 1})

        response = self.client.post(
            "/move_balance",
            json={
                "from_uuid": from_uuid,
                "to_uuid": to_uuid,
                "amount": random.randint(1, 100),
            },
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] move_balance failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def compute_total_balance(self):
        # Pick a UUID from any level
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        response = self.client.get(
           "/compute_total_balance",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_balance failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def compute_total_due(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        response = self.client.get(
            "/compute_total_due",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_due failed: {response.status_code} - {response.json().get("error")}")

    @task(1)
    def compute_total_due_within_days(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        days = random.choice([0, 1, 7, 14, 30, 60])  # common forecasting windows

        response = self.client.get(
            "/compute_total_due_within_days",
            json={"uuid": uuid, "days": days},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_due_within_days failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def spending(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        # 50% chance to specify a date range
        if random.random() < 0.5:
            from_date = (datetime.now() - timedelta(days=random.randint(30, 365))).date().isoformat()
            to_date = datetime.now().date().isoformat()
            payload = {"uuid": uuid, "from": from_date, "to": to_date}
        else:
            payload = {"uuid": uuid}

        response = self.client.get(
            "/spending",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] spending failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def total_income(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        # 50% chance to specify a date range
        if random.random() < 0.5:
            from_date = (datetime.now() - timedelta(days=random.randint(30, 365))).date().isoformat()
            to_date = datetime.now().date().isoformat()
            payload = {"uuid": uuid, "from": from_date, "to": to_date}
        else:
            payload = {"uuid": uuid}

        response = self.client.get(
            "/total_income",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] total_income failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def allocated_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        # 50% chance to include date range
        if random.random() < 0.5:
            from_date = (datetime.now() - timedelta(days=random.randint(30, 365))).date().isoformat()
            to_date = datetime.now().date().isoformat()
            payload = {"uuid": uuid, "from": from_date, "to": to_date}
        else:
            payload = {"uuid": uuid}

        response = self.client.get(
            "/allocated_amount",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] allocated_amount failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def income_utilization(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})

        # Optional date range
        if random.random() < 0.5:
            from_date = (datetime.now() - timedelta(days=random.randint(30, 365))).date().isoformat()
            to_date = datetime.now().date().isoformat()
            payload = {"uuid": uuid, "from": from_date, "to": to_date}
        else:
            payload = {"uuid": uuid}

        response = self.client.get(
            "/income_utilization",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] income_utilization failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def walking_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 3, "children": 2, "grandchildren": 1})

        # Randomly select amt_type
        amt_type = random.choice(["amount_due", "balance"])

        # Optional date filtering
        if random.random() < 0.5:
            from_date = (datetime.now() - timedelta(days=random.randint(30, 180))).date().isoformat()
            to_date = datetime.now().date().isoformat()
            payload = {"uuid": uuid, "amt_type": amt_type, "from": from_date, "to": to_date}
        else:
            payload = {"uuid": uuid, "amt_type": amt_type}

        response = self.client.get(
            "/walking_amount",
            json = payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] walking_amount failed: {response.status_code} - {response.json().get("error")}")



    @task(2)
    def change_account_status(self):
        # Only pick from child/grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})
        
        if not uuid:
            return
        
        status = random.choice(["active", "inactive", "closed"])

        payload = {
            "uuid": uuid,
            "status": status
        }

        response = self.client.post(
            "/change_account_status",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] change_account_status failed: {response.status_code} - {response.json().get("error")}")


    @task(2)
    def get_account_status(self):
        # Only use child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})
        if not uuid:
            return
        
        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_status",
            json=payload,
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_status failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_account_priority(self):
        # Only apply to child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})

        if not uuid:
            return
        
        # Generate a random priority (can be int or string label)
        priority = random.choice([str(random.randint(1, 10))])

        payload = {
            "uuid": uuid,
            "priority": priority
        }

        response = self.client.post(
            "/set_priority",
            json=payload,
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] set_priority failed: {response.status_code} - {response.json().get("error")}")

    def get_account_priority(self):
        # Only pick UUIDs from child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})

        if not uuid:
            return

        payload = {
            "uuid": uuid
        }

        response = self.client.get(
            "/get_priority",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_priority failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_due_date(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})

        if not uuid:
            return        
        offset_days = random.randint(-90, 90)
        future_date = datetime.utcnow() + timedelta(days=offset_days)
        iso_due_date = future_date.strftime("%Y-%m-%dT%H:%M:%SZ")

        payload = {
            "uuid": uuid,
            "due_date": iso_due_date
        }

        response = self.client.post(
            "/set_due_date",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] set_due_date failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def get_due_date(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        
        params = {
            "uuid": uuid
        }

        response = self.client.get(
            "/get_due_date",
            params=params,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_due_date failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_fixed_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        
        payload = {
            "uuid": uuid,
            "fixed_amount": round(random.uniform(10, 500), 2)
        }

        response = self.client.post(
            "/set_fixed_amount",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] set_fixed_amount failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def get_fixed_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        
        response = self.client.get(
            "/get_fixed_amount",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_fixed_amount failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_account_type(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})

        if not uuid:
            return
                
        account_type = random.choice(["FixedSaving", "NonFixedSaving", "Debt", "Bill","Expense"])

        payload = {
            "uuid": uuid,
            "account_type": account_type
        }

        response = self.client.post(
            "/set_account_type",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] set_account_type failed: {response.status_code} - {response.json().get("error")}")


    @task(2)
    def get_account_type(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        
        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_type",
            json = payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_type failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_account_freq(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        freq_options = [7, 14, 30]
        freq = random.choice(freq_options)

        payload = {
            "uuid": uuid,
            "account_freq": freq
        }

        response = self.client.post(
            "/set_account_freq",
            json=payload,
            headers=self.headers
        )
        

        if response.status_code != 200:
            print(f"[{self.user_id}] set_account_freq failed: {response.status_code} - {response.json().get("error")}")

    @task(4)
    def get_account_freq(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_freq",
            json=payload,
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_freq failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def set_account_periods(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        periods = random.randint(1, 30)

        payload = {
            "uuid": uuid,
            "periods": periods
        }

        response = self.client.post(
            "/set_account_periods",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] set_account_periods failed: {response.status_code} - {response.json().get("error")}")

    @task(2)
    def get_account_periods(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        
        payload = {
            "uuid": uuid
        }

        response = self.client.get(
            "/get_account_periods",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_periods failed: {response.status_code} - {response.json().get("error")}")

class StepLoadShape(LoadTestShape):
    step_time = 60   # seconds per step
    step_load = 50  # users to add each step
    spawn_rate = 50  # users/sec
    max_users = 1000

    def tick(self):
        run_time = self.get_run_time()
        current_step = run_time // self.step_time
        users = (current_step + 1) * self.step_load

        if users > self.max_users:
            return None
        return (users, self.spawn_rate)
