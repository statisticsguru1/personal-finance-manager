from datetime import datetime, timedelta
from locust import HttpUser, task, between, LoadTestShape
from locust.exception import StopUser
import time, uuid, random


def generate_token(client, user_id, role="user", exp=None):
    if exp is None:
        # Default: 1 hour from now
        exp = int(time.time()) + 3600  

    payload = {
        "user_id": user_id,
        "role": role,
        "type": "Expiration",  # match your API's default
        "exp": exp
    }

    response = client.post("/generate_access_token", json=payload)
    if response.status_code == 200:
        token = response.json()["token"]
        if isinstance(token, list):
            token = token[0]
        return token
    else:
        raise Exception(f"Failed to generate token: {response.status_code} - {response.text}")


class APIUser(HttpUser):
    wait_time = between(1, 3)

    def on_start(self):
        try:
            self.user_id = f"user_{uuid.uuid4().hex[:8]}"
            self.token = generate_token(self.client, self.user_id)
            self.headers = {"Authorization": f"Bearer {self.token}"}
            

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
                self.deleted_uuids = set()
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
        
        if isinstance(parent_uuid, list):
            if len(parent_uuid) > 0:
                parent_uuid = parent_uuid[0]  # take first UUID
            else:
                return
        if parent_uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] add_sub_account failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def ping(self):
        self.client.get("/__ping__")

    @task(4)
    def deposit(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid_to_use:
            return  # no accounts to query, skip

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return

        if uuid_to_use in self.deleted_uuids:
            return  
        amount = round(random.uniform(500, 10000), 2)
        #amount = 500
        response = self.client.post(
            "/deposit",
            json={"uuid": uuid_to_use, "amount": amount, "channel": "ABSA"},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] Deposit failed: {response.status_code} - {response.json().get('error')}")

    @task(4)
    def withdraw(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 0.5, "children": 1, "grandchildren": 4})
        if not uuid_to_use:
            return  # no accounts to query, skip

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return

        if uuid_to_use in self.deleted_uuids:
            return  
        response = self.client.post(
            "/withdraw",
            json={"uuid": uuid_to_use, "amount": 200, "channel": "Barclays"},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] Withdraw failed: {response.status_code} - {response.json().get('error')}")

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
            "initiated_by": "LocustBot"
        }

        response = self.client.post(
            "/distribute",
            json=payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] Distribute failed: {response.status_code} - {response.json().get('error')}")

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
            print(f"[{self.user_id}] set_child_allocation failed: {response.status_code} - {response.json().get('error')}")

    @task(4)
    def get_balance(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 1, "children": 1, "grandchildren": 1})
        if not uuid_to_use:
            return
        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return
        if uuid_to_use in self.deleted_uuids:
            return  
        response = self.client.get(
            "/get_balance",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_balance failed: {response.status_code} - {response.json().get('error')}")

    @task(4)
    def get_transactions(self):
        uuid_to_use = self.get_eligible_uuid( level_probs={"main": 1, "children": 1, "grandchildren": 1})
        if not uuid_to_use:
            return

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return
        if uuid_to_use in self.deleted_uuids:
            return  

        response = self.client.get(
            "/get_transactions",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_transactions failed: {response.status_code} - {response.json().get('error')}")

    @task(3)
    def list_child_accounts(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 3, "children": 3, "grandchildren": 1})
        if not uuid_to_use:
            return  

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return   
        if uuid_to_use in self.deleted_uuids:
            return  
        response = self.client.get(
            "/list_child_accounts",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] list_child_accounts failed: {response.status_code} - {response.json().get('error')}")

    @task(3)
    def list_all_accounts(self):
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 3, "children": 3, "grandchildren": 3})
        if not uuid_to_use:
            return

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return
        if uuid_to_use in self.deleted_uuids:
            return  

        response = self.client.get(
            "/list_all_accounts",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] list_all_accounts failed: {response.status_code} - {response.json().get('error')}")

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
            print(f"[{self.user_id}] find_account_by_name failed: {response.status_code} - {response.json().get('error')}")

    @task(3)
    def find_account_by_uuid(self):
        uuid_to_use = self.get_eligible_uuid( level_probs={"main": 3, "children": 3, "grandchildren": 3})
        if not uuid_to_use:
            return

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return
        if uuid_to_use in self.deleted_uuids:
            return  

        response = self.client.get(
            "/find_account_by_uuid",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] find_account_by_uuid failed: {response.status_code} - {response.json().get('error')}")

    @task(3)
    def move_balance(self):
        from_uuid = self.get_eligible_uuid(level_probs={"main": 0.5, "children": 1, "grandchildren": 4})
        if not from_uuid:
            return  
        to_uuid = self.get_eligible_uuid(level_probs={"main": 1, "children": 1, "grandchildren": 1})
        if not to_uuid:
            return

        if isinstance(from_uuid, list):
            if len(from_uuid) > 0:
                from_uuid = from_uuid[0]  # take first UUID
            else:
                return
        if from_uuid in self.deleted_uuids:
            return 

        if isinstance(to_uuid, list):
            if len(to_uuid) > 0:
                to_uuid = to_uuid[0]  # take first UUID
            else:
                return
        if to_uuid in self.deleted_uuids:
            return
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
            print(f"[{self.user_id}] move_balance failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def compute_total_balance(self):
        # Pick a UUID from any level
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return

        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
        response = self.client.get(
           "/compute_total_balance",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_balance failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def compute_total_due(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  

        response = self.client.get(
            "/compute_total_due",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_due failed: {response.status_code} - {response.json().get('error')}")

    @task(1)
    def compute_total_due_within_days(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return 
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return  
        if uuid in self.deleted_uuids:
            return       
        days = random.choice([0, 1, 7, 14, 30, 60])  # common forecasting windows

        response = self.client.get(
            "/compute_total_due_within_days",
            json={"uuid": uuid, "days": days},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] compute_total_due_within_days failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def spending(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
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
            print(f"[{self.user_id}] spending failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def total_income(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
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
            print(f"[{self.user_id}] total_income failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def allocated_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
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
            print(f"[{self.user_id}] allocated_amount failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def income_utilization(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 4, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
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
            print(f"[{self.user_id}] income_utilization failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def walking_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"main": 3, "children": 2, "grandchildren": 1})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
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
            print(f"[{self.user_id}] walking_amount failed: {response.status_code} - {response.json().get('error')}")


    @task(2)
    def change_account_status(self):
        # Only pick from child/grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})
        
        if not uuid:
            return   

        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return        

        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] change_account_status failed: {response.status_code} - {response.json().get('error')}")


    @task(2)
    def get_account_status(self):
        # Only use child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  
        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_status",
            json=payload,
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_status failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_account_priority(self):
        # Only apply to child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})

        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return  
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_priority failed: {response.status_code} - {response.json().get('error')}")

    def get_account_priority(self):
        # Only pick UUIDs from child or grandchild accounts
        uuid = self.get_eligible_uuid(level_probs={"children": 3, "grandchildren": 2})

        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] get_priority failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_due_date(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})

        if not uuid:
            return 
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return 
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_due_date failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def get_due_date(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return 
                
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] get_due_date failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_fixed_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return  
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_fixed_amount failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def get_fixed_amount(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  

        response = self.client.get(
            "/get_fixed_amount",
            json={"uuid": uuid},
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_fixed_amount failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_account_type(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})

        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_account_type failed: {response.status_code} - {response.json().get('error')}")


    @task(2)
    def get_account_type(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  

        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_type",
            json = payload,
            headers=self.headers
        )

        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_type failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_account_freq(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_account_freq failed: {response.status_code} - {response.json().get('error')}")

    @task(4)
    def get_account_freq(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return

        if uuid in self.deleted_uuids:
            return  
        payload = {"uuid": uuid}

        response = self.client.get(
            "/get_account_freq",
            json=payload,
            headers=self.headers
        )
        if response.status_code != 200:
            print(f"[{self.user_id}] get_account_freq failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def set_account_periods(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] set_account_periods failed: {response.status_code} - {response.json().get('error')}")

    @task(2)
    def get_account_periods(self):
        uuid = self.get_eligible_uuid(level_probs={"grandchildren": 5})
        if not uuid:
            return
        if isinstance(uuid, list):
            if len(uuid) > 0:
                uuid = uuid[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
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
            print(f"[{self.user_id}] get_account_periods failed: {response.status_code} - {response.json().get('error')}")

    @task(1)
    def delete_account(self):
        """
        Deletes a random account UUID based on weighted level probabilities.
        If the main account is deleted, the user is stopped.
        """
        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 0.4, "children": 0.3, "grandchildren": 0.3})

        if not uuid_to_use:
            return  # nothing to delete, skip task

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return
        if uuid in self.deleted_uuids:
            return  

        response = self.client.delete(
            "/delete",
            json={"uuid": uuid_to_use},
            headers=self.headers
        )

        if response.status_code == 200:
            print(f"[{self.user_id}] Deleted account {uuid_to_use} successfully.")
            self.deleted_uuids.add(uuid_to_use)
            
            # If this was the main account -> stop this user
            if uuid_to_use == self.uuids_by_level["main"]:
                print(f"[{self.user_id}] Main account deleted — stopping user.")
                raise StopUser()  # Locust will stop scheduling tasks for this user

            # Otherwise, just remove UUID from memory
            self._remove_uuid_from_state(uuid_to_use)

        else:
            try:
                error = response.json().get("error")
            except Exception:
                error = response.text
            print(f"[{self.user_id}] Delete failed: {response.status_code} - {error}")

            # If backend says "not found", remove locally to avoid retrying a dead UUID
            if response.status_code in (400, 404) and "not found" in str(error).lower():
                self._remove_uuid_from_state(uuid_to_use)

    def _remove_uuid_from_state(self, uuid_to_remove):
        """Safely removes a UUID from the stored user state."""
        for level, uuids in self.uuids_by_level.items():
            if isinstance(uuids, list) and uuid_to_remove in uuids:
                uuids.remove(uuid_to_remove)
                break
            elif uuids == uuid_to_remove:
                self.uuids_by_level[level] = None
                break

    @task(2)
    def get_minimal_tree(self):
        """
        Calls the /get_minimal_tree endpoint for a randomly chosen account.
        Includes randomized date ranges to simulate real usage.
        """

        uuid_to_use = self.get_eligible_uuid(level_probs={"main": 0.4, "children": 0.4, "grandchildren": 0.2})
        if not uuid_to_use:
            return  # no accounts to query, skip

        if isinstance(uuid_to_use, list):
            if len(uuid_to_use) > 0:
                uuid_to_use = uuid_to_use[0]  # take first UUID
            else:
                return

        if uuid in self.deleted_uuids:
            return  
        # Randomize date range within last 90 days
        end_date = datetime.now()
        start_date = end_date - timedelta(days=random.randint(0, 90))

        payload = {
            "uuid": uuid_to_use,
            "n": random.choice([7, 14, 30]),  # days to compute due within
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d"),
        }

        response = self.client.get(
            "/get_minimal_tree",
            json=payload,
            headers=self.headers
        )

        if response.status_code == 200:
            data = response.json()
            if not data.get("success"):
                print(f"[{self.user_id}] Minimal tree returned failure: {data}")
        else:
            try:
                error = response.json().get("error")
            except Exception:
                error = response.text
            print(f"[{self.user_id}] get_minimal_tree failed: {response.status_code} - {error}")


class StepLoadShape(LoadTestShape):
    step_time = 60   # seconds per step
    step_load = 50  # users to add each step
    spawn_rate = 20  # users/sec
    max_users = 500
    max_duration = 500       # total duration in seconds (e.g., 10 minutes)

    def tick(self):
        run_time = self.get_run_time()

        if run_time > self.max_duration:
            return None

        current_step = run_time // self.step_time
        users = (current_step + 1) * self.step_load

        if users > self.max_users:
            users = self.max_users
        return (users, self.spawn_rate)
