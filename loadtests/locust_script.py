from locust import HttpUser, task, between, LoadTestShape

class APIUser(HttpUser):
    wait_time = between(1, 3)

    @task
    def ping(self):
        self.client.get("/__ping__")
    

class StepLoadShape(LoadTestShape):
    step_time = 60  # seconds per step
    step_load = 1000  # users to add each step
    spawn_rate = 1000  # users/sec
    max_users = 50000

    def tick(self):
        run_time = self.get_run_time()

        current_step = run_time // self.step_time
        users = (current_step + 1) * self.step_load

        if users > self.max_users:
            return None  # Stop the test

        return (users, self.spawn_rate)
