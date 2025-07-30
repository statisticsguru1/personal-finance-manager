from locust import HttpUser, task, between, LoadTestShape

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
    
help(LoadTestShape.get_run_time)