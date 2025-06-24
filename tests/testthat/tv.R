due <- as.POSIXct("2025-07-01 12:00:00", tz = "UTC")
acc <- GrandchildAccount$new(
  name = "Rent",
  allocation = 0.5,
  priority = 2,
  fixed_amount = 1000,
  due_date = due,
  account_type = "Fixed Expense",
  freq = "Monthly"
)
