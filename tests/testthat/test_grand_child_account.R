library(tidyverse)
library(lubridate)
# =========================================================
# Test child initialization method
# =========================================================

test_that("GrandchildAccount initializes correctly with defaults", {
  acc <- GrandchildAccount$new("Electricity")

  expect_s3_class(acc, "GrandchildAccount")
  expect_equal(acc$name, "Electricity")
  expect_equal(acc$allocation, 0)
  expect_equal(acc$priority, 0)
  expect_equal(acc$fixed_amount, 0)
  expect_equal(acc$amount_due, 0)
  expect_null(acc$due_date)
  expect_equal(acc$account_type, "Expense")
  expect_null(acc$freq)
  expect_equal(acc$num_periods, 1)
  expect_s3_class(acc$Track_dues_and_balance, "data.frame")
  expect_equal(ncol(acc$Track_dues_and_balance), 3)
  expect_equal(colnames(acc$Track_dues_and_balance),
               c("Date", "Amount_due", "Balance"))
})

test_that("GrandchildAccount initializes correctly with values", {
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

  expect_equal(acc$name, "Rent")
  expect_equal(acc$allocation, 0.5)
  expect_equal(acc$priority, 2)
  expect_equal(acc$fixed_amount, 1000)
  expect_equal(acc$amount_due, 1000)
  expect_equal(acc$due_date, due)
  expect_equal(acc$account_type, "Fixed Expense")
  expect_equal(acc$freq, "Monthly")
  expect_equal(acc$num_periods, 1)
})



# =========================================================
# Test get due dates method
# =========================================================
test_that("get_due_date returns NULL if not set", {
  acc <- GrandchildAccount$new("Water Bill")
  expect_null(acc$get_due_date())
})

test_that("get_due_date returns correct due date", {
  due <- as.POSIXct("2025-12-01 10:00:00", tz = "UTC")
  acc <- GrandchildAccount$new("Internet", due_date = due)
  expect_equal(acc$get_due_date(), due)
})

# =========================================================
# Test set_fixed_amount method
# =========================================================


test_that("set_fixed_amount sets fixed_amount and updates amount_due correctly", {
  acc <- GrandchildAccount$new("Rent")
  acc$balance <- 100
  acc$num_periods <- 2
  
  acc$set_fixed_amount(500)  # Expect: 500 * 2 - 100 = 900
  expect_equal(acc$fixed_amount, 500)
  expect_equal(acc$amount_due, 900)
})

test_that("set_fixed_amount handles zero balance correctly", {
  acc <- GrandchildAccount$new("Electricity")
  acc$balance <- 0
  acc$num_periods <- 3
  
  acc$set_fixed_amount(200)  # Expect: 200 * 3 - 0 = 600
  expect_equal(acc$amount_due, 600)
})

test_that("set_fixed_amount handles zero fixed_amount", {
  acc <- GrandchildAccount$new("Water")
  acc$balance <- 50
  acc$num_periods <- 2
  
  acc$set_fixed_amount(0)  # Expect: 0 * 2 - 50 = -50
  expect_equal(acc$fixed_amount, 0)
  expect_equal(acc$amount_due, -50)
})
# =========================================================
# Test get_fixed_amount method 
# =========================================================

test_that("get_account_type returns correct account type", {
  acc <- GrandchildAccount$new("Utilities", account_type = "Expense")
  expect_equal(acc$get_account_type(), "Expense")
  
  acc2 <- GrandchildAccount$new("IncomeAccount", account_type = "Income")
  expect_equal(acc2$get_account_type(), "Income")
})

# =========================================================
# Test deposit method
# =========================================================

test_that("deposit updates balance and logs transaction", {
  acc <- GrandchildAccount$new("Savings")
  acc$deposit(100, by = "User", channel = "Bank Transfer")

  expect_equal(acc$balance, 100)
  expect_equal(nrow(acc$transactions), 1)
  expect_equal(acc$transactions$Amount[1], 100)
  expect_equal(acc$transactions$Type[1], "Deposit")
  expect_equal(acc$transactions$Channel[1], "Bank Transfer")
  expect_equal(acc$transactions$By[1], "User")
})

test_that("deposit uses provided transaction_number", {
  acc <- GrandchildAccount$new("Emergency")
  acc$deposit(
    50,
    transaction_number = "TXN001",
    by = "System",
    channel = "MPESA"
  )

  expect_equal(acc$transactions$TransactionID[1], "TXN001")
})

test_that("deposit assigns generated transaction_id when NULL", {
  acc <- GrandchildAccount$new("GeneratedIDTest")
  acc$deposit(70,channel = "MPESA")
  # Assuming sysX format
  expect_true(grepl("sys", acc$transactions$TransactionID[1]))
})

test_that("deposit respects inactive account status", {
  acc <- GrandchildAccount$new("Inactive")
  acc$status <- "closed"
  acc$deposit(200,channel = "MPESA")

  expect_equal(acc$balance, 0)
  expect_equal(nrow(acc$transactions), 0)
})

test_that("deposit fails when channel is NULL", {
  acc <- GrandchildAccount$new("NoChannel")
  expect_error(acc$deposit(80), "`channel` is required")
})


test_that("deposit updates amount_due and overall_balance in log", {
  acc <- GrandchildAccount$new("Tracker", fixed_amount = 300)
  acc$num_periods <- 1
  acc$deposit(100,channel = "MPESA")

  expect_equal(acc$Track_dues_and_balance$Amount_due[1], 200)
  expect_equal(acc$transactions$overall_balance[1], acc$balance)
})

test_that("deposit refunds extra to parent account", {
  # Step 1: Create main & child (parents of grand child)
  main<- MainAccount$new("main")
  child <- ChildAccount$new(
    "child",
    allocation=1 # you need allocation otherwise it will be turned inactive
  )
  grandchild_acc <- GrandchildAccount$new(
    name = "Rent",
    allocation = 1,
    priority = 1,
    fixed_amount = 100,
    due_date = Sys.time() + lubridate::days(5), # Overdue
    account_type = "Bill",
    freq = 30 # 30-day billing cycle
  )
  
  # Step 2: Attach grandchild to parent & child to main
  main$add_child_account(child)
  child$add_child_account(grandchild_acc)
  
  # Step 3: Deposit more than needed into grandchild
  # Fixed amount = 100, periods = 1, balance = 0 => due = 100
  # We deposit 150 → 50 should go to parent
  grandchild_acc$deposit(150, channel = "Bank")
  
  # Step 4: Check grandchild was fully funded and is inactive
  expect_equal(grandchild_acc$status, "inactive")
  expect_equal(grandchild_acc$balance, 100)
  expect_equal(grandchild_acc$amount_due, 0)
  
  # Step 5: Check refund to parent
  expect_equal(nrow(child$transactions), 1)
  expect_equal(child$transactions$Amount[1], 50)
  expect_equal(child$transactions$Channel[1], "Returned Extra Allocation")
  expect_equal(child$transactions$By[1], "System")
})

# =========================================================
# Test withdrawal method
# =========================================================


test_that("withdraw handles all edge cases and updates states correctly", {
  acc <- GrandchildAccount$new("Rent", fixed_amount = 75000)
  acc$deposit(75000, channel = "Bank")
  
  # Valid withdrawal: should succeed and update balance, Track_dues_and_balance,
  # num_periods
  acc$withdraw(35000, channel = "Bank")
  expect_equal(acc$balance, 40000)
  
  expect_equal(nrow(acc$Track_dues_and_balance), 2)
  # should reduce proportionally
  expect_equal(acc$num_periods, 1 - (35000 / 75000))
  
  # Withdrawal with amount = 0: should be ignored silently
  acc$withdraw(0, channel = "Bank")
  expect_equal(acc$balance, 40000)  # balance remains unchanged
  
  # Withdraw remaining amount to test zero balance condition
  acc$withdraw(40000, channel = "Bank")
  expect_equal(acc$balance, 0)
  expect_equal(nrow(acc$Track_dues_and_balance), 3)
  
  # Withdrawal with insufficient funds: should not update anything
  result <- acc$withdraw(1000, channel = "Bank")
  expect_null(result)
  expect_equal(acc$balance, 0)
  expect_equal(nrow(acc$Track_dues_and_balance), 3)  # no new entry
  
  # Withdrawal without fixed_amount (should not touch num_periods)
  acc2 <- GrandchildAccount$new("Random")
  acc2$deposit(5000, channel = "Bank")
  acc2$withdraw(2000, channel = "Bank")
  expect_equal(acc2$balance, 3000)
  expect_equal(acc2$num_periods, 1)  # untouched
})

# =========================================================
# Test get_account_freq method
# =========================================================


test_that("get_account_freq returns correct frequency", {
  # Explicitly set frequency
  acc1 <- GrandchildAccount$new("MonthlyBill", freq = "Monthly")
  expect_equal(acc1$get_account_freq(), "Monthly")
  
  # Another frequency type
  acc2 <- GrandchildAccount$new("WeeklyBill", freq = "Weekly")
  expect_equal(acc2$get_account_freq(), "Weekly")
  
  # NULL frequency (default)
  acc3 <- GrandchildAccount$new("UnspecifiedFreq")
  expect_null(acc3$get_account_freq())
})

# =========================================================
# Test set_account_freq method
# =========================================================

test_that("set_account_freq updates the frequency and prints message", {
  acc <- GrandchildAccount$new("InternetBill", freq = "Monthly")
  
  # Change frequency
  expect_output(
    acc$set_account_freq("Quarterly"),
    "Frequency for InternetBill set to Quarterly"
  )
  
  # Check if internal state is updated
  expect_equal(acc$get_account_freq(), "Quarterly")
  
  # Change to another valid string
  expect_output(
    acc$set_account_freq("Annually"),
    "Frequency for InternetBill set to Annually"
  )
  expect_equal(acc$get_account_freq(), "Annually")
  
  # Edge case: setting to NULL
  expect_output(
    acc$set_account_freq(NULL),
    "Frequency for InternetBill set to "
  )
  expect_null(acc$get_account_freq())
})

# =========================================================
# Test get_account_periods method
# =========================================================
test_that("get_account_periods returns the correct number of periods", {
  acc <- GrandchildAccount$new("Health Insurance")
  
  # Default value should be 1
  expect_equal(acc$get_account_periods(), 1)
  
  # Manually change the number of periods (simulating internal logic changes)
  acc$num_periods <- 3
  expect_equal(acc$get_account_periods(), 3)
  
  # Set to 0 (edge case)
  acc$num_periods <- 0
  expect_equal(acc$get_account_periods(), 0)
  
  # Set to fractional period (possible from withdraw adjustments)
  acc$num_periods <- 2.5
  expect_equal(acc$get_account_periods(), 2.5)
})

# =========================================================
# Test get_account_periods method
# =========================================================
test_that("set_account_periods correctly updates the number of periods", {
  acc <- GrandchildAccount$new("Internet Subscription")
  
  # Set to 5 periods
  acc$set_account_periods(5)
  expect_equal(acc$num_periods, 5)
  
  # Set to 0 (edge case: may be logically invalid but allowed in current
  # vimplementation)
  acc$set_account_periods(0)
  expect_equal(acc$num_periods, 0)
  
  # Set to a decimal value (e.g., withdrawal adjusted)
  acc$set_account_periods(2.75)
  expect_equal(acc$num_periods, 2.75)
  
  # Set to a negative value (not prevented by current implementation)
  acc$set_account_periods(-3)
  expect_equal(acc$num_periods, -3)
})

# =========================================================
# Test get_fixed amount method
# =========================================================
test_that("get_fixed_amount returns correct value", {
  acc <- GrandchildAccount$new(
    name = "FixedTest",
    allocation = 1,
    priority = 1,
    fixed_amount = 200,
    due_date = NULL,
    account_type = "Expense",
    freq = NULL
  )
  
  expect_equal(acc$get_fixed_amount(), 200)
})


# =========================================================
# Test set_due_date  method
# =========================================================

test_that("set_due_date sets the due_date correctly", {
  acc <- GrandchildAccount$new(
    name = "DueDateTest",
    allocation = 1,
    priority = 1,
    fixed_amount = 100,
    due_date = NULL,
    account_type = "Expense",
    freq = NULL
  )
  
  acc$set_due_date("2025-07-01")
  expect_equal(acc$due_date, "2025-07-01")
})


# =========================================================
# Test  set_account_type   method
# =========================================================

test_that("set_account_type sets the account_type correctly", {
  acc <- GrandchildAccount$new(
    name = "AccountTypeTest",
    allocation = 1,
    priority = 1,
    fixed_amount = 100,
    due_date = NULL,
    account_type = "Income",  # Initial value
    freq = NULL
  )
  
  acc$set_account_type("Expense")  # Change it
  expect_equal(acc$account_type, "Expense")
})
