library(tidyverse)

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