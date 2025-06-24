library(tidyverse)
# =========================================================
# Test child initialization method
# =========================================================

test_that("ChildAccount: initializes with defaults correctly", {
  child <- ChildAccount$new("Child1")
  expect_equal(child$name, "Child1")
  expect_equal(child$allocation, 0)
  expect_equal(child$status, "active")
  expect_equal(child$priority, 0)
})

test_that("ChildAccount: accepts custom allocation and priority", {
  child <- ChildAccount$new("Child2", allocation = 0.6, priority = 2)
  expect_equal(child$allocation, 0.6)
  expect_equal(child$priority, 2)
})

test_that("ChildAccount: handles NULL path correctly", {
  child <- ChildAccount$new("Child3", path = NULL)
  expect_null(child$path)
})

test_that("ChildAccount: allows non-default status", {
  child <- ChildAccount$new("Child4", status = "inactive")
  expect_equal(child$status, "inactive")
})

# =========================================================
# Test deposit method
# =========================================================

test_that("deposit: increases balance correctly when active", {
  acc <- ChildAccount$new("Child", allocation = 1, priority = 1)
  acc$deposit(100, channel = "User")
  expect_equal(acc$balance, 100)  # because balance is immediately distributed
  expect_equal(nrow(acc$transactions), 1)
  expect_equal(acc$transactions$Amount[1], 100)
})

test_that("deposit: does nothing when status is inactive", {
  acc <- ChildAccount$new(
    "Inactive",
    allocation = 1,
    priority = 1,
    status = "inactive"
  )
  acc$deposit(100, channel = "User")
  expect_equal(acc$balance,0)
  expect_equal(nrow(acc$transactions), 0)
})

test_that("deposit: assigns transaction ID if none is provided", {
  acc <- ChildAccount$new("Child", allocation = 1, priority = 1)
  acc$deposit(100, channel = "Test")
  expect_true(!is.null(acc$transactions$TransactionID[1]))
})

test_that("deposit: uses provided transaction ID if supplied", {
  acc <- ChildAccount$new("Child", allocation = 1, priority = 1)
  acc$deposit(50, transaction_number = "custom_txn_123", channel = "Test")
  expect_equal(acc$transactions$TransactionID[1], "custom_txn_123")
})

test_that("deposit: triggers distribution to children", {
  parent <- ChildAccount$new("Parent", allocation = 1, priority = 1)
  child <- GrandchildAccount$new("GC", allocation = 1, priority = 1)
  parent$add_child_account(child)
  parent$deposit(100, channel = "Test")
  
  expect_gt(nrow(child$transactions), 0)
  expect_equal(sum(child$transactions$Amount), 100)
})

test_that("deposit: logs full transaction info", {
  acc <- ChildAccount$new("TestAcc", allocation = 1, priority = 1)
  acc$deposit(100, channel = "Test")
  txn <- acc$transactions[1, ]
  expect_equal(txn$Type, "Deposit")
  expect_equal(txn$By, "User")
  expect_equal(txn$Channel, "Test")
  expect_true(!is.na(txn$overall_balance))
})

test_that("deposit: accepts custom date", {
  acc <- ChildAccount$new("Child", allocation = 1, priority = 1)
  custom_date <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  acc$deposit(100, date = custom_date, channel = "Test")
  expect_equal(acc$transactions$Date[1], custom_date)
})

# =========================================================
# Test change status method
# =========================================================
test_that("change_status: does nothing if already closed", {
  acc <- ChildAccount$new("Test", allocation = 1, priority = 1)
  acc$status <- "closed"
  expect_output(acc$change_status("closed"), "already closed")
  expect_equal(acc$status, "closed")
})

test_that("change_status: throws error if closing with balance", {
  acc <- ChildAccount$new("Test", allocation = 1, priority = 1)
  acc$balance <- 100
  expect_error(acc$change_status("closed"), "Withdraw from this account")
})

test_that("change_status: closes account with zero balance", {
  acc <- ChildAccount$new("Test", allocation = 1, priority = 1)
  acc$balance <- 0
  acc$change_status("closed")
  expect_equal(acc$status, "closed")
})

test_that("change_status: sets a new status like frozen", {
  acc <- ChildAccount$new("Test", allocation = 1, priority = 1)
  acc$change_status("frozen")
  expect_equal(acc$status, "frozen")
})


test_that("change_status: prints closure confirmation", {
  acc <- ChildAccount$new("Test", allocation = 1, priority = 1)
  expect_output(acc$change_status("closed"), "Test has been closed")
})

# =========================================================
# Test get status method
# =========================================================

test_that("get_account_status: returns the correct status", {
  acc <- ChildAccount$new("Child1", allocation = 1, priority = 1)
  acc$status <- "frozen"
  expect_equal(acc$get_account_status(), "frozen")
})

test_that("get_account_status: prints account name and status", {
  acc <- ChildAccount$new("Child2", allocation = 1, priority = 1)
  acc$status <- "active"
  expect_output(acc$get_account_status(), "Child2 is active")
})


# =========================================================
# Test get priority method
# =========================================================

test_that("get_priority: returns the correct priority value", {
  acc <- ChildAccount$new("ChildX", allocation = 0.3, priority = 5)
  expect_equal(acc$get_priority(), 5)
})


test_that("get_priority: reflects changes to priority", {
  acc <- ChildAccount$new("ChildY", allocation = 0.3, priority = 1)
  acc$priority <- 9
  expect_equal(acc$get_priority(), 9)
})



# =========================================================
# Test set priority method
# =========================================================
test_that("set_priority: updates priority value", {
  acc <- ChildAccount$new("ChildZ", allocation = 0.2, priority = 1)
  acc$set_priority(7)
  expect_equal(acc$get_priority(), 7)
})


test_that("set_priority: outputs correct message", {
  acc <- ChildAccount$new("ChildZ", allocation = 0.2, priority = 1)
  expect_output(acc$set_priority(3), "Priority for ChildZ set to 3")
})
