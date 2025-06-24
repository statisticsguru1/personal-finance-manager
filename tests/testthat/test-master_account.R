library(tidyverse)

# =========================================================
# Test that  the class initializes correctly
# =========================================================
test_that("MainAccount initializes all fields correctly", {
  acc <- MainAccount$new(name = "Main")

  # Class and name
  expect_s3_class(acc, "MainAccount")
  expect_equal(acc$name, "Main")

  # UUID format check
  expect_type(acc$uuid, "character")
  expect_match(acc$uuid, "^acc[0-9a-f\\-]+$")

  # Balance
  expect_equal(acc$balance, 0)

  # Path
  expect_equal(acc$path, "main_account")

  # Transactions dataframe structure
  expect_s3_class(acc$transactions, "data.frame")
  expect_named(acc$transactions, c(
    "Type", "By", "TransactionID", "Channel",
    "Amount", "Balance", "amount_due", "overall_balance", "Date"
  ))
  expect_equal(nrow(acc$transactions), 0)
  expect_equal(ncol(acc$transactions), 9)

  # Data types of transaction columns
  expect_type(acc$transactions$Type, "character")
  expect_type(acc$transactions$By, "character")
  expect_type(acc$transactions$TransactionID, "character")
  expect_type(acc$transactions$Channel, "character")
  expect_type(acc$transactions$Amount, "double")
  expect_type(acc$transactions$Balance, "double")
  expect_type(acc$transactions$amount_due, "double")
  expect_type(acc$transactions$overall_balance, "double")
  expect_s3_class(acc$transactions$Date, "POSIXct")
})

# =========================================================
# generate_transaction_id creates unique incrementing IDs
# =========================================================

test_that("generate_transaction_id creates unique incrementing IDs", {
  acc <- MainAccount$new(name = "Main")

  # First transaction
  id1 <- acc$generate_transaction_id()
  expect_equal(id1, "sys1")
  expect_equal(acc$transaction_counter, 2)

  # Second transaction
  id2 <- acc$generate_transaction_id()
  expect_equal(id2, "sys2")
  expect_equal(acc$transaction_counter, 3)

  # Third transaction
  id3 <- acc$generate_transaction_id()
  expect_equal(id3, "sys3")
  expect_equal(acc$transaction_counter, 4)

  # Format check
  expect_true(all(grepl("^sys[0-9]+$", c(id1, id2, id3))))
})

# ======================================================================
# Test for Duplicates
# =====================================================================
test_that("is_duplicate_transaction correctly identifies duplicates", {
  acc <- MainAccount$new(name = "Main")

  # Simulate a transaction
  acc$transactions <- data.frame(
    Type = "Deposit",
    By = "User",
    TransactionID = "sys123",
    Channel = "Online",
    Amount = 100,
    Balance = 100,
    amount_due = 0,
    overall_balance = 100,
    Date = Sys.time(),
    stringsAsFactors = FALSE
  )

  # Positive test: ID exists
  expect_true(acc$is_duplicate_transaction("sys123"))

  # Negative test: ID does not exist
  expect_false(acc$is_duplicate_transaction("sys999"))

  # Edge case: NULL/NA input
  expect_false(acc$is_duplicate_transaction(NA_character_))
  expect_false(acc$is_duplicate_transaction(NULL))
})

# ====================================================================
# Test for transactions
# ====================================================================
# ---- Deposits----------------------------------------------
test_that("Valid deposit updates balance and transaction log", {
  acc <- MainAccount$new(name = "Main")
  acc$deposit(amount = 100, channel = "Bank")

  expect_equal(acc$balance, 100)  # Because no children to dostribute to.
  expect_equal(nrow(acc$transactions), 1) # only 1 transacton was added.
  expect_equal(acc$transactions$Amount[1], 100)
  expect_equal(acc$transactions$Channel[1], "Bank")
  expect_equal(acc$transactions$Type[1], "Deposit")
})

test_that("Deposit requires amount > 0", {
  acc <- MainAccount$new(name = "Main")
  expect_error(
    acc$deposit(amount = 0, channel = "Bank"),
    "Deposit amount must be greater than zero"
  )
  expect_error(
    acc$deposit(amount = -50, channel = "Bank"),
    "Deposit amount must be greater than zero"
  )
})

test_that("Deposit requires a channel", {
  acc <- MainAccount$new(name = "Main")
  expect_error(
    acc$deposit(amount = 100),
    "Channel is required for deposits"
  )
})

test_that("Transaction number is auto-generated if not provided", {
  acc <- MainAccount$new(name = "Main")
  acc$deposit(amount = 100, channel = "Bank")

  expect_true(grepl("^sys", acc$transactions$TransactionID[1]))
})

test_that("Deposit prevents duplicate transaction numbers", {
  acc <- MainAccount$new(name = "Main")
  acc$deposit(amount = 100, channel = "Bank", transaction_number = "T123")

  expect_error(
    acc$deposit(amount = 50, channel = "Bank", transaction_number = "T123"),
    "This deposit has already been made"
  )
})

test_that("Deposit handles custom date input correctly", {
  acc <- MainAccount$new(name = "Main")
  custom_date <- as.POSIXct("2024-01-01 12:00:00")
  acc$deposit(amount = 100, channel = "Mobile", date = custom_date)

  expect_equal(as.Date(acc$transactions$Date[1]), as.Date(custom_date))
})

# ====================================================================
# Test distribute to children.
# ====================================================================
test_that("MainAccount: no children results in no distribution", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(1000, channel = "Equity")
  expect_silent(
    main_account$distribute_to_children(500, "sys1")
  )
  expect_equal(main_account$balance, 1000)
})


test_that("MainAccount: no active children returns message", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(1000, channel = "Equity")

  child <- ChildAccount$new("InactiveChild", allocation = 0.5)
  child$change_status("inactive")
  main_account$add_child_account(child)

  expect_message(main_account$distribute_to_children(500, "TXN002"),
                 "No active child accounts available.")
  expect_equal(main_account$balance, 1000)
})


test_that("MainAccount: amount < 0.10 allocated to highest priority active child", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(1, channel = "Equity")

  child1 <- ChildAccount$new("LowPriority", allocation = 0.5, priority = 1)
  child2 <- ChildAccount$new("HighPriority", allocation = 0.1, priority = 5)

  main_account$add_child_account(child1)
  main_account$add_child_account(child2)

  # Expect a message about low amount being redirected to highest priority child
  expect_message(
    main_account$distribute_to_children(0.05, "TXN003"),
    "Amount too small to distribute"
  )

  expect_equal(round(main_account$balance, 2), 0.95)
  expect_gt(main_account$child_accounts$HighPriority$balance, 0)
})

test_that("MainAccount: distribution is proportional to relative allocation", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(300, channel = "Equity")

  child1 <- ChildAccount$new("Needs", allocation = 0.1, priority = 1)
  child2 <- ChildAccount$new("Goals", allocation = 0.2, priority = 2)

  main_account$add_child_account(child1)
  main_account$add_child_account(child2)

  main_account$distribute_to_children(300, "TXN004")

  total_allocation <- child1$allocation + child2$allocation

  expect_equal(round(child1$balance, 2),
               round((child1$allocation / total_allocation) * 300, 2))
  expect_equal(round(child2$balance, 2),
               round((child2$allocation / total_allocation) * 300, 2))
  expect_equal(round(main_account$balance, 2), 0)
})


test_that("MainAccount: skips inactive child accounts", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(100, channel = "Equity")

  active_child <- ChildAccount$new(
    "Active",
    allocation = 0.5,
    priority = 5
  )
  inactive_child <- ChildAccount$new(
    "Inactive", allocation = 0.5,
    priority = 10
  )
  inactive_child$status <- "inactive"

  main_account$add_child_account(active_child)
  main_account$add_child_account(inactive_child)

  main_account$distribute_to_children(100, "TXN005")

  expect_equal(active_child$balance, 100)
  expect_equal(inactive_child$balance, 0)
})

test_that("MainAccount: children are distributed to in priority order", {
  main_account <- MainAccount$new("Main")
  main_account$deposit(100, channel = "Equity")

  child1 <- ChildAccount$new("Child1", allocation = 0.5, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.5, priority = 10)

  main_account$add_child_account(child1)
  main_account$add_child_account(child2)

  # Clear child transactions before distribution
  child1$transactions <- child1$transactions[0, ]
  child2$transactions <- child2$transactions[0, ]

  main_account$distribute_to_children(100, "TXN008")

  # Get timestamps of the deposit transactions
  t1 <- child2$transactions$Date[1]  # child2 has higher priority
  t2 <- child1$transactions$Date[1]  # child1 has lower priority

  # child2 should have received deposit before child1
  expect_lt(t1, t2)
})


test_that("Distributing to Main cascades down to tier 3 accounts", {
  main_account <- MainAccount$new("Main")

  # Add tier 2 child
  needs <- ChildAccount$new("Needs", allocation = 1.0, priority = 10)
  main_account$add_child_account(needs)

  # Add tier 3 grandchildren under 'Needs'
  rent <- GrandchildAccount$new(
    "Rent", allocation = 0.6, fixed_amount = 7500,
    account_type = "Bill", freq = 30, due_date = dmy("28-01-2025")
  )
  househelp <- GrandchildAccount$new(
    "Househelp", allocation = 0.4, fixed_amount = 10000,
    account_type = "Bill", freq = 30, due_date = dmy("28-01-2025")
  )

  needs$add_child_account(rent)
  needs$add_child_account(househelp)

  # Deposit into main (triggers cascading distribution)
  main_account$deposit(1000, channel = "Equity")

  # Expect 'Needs' to receive full 1000 due to 100% allocation
  expect_equal(round(main_account$child_accounts$Needs$balance, 2), 0)
  expect_equal(round(main_account$balance, 2), 0)

  # Expect grandchildren to receive their shares
  expect_equal(round(rent$balance, 2), 600)      # 60% of 1000
  expect_equal(round(househelp$balance, 2), 400) # 40% of 1000
})



test_that("add_child_account: adds child correctly within allocation limit", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("Child1", allocation = 0.5, priority = 1)

  main_account$add_child_account(child)

  expect_true("Child1" %in% names(main_account$child_accounts))
  expect_equal(main_account$child_accounts$Child1$allocation, 0.5)
  expect_equal(main_account$total_allocation, 0.5)
})

# ====================================================================
# Test add child accounts
# ====================================================================
test_that("add_child_account: adds child correctly within allocation limit", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("Child1", allocation = 0.5, priority = 1)

  main_account$add_child_account(child)

  expect_true("Child1" %in% names(main_account$child_accounts))
  expect_equal(main_account$child_accounts$Child1$allocation, 0.5)
  expect_equal(main_account$total_allocation, 0.5)
})

test_that("add_child_account: throws error if total allocation exceeds 1", {
  main_account <- MainAccount$new("Main")
  child1 <- ChildAccount$new("Child1", allocation = 0.7, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.5, priority = 1)

  main_account$add_child_account(child1)
  expect_error(
    main_account$add_child_account(child2),
    "Total allocation exceeds 100%"
  )
})

test_that("add_child_account: sets status to inactive if allocation is 0", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("ChildZero", allocation = 0, priority = 1)

  main_account$add_child_account(child)

  expect_equal(child$status, "inactive")
  expect_true("ChildZero" %in% names(main_account$child_accounts))
})

test_that("add_child_account: assigns parent correctly", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("ChildX", allocation = 0.1, priority = 1)

  main_account$add_child_account(child)

  expect_identical(child$parent, main_account)
})

test_that("add_child_account: constructs correct path", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("ChildY", allocation = 0.1, priority = 1)

  main_account$add_child_account(child)

  expected_path <- paste0(main_account$path, "$child_accounts$`ChildY`")
  expect_equal(child$path, expected_path)
})

test_that("add_child_account: updates total_allocation correctly", {
  main_account <- MainAccount$new("Main")
  child1 <- ChildAccount$new("Child1", allocation = 0.3, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.4, priority = 1)

  main_account$add_child_account(child1)
  expect_equal(main_account$total_allocation, 0.3)

  main_account$add_child_account(child2)
  expect_equal(main_account$total_allocation, 0.7)
})

# ====================================================================
# Test set_child_allocation
# ====================================================================



test_that("set_child_allocation: updates allocation correctly", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("Child1", allocation = 0.3, priority = 1)
  main_account$add_child_account(child)

  main_account$set_child_allocation("Child1", 0.5)

  expect_equal(main_account$child_accounts$Child1$allocation, 0.5)
  expect_equal(main_account$total_allocation, 0.5)
})

test_that("set_child_allocation: throws error for allocation > 1", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("Child1", allocation = 0.5, priority = 1)
  main_account$add_child_account(child)

  expect_error(
    main_account$set_child_allocation("Child1", 1.1),
    "Allocation must be between 0 and 1"
  )
})


test_that("set_child_allocation: throws error for negative allocation", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("Child1", allocation = 0.2, priority = 1)
  main_account$add_child_account(child)

  expect_error(
    main_account$set_child_allocation("Child1", -0.1),
    "Allocation must be between 0 and 1"
  )
})

test_that("set_child_allocation: throws error if total allocation exceeds 1", {
  main_account <- MainAccount$new("Main")
  child1 <- ChildAccount$new("Child1", allocation = 0.4, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.5, priority = 1)

  main_account$add_child_account(child1)
  main_account$add_child_account(child2)

  expect_error(
    main_account$set_child_allocation("Child1", 0.7),
    "Total allocation exceeds 100%"
  )
})

test_that("set_child_allocation: sets status to inactive when allocation is zero", {
  main_account <- MainAccount$new("Main")
  child <- ChildAccount$new("ChildZero", allocation = 0.3, priority = 1)
  main_account$add_child_account(child)

  main_account$set_child_allocation("ChildZero", 0)

  expect_equal(main_account$child_accounts$ChildZero$status, "inactive")
  expect_equal(main_account$child_accounts$ChildZero$allocation, 0)
})

test_that("set_child_allocation: updates total_allocation correctly", {
  main_account <- MainAccount$new("Main")
  child1 <- ChildAccount$new("Child1", allocation = 0.2, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.3, priority = 1)

  main_account$add_child_account(child1)
  main_account$add_child_account(child2)

  main_account$set_child_allocation("Child1", 0.4)
  expect_equal(round(main_account$total_allocation, 2), 0.7)
})

# ====================================================================
# Test withdrawal method
# ====================================================================
test_that("withdraw: successful withdrawal reduces balance and logs transaction", {
  account <- MainAccount$new("Main")
  account$deposit(100, channel = "Initial")
  
  account$withdraw(
    amount = 30,
    transaction_number = "TXN100",
    by = "User",
    channel = "M-Pesa"
  )

  expect_equal(round(account$balance, 2), 70)
  last_txn <- tail(account$transactions, 1)
  expect_equal(last_txn$Amount, 30)
  expect_equal(last_txn$Type, "Withdrawal")
  expect_equal(last_txn$TransactionID, "TXN100")
  expect_equal(last_txn$Channel, "M-Pesa")
})

test_that("withdraw: fails if amount exceeds balance", {
  account <- MainAccount$new("Main")
  account$deposit(20, channel = "Initial")

  expect_error(
    account$withdraw(50, transaction_number = "TXN101", channel = "Bank"),
    "Insufficient balance"
  )
})

test_that("withdraw: fails if channel is missing", {
  account <- MainAccount$new("Main")
  account$deposit(100, channel = "Initial")

  expect_error(
    account$withdraw(10, transaction_number = "TXN102"),
    "Channel is required for withdrawals"
  )
})

test_that("withdraw: generates transaction ID if not provided", {
  account <- MainAccount$new("Main")
  account$deposit(50, channel = "Initial")

  account$withdraw(amount = 10, channel = "Equity")

  last_txn <- tail(account$transactions, 1)
  expect_equal(last_txn$Amount, 10)
  # Assuming sys-prefixed IDs
  expect_true(grepl("^sys", last_txn$TransactionID))
})

test_that("withdraw: logs the provided date correctly", {
  account <- MainAccount$new("Main")
  account$deposit(200, channel = "Initial")

  date_used <- as.POSIXct("2024-12-31 10:00:00")
  account$withdraw(
    50,
    channel = "Bank",
    transaction_number = "TXN900",
    date = date_used
  )

  last_txn <- tail(account$transactions, 1)
  expect_true(all.equal(last_txn$Date, date_used))
})

test_that("withdraw: does nothing if amount <= 0", {
  account <- MainAccount$new("Main")
  account$deposit(100, channel = "Initial")

  account$withdraw(0, channel = "Bank", transaction_number = "TXN500")
  expect_equal(nrow(account$transactions), 1)  # Only deposit exists
  expect_equal(account$balance, 100)
})

# ====================================================================
# Test get balance method
# ====================================================================

test_that("get_balance: returns correct balance and prints output", {
  account <- MainAccount$new("Main")

  # Deposit some money to update balance
  account$deposit(250, channel = "Initial")

  # Capture the printed output from cat()
  output <- capture.output({
    balance_returned <- account$get_balance()
  })

  # Test the return value
  expect_equal(balance_returned, 250)

  # Test the printed message
  expect_true(any(grepl("Current Balance: 250", output)))
})


# ====================================================================
# Test get_transactions method
# ====================================================================

test_that("get_transactions: prints message and returns empty data.frame when no transactions", {
  account <- MainAccount$new("Main")

  # Capture output when there are no transactions
  output <- capture.output({
    txns <- account$get_transactions()
  })

  expect_true(any(grepl("No transactions found\\.", output)))
  expect_s3_class(txns, "data.frame")
  expect_equal(nrow(txns), 0)
})

test_that("get_transactions: prints transactions and returns data.frame when transactions exist", {
  account <- MainAccount$new("Main")

  # Add a transaction
  account$deposit(100, channel = "Initial")

  # Capture output when there are transactions
  output <- capture.output({
    txns <- account$get_transactions()
  })

  expect_true(any(grepl("Transaction History:", output)))
  expect_s3_class(txns, "data.frame")
  expect_gte(nrow(txns), 1)
  expect_equal(txns$Amount[1], 100)
})

# ====================================================================
# Test list_child_accounts method
# ====================================================================
test_that("list_child_accounts: prints message when no children exist", {
  account <- MainAccount$new("Main")

  output <- capture.output({
    account$list_child_accounts()
  })

  expect_true(any(grepl("No child accounts found\\.", output)))
})

test_that("list_child_accounts: prints names of child accounts", {
  account <- MainAccount$new("Main")

  # Add two children
  child1 <- ChildAccount$new("Savings", allocation = 0.4, priority = 1)
  child2 <- ChildAccount$new("Emergency", allocation = 0.6, priority = 2)

  account$add_child_account(child1)
  account$add_child_account(child2)

  output <- capture.output({
    account$list_child_accounts()
  })

  expect_true(any(grepl("Child Accounts of Main", output)))
  expect_true(any(grepl("- Savings", output)))
  expect_true(any(grepl("- Emergency", output)))
})

# ====================================================================
# Test list_find_account method
# ====================================================================
test_that("find_account: finds self if target name matches", {
  main <- MainAccount$new("Main")
  result <- main$find_account("Main")
  expect_identical(result, main)
})

test_that("find_account: finds immediate child account", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  main$add_child_account(child)

  result <- main$find_account("Child")
  expect_identical(result, child)
})

test_that("find_account: finds grandchild account recursively", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- GrandchildAccount$new(
    "Grandchild",
    allocation = 0.5,
    priority = 1
  )

  child$add_child_account(grandchild)
  main$add_child_account(child)

  result <- main$find_account("Grandchild")
  expect_identical(result, grandchild)
})

test_that("find_account: returns NULL if account not found", {
  main <- MainAccount$new("Main")
  result <- main$find_account("NonExistent")
  expect_null(result)
})

test_that("find_account: can traverse up to find parent", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  main$add_child_account(child)

  result <- child$find_account("Main")
  expect_identical(result, main)
})

test_that("find_account: avoids infinite recursion in the presence of cycles", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.3, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Inject a cycle
  grandchild$add_child_account(child)

  # Should not crash or loop forever
  result <- main$find_account("GhostAccount")
  expect_null(result)
})



# ====================================================================
# Test list_find_account_by uuid method
# ====================================================================
test_that("find_account_by_uuid: finds self when UUID matches", {
  main <- MainAccount$new("Main")
  expect_identical(main$find_account_by_uuid(main$uuid), main)
})

test_that("find_account_by_uuid: finds direct child by UUID", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.4, priority = 1)
  main$add_child_account(child)

  expect_identical(main$find_account_by_uuid(child$uuid), child)
})

test_that("find_account_by_uuid: finds grandchild by UUID", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.4, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.3, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  expect_identical(main$find_account_by_uuid(grandchild$uuid), grandchild)
})

test_that("find_account_by_uuid: returns NULL when UUID is not found", {
  main <- MainAccount$new("Main")
  ghost_uuid <- paste0("uuid-", sample(100000:999999, 1))
  expect_null(main$find_account_by_uuid(ghost_uuid))
})

test_that("find_account_by_uuid: avoids infinite recursion in the presence of cycles", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.4, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.3, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Inject a cycle: grandchild adds child again
  grandchild$add_child_account(child)

  # Should still return NULL gracefully without infinite loop
  result <- main$find_account_by_uuid("non-existent-uuid-12345")
  expect_null(result)
})


test_that("find_account_by_uuid: skips paths already visited", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.4, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.3, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  visited <- list()
  visited[[grandchild$path]] <- TRUE  # Pretend grandchild already visited

  result <- main$find_account_by_uuid(grandchild$uuid, visited_paths = visited)
  expect_null(result)
})

test_that("find_account_by_uuid: does not re-traverse already visited parent", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.4, priority = 1)
  main$add_child_account(child)

  visited <- list()
  visited[[main$path]] <- TRUE  # Simulate parent already visited

  result <- child$find_account_by_uuid(main$uuid, visited_paths = visited)
  expect_null(result)
})

# ====================================================================
# Test Move balance method
# ====================================================================
test_that("move_balance: transfers from one child to another", {
  main <- MainAccount$new("Main")

  child1 <- ChildAccount$new("Child1", allocation = 0.6, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.4, priority = 2)

  main$add_child_account(child1)
  main$add_child_account(child2)

  main$deposit(100, channel = "Initial")

  # By now, both children received funds
  # Let's transfer from child1 to child2
  child1$move_balance("Child2", 30)

  expect_equal(child1$get_balance(), 30)  # started with 60
  expect_equal(child2$get_balance(), 70)  # started with 40 + 30
})


test_that("move_balance: throws error if target account does not exist", {
  main <- MainAccount$new("Main")
  main$deposit(100, channel = "Initial")

  expect_error(
    main$move_balance("Ghost", 30),
    "Target account not found"
  )
})

test_that("move_balance: throws error if target account does not exist", {
  main <- MainAccount$new("Main")
  main$deposit(100, channel = "Initial")

  expect_error(
    main$move_balance("Ghost", 30),
    "Target account not found"
  )
})

test_that("move_balance: throws error if balance is insufficient", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  main$add_child_account(child)

  # No deposit, so main has 0
  expect_error(
    main$move_balance("Child", 10),
    "Insufficient balance"
  )
})

test_that("move_balance: uses 'Internal Transfer' when transferring between children", {
  main <- MainAccount$new("Main")
  child1 <- ChildAccount$new("Child1", allocation = 0.5, priority = 1)
  child2 <- ChildAccount$new("Child2", allocation = 0.5, priority = 1)
  main$add_child_account(child1)
  main$add_child_account(child2)

  main$deposit(100, channel = "Initial")  # distributes to both children

  child1$move_balance("Child2", 20)  # internal move

  last_txn1 <- tail(child1$get_transactions(), 1)
  last_txn2 <- tail(child2$get_transactions(), 1)

  expect_equal(last_txn1$Channel, "Internal Transfer")
  expect_equal(last_txn2$Channel, "Internal Transfer")
})

# ====================================================================
# Test list_all_accounts method
# ====================================================================

test_that("list_all_accounts: returns only self when no children or parents", {
  acc <- MainAccount$new("Main")
  expect_equal(acc$list_all_accounts(), "Main")
})

test_that("list_all_accounts: returns self and child account names", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  main$add_child_account(child)

  result <- main$list_all_accounts()
  expect_true(all(c("Main", "Child") %in% result))
})

test_that("list_all_accounts: returns names from deeply nested structure", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.5, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  result <- main$list_all_accounts()
  expect_true(all(c("Main", "Child", "GrandChild") %in% result))
})

test_that("list_all_accounts: traverses upwards to parent", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  main$add_child_account(child)

  result <- child$list_all_accounts()
  expect_true(all(c("Child", "Main") %in% result))
})

test_that("list_all_accounts: avoids infinite loops due to cycles", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.5, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Inject cycle: GrandChild has child as a child
  grandchild$add_child_account(child)

  result <- main$list_all_accounts()
  expect_true(all(c("Main", "Child", "GrandChild") %in% result))
  expect_length(result, 4)
})

test_that("list_all_accounts: does not duplicate account names", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- ChildAccount$new("GrandChild", allocation = 0.5, priority = 1)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  result <- main$list_all_accounts()
  expect_equal(length(result), length(unique(result)))
})

# ====================================================================
# Test compute total balance method
# ====================================================================
test_that("compute_total_balance: single account", {
  acc <- MainAccount$new("Solo")
  acc$deposit(100, channel = "Initial")
  expect_equal(acc$compute_total_balance(), 100)
})

test_that("compute_total_balance: includes child balances", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)

  main$add_child_account(child)
  child$deposit(50, channel = "Initial")
  main$deposit(100, channel = "Initial")

  total_expected <- main$balance + child$balance
  expect_equal(main$compute_total_balance(), total_expected)
})

test_that("compute_total_balance: includes grandchildren balances", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 0.5, priority = 1)
  grandchild <- ChildAccount$new("Grand", allocation = 0.3, priority = 2)

  main$add_child_account(child)
  child$add_child_account(grandchild)

  main$deposit(100, channel = "Initial")
  child$deposit(30, channel = "Initial")
  grandchild$deposit(20, channel = "Initial")

  total_expected <- main$balance + child$balance + grandchild$balance
  expect_equal(main$compute_total_balance(), total_expected)
})

# ====================================================================
# Test compute total due method
# ====================================================================

test_that("compute_total_due: returns 0 when no dues exist", {
  acc <- MainAccount$new("Main")
  expect_equal(acc$compute_total_due(), 0)
})

test_that("compute_total_due: returns 0 when no dues exist", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  main$add_child_account(child)
  expect_equal(main$compute_total_due(), 0)
})


test_that("compute_total_due: skips grandchildren with NULL amount_due", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  gc1 <- GrandchildAccount$new("GC1", allocation = 0.5, priority = 1)
  gc2 <- GrandchildAccount$new("GC2", allocation = 0.5, priority = 1)

  main$add_child_account(child)
  child$add_child_account(gc1)
  child$add_child_account(gc2)

  gc1$amount_due <- 100
  # gc2$amount_due is NULL

  expect_equal(main$compute_total_due(), 100)
})

# ====================================================================
# Test spending method
# ====================================================================

test_that("spending: sums user withdrawals in main account only", {
  main <- MainAccount$new("Main")
  main$deposit(100, by = "User", channel = "Initial")
  main$withdraw(40, by = "User", channel = "Spending")
  main$withdraw(10, by = "System", channel = "Fee")  # Should be ignored

  expect_equal(main$spending(), 40)
})

test_that("spending: includes withdrawals from all nested accounts", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grandchild <- GrandchildAccount$new("GC", allocation = 1, priority = 1)

  # Deposit *before* hierarchy is built
  main$deposit(100, by = "User", channel = "Initial")
  child$deposit(100, by = "User", channel = "Initial")
  grandchild$deposit(100, by = "User", channel = "Initial")

  # Now build the tree (no redistribution triggered)
  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Withdrawals by user at each level
  main$withdraw(10, by = "User", channel = "Purchase")
  child$withdraw(20, by = "User", channel = "Purchase")
  grandchild$withdraw(30, by = "User", channel = "Purchase")

  expect_equal(main$spending(), 60)
})

test_that("spending: applies date range filter", {
  main <- MainAccount$new("Main")

  # Deposit and withdraw on two different dates
  main$deposit(100, by = "User", channel = "Initial")
  main$withdraw(10, by = "User", channel = "OldTxn")
  Sys.sleep(2)
  main$withdraw(20, by = "User", channel = "RecentTxn")

  # Set daterange to exclude the first withdrawal
  start <- Sys.time() - seconds(2)
  end <- Sys.time() + seconds(2)

  expect_equal(main$spending(c(start, end)), 20)
})

test_that("spending: excludes non-user withdrawals", {
  main <- MainAccount$new("Main")
  main$deposit(100, by = "System", channel = "Funding")
  main$withdraw(40, by = "System", channel = "Fee")
  expect_equal(main$spending(), 0)
})

# ====================================================================
# Test total income method
# ====================================================================

test_that("total_income: sums user deposits across all account levels", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grandchild <- GrandchildAccount$new("GC", allocation = 1, priority = 1)

  # Deposit before attaching to preserve income at each node
  main$deposit(50, by = "User", channel = "Initial")
  child$deposit(100, by = "User", channel = "Initial")
  grandchild$deposit(150, by = "User", channel = "Initial")

  # Build the hierarchy
  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Total user deposits = 50 + 100 + 150 = 300
  expect_equal(main$total_income(), 300)
})

test_that("total_income: handles no children", {
  acc <- MainAccount$new("Main")
  acc$deposit(80, by = "User", channel = "Initial")
  expect_equal(acc$total_income(), 80)
})

test_that("total_income: excludes non-user deposits", {
  acc <- MainAccount$new("Main")
  acc$deposit(80, by = "System", channel = "Adjustment")
  expect_equal(acc$total_income(), 0)
})

# ====================================================================
# Test allocated amount method
# ====================================================================
test_that("allocated_amount: includes own deposits (user + system) and child user deposits", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grandchild <- GrandchildAccount$new("GC", allocation = 1, priority = 1)

  # Own deposits
  main$deposit(50, by = "User", channel = "Initial")
  main$deposit(20, by = "System", channel = "Adjustment")

  # Deposit before attaching children
  child$deposit(100, by = "User", channel = "Initial")
  grandchild$deposit(30, by = "User", channel = "Initial")

  # Assemble hierarchy
  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Expect 50 + 20 + 100 + 30 = 200
  expect_equal(main$allocated_amount(), 200)
})

test_that("allocated_amount: returns only own deposits if no children", {
  acc <- MainAccount$new("Main")
  acc$deposit(60, by = "User", channel = "Initial")
  acc$deposit(40, by = "System", channel = "Top-up")

  expect_equal(acc$allocated_amount(), 100)
})

test_that("allocated_amount: ignores child system deposits", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)

  main$deposit(20, by = "User", channel = "Initial")
  child$deposit(80, by = "System", channel = "Top-up")

  main$add_child_account(child)

  # Only 20 from main (user) and 0 from child (system-only)
  expect_equal(main$allocated_amount(), 20)
})


test_that("allocated_amount: correctly sums deep descendant user deposits", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grandchild <- GrandchildAccount$new("GC", allocation = 1, priority = 1)
  great_grandchild <- GrandchildAccount$new("GGC", allocation = 1, priority = 1)

  main$deposit(100, by = "User", channel = "Initial")
  grandchild$deposit(40, by = "User", channel = "Initial")
  great_grandchild$deposit(10, by = "User", channel = "Initial")

  main$add_child_account(child)
  child$add_child_account(grandchild)
  grandchild$add_child_account(great_grandchild)

  # Expected: 100 + 40 + 10
  expect_equal(main$allocated_amount(), 150)
})

test_that("allocated_amount: returns 0 if no transactions field", {
  acc <- MainAccount$new("main")
  acc$transactions <- NULL

  expect_equal(acc$allocated_amount(), 0)
})


test_that("allocated_amount: returns 0 if transactions is NULL (coverage)", {
  main <- MainAccount$new("Main")
  ch1 <- ChildAccount$new("child")
  # simulate broken or unset transactions
  ch1$transactions <- NULL
  main$add_child_account(ch1)
  expect_equal(main$allocated_amount(), 0)
})

# ====================================================================
# Test income_utilization method
# ====================================================================

test_that("income_utilization: correct ratio when spending is half of allocated amount", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  main$add_child_account(child)
  main$deposit(100, by = "User", channel = "Initial")  # main allocated = 100
  child$withdraw(50, by = "User", channel = "Purchase")  # spending = 50

  expect_equal(main$income_utilization(), 0.5)
})

test_that("income_utilization: returns 0 when no spending", {
  main <- MainAccount$new("Main")
  main$deposit(100, by = "User", channel = "Initial")

  expect_equal(main$income_utilization(), 0)
})

test_that("income_utilization: handles zero allocated by avoiding division by zero", {
  main <- MainAccount$new("Main")
  #main$withdraw(20, by = "User", channel = "Emergency")

  utilization <- main$income_utilization()
  expect_equal(utilization, 0)
  expect_true(is.finite(utilization))
})

test_that("income_utilization: returns 1 when all allocated amount is spent", {
  main <- MainAccount$new("Main")
  main$deposit(80, by = "User", channel = "Initial")
  main$withdraw(80, by = "User", channel = "Usage")
  expect_equal(main$income_utilization(), 1)
})


test_that("income_utilization: recursively accounts for child deposits and withdrawals", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grandchild <- GrandchildAccount$new("GC", allocation = 1, priority = 1)

  main$deposit(50, by = "User", channel = "Initial")
  child$deposit(50, by = "User", channel = "Initial")
  grandchild$deposit(100, by = "User", channel = "Initial")

  main$add_child_account(child)
  child$add_child_account(grandchild)

  # Only grandchild spends
  grandchild$withdraw(50, by = "User", channel = "Purchase")

  # Total allocated = 50 + 50 + 100 = 200
  # Total spending = 50
  expect_equal(main$income_utilization(), 0.25)
})

# ====================================================================
# Test walking amount
# ====================================================================

test_that("walking_amount: returns 0 when Track_dues_and_balance is NULL", {
  acc <- MainAccount$new("Main")
  expect_equal(acc$walking_amount("amount_due"), 0)
  expect_equal(acc$walking_amount("Balance"), 0)
})

test_that("walking_amount: returns latest due or balance from Track_dues_and_balance", {
  acc <- GrandchildAccount$new("GC", allocation = 1, priority = 1)
  acc$Track_dues_and_balance <- data.frame(
    Date = as.POSIXct(c("2024-01-01", "2024-02-01")),
    Amount_due = c(100, 150),
    Balance = c(200, 250)
  )

  expect_equal(acc$walking_amount("amount_due"), 150)
  expect_equal(acc$walking_amount("Balance"), 250)
})

test_that("walking_amount: respects date range filter", {
  acc <- GrandchildAccount$new("GC", allocation = 1, priority = 1)
  acc$Track_dues_and_balance <- data.frame(
    Date = as.POSIXct(c("2022-01-01", "2024-01-01"), tz = "UTC"),
    Amount_due = c(300, 400),
    Balance = c(600, 700)
  )
  range <- c(as.Date("2023-01-01"), as.Date("2023-12-31"))

  #print(acc$walking_amount("amount_due", daterange = range))
  expect_equal(acc$walking_amount("amount_due", daterange = range), 0)
  expect_equal(acc$walking_amount("Balance", daterange = range), 0)
})



test_that("walking_amount: aggregates amount_due from nested accounts", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  gc1 <- GrandchildAccount$new("GC1", allocation = 0.5, priority = 1)
  gc2 <- GrandchildAccount$new("GC2", allocation = 0.5, priority = 1)

  gc1$Track_dues_and_balance <- data.frame(
    Date = as.POSIXct("2024-05-01"),
    Amount_due = 100,
    Balance = 200
  )
  gc2$Track_dues_and_balance <- data.frame(
    Date = as.POSIXct("2024-05-01"),
    Amount_due = 150,
    Balance = 250
  )

  main$add_child_account(child)
  child$add_child_account(gc1)
  child$add_child_account(gc2)

  expect_equal(main$walking_amount("amount_due"), 250)
  expect_equal(main$walking_amount("Balance"), 450)
})

test_that("walking_amount: handles children without tracking data", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  gc <- GrandchildAccount$new("GC", allocation = 1, priority = 1)

  gc$Track_dues_and_balance <- data.frame(
    Date = as.POSIXct("2024-05-01"),
    Amount_due = 120,
    Balance = 300
  )

  main$add_child_account(child)
  child$add_child_account(gc)

  expect_equal(main$walking_amount("amount_due"), 120)
  expect_equal(main$walking_amount("Balance"), 300)
})


# ====================================================================
# Test compute_total_due_within_n_days
# ====================================================================

test_that("compute_total_due_within_n_days: returns 0 when no due info", {
  acc <- GrandchildAccount$new("Test", allocation = 1, priority = 1)
  expect_equal(acc$compute_total_due_within_n_days(7), 0)
})

test_that("compute_total_due_within_n_days: counts amount due within range", {
  acc <- GrandchildAccount$new("Test", allocation = 1, priority = 1)
  acc$amount_due <- 200
  acc$due_date <- Sys.time() + days(3)
  expect_equal(acc$compute_total_due_within_n_days(7), 200)
})

test_that("compute_total_due_within_n_days: ignores amount due beyond range", {
  acc <- GrandchildAccount$new("Test", allocation = 1, priority = 1)
  acc$amount_due <- 200
  acc$due_date <- Sys.time() + days(10)
  expect_equal(acc$compute_total_due_within_n_days(7), 0)
})

test_that("compute_total_due_within_n_days: ignores past due_date", {
  acc <- GrandchildAccount$new("Test", allocation = 1, priority = 1)
  acc$amount_due <- 150
  acc$due_date <- Sys.time() - days(1)
  expect_equal(acc$compute_total_due_within_n_days(7), 0)
})

test_that("compute_total_due_within_n_days: aggregates recursively", {
  main <- MainAccount$new("Main")
  child <- ChildAccount$new("Child", allocation = 1, priority = 1)
  grand1 <- GrandchildAccount$new("GC1", allocation = 0.5, priority = 1)
  grand2 <- GrandchildAccount$new("GC2", allocation = 0.5, priority = 1)

  grand1$amount_due <- 100
  grand1$due_date <- Sys.time() + days(3)

  grand2$amount_due <- 150
  grand2$due_date <- Sys.time() + days(10)

  child$add_child_account(grand1)
  child$add_child_account(grand2)
  main$add_child_account(child)

  expect_equal(main$compute_total_due_within_n_days(7), 100)
})

test_that("compute_total_due_within_n_days: ignores amount_due with missing due_date", {
  acc <- GrandchildAccount$new("Test", allocation = 1, priority = 1)
  acc$amount_due <- 200
  expect_equal(acc$compute_total_due_within_n_days(7), 0)
})
