main <- MainAccount$new("main")
child <- ChildAccount$new("child", allocation = 1)
grandchild_acc <- GrandchildAccount$new(
  name = "Rent",
  allocation = 1,
  priority = 1,
  fixed_amount = 100,
  due_date = Sys.time() + lubridate::days(5), # Overdue
  account_type = "Bill",
  freq = 30 # 30-day billing cycle
)
main$add_child_account(child)
child$add_child_account(grandchild_acc)

grandchild_acc$deposit(150, channel = "Bank")

grandchild_acc$Track_dues_and_balance
