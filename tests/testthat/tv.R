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

main$allocated_amount()