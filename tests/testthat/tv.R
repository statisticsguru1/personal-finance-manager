acc <- GrandchildAccount$new("Tracker", fixed_amount = 300)
acc$num_periods <- 1
acc$deposit(100,channel = "MPESA")
