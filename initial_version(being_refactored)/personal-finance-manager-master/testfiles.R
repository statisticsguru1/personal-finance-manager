source("accounts.R")
# main account
main_account <- MainAccount$new("Main")

#tier 2
main_account$add_child_account(
  ChildAccount$new("Needs",
                   allocation = .55))

main_account$add_child_account(
  ChildAccount$new("Goals",
                   allocation = .25))

main_account$add_child_account(
  ChildAccount$new("Debt Repayment",
                   allocation = .2)
)


#tier 3
# needs
main_account$child_accounts$`Needs`$add_child_account(
  GrandchildAccount$new("Rent",
                        allocation = .2,
                        fixed_amount=7500,
                        account_type="Bill",
                        freq=30,
                        due_date = dmy("28-1-2025")
  )
)
main_account$child_accounts$`Need`$add_child_account(
  GrandchildAccount$new("househelp",
                        allocation = .2,
                        fixed_amount=10000,
                        account_type="Bill",
                        freq=30,
                        due_date = dmy("28-1-2025")
  )
)
main_account$child_accounts$`Needs`$add_child_account(
  GrandchildAccount$new("Kithuia youth",
                        allocation = .1,
                        fixed_amount=3000,
                        account_type="Bill",
                        freq=30,
                        due_date = dmy("04-2-2025")
  )
  
)

# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("Hope",
#                         allocation = .05,
#                         fixed_amount=3500,
#                         account_type="Bill",
#                         freq=70,
#                         due_date = dmy("06-2-2025")
#   )
#   
# )
# 
# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("Shamba boy",
#                         allocation = .1,
#                         fixed_amount=3000,
#                         account_type="Bill",
#                         freq=30,
#                         due_date = dmy("28-1-2025")
#   )
#   
# )
# 
# 
# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("Internet",
#                         allocation = .15,
#                         fixed_amount=3000,
#                         account_type="Bill",
#                         freq=30,
#                         due_date = dmy("10-2-2025")
#   )
#   
# )
# 
# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("miscellaneous",
#                         allocation = .15,
#                         fixed_amount=3500,
#                         account_type="Bill",
#                         freq=30,
#                         due_date = dmy("10-2-2025")
#   )
#   
# )
# 
# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("Farm inputs",
#                         allocation = .05,
#                         fixed_amount=1500,
#                         account_type="Bill",
#                         freq=30,
#                         due_date = dmy("16-2-2025")
#   )
# )
# 
# main_account$child_accounts$`Needs`$add_child_account(
#   GrandchildAccount$new("Insurance",
#                         allocation = .0,
#                         fixed_amount=1500,
#                         account_type="Bill",
#                         freq=30,
#                         due_date = dmy("16-2-2025")
#   )
# )
# Debts

main_account$child_accounts$`Debt Repayment`$add_child_account(
  GrandchildAccount$new("Helb",
                        allocation = .20,
                        fixed_amount=250000,
                        account_type ="Debt"
  )
)

main_account$child_accounts$`Debt Repayment`$add_child_account(
  GrandchildAccount$new("Loan apps",
                        allocation = .4,
                        fixed_amount=15000,
                        account_type ="Debt"
  )
)


main_account$child_accounts$`Debt Repayment`$add_child_account(
  GrandchildAccount$new("Insurance-Debt",
                        allocation = .4,
                        fixed_amount=100000,
                        account_type ="Debt"
  )
)

# Goals 
main_account$child_accounts$`Goals`$add_child_account(
  GrandchildAccount$new("Farming",
                        allocation = .35,
                        account_type ="NonFixedSaving"
  ))

main_account$child_accounts$`Goals`$add_child_account(
  GrandchildAccount$new("saving",
                        allocation = .65,
                        account_type ="NonFixedSaving"
  ))



# Example usage
main_account$deposit(10000, transaction_number = "TXN001", channel = "Mpesa")
main_account$deposit(10000, transaction_number = "TXN002", channel = "EQuity")
main_account$deposit(10000, transaction_number = "TXN003", channel = "EQuity")
main_account$find_account("Rent")$deposit(10000, transaction_number = "TXN004", channel = "EQuity")
main_account$find_account("Rent")$withdraw(200, transaction_number = "TXN005", channel = "Cash")
#main_account$deposit(40000, transaction_number = "TXN005", channel = "EQuity")

