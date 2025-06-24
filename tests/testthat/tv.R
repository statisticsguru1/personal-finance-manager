main <- MainAccount$new("Main")
ch1 <- ChildAccount$new("child")
# simulate broken or unset transactions
ch1$transactions <- NULL

main$add_child_account(ch1)
print(acc$allocated_amount())


library(R6)