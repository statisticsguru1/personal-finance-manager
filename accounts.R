library(R6)
library(tidyverse)
library(uuid)
# MainAccount Account
MainAccount <- R6Class("MainAccount",
                       public = list(
                         uuid = NULL,
                         name = NULL,
                         balance = 0,
                         transactions = NULL,
                         transaction_counter = 1, # Counter for system-generated transaction numbers
                         child_accounts = list(), # List of child accounts
                         total_allocation = 0, # Total allocation across child accounts
                         path="main_account",
                  
                         
                         # Constructor
                         initialize = function(name) {
                           self$uuid <- paste0("acc",uuid::UUIDgenerate()) 
                           self$name <- name
                           self$balance <- 0
                           self$path<-"main_account"
                           self$transactions <- data.frame(
                             Type = character(),
                             By = character(),
                             TransactionID = character(),
                             Channel = character(),
                             Amount = numeric(),
                             Balance=numeric(),
                             Date = as.Date(character()),
                             stringsAsFactors = FALSE
                           )
                         },
                         
                         # Generate Transaction ID
                         generate_transaction_id = function() {
                           transaction_id <- paste0("sys", self$transaction_counter)
                           self$transaction_counter <- self$transaction_counter + 1
                           return(transaction_id)
                         },
                         
                         # Check for Duplicate Transaction Number
                         is_duplicate_transaction = function(transaction_number) {
                           return(transaction_number %in% self$transactions$TransactionID)
                         },
                         
                         # Deposit Method
                         deposit = function(amount, transaction_number = NULL,By="User", channel = NULL, date = Sys.Date()) {
                           if (amount <= 0) stop("Deposit amount must be greater than zero!")
                           if (is.null(channel)) stop("Channel is required for deposits!")
                           
                           if (is.null(transaction_number)) {
                             transaction_number <- self$generate_transaction_id()
                           } else {
                             transaction_number<- transaction_number
                           }
                           
                           if (self$is_duplicate_transaction(transaction_number)) {
                             stop("This deposit has already been made. Transaction ID:", transaction_number)
                           }
                           
                           self$balance <- self$balance + amount
                           self$transactions <- rbind(self$transactions, data.frame(
                             Type = "Deposit",
                             By=By,
                             TransactionID = transaction_number,
                             Channel = channel,
                             Amount = amount,
                             Balance=self$balance,
                             Date = as.Date(date),
                             stringsAsFactors = FALSE
                           ))
                           
                           # Distribute funds to child accounts and reset balance
                           self$distribute_to_children(amount,transaction_number)
                           cat("Deposited:", amount, "via", channel, "- Transaction ID:", transaction_number, "\n")
                         },
                         
                         # Distribute Funds to Child Accounts
                         distribute_to_children = function(amount, transaction, By = "System") {
                           if (length(self$child_accounts) == 0) {
                             return() 
                           }
                           
                           # Filter active child accounts
                           active_accounts <- purrr::keep(self$child_accounts, ~ .x$status == "active")
                           
                           if(length(active_accounts) == 0) {
                             message("No active child accounts available.")
                             return()
                           }
                           
                           # Check if the amount is too small to distribute
                           if (amount < 0.10) {
                             message("Amount too small to distribute. Depositing into the highest-priority child.")
                             highest_priority_child <- active_accounts[[which.max(purrr::map_dbl(active_accounts, "priority"))]]
                             self$withdraw(amount = amount, By = By, channel = paste("Allocation to", highest_priority_child$name))
                             highest_priority_child$deposit(amount, transaction, By = By, channel = paste("Allocation from", self$name))
                             return()
                           }
                           
                           # Calculate total active allocation for distribution
                           total_active_allocation <- sum(purrr::map_dbl(active_accounts, "allocation"))
                           
                           # Distribute funds to active children
                           for (child_account in active_accounts[order(-purrr::map_dbl(active_accounts, "priority"))]) {
                             allocation <- (child_account$allocation / total_active_allocation) * amount
                             self$withdraw(amount = allocation, By = By, channel = paste("Allocation to", child_account$name))
                             child_account$deposit(allocation, transaction, By = By, channel = paste("Allocation from", self$name))
                           }
                         },
                         
                         # Add Child Account
                         add_child_account = function(child_account) {
                           child_account$parent <- self  #add parent 
                           child_account$path<-paste0(self$path,"$child_accounts$", "`", child_account$name, "`")
                           if ((self$total_allocation + child_account$allocation) > 1) {
                             stop("Total allocation exceeds 100%. Please adjust the allocations.")
                           }
                           
                           if(child_account$allocation==0){ # With allocation of 0 deactivate
                             child_account$status<-"inactive"
                             
                           }
                           
                           self$child_accounts[[child_account$name]] <- child_account
                           self$total_allocation <- self$child_accounts%>%map("allocation")%>%unlist()%>%sum(na.rm = T)
                         },
                         
                         # Set Allocation for Child Account
                         set_child_allocation = function(child_account_name, new_allocation) {
                           if (new_allocation < 0 || new_allocation > 1) {
                             stop("Allocation must be between 0 and 1.")
                           }
                           
                           
                           current_allocation <- self$child_accounts[[child_account_name]]$allocation
                           allocation_difference <- new_allocation - current_allocation
                           
                           if ((self$total_allocation + allocation_difference) > 1) {
                             stop("Total allocation exceeds 100%. Please adjust the allocations.")
                           }
                           
                           if(self$child_accounts[[child_account_name]]$allocation==0){ # With allocation of 0 deactivate
                             self$child_accounts[[child_account_name]]$status<-"inactive"
                             
                           }
                           
                           self$child_accounts[[child_account_name]]$allocation <- new_allocation
                           self$total_allocation <- self$child_accounts%>%map("allocation")%>%unlist()%>%sum(na.rm = T)
                         },
                         
                         # Withdraw Method
                         withdraw = function(amount, transaction_number = NULL,By="User", channel = NULL, date = Sys.Date()) {
                           if (amount > 0) {
                           if (self$balance < amount) {
                             stop("Insufficient balance! Your current balance is:", self$balance)
                           }
                           if (is.null(channel)) stop("Channel is required for withdrawals!")
                           
                           if (is.null(transaction_number)) {
                             transaction_number <- self$generate_transaction_id()
                           } else {
                             transaction_number <-transaction_number
                           }
                           
                           self$balance <- self$balance - amount
                           self$transactions <- rbind(self$transactions, data.frame(
                             Type = "Withdrawal",
                             By=By,
                             TransactionID = transaction_number,
                             Channel = channel,
                             Amount = amount,
                             Balance=self$balance,
                             Date = as.Date(date),
                             stringsAsFactors = FALSE
                           ))
                           cat("Withdrew:", amount, "via", channel, "- Transaction ID:", transaction_number, "\n")
                           }
                         },
                         
                         # Check Balance Method
                         get_balance = function() {
                           cat("Current Balance:", self$balance, "\n")
                           return(self$balance)
                         },
                         
                         # View Transactions (Print + Return as Data Frame)
                         get_transactions = function() {
                           if (nrow(self$transactions) == 0) {
                             cat("No transactions found.\n")
                           } else {
                             cat("\nTransaction History:\n")
                             print(self$transactions)
                           }
                           return(self$transactions)
                         },
                         
                         # List child accounts
                         list_child_accounts = function() {
                           if (length(self$child_accounts) == 0) {
                             cat("No child accounts found.\n")
                           } else {
                             cat("\nChild Accounts of", self$name, ":\n")
                             for (child_account in self$child_accounts) {
                               cat("-", child_account$name, "\n")
                             }
                           }
                         },
                         # Method to find an account by name
                         find_account = function(target_name, visited_paths = NULL) {
                           
                           # Check if the current account is the target
                           if (self$name == target_name) {
                             return(self)
                           }
                           
                           # Mark the current path as visited to avoid circular recursion
                           if (is.null(visited_paths)) {
                             visited_paths <- list()
                           }
                           visited_paths[[self$path]] <- TRUE
                           
                           # Recursively search through child accounts
                           for (child_name in names(self$child_accounts)) {
                             child_account <- self$child_accounts[[child_name]]
                             
                             # If this child account or its path has already been visited, skip it
                             if (child_account$path %in% names(visited_paths)) {
                               next
                             }
                             
                             # Search within the child account
                             result <- child_account$find_account(target_name, visited_paths)
                             if (!is.null(result)) {
                               return(result)
                             }
                           }
                           
                           # If not found in current node or its children, move up to the parent (if exists)
                           if (!is.null(self$parent) && !(self$parent$path %in% names(visited_paths))) {
                             return(self$parent$find_account(target_name, visited_paths))
                           }
                           
                           # If the account is not found in the entire tree, return NULL
                           return(NULL)
                         },
                         find_account_by_uuid = function(target_uuid, visited_paths = NULL) {
                           # Check if the current account matches the target UUID
                           if (self$uuid == target_uuid) {
                             return(self)
                           }
                           
                           # Mark the current path as visited to avoid circular recursion
                           if (is.null(visited_paths)) {
                             visited_paths <- list()
                           }
                           visited_paths[[self$path]] <- TRUE
                           
                           # Recursively search through child accounts
                           for (child_name in names(self$child_accounts)) {
                             child_account <- self$child_accounts[[child_name]]
                             
                             # If this child account or its path has already been visited, skip it
                             if (child_account$path %in% names(visited_paths)) {
                               next
                             }
                             
                             # Search within the child account
                             result <- child_account$find_account_by_uuid(target_uuid, visited_paths)
                             if (!is.null(result)) {
                               return(result)
                             }
                           }
                           
                           # If not found in current node or its children, move up to the parent (if exists)
                           if (!is.null(self$parent) && !(self$parent$path %in% names(visited_paths))) {
                             return(self$parent$find_account_by_uuid(target_uuid, visited_paths))
                           }
                           
                           # If the account is not found in the entire tree, return NULL
                           return(NULL)
                         },
                         
                         move_balance = function(target_account_name, amount) {
                           # Use the existing withdraw method to remove funds from self
                           self$withdraw(amount=amount,By="System",channel = "Internal Transfer")
                           # Find the target account using the find_account method
                           target_account <- self$find_account(target_account_name)
                           
                           if (is.null(target_account)) {
                             stop("Target account not found")
                           }
                           
                           # Use the existing deposit method to add funds to the target account
                           target_account$deposit(amount=amount,By="System",channel = "Internal Transfer")
                           
                           cat("Moved", amount, "from", self$name, "to", target_account$name, "\n")
                         },
                         list_all_accounts = function(visited_paths = NULL) {
                           # Initialize visited_paths if it's NULL (this tracks visited paths to avoid revisiting)
                           if (is.null(visited_paths)) {
                             visited_paths <- list()
                           }
                           
                           # Add the current account's path to the list of visited paths
                           visited_paths[[self$path]] <- TRUE
                           
                           # Start with the current account
                           all_accounts <- list(self$name)
                           
                           # Recursively traverse through child accounts and collect their names
                           for (child_name in names(self$child_accounts)) {
                             child_account <- self$child_accounts[[child_name]]
                             
                             # If this child account's path has already been visited, skip it
                             if (child_account$path %in% names(visited_paths)) {
                               next
                             }
                             
                             # Recursively find accounts in the child account
                             all_accounts <- c(all_accounts, child_account$list_all_accounts(visited_paths))
                           }
                           
                           # Recursively check parent accounts if they exist (go up the tree)
                           if (!is.null(self$parent) && !(self$parent$path %in% names(visited_paths))) {
                             all_accounts <- c(all_accounts, self$parent$list_all_accounts(visited_paths))
                           }
                           
                           # Return the list of all accounts collected
                           return(unlist(all_accounts))
                         },
                         compute_total_balance = function() {
                           total_balance <- self$balance  # Start with the account's own balance
                           
                           # Recursively sum balances of all child accounts
                           if (length(self$child_accounts) > 0) {
                             total_balance <- total_balance + sum(purrr::map_dbl(self$child_accounts, ~ .x$compute_total_balance()))
                           }
                           
                           return(total_balance)  # Return the total balance
                         },
                         
                         # Recursive method to compute the total due for the account and its children
                         compute_total_due = function() {
                           # Start with the current account's due amount (if it exists), otherwise set to 0
                           total_due <- ifelse(!is.null(self$amount_due), self$amount_due, 0)
                           
                           # Recursively add the due amounts from all child accounts
                           for (child_account in self$child_accounts) {
                             total_due <- total_due + child_account$compute_total_due()
                           }
                           
                           return(total_due)
                         },
                         compute_total_due_within_n_days = function(n) {
                           # Initialize the total due amount to 0
                           total_due <- 0
                           
                           # Check if the current account has an amount_due and a valid due_date
                           if (!is.null(self$amount_due)) {
                             if (!is.null(self$due_date)) {
                               # Calculate the difference between the current date and the due_date
                               days_until_due <- as.numeric(difftime(self$due_date, Sys.Date(), units = "days"))
                               
                               # Add to the total due only if the due_date is within the next 'n' days
                               if (days_until_due >= 0 && days_until_due <= n) {
                                 total_due <- total_due + self$amount_due
                               }
                             } else {
                               # If there's no due_date, treat the amount_due as 0
                               total_due <- total_due + 0
                             }
                           }
                           
                           # Recursively add the amounts due from all child accounts
                           for (child_account in self$child_accounts) {
                             total_due <- total_due + child_account$compute_total_due_within_n_days(n)
                           }
                           
                           return(total_due)
                         }
                         
                         
                       )
)

# ChildAccount Class (Needs, Goals, Debt Repayment)
ChildAccount <- R6Class("ChildAccount",
                        inherit = MainAccount,
                        public = list(
                          allocation = 0, # Allocation percentage (e.g., 0.5 for 50%)
                          status = "active", # Account status
                          parent=NULL,
                          path=NULL,
                          priority = 0,           # Priority level (higher value means higher priority)
                          # Constructor
                          initialize = function(name, allocation = 0, status = "active",parent=NULL,path=NULL,priority=0) {
                            super$initialize(name) # Call parent constructor
                            self$allocation <- allocation
                            self$status <- status
                            self$path<-path
                            self$priority <- priority
                          },
                          
                          # Override Deposit Method (Transfer funds to child account)
                          deposit = function(amount, transaction_number = NULL,By="User", channel = NULL, date = Sys.Date()) {
                            if (self$status == "active") {
                              if (is.null(transaction_number)) {
                                transaction_number <- self$generate_transaction_id()
                              } else {
                                transaction_number <- transaction_number
                              }
                              
                              self$balance <- self$balance + amount
                              self$transactions <- rbind(self$transactions, data.frame(
                                Type = "Deposit",
                                By=By,
                                TransactionID = transaction_number,
                                Channel = channel,
                                Amount = amount,
                                Balance=self$balance,
                                Date = as.Date(date),
                                stringsAsFactors = FALSE
                              ))
                              
                              # Distribute funds to child accounts and reset balance
                              self$distribute_to_children(amount,transaction_number)
                              
                              cat("Deposited:", amount, "via", channel, "- Transaction ID:", transaction_number, "\n")
                            } else {
                              cat("Deposit not allowed. Account is not active.\n")
                            }
                          },
                          
                          # Change account status
                          change_status = function(status) {
                            if (self$status == "closed") {
                              print("This account is already closed.")
                            } else if (status == "closed") {
                              if (self$balance > 0) {
                                stop("Withdraw from this account or move the balance before closure.")
                              } else {
                                self$status <- "closed"
                                cat(self$name, "has been closed.\n")
                              }
                            } else {
                              self$status <- status
                              cat(self$name, paste("has become", status, ".\n"))
                            }
                          },
                          
                          # Get account status
                          get_account_status = function() {
                            cat(self$name, "is", self$status, "\n")
                            return(self$status)
                          },
                          # get the priority of the account
                          get_priority = function() {
                            return(self$priority)
                          },
                          # Set the priority of the account
                          set_priority = function(priority) {
                            self$priority <- priority
                            cat("Priority for", self$name, "set to", priority, "\n")
                          }
                        )
)

GrandchildAccount <- R6Class(
  "GrandchildAccount",
  inherit = ChildAccount,
  public = list(
    status = "active",      # Account status (active/inactive)
    due_date = NULL,        # Due date for periodic accounts (e.g., rent)
    amount_due = 0,         # Amount due for accounts with fixed amounts (e.g., bills, debts)
    fixed_amount = 0,       # Fixed amount for accounts like Rent, Debt, Fixed Savings
    account_type = NULL,    # Type of account (Bill, Debt, Expense, FixedSaving, NonFixedSaving)
    freq = NULL,            # Frequency of bill recurrence (days, monthly, etc.)
    num_periods = 1,        # Number of periods for unpaid bills
    
    # Constructor
    initialize = function(name, allocation = 0, priority = 0, fixed_amount = 0, due_date = NULL, account_type = "Expense", freq = NULL) {
      super$initialize(name, allocation)
      self$fixed_amount <- fixed_amount
      self$due_date <- due_date
      self$amount_due <- fixed_amount
      self$account_type <- account_type
      self$freq <- freq
      self$num_periods <- 1
    },
    
    # Getter for due_date
    get_due_date = function() {
      return(self$due_date)
    },
    
    # Setter for due_date
    set_due_date = function(due_date) {
      self$due_date <- due_date
      cat("Due date for", self$name, "set to", due_date, "\n")
    },
    
    # Getter for fixed_amount
    get_fixed_amount = function() {
      return(self$fixed_amount)
    },
    
    # Setter for fixed_amount
    set_fixed_amount = function(fixed_amount) {
      self$fixed_amount <- fixed_amount
      self$amount_due <- fixed_amount * self$num_periods - self$balance
      cat("Fixed amount for", self$name, "set to", fixed_amount, "\n")
    },
    
    # Getter for account_type
    get_account_type = function() {
      return(self$account_type)
    },
    
    # Setter for account_type
    set_account_type = function(account_type) {
      self$account_type <- account_type
      cat("Account type for", self$name, "set to", account_type, "\n")
    },
    
    # Deposit method with period handling
    deposit = function(amount, transaction_number = NULL,By="User", channel = NULL, date = Sys.Date()) {
      # Check if bill is overdue and update periods
      if (self$account_type == "Bill" & !is.null(self$due_date)) {
        if(date > self$due_date){
        # Extend due date by frequency and increment periods
        self$due_date <- self$due_date + self$freq
        self$num_periods <- self$num_periods + 1
        cat("Due date extended. Number of periods unpaid:", self$num_periods, "\n")
        self$status <- "active"
        cat(self$name, "reactivated. Outstanding balance due:", self$num_periods * self$fixed_amount - self$balance, "\n")
        }
      }
      
      # Handle fixed amount logic (Bill, Debt, FixedSaving)
      if(self$fixed_amount>0){
        if (amount >= (self$num_periods * self$fixed_amount - self$balance)) {
          deposit_amount <- self$num_periods * self$fixed_amount - self$balance
          super$deposit(amount=deposit_amount, transaction_number,By=By, channel, date)
          self$status <- "inactive"  # Fully funded for the period
          
          # Move any extra amount to the parent account
          extra_amount <- amount - deposit_amount
          if (extra_amount > 0) {
            parent_name<-self$parent$name  #get parent
            self$find_account(parent_name)$deposit(amount=extra_amount,By="System", channel = "Returned Extra Allocation")
            cat("Extra amount of", extra_amount, "moved to", self$parent$name, "\n")
          }
          self$amount_due <- 0  # No amount due
          cat(self$name, "fully funded for", self$num_periods, "period(s)\n")
          return()
        }
      }
      
      # Regular deposit if no overflow
      # you might think this double deposits but its not 
      super$deposit(amount=amount, transaction_number=transaction_number,By=By, channel=channel, date=date)
      if(self$fixed_amount>0){
      self$amount_due <- self$num_periods * self$fixed_amount - self$balance
      }
    },
    
    # Withdrawal method with borrowing logic
    withdraw = function(amount,transaction_number = NULL,By="User", channel = NULL, date = Sys.Date()) {
      if(amount>0){
      if (amount > self$balance) {
        cat("Insufficient balance in", self$name, "to withdraw", amount, "\n")
        return(NULL)
      }
      super$withdraw(amount=amount,By=By,channel = "Internal Transfers")
      
      # for bills and fixed savings withdrawal reduce balance we need to compensate
      # for istance with a monthly rent of $75000 if you withdraw 35000 the rent remains 40000
      #but fixed amount reads 75000, instead of changing fixed amount we can adjust the period 
      
      if(self$fixed_amount>0){
        self$num_periods<-self$num_periods-(amount/self$fixed_amount)
      }
    }
      },
    
    # Getter and Setter for frequency and periods
    get_account_freq = function() {
      return(self$freq)
    },
    
    set_account_freq = function(account_freq) {
      self$freq <- account_freq
      cat("Frequency for", self$name, "set to", account_freq, "\n")
    },
    
    get_account_periods = function() {
      return(self$num_periods)
    },
    
    set_account_periods = function(periods) {
      self$num_periods <- periods
      cat(self$name, "has", periods, "period(s)\n")
    }
  )
)

