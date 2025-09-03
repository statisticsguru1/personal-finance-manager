library(R6)
library(tidyverse)
library(uuid)
# ==============================================================================
# MainAccount Account
# ==============================================================================
#' @title MainAccount Class
#'
#' @description
#' The `MainAccount` R6 class represents the top-level virtual account in a
#' hierarchical financial structure. It receives all income and is responsible
#' for distributing funds to its child accounts based on allocation rules.
#'
#' This class is the core of the budgeting engine and manages:
#' \itemize{
#'   \item Income reception and logging.
#'   \item Transaction tracking via a structured data frame.
#'   \item A list of linked child accounts.
#'   \item Auto-generated UUID for identification.
#'   \item A balance that reflects both manual and system transactions.
#' }
#'
#' @details
#' The account includes the following core attributes:
#' \describe{
#'   \item{uuid}{A unique identifier for the account, auto-generated.}
#'   \item{name}{Human-readable name for the account.}
#'   \item{balance}{Current balance in the account.}
#'   \item{transactions}{A `data.frame` tracking transaction logs, including
#'   custom system transactions.}
#'   \item{transaction_counter}{Used internally to create unique transaction
#'   IDs.}
#'   \item{child_accounts}{A list containing attached child accounts.}
#'   \item{total_allocation}{Tracks the total allocation distributed to
#'   children.}
#'   \item{path}{Logical traversal path from the root (used to locate accounts
#'   hierarchically).}
#' }
#'
#' @section Hierarchy:
#' This is the top-level class in a hierarchy that includes:
#' \itemize{
#'   \item \strong{MainAccount}: Root of the virtual budget system.
#'   \item \strong{ChildAccount}: Draws a portion of income from the
#'   MainAccount.
#'   \item \strong{GrandChildAccount}: Represents specific budget goals
#'   (e.g., rent, savings) and has due dates, frequency, and priority logic.
#' }
#'
#' @param name Character. A name for the account.
#' @param balance Numeric. Initial balance for the account.
#'
#' @field uuid Auto-generated unique identifier.
#' @field name Account name.
#' @field balance Current balance.
#' @field transactions A `data.frame` of transaction logs.
#' @field transaction_counter Counter used to generate unique transaction IDs.
#' @field child_accounts List of child accounts.
#' @field total_allocation Numeric. Sum of allocated funds to children.
#' @field path Character vector representing the account's hierarchy path.
#'
#' # some imports
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom tidyverse tidyverse_packages
#' @importFrom lubridate POSIXct
#'
#' @examples
#' library(R6)
#' library(uuid)
#' library(tidyverse)
#' # Create a main account
#'
#' acc <- MainAccount$new(name = "Salary Pool", balance = 1000)
#'
#' # Generate a system transaction ID
#' acc$generate_transaction_id()
#'
#' # Check for duplicate transaction ID
#' acc$is_duplicate_transaction("sys1")
#'
#' # Inspect balance
#' acc$balance
#'
#' # Inspect UUID
#' acc$uuid
#'
#' @export
MainAccount <- R6Class(
  "MainAccount",
  public = list(
    uuid = NULL,
    name = NULL,
    balance = 0,
    transactions = NULL,
    transaction_counter = 1, # Counter for system-generated transaction numbers
    child_accounts = list(), # List of child accounts
    total_allocation = 0, # Total allocation across child accounts
    path = "main_account",

    # --------- Main class constructor ----------------------------------------
    #' @description
    #' Constructor for the `MainAccount` class. Initializes a new main account
    #' with a unique identifier, user-defined name, zero balance, and an empty
    #' transaction data frame. Also sets the default account path to
    #' "main_account".
    #'
    #' @param name Character. The name of the main account. Used for
    #' identification in the app UI and reports.
    #' @examples
    #' \dontrun{
    #' main_acc <- MainAccount$new(name = "My Main Account")
    #' print(main_acc$uuid)
    #' print(main_acc$balance)
    #' print(main_acc$transactions)
    #'}
    initialize = function(name, balance = 0) {
      self$uuid <- paste0("acc", uuid::UUIDgenerate())
      self$name <- name
      self$balance <- balance
      self$path <- "main_account"
      self$transactions <- data.frame(
        Type = character(),
        By = character(),
        TransactionID = character(),
        Channel = character(),
        Amount = numeric(),
        Balance = numeric(),
        amount_due = numeric(),
        overall_balance = numeric(),
        Date = POSIXct(),
        stringsAsFactors = FALSE
      )
    },

    #---- Generate Transaction ID------------------------------------------
    #' @description
    #' Generates a unique system transaction ID by appending an incrementing
    #' counter to a fixed prefix ("sys"). The counter is then incremented for
    #' future calls.
    #'
    #' @return A character string representing the generated transaction ID.
    #'
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new(name = "Salary Pool")
    #'   txn_id1 <- main_acc$generate_transaction_id()
    #'   txn_id2 <- main_acc$generate_transaction_id()
    #'   print(txn_id1)  # e.g., "sys1"
    #'   print(txn_id2)  # e.g., "sys2"
    #' }
    generate_transaction_id = function() {
      transaction_id <- paste0("sys", self$transaction_counter)
      self$transaction_counter <- self$transaction_counter + 1
      transaction_id
    },

    # -------Check for Duplicate Transaction Number---------------------------
    #' @description
    #' Checks if a given transaction number already exists in the transaction
    #' log. This is used to prevent duplicate transaction entries.
    #'
    #' @param transaction_number Character. The transaction ID to be checked.
    #'
    #' @return Logical. \code{TRUE} if the transaction number exists,
    #' \code{FALSE} otherwise.
    #'
    #' @examples
    #' \dontrun{
    #'   # Create a new main account
    #'   main_acc <- MainAccount$new(name = "Salary Pool")
    #'
    #'   # Manually add a transaction with ID "sys1"
    #'   main_acc$transactions <- data.frame(
    #'     Type = "Income",
    #'     By = "User",
    #'     TransactionID = "sys1",
    #'     Channel = "Bank",
    #'     Amount = 5000,
    #'     Balance = 5000,
    #'     amount_due = 0,
    #'     overall_balance = 5000,
    #'     Date = Sys.time(),
    #'     stringsAsFactors = FALSE
    #'   )
    #'
    #'   # Check for duplicate
    #'   main_acc$is_duplicate_transaction("sys1")  # Returns TRUE
    #'   main_acc$is_duplicate_transaction("sys2")  # Returns FALSE
    #' }
    is_duplicate_transaction = function(transaction_number) {
      if (is.null(transaction_number) || length(transaction_number) != 1) {
        return(FALSE)
      }
      transaction_number %in% self$transactions$TransactionID
    },
    #-----------------Deposit Method-------------------------------------------
    #' @description
    #' Deposits a specified amount into the account and distributes it
    #' to child accounts based on allocation rules. This method also records
    #' the transaction in the internal ledger.
    #'
    #' @param amount Numeric. The amount of money to deposit. Must be greater
    #' than zero.
    #' @param transaction_number Optional character. A unique identifier for
    #' this transaction. If not provided, the system generates one
    #' automatically.
    #' @param by Character. Identifier of the depositor (default is `"User"`).
    #' @param channel Character. The source of funds
    #' (e.g., `"ABSA"`, `"MPESA"`). Required.
    #' @param date POSIXct or character. The timestamp of the transaction
    #' (defaults to current time).
    #'
    #' @return No return value. The method updates the account balance,
    #' transaction log, and distributes funds to child accounts.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new(name = "Salary Pool")
    #'   main_acc$deposit(
    #'     amount = 1000,
    #'     channel = "Bank Transfer"
    #'   )
    #' }
    #'
    deposit = function(
      amount,
      transaction_number = NULL,
      by = "User",
      channel = NULL,
      date = Sys.time()
    ) {
      if (amount <= 0) stop("Deposit amount must be greater than zero!")
      if (is.null(channel)) stop("Channel is required for deposits!")
      if (is.null(transaction_number)) {
        transaction_number <- self$generate_transaction_id()
      } else {
        transaction_number <- transaction_number
      }
      if (self$is_duplicate_transaction(transaction_number)) {
        stop(
          "This deposit has already been made. Transaction ID:",
          transaction_number
        )
      }
      self$balance <- self$balance + amount
      balance <- self$balance
      amount_due <- self$compute_total_due()
      overall_balance <- self$compute_total_balance()
      date <- as.POSIXct(date)
      self$transactions <- rbind(
        self$transactions,
        data.frame(
          Type = "Deposit",
          By = by,
          TransactionID = transaction_number,
          Channel = channel,
          Amount = as.numeric(amount),
          Balance = as.numeric(balance),
          amount_due = as.numeric(amount_due),
          overall_balance = as.numeric(overall_balance),
          Date = as.POSIXct(date),
          stringsAsFactors = FALSE
        )
      )

      # Distribute funds to child accounts and reset balance
      self$distribute_to_children(amount, transaction_number)
      cat(
        "Deposited:", amount, "via", channel, "- Transaction ID:",
        transaction_number, "\n"
      )
    },

    #------------ Distribute Funds to Child Accounts----------------------------
    #' @description
    #' Distributes a given amount from the an account to active child accounts
    #' based on their allocation weights and priorities. If the amount is too
    #' small (less than 0.10), it is routed entirely to the highest-priority
    #' child.
    #'
    #' This method is automatically called after a deposit into a parent
    #' account. It performs internal withdrawals and instructs child accounts
    #' to deposit their corresponding shares.
    #'
    #' @param amount Numeric. Total amount available for distribution.
    #' @param transaction Character. The transaction ID associated with the
    #' distribution.
    #' @param by Character. Identifier of the actor performing the transfer
    #' (default is `"System"`).
    #'
    #' @return No return value. This method updates balances and transaction
    #' logs in both the main account and all affected child accounts.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new(name = "Salary Pool")
    #'   child1 <- ChildAccount$new(name = "Food Fund", allocation = 0.6)
    #'   child2 <- ChildAccount$new(name = "Savings", allocation = 0.4)
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   main_acc$deposit(amount = 1000, channel = "Bank")
    #'   # This will trigger distribute_to_children internally.
    #' }
    #'
    distribute_to_children = function(amount, transaction, by = "System") {

      if (length(self$child_accounts) == 0) {
        return()
      }

      # Filter active child accounts
      active_accounts <- purrr::keep(
        self$child_accounts, ~ .x$status == "active"
      )

      if (length(active_accounts) == 0) {
        message("No active child accounts available.")
        return()
      }

      # Check if the amount is too small to distribute
      if (amount < 0.10) {
        message(
          paste0(
            "Amount too small to distribute. ",
            "Depositing into the highest-priority child."
          )
        )
        highest_priority_child <- active_accounts[[
          which.max(purrr::map_dbl(active_accounts, "priority"))
        ]]

        self$withdraw(
          amount = amount,
          by = by,
          channel = paste("Allocation to", highest_priority_child$name)
        )
        highest_priority_child$deposit(
          amount, transaction,
          by = by,
          channel = paste("Allocation from", self$name)
        )
        return()
      }

      # Calculate total active allocation for distribution
      total_active_allocation <- sum(
        purrr::map_dbl(active_accounts, "allocation")
      )

      # Distribute funds to active children
      for (
        child_account in active_accounts[
          order(
            -purrr::map_dbl(active_accounts, "priority")
          )
        ]
      ) {
        allocation <- (
          child_account$allocation / total_active_allocation
        ) * amount

        self$withdraw(
          amount = allocation,
          by = by,
          channel = paste("Allocation to", child_account$name)
        )
        child_account$deposit(
          allocation,
          transaction,
          by = by,
          channel = paste("Allocation from", self$name)
        )
      }
    },

    # ---------------Add Child Account----------------------------------------
    #' @description
    #' Adds a `ChildAccount` object to the list of child accounts of the
    #' account.
    #' It checks for valid allocation percentages (must not exceed 100%), sets
    #' hierarchical path references, and updates total allocation.
    #'
    #' @param child_account An object of class `ChildAccount`or
    #' `GrandChildAccount`, representing the account to be added as a
    #' subordinate of the current main account.
    #'
    #' @details
    #' If the child's allocation is zero, it will be automatically marked as
    #' inactive. The method also updates the logical `path` of the child and
    #' attaches a `parent` reference to maintain the hierarchy.
    #'
    #' @return None. This method modifies the object in-place.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new(name = "Master")
    #'   child_acc <- ChildAccount$new(name = "Savings", allocation = 0.4)
    #'   main_acc$add_child_account(child_acc)
    #'}
    add_child_account = function(child_account) {
      child_account$parent <- self  #add parent
      child_account$path <- paste0(
        self$path,
        "$child_accounts$",
        "`",
        child_account$name,
        "`"
      )

      if ((self$total_allocation + child_account$allocation) > 1) {
        stop("Total allocation exceeds 100%. Please adjust the allocations.")
      }

      if (child_account$allocation == 0) {
        # With allocation of 0 deactivate
        child_account$status <- "inactive"
      }
      self$child_accounts[[child_account$name]] <- child_account
      self$total_allocation <- self$child_accounts %>%
        purrr::map("allocation") %>%
        unlist() %>%
        sum(na.rm = TRUE)
    },

    # --------------- Set Allocation for Child Account--------------------------
    #' @description
    #' Updates the allocation percentage for a specified child account.
    #' Ensures that the total allocation across all children does not exceed
    #'  100% (1.0).
    #'
    #' @param child_account_name Character. The name of the child account
    #' whose allocation is to be updated.
    #' @param new_allocation Numeric. The new allocation proportion
    #' (between 0 and 1).
    #'
    #' @details
    #' If the allocation is set to zero, the child account is marked as
    #' inactive. The function automatically updates the total allocation
    #' tracker.
    #'
    #' @return None. This method modifies the object in-place.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child <- ChildAccount$new(name = "Emergency", allocation = 0.2)
    #'   main_acc$add_child_account(child)
    #'   main_acc$set_child_allocation("Emergency", 0.3)
    #' }
    set_child_allocation = function(child_account_name, new_allocation) {
      if(!(child_account_name %in% self$list_child_accounts())){
        stop(paste(child_account_name, "is not a child of",self$name))
      }
      if (new_allocation < 0 || new_allocation > 1) {
        stop("Allocation must be between 0 and 1.")
      }
      current_allocation <- self$child_accounts[[child_account_name]]$allocation
      allocation_difference <- new_allocation - current_allocation

      if ((self$total_allocation + allocation_difference) > 1) {
        stop("Total allocation exceeds 100%. Please adjust the allocations.")
      }

      # set new allocation
      self$child_accounts[[child_account_name]]$allocation <- new_allocation
      # With allocation of 0 deactivate
      if (self$child_accounts[[child_account_name]]$allocation == 0) {
        self$child_accounts[[child_account_name]]$status <- "inactive"
      }

      self$total_allocation <- self$child_accounts %>%
        purrr::map("allocation") %>%
        unlist() %>%
        sum(na.rm = TRUE)
    },

    #--------------- Withdraw Method ------------------------------------------
    #' @description
    #' Withdraws a specified amount from the account and logs the
    #' transaction. A transaction number is generated automatically if not
    #'  provided. Withdrawals require a valid channel and sufficient balance.
    #'
    #' @param amount Numeric. The amount to withdraw. Must be greater than
    #' zero and
    #' #' not exceed the current balance.
    #' @param transaction_number Optional character. Custom transaction ID.
    #' If NULL,
    #' a system-generated ID will be used.
    #' @param by Character. The entity initiating the withdrawal. Default is
    #' `"User"`.
    #' @param channel Character. The withdrawal channel (e.g., "Bank Transfer").
    #' Required.
    #' @param date POSIXct. Timestamp for the transaction. Defaults to
    #' `Sys.time()`.
    #'
    #' @return None. Modifies the object's internal state by reducing the
    #' balance and appending a new transaction to the log.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Primary Pool")
    #'   main_acc$deposit(amount = 1000, channel = "Mobile", by = "User")
    #'   main_acc$withdraw(amount = 200, channel = "ATM", by = "User")
    #' }
    withdraw = function(
      amount,
      transaction_number = NULL,
      by = "User",
      channel = NULL,
      date = Sys.time()
    ) {
      if (amount > 0) {
        if (self$balance < amount) {
          stop("Insufficient balance! Your current balance is:", self$balance)
        }

        if (is.null(channel)) stop("Channel is required for withdrawals!")

        if (is.null(transaction_number)) {
          transaction_number <- self$generate_transaction_id()
        } else {
          transaction_number <- transaction_number
        }

        self$balance <- self$balance - amount
        balance <- self$balance
        amount_due <- self$compute_total_due()
        overall_balance <- self$compute_total_balance()
        date <- as.POSIXct(date)
        self$transactions <- rbind(
          self$transactions,
          data.frame(
            Type = "Withdrawal",
            By = by,
            TransactionID = transaction_number,
            Channel = channel,
            Amount = amount,
            Balance = balance,
            amount_due = amount_due,
            overall_balance = overall_balance,
            Date = date,
            stringsAsFactors = FALSE
          )
        )

        cat(
          "Withdrew:",
          amount,
          "via",
          channel,
          "- Transaction ID:",
          transaction_number,
          "\n"
        )
      }
    },

    # ---------------------Check Balance Method---------------------------------
    #' @description
    #' Returns the current balance of the account and prints it to the
    #' console.
    #'
    #' @return Numeric. The current account balance.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Primary Pool")
    #'   main_acc$deposit(amount = 500, channel = "Bank Transfer")
    #'   main_acc$get_balance()
    #' }
    get_balance = function() {
      cat("Current Balance:", self$balance, "\n")
      self$balance
    },

    # -------View Transactions (Print + Return as Data Frame)-------------------
    #' @description
    #' Retrieves and displays the transaction history for the account.
    #' If no transactions are found, a message is printed. Otherwise, the
    #' transaction log is displayed in the console.
    #'
    #' This method is inherited by child and grandchild account classes.
    #'
    #' @return
    #' A data frame containing the account's transaction history.
    #'
    #' @examples
    #' \dontrun{
    #'   acc <- MainAccount$new("Main Budget")
    #'   acc$deposit(500, channel = "M-Pesa")
    #'   acc$get_transactions()
    #' }
    get_transactions = function() {
      if (nrow(self$transactions) == 0) {
        cat("No transactions found.\n")
      } else {
        cat("\nTransaction History:\n")
        print(self$transactions)
      }
      self$transactions
    },

    #----------------- List child accounts--------------------------------------
    #' @description
    #' Lists all direct child accounts attached to this account.
    #' If no child accounts are found, a message is printed.
    #'
    #' This method is inherited by both `ChildAccount` and `GrandChildAccount`,
    #' allowing recursive visibility into nested account structures.
    #'
    #' @return
    #' Invisibly returns a character vector of child account names.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main Budget")
    #'   child <- ChildAccount$new("Bills", allocation = 0.5)
    #'   main_acc$add_child_account(child)
    #'   main_acc$list_child_accounts()
    #' }
    list_child_accounts = function() {
      if (length(self$child_accounts) == 0) {
        cat("No child accounts found.\n")
        invisible(character())
      } else {
        cat("\nChild Accounts of", self$name, ":\n")
        names <- character()
        for (child_account in self$child_accounts) {
          cat("-", child_account$name, "\n")
          names <- c(names, child_account$name)
        }
        invisible(names)
      }
    },

    #------------- Method to find an account by name ------------------------
    #' # Recursively find all accounts by name
    #'
    #' @description
    #' Recursively searches the current account, its children, and its parent
    #' chain to collect **all** accounts with a given name. This version differs
    #'from the original by not stopping at the first match—it returns a list of
    #'**all** matches instead.
    #'
    #' It avoids infinite recursion by tracking visited account paths.
    #'
    #' @param target_name Character. The name of the account(s) to locate.
    #' @param visited_paths (Internal use only) Tracks visited paths to avoid
    #' cycles.
    #' @param matches (Internal use only) A list to accumulate matches during
    #' recursion.
    #'
    #' @return
    #' A list of account objects matching the given name. If no matches are
    #' found, returns an empty list.
    #'
    #' @examples
    #' \dontrun{
    #'   main <- MainAccount$new("Main")
    #'   savings1 <- ChildAccount$new("Savings", allocation = 0.5)
    #'   savings2 <- ChildAccount$new("Savings", allocation = 0.3)
    #'   main$add_child_account(savings1)
    #'   main$add_child_account(savings2)
    #'   found <- main$find_account("Savings")
    #'   length(found)  # 2
    #'   found[[1]]$uuid
    #' }
    find_account = function(target_name, visited_paths = NULL, matches = NULL) {
      # Initialize visited paths and match list
      if (is.null(visited_paths)) {
        visited_paths <- list()
      }
      if (is.null(matches)) {
        matches <- list()
      }

      # Mark current path as visited
      visited_paths[[self$path]] <- TRUE

      # Check current node
      if (self$name == target_name) {
        matches[[length(matches) + 1]] <- self
      }

      # Recursively search children
      for (child in self$child_accounts) {
        if (!(child$path %in% names(visited_paths))) {
          matches <- child$find_account(
            target_name,
            visited_paths = visited_paths,
            matches = matches
          )
        }
      }

      # Recurse up to parent if not visited
      if (
        !is.null(self$parent) && !(self$parent$path %in% names(visited_paths))
      ) {
        matches <- self$parent$find_account(
          target_name,
          visited_paths = visited_paths,
          matches = matches
        )
      }

      return(matches)
    },
    #------------- Method to find an account by UUID ------------------------
    #' @description
    #' Recursively searches for an account by its unique UUID. The method
    #' traverses downward through all child accounts and upward to the parent,
    #' if needed, while preventing circular recursion by tracking visited paths.
    #' This is especially useful when account names are not unique but UUIDs
    #' are.
    #'
    #' @param target_uuid Character. The UUID of the account to find.
    #' @param visited_paths Internal use only. A list used to track visited
    #' paths and prevent infinite loops in cyclic or nested account structures.
    #'
    #' @return
    #' Returns the account object whose UUID matches the target, or \code{NULL}
    #' if no match is found.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Root")
    #'   groceries <- ChildAccount$new("Groceries", allocation = 0.3)
    #'   main_acc$add_child_account(groceries)
    #'   found <- main_acc$find_account_by_uuid(groceries$uuid)
    #'   if (!is.null(found)) cat("Found UUID:", found$uuid)
    #' }
    #' @seealso [remove_account()]
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

      # If not found in current node or its children,
      # move up to the parent (if exists)
      if (
        !is.null(self$parent) && !(self$parent$path %in% names(visited_paths))
      ) {
        return(self$parent$find_account_by_uuid(target_uuid, visited_paths))
      }
      # If the account is not found in the entire tree, return NULL
      return(NULL)
    },
    #------------- Method to Move amount  ------------------------
    #' # Move balance to another account (by UUID)
    #'
    #' @description
    #' Moves a specified amount from the current account to another account
    #' identified by its **UUID**. This is intended for internal transfers
    #' within the account tree. It reuses `withdraw()` and `deposit()` logic
    #' and logs the transfer using the "Internal Transfer" channel.
    #'
    #' The target account is resolved using `find_account_by_uuid()`,
    #' not by name. This ensures unambiguous targeting even when accounts
    #' share the same name.
    #'
    #' @param target_account_uuid Character. The UUID of the account to which
    #' the funds will be moved.
    #' @param amount Numeric. The amount to transfer. Must be less than or equal
    #' to the current account's balance.
    #'
    #' @return
    #' No return value. Side effects include balance updates and transaction
    #' logs for both the source and target accounts.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   savings <- ChildAccount$new("Savings", allocation = 0.5)
    #'   emergency <- ChildAccount$new("Emergency", allocation = 0.5)
    #'   main_acc$add_child_account(savings)
    #'   main_acc$add_child_account(emergency)
    #'
    #'   # Initial deposit
    #'   main_acc$deposit(1000, channel = "Bank")
    #'
    #'   # Move 200 to savings using UUID
    #'   main_acc$move_balance(savings$uuid, 200)
    #' }
    move_balance = function(target_account_uuid, amount) {
      target_account <- self$find_account_by_uuid(target_account_uuid)
      if (is.null(target_account)) {
        stop("Target account not found")
      }
      self$withdraw(
        amount = amount,
        by = "System",
        channel = "Internal Transfer"
      )

      target_account$deposit(
        amount = amount,
        by = "System",
        channel = "Internal Transfer"
      )

      cat("Moved", amount, "from", self$name, "to", target_account$name, "\n")
    },

    #------------- List all accounts ------------------------
    #' @description
    #' Recursively lists the names of all accounts in the hierarchy, both upward
    #' (towards ancestors) and downward (towards descendants), starting from the
    #' current account. This method avoids revisiting any account by tracking
    #' visited paths, preventing infinite loops in case of circular references.
    #'
    #' @param visited_paths Optional list. Used internally for recursion to
    #' avoid revisiting accounts. Should generally be left as `NULL` by the
    #' user.
    #'
    #' @return
    #' A character vector of account names found across the full reachable tree
    #' (both children and ancestors) from the current account.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   savings <- ChildAccount$new("Savings", allocation = 0.4)
    #'   emergency <- ChildAccount$new("Emergency", allocation = 0.6)
    #'
    #'   main_acc$add_child_account(savings)
    #'   main_acc$add_child_account(emergency)
    #'
    #'   # List all accounts from the root
    #'   main_acc$list_all_accounts()
    #'   # Output: "Main" "Savings" "Emergency"
    #'
    #'   # List all accounts from a child node (will include parents)
    #'   savings$list_all_accounts()
    #'   # Output: "Savings" "Main" "Emergency"
    #' }

    list_all_accounts = function(visited_paths = NULL) {
      # Initialize visited_paths if it's NULL
      #(this tracks visited paths to avoid revisiting)
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
        all_accounts <- c(
          all_accounts,
          child_account$list_all_accounts(visited_paths)
        )
      }

      # Recursively check parent accounts if they exist (go up the tree)
      if (
        !is.null(self$parent) && !(self$parent$path %in% names(visited_paths))
      ) {
        all_accounts <- c(
          all_accounts,
          self$parent$list_all_accounts(visited_paths)
        )
      }

      # Return the list of all accounts collected
      return(unlist(all_accounts))
    },

    # ------------ Compute total balance ---------------------------------------
    #' @description
    #' Recursively computes the total balance held by the current account
    #' and all of its child accounts. This method includes the balance of the
    #' account on which it's called and traverses down the tree to sum balances
    #' of all active descendants.
    #'
    #' @return
    #' Numeric. The total aggregated balance of this account and its entire
    #' descendant subtree.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.5)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.5)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   main_acc$deposit(100, channel = "Bank", transaction_number = "txn1")
    #'   # This distributes 100 into child1 and child2 based on allocation
    #'
    #'   # Check total balance recursively
    #'   total <- main_acc$compute_total_balance()
    #'   print(total)
    #'   # Should return 100 (main + children)
    #' }
    compute_total_balance = function() {
      total_balance <- self$balance  # Start with the account's own balance
      # Recursively sum balances of all child accounts
      if (length(self$child_accounts) > 0) {
        total_balance <- total_balance + sum(
          purrr::map_dbl(self$child_accounts, ~ .x$compute_total_balance())
        )
      }
      total_balance  # Return the total balance
    },
    # ------------- Compute total due --------------------------------------
    # Recursive method to compute the total due for the account and its children
    #' @description
    #' Recursively computes the total amount due for the current account and all
    #' of its descendant child accounts. If `amount_due` is not defined in an
    #' account, it defaults to zero.
    #'
    #' This is useful for aggregating outstanding dues across the entire
    #' hierarchical account structure (e.g., main → child → grandchild).
    #'
    #' @return
    #' Numeric. The total due amount for this account and all its descendants.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.6)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.4)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   # Manually set dues
    #'   main_acc$amount_due <- 50
    #'   child1$amount_due <- 20
    #'   child2$amount_due <- 30
    #'
    #'   total_due <- main_acc$compute_total_due()
    #'   print(total_due)
    #'   # Should return 100 (50 + 20 + 30)
    #' }
    compute_total_due = function() {
      # Start with the current account's due amount (if it exists),
      # otherwise set to 0
      total_due <- ifelse(!is.null(self$amount_due), self$amount_due, 0)

      # Recursively add the due amounts from all child accounts
      for (child_account in self$child_accounts) {
        total_due <- total_due + child_account$compute_total_due()
      }
      total_due
    },
    # ------------- Compute total due within n days ----------------------------
    #' @description
    #' Recursively computes the total amount due for the current account and all
    #' child accounts where the due date is within the next \code{n} days from
    #' the current system time.
    #'
    #' This method is useful for identifying upcoming payments or obligations in
    #' a multi-account hierarchy and prioritizing them based on urgency.
    #'
    #' @param n Integer. The number of days from today within which dues should
    #' be considered. Dues without a due date are ignored.
    #'
    #' @return Numeric. The total due amount within the next \code{n} days
    #' across the account hierarchy.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.6)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.4)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   # Assign dues and due dates
    #'   main_acc$amount_due <- 100
    #'   main_acc$due_date <- Sys.time() + 2 * 24 * 60 * 60  # Due in 2 days
    #'
    #'   child1$amount_due <- 50
    #'   child1$due_date <- Sys.time() + 5 * 24 * 60 * 60    # Due in 5 days
    #'
    #'   child2$amount_due <- 70
    #'   child2$due_date <- Sys.time() + 10 * 24 * 60 * 60   # Due in 10 days
    #'
    #'   # Compute total dues within next 7 days
    #'   total_due_7_days <- main_acc$compute_total_due_within_n_days(7)
    #'   print(total_due_7_days)
    #'   # Should return 100 + 50 = 150
    #' }
    compute_total_due_within_n_days = function(n) {
      # Initialize the total due amount to 0
      total_due <- 0
      # Check if the current account has an amount_due and a valid due_date
      if (!is.null(self$amount_due)) {
        if (!is.null(self$due_date)) {
          # Calculate the difference between the current date and the due_date
          days_until_due <- as.numeric(
            difftime(
              self$due_date, Sys.time(),
              units = "days"
            )
          )

          # Add to the total due only if the due_date is within
          # the next 'n' days
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
        total_due <- total_due +
          child_account$compute_total_due_within_n_days(n)
      }
      total_due
    },


    # ------------- Compute spending ----------------------------
    #' @description
    #' Recursively computes the total spending (i.e., user-initiated
    #' withdrawals) for the current account and all child accounts within
    #'  a specified date range.
    #'
    #' Spending is defined as withdrawals made by the user (`By == "User"`),
    #' excluding system-initiated or internal transfers. The function includes
    #' both the current account and all of its descendants in the calculation.
    #'
    #' @param daterange A Date or POSIXct vector of length 2. The start and
    #' end dates for the period over which to compute spending. Defaults to
    #' the entire timeline.
    #'
    #' @return Numeric. The total spending amount over the specified date range.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.5)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.5)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   # Simulate some deposits and withdrawals
    #'   main_acc$deposit(500, "T1", By = "User", channel = "Cash")
    #'   main_acc$withdraw(200, By = "User", channel = "Spending",
    #'   date = Sys.time() - 10)
    #'   child1$deposit(300, "T2", By = "User", channel = "Mobile")
    #'   child1$withdraw(100, By = "User", channel = "Shopping",
    #'   date = Sys.time() - 5)
    #'
    #'   # Get total user spending in last 30 days
    #'   main_acc$spending(c(Sys.Date() - 30, Sys.Date()))
    #'   # Should return 200 + 100 = 300
    #' }
    spending = function(
      daterange = c(Sys.Date() - 365000, Sys.Date())
    ) {
      daterange <- as.POSIXct(daterange)
      # Sum of deposits made by the user in the main account
      transactions <- self$transactions %>%
        filter(
          Type == "Withdrawal" & By == "User" & between(
            Date,
            daterange[1], (daterange[2] + hours(23) + minutes(59) + seconds(59))
          )
        ) %>%
        pull(Amount) %>%
        sum(na.rm = TRUE)

      # If there are child accounts, recursively accumulate their spending
      if (length(self$child_accounts) > 0) {
        for (child in self$child_accounts) {
          transactions <- transactions + child$spending(daterange)
        }
      }
      return(transactions)
    },

    #-------------------- Compute total income -------------------------------
    #' @description
    #' Recursively computes the total income (i.e., user-initiated deposits)
    #' for the current account and all of its child accounts within a specified
    #' date range.
    #'
    #' This function sums up all "Deposit" transactions where the `By` field is
    #' set to `"User"`. It includes both the current account and all of its
    #' descendants in the income calculation.
    #'
    #' @param daterange A vector of two Dates or POSIXct objects specifying the
    #' start and end dates for the income calculation. Defaults to the entire
    #' timeline.
    #'
    #' @return A numeric value representing the total income across all relevant
    #' accounts within the specified date range.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.5)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.5)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   # Simulate some deposits
    #'   main_acc$deposit(500, "TX01", By = "User", channel = "Cash",
    #'   date = Sys.time() - 7)
    #'   child1$deposit(300, "TX02", By = "User", channel = "Mobile",
    #'   date = Sys.time() - 3)
    #'
    #'   # Get total income in last 10 days
    #'   main_acc$total_income(c(Sys.Date() - 10, Sys.Date()))
    #'   # Should return 500 + 300 = 800
    #' }
    total_income = function(daterange = c(Sys.Date() - 365000, Sys.Date())) {
      daterange <- as.POSIXct(daterange)

      # Sum of deposits made by the user in the main account
      transactions <- self$transactions %>%
        filter(
          Type == "Deposit" & By == "User",
          between(
            Date,
            daterange[1], (daterange[2] + hours(23) + minutes(59) + seconds(59))
          )
        ) %>%
        pull(Amount) %>%
        sum(na.rm = TRUE)

      # If there are child accounts, recursively accumulate their income
      if (length(self$child_accounts) > 0) {
        for (child in self$child_accounts) {
          transactions <- transactions + child$total_income(daterange)
        }
      }
      return(transactions)
    },

    # ------------ Allocated amount -------------------------------------------
    #' @description
    #' Calculates the total allocated amount to this account and its child
    #' accounts over a specified date range.
    #'
    #' This includes **all deposits** (from both `"User"` and `"System"`)
    #' into the current account, **plus** all **user-initiated** deposits
    #' into child and deeper-level descendant accounts. It provides insight
    #' into how much funding (regardless of origin) has been allocated directly
    #'  or indirectly to this node in the account tree.

    #' @param daterange A vector of two `Date` or `POSIXct` objects specifying
    #' the start and end dates for the deposit aggregation. Defaults to a very
    #' wide range.
    #'
    #' @return A numeric value representing the total allocated amount across
    #' the account and its descendants within the specified date range.
    #'
    #' @examples
    #' \dontrun{
    #'   main_acc <- MainAccount$new("Main")
    #'   child1 <- ChildAccount$new("Child1", allocation = 0.5)
    #'   child2 <- ChildAccount$new("Child2", allocation = 0.5)
    #'
    #'   main_acc$add_child_account(child1)
    #'   main_acc$add_child_account(child2)
    #'
    #'   main_acc$deposit(1000, "TX001", By = "System", channel = "Bank",
    #'   date = Sys.time() - 5)
    #'   child1$deposit(300, "TX002", By = "User", channel = "Mobile",
    #'   date = Sys.time() - 3)
    #'   child2$deposit(200, "TX003", By = "User", channel = "Cash",
    #'   date = Sys.time() - 2)
    #'
    #'   # Get total allocated amount within last 7 days
    #'   main_acc$allocated_amount(c(Sys.Date() - 7, Sys.Date()))
    #'   # Expected output: 1000 (System) + 300 + 200 = 1500
    #' }
    allocated_amount = function(
      daterange = c(Sys.Date() - 365000, Sys.Date())
    ) {
      daterange <- as.POSIXct(daterange)

      # Step 1: Sum all deposits (User + System) for this account
      transactions <- if (!is.null(self$transactions)) {
        self$transactions %>%
          filter(
            Type == "Deposit" & between(
              Date, daterange[1],
              (daterange[2] + hours(23) + minutes(59) + seconds(59))
            )
          ) %>%
          pull(Amount) %>%
          sum(na.rm = TRUE)
      } else {
        0
      }
      # Step 2: Inner function to recursively sum user deposits in
      # child accounts
      user_deposits_sum <- function(account, daterange) {
        if (length(account$child_accounts) == 0) return(0) # Base case
        child_user_deposits <- sum(
          sapply(
            account$child_accounts,
            function(child) {
              child_deposits <- if (!is.null(child$transactions)) {
                child$transactions %>%
                  filter(
                    Type == "Deposit" &
                      (
                        By == "User" |
                          (By == "System" & grepl(
                            "Returned Extra Allocation-up",
                            Channel, ignore.case = TRUE
                          ))
                      ) &
                      between(
                        Date,
                        daterange[1],
                        daterange[2] + hours(23) + minutes(59) + seconds(59)
                      )
                  )%>%
                  pull(Amount)%>%
                  sum(na.rm = TRUE)
              } else {
                0
              }
              child_deposits + user_deposits_sum(child, daterange)

            }
          )
        )
        child_user_deposits
      }

      # Step 3: Add user deposits from children
      # (using the recursive function)
      transactions <- transactions + user_deposits_sum(self, daterange)
      return(transactions)
    },

    # ---- Income utilization-----------------------------------------------
    #' @description
    #' Computes the **income utilization ratio** over a specified date range.
    #'
    #' This is calculated as the ratio of total user withdrawals
    #' (`spending`) to the total allocated income (`allocated_amount`).
    #' It measures how effectively funds are being used relative to the total
    #'  amount deposited (both by user and system).
    #'
    #' A value close to 1 indicates high utilization (most funds spent),
    #' while a valueclose to 0 indicates low spending relative to funds
    #' received.
    #'
    #' @param daterange A vector of two `Date` or `POSIXct` values defining the
    #' date range for the calculation. Defaults to a large historical window.
    #'
    #' @return A numeric value (between 0 and potentially >1) representing
    #' the income utilization ratio. If no income has been allocated, a small
    #' epsilon is added to the denominator to avoid division by zero.
    #'
    #' @examples
    #' \dontrun{
    #'   account <- MainAccount$new("Parent")
    #'   account$deposit(1000, "TX001", By = "System", channel = "Bank")
    #'   account$withdraw(200, By = "User", channel = "Mobile")
    #'
    #'   account$income_utilization()
    #'   # Expected output: 200 / 1000 = 0.2
    #' }
    income_utilization = function(
      daterange = c(Sys.Date() - 365000, Sys.Date())
    ) {
      daterange <- as.POSIXct(daterange)
      self$spending(daterange) / ifelse(
        self$allocated_amount(daterange) ==  0,
        self$allocated_amount(daterange) + .Machine$double.eps,
        self$allocated_amount(daterange)
      )
    },

    # ------ Walking amount/ balance -----------------------------------------
    #' @description
    #' Computes the **latest recorded amount** (either `amount_due` or
    #' `balance`) from the `Track_dues_and_balance` tracker for the current
    #' account and all its child accounts within a specified date range.
    #'
    #' It retrieves the latest entry in the given date range, and
    #' recursively sums the values for all child accounts.
    #'
    #' @param amt_type A string specifying the metric to return:
    #'   - `"amount_due"`: Return the last tracked `Amount_due`.
    #'   - `"Balance"`: Return the last tracked `Balance`.
    #'
    #' @param daterange A vector of two dates (of class `Date` or
    #' `POSIXct`)
    #' specifying the time window. Default: wide historical range.
    #'
    #' @return A numeric value representing the sum of either latest
    #' `Amount_due` or `Balance` from this account and all its children.
    #'
    #' @examples
    #' \dontrun{
    #' account$walking_amount("amount_due", c(Sys.Date() - 30,
    #' Sys.Date()))
    #' account$walking_amount("Balance", c(Sys.Date() - 7, Sys.Date()))
    #' }
    walking_amount = function(
      amt_type = "amount_due",
      daterange = c(Sys.Date() - 365000, Sys.Date())
    ) {
      daterange <- as.POSIXct(daterange)
      if (amt_type == "amount_due") {
        if (!is.null(self$Track_dues_and_balance)) {
          amt <- self$Track_dues_and_balance %>%
            filter(
              between(
                Date,
                daterange[1],
                (daterange[2] + hours(23) + minutes(59) + seconds(59))
              )
            ) %>%
            tail(1) %>%
            pull(Amount_due)

          if (length(amt) == 0) {
            amt <- 0
          }
          transactions <- amt
        } else {
          transactions <- 0
        }

        if (length(self$child_accounts) > 0) {
          for (child in self$child_accounts) {
            transactions <- transactions + child$walking_amount(
              daterange = daterange
            )
          }
        }
        return(transactions)
      } else {
        if (!is.null(self$Track_dues_and_balance)) {
          amt <- self$Track_dues_and_balance %>%
            filter(
              between(
                Date,
                daterange[1],
                (daterange[2] + hours(23) + minutes(59) + seconds(59))
              )
            ) %>%
            tail(1) %>%
            pull(Balance)

          if (length(amt) == 0) {
            amt <- 0
          }

          transactions <-  amt
        } else {
          transactions <- 0
        }

        if (length(self$child_accounts) > 0) {
          for (child in self$child_accounts) {
            transactions <- transactions + child$walking_amount(
              amt_type = amt_type,
              daterange = daterange
            )
          }
        }
        return(transactions)
      }
    }
  )
)
