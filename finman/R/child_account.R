# ChildAccount Class (Needs, Goals, Debt Repayment)
library(R6)
library(tidyverse)
library(uuid)
# ==============================================================================
# ChildAccount Account
# ==============================================================================

#'@title ChildAccount Class
#'
#' @description
#' An extension of the \code{MainAccount} class used to model specialized
#' sub-accounts such as goals, needs, or debt repayment accounts.
#' Child accounts inherit all the core functionality of \code{MainAccount}
#' while adding features like allocation percentage, account status, and
#' priority levels for fund distribution.
#'
#' @details
#' \itemize{
#'   \item Inherits from \code{MainAccount}.
#'   \item Can be part of a parent account and participate in fund distribution.
#'   \item Tracks its own balance, transaction history, and priority for
#'    receiving funds.
#' }
#'
#' @field allocation Numeric. Share of distributed income (0-1 scale).
#' @field status Character. Indicates whether the account is "active" or
#' "closed".
#' @field parent Optional. Reference to the parent account (if hierarchical).
#' @field path Character. Logical path to the account (used for organizing
#' accounts).
#' @field priority Numeric. Determines order of distribution among children
#' (higher = more).
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(name, allocation, status, parent, path,
#'   priority)}}{
#'     Constructor method.
#'   }
#'   \item{\code{deposit(amount, transaction_number, by, channel, date)}}{
#'     Overridden deposit method with status check.
#'   }
#'   \item{\code{change_status(status)}}{
#'     Updates the status of the account.
#'   }
#'   \item{\code{get_account_status()}}{
#'     Returns and prints the account's status.
#'   }
#'   \item{\code{get_priority()}}{
#'     Returns the priority level of the account.
#'   }
#'   \item{\code{set_priority(priority)}}{
#'     Sets a new priority level.
#'   }
#' }
#'
#' @examples
#' library(R6)
#' library(uuid)
#' library(tidyverse)
#' # Create a basic ChildAccount instance
#' child <- ChildAccount$new(
#'   name = "Emergency Fund",
#'   allocation = 0.3,
#'   priority = 2
#' )
#'
#' # Check initial status and priority
#' child$get_account_status()
#' child$get_priority()
#'
#' # Deposit into the child account
#' child$deposit(
#'   amount = 1000,
#'   channel = "Bank Transfer"
#' )
#'
#' # Change account status to inactive
#' child$change_status("inactive")
#'
#' # Try another deposit (won't proceed if inactive)
#' child$deposit(
#'   amount = 500,
#'   channel = "Bank Transfer"
#' )
#'
#' # Close the account after setting balance to zero
#' child$withdraw(
#'   amount = child$balance,
#'   channel = "Transfer to Main"
#' )
#' child$change_status("closed")
#'
#' @seealso \code{\link{MainAccount}}
#' @export
ChildAccount <- R6Class(
  "ChildAccount",
  inherit = MainAccount,
  public = list(
    allocation = 0, # Allocation percentage (e.g., 0.5 for 50%)
    status = "active", # Account status
    parent = NULL,
    path = NULL,
    priority = 0, # Priority level (higher value means higher priority)

    # ----------------------------- Class Constructor----------------------
    #' @description
    #' Initializes a new ChildAccount object by setting the name, allocation,
    #' status, parent, path, and priority. Inherits initialization logic
    #' from the MainAccount class.
    #'
    #' @param name Character. The name of the child account.
    #' @param allocation Numeric. Allocation weight for distributing funds
    #'   (default is 0).
    #' @param status Character. Status of the account: "active", "inactive", or
    #'   "closed" (default is "active").
    #' @param parent MainAccount or NULL. Optional parent account reference.
    #' @param path Character or NULL. Path or label to identify account lineage.
    #' @param priority Numeric. Priority value used when distributing small
    #'   amounts (default is 0).
    #' @examples
    #' # Create a basic ChildAccount with default values
    #' acc <- ChildAccount$new(name = "Emergency Fund")
    #'
    #' # Create a ChildAccount with custom allocation and priority
    #' acc2 <- ChildAccount$new(
    #'   name = "Education",
    #'   allocation = 0.3,
    #'   status = "active",
    #'   path = "main_account/education",
    #'   priority = 2
    #' )
    #'
    #' # View the account status
    #' acc2$get_account_status()

    initialize = function(
      name,
      allocation = 0,
      status = "active",
      parent = NULL,
      path = NULL,
      priority = 0
    ) {
      super$initialize(name) # Call parent constructor
      self$allocation <- allocation
      self$status <- status
      self$path <- path
      self$priority <- priority
    },

    # ---- Override Deposit Method (Transfer funds to child account)-------
    #' @description
    #' Deposits funds into a ChildAccount if the account is active. Records
    #' the transaction, updates the balance, and distributes the funds to any
    #' nested child accounts, if applicable.
    #'
    #' @param amount Numeric. Amount of money to deposit. Must be greater
    #' than 0.
    #' @param transaction_number Character or NULL. Optional transaction ID. If
    #'   NULL, an ID is generated automatically.
    #' @param by Character. Identifier for who made the deposit
    #' (default is "User").
    #' @param channel Character or NULL. Channel through which the deposit
    #' is made (e.g., "Mobile", "Bank").
    #' @param date POSIXct. Timestamp of the transaction
    #' (default is current time).
    #'
    #' @details
    #' This method overrides the \code{deposit()} method from the MainAccount
    #' class. It includes a status check to ensure the account is active before
    #' proceeding. If the account is inactive or closed, the deposit is blocked
    #' and a message is printed.
    #'
    #' @return No return value. Updates the account state and prints a summary.
    #'
    #' @examples
    #' # Create an active child account
    #' acc <- ChildAccount$new(name = "Savings", allocation = 0.5)
    #'
    #' # Deposit funds into the account
    #' acc$deposit(amount = 100, channel = "Mobile")
    #'
    #' # Attempting to deposit into an inactive account
    #' acc$change_status("inactive")
    #' acc$deposit(amount = 50, channel = "Mobile")  # Will not proceed

    deposit = function(
      amount,
      transaction_number = NULL,
      by = "User",
      channel = NULL,
      date = Sys.time()
    ) {
      if (self$status == "active") {
        if (is.null(transaction_number)) {
          transaction_number <- self$generate_transaction_id()
        } else {
          transaction_number <- transaction_number
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
        if (!grepl("Returned Extra Allocation", channel, ignore.case = TRUE)) {
          # exclude refunds
          self$distribute_to_children(
            amount,
            transaction_number
          )

          cat(
            "Deposited:",
            amount,
            "via",
            channel,
            "- Transaction ID:",
            transaction_number,
            "\n"
          )
        }
      } else {
        if (grepl("Returned Extra Allocation", channel, ignore.case = TRUE)){
        } else {
          cat("Deposit not allowed. Account is not active.\n")
        }
      }
    },

    # -----------Change account status ------------------------------------
    #' @description
    #' Changes the status of the ChildAccount. If the new status is "closed",
    #' the account must have a zero balance; otherwise, an error is thrown.
    #'
    #' @param status Character. The desired new status of the account.
    #' Acceptable values include "active", "inactive", or "closed".
    #'
    #' @details
    #' If the account is already "closed", a message is printed and no changes
    #' are made. If closing is requested and the balance is not zero, an error
    #' is raised to ensure proper fund handling before deactivation.
    #'
    #' @return No return value. Modifies the account's status in place and
    #' prints a message.
    #'
    #' @examples
    #' acc <- ChildAccount$new(name = "Emergency Fund", allocation = 0.3)
    #' acc$change_status("inactive")  # Changes status to inactive
    #' acc$change_status("active")    # Re-activates the account
    #'
    #' # Attempting to close with non-zero balance triggers error
    #' acc$deposit(100, channel = "Mobile")
    #' \dontrun{
    #' acc$change_status("closed")    # Will raise an error
    #' }
    #'
    #' # Withdraw funds then close
    #' acc$withdraw(100, channel = "Transfer")
    #' acc$change_status("closed")    # Successful closure

    change_status = function(status) {
      if (self$status == "closed"&& status == "closed") {
        stop("This account is already closed.")
      }

      if (status == "closed") {
        if (self$balance > 0) {
          stop("Withdraw from this account or move the balance before closure.")
        } else {
          cat(self$name, "has been closed.\n")
          self$status <- "closed"
        }
      } else {
        self$status <- status
        cat(self$name, paste("has become", status, ".\n"))
      }
    },

    # ------------- Get account status --------------------------------
    #' @description
    #' Retrieves and prints the current status of the child account.
    #'
    #' @return Character. The current status of the account: typically
    #' "active", "inactive", or "closed".
    #'
    #' @examples
    #' acc <- ChildAccount$new(name = "School Fees", allocation = 0.4)
    #' acc$get_account_status()
    #' # Output: "School Fees is active"

    get_account_status = function() {
      cat(self$name, "is", self$status, "\n")
      self$status
    },

    # ---------------get the priority of the account ---------------
    #' @description
    #' Returns the current priority level assigned to the child account.
    #'
    #' @return Numeric. The priority value used in fund distribution
    #' (higher values indicate higher priority).
    #'
    #' @examples
    #' acc <- ChildAccount$new(name = "Emergency Fund", priority = 3)
    #' acc$get_priority()
    #' # [1] 3

    get_priority = function() {
      self$priority
    },

    #----------- Set the priority of the account ------------------
    #' @description
    #' Updates the priority level of the child account.
    #' Higher priority values indicate a stronger preference for receiving funds
    #' during distribution.
    #'
    #' @param priority Numeric. The new priority value to assign to the account.
    #'
    #' @return No return value. Prints a message confirming the new priority.
    #'
    #' @examples
    #' acc <- ChildAccount$new(name = "Education Fund", priority = 1)
    #' acc$set_priority(5)
    #' # Priority for Education Fund set to 5
    set_priority = function(priority) {
      self$priority <- priority
      cat("Priority for", self$name, "set to", priority, "\n")
    }
  )
)
