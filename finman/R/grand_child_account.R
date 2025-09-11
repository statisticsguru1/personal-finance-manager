# ChildAccount Class (Needs, Goals, Debt Repayment)
library(R6)
library(uuid)
library(tidyverse)
# ==============================================================================
# ChildAccount Account
# ==============================================================================
#' @title GrandAccount Class
#'
#' @description
#' Extends \code{ChildAccount} to model low-level accounts such as bills,
#' loans, and targeted savings. Adds time-based logic, due tracking, and
#' automatic closure/reactivation to ensure intelligent fund allocation.
#'
#' @details
#' This class introduces behavior tailored to two main categories:
#'
#' \strong{1. Periodic Accounts (e.g., Bills, Rent, Fixed Savings):}
#' \itemize{
#'   \item Require recurring payments before a \code{due_date}.
#'   \item If fully funded before the due date, the account is marked
#'   \code{"inactive"} and any surplus is returned to the parent.
#'   \item Upon reaching the next due date, the account reactivates
#'   and begins tracking the next cycle's funding needs.
#' }
#'
#' \strong{2. Open-Ended Accounts (e.g., Long-Term Debts, Target Savings):}
#' \itemize{
#'   \item Do not rely on \code{due_date} or cycles.
#'   \item Once the \code{fixed_amount} target is met, the account is
#'   permanently closed and surplus funds are redirected.
#'   \item This prevents over-allocation to already satisfied targets.
#' }
#'
#' These behaviors:
#' \itemize{
#'   \item Guard against poor user allocation strategies by reallocating
#'   excess funds from fully funded accounts.
#'   \item Adapt automatically to variable incomes, ensuring flexible
#'   prioritization (e.g., for freelancers).
#'   \item Allow non-expert users to benefit from dynamic, self-adjusting
#'   savings and debt repayment logic over time.
#' }
#'
#' @field status Character. "active", "inactive", or "closed".
#' @field due_date POSIXct or NULL. When funding is due (for bills, etc.).
#' @field amount_due Numeric. Amount left to fully fund the account.
#' @field fixed_amount Numeric. Fixed target for each funding cycle.
#' @field account_type Character. e.g., "Bill", "Debt", "FixedSaving".
#' @field freq Numeric or NULL. Cycle length in days (for recurring accounts).
#' @field num_periods Numeric. Number of unpaid cycles.
#' @field Track_dues_and_balance Data frame. History of balance and dues.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(...)}}{
#'     Constructor. Sets allocation, type, due date, fixed target, etc.
#'   }
#'   \item{\code{deposit(...)}}{
#'     Handles reactivation on due date and closes account when target met.
#'     Returns surplus to parent account for redistribution.
#'   }
#'   \item{\code{withdraw(...)}}{
#'     Withdraws funds and adjusts period tracking accordingly.
#'   }
#'   \item{\code{get_/set_} methods}{
#'     Get/set values for due date, amount, account type, freq, periods.
#'   }
#' }
#'
#' @seealso \code{\link{ChildAccount}}, \code{\link{MainAccount}}
#' @export
#'
GrandchildAccount <- R6Class(
  "GrandchildAccount",
  inherit = ChildAccount,
  public = list(
    status = "active",
    due_date = NULL,
    amount_due = 0,
    fixed_amount = 0,
    account_type = NULL,
    freq = NULL,
    num_periods = 1,
    Track_dues_and_balance = NULL,

    # -------------------- Constructor----------------------------------------
    #' @description
    #' Initializes a new \code{GrandchildAccount} instance with attributes and
    #' tracking suitable for both periodic (e.g., bills, rent, fixed savings)
    #' and open-ended (e.g., long-term debts, target savings) accounts.
    #' Inherits from \code{ChildAccount} and sets up account-specific
    #' parameters like due dates, target amounts, and funding cycles.
    #'
    #' @param name Character. Name or label of the account
    #' (e.g., "Rent", "Car Loan").
    #' @param allocation Numeric. Proportion of parent funds to allocate to
    #' this account. Used during distribution logic. Defaults to 0.
    #' @param priority Numeric. Priority weight for redistribution of residual
    #' funds, especially in cases of overflow or unmet allocations.
    #' Defaults to 0.
    #' @param fixed_amount Numeric. Target amount required to fully fund the
    #' account per period or in total. Used for both bills and savings goals.
    #' @param due_date POSIXct or NULL. Optional due date indicating when
    #' the next funding cycle is expected (for recurring accounts).
    #' @param account_type Character. Type of the account: e.g., "Bill", "Debt",
    #' "FixedSaving", or "Expense". Influences reactivation and closure
    #' behavior. Defaults to "Expense".
    #' @param freq Numeric or NULL. Frequency in days for periodic accounts to
    #'   recur. Required for automated reactivation logic.
    #' @param status Character. "active", "inactive", or "closed".
    #'
    #' @details
    #' The constructor also initializes a data frame
    #' \code{Track_dues_and_balance}
    #' to monitor the account's funding status over time. Each row logs
    #' the current due amount and balance upon deposit or withdrawal.
    #'
    #' For accounts of type "Bill", "FixedSaving", or "Debt"
    #' (with a \code{due_date}), the constructor sets up fields that support
    #'  automated activation, deactivation, and fund tracking per cycle.
    #'
    #' @examples
    #' # Initialize a rent account due every 30 days with a fixed monthly cost
    #' library(R6)
    #' library(uuid)
    #' library(tidyverse)
    #' rent <- GrandchildAccount$new(
    #'   name = "Rent",
    #'   allocation = 0.3,
    #'   priority = 2,
    #'   fixed_amount = 75000,
    #'   due_date = Sys.Date() + 30,
    #'   account_type = "Bill",
    #'   freq = 30
    #' )
    #'
    #' # Initialize a target savings account without a due date
    #' car_saving <- GrandchildAccount$new(
    #'   name = "Car Fund",
    #'   allocation = 0.2,
    #'   fixed_amount = 500000,
    #'   account_type = "FixedSaving"
    #')
    initialize = function(
      name,
      allocation = 0,
      priority = 0,
      fixed_amount = 0,
      due_date = NULL,
      account_type = "Expense",
      freq = NULL,
      status = "active"
    ) {
      super$initialize(name, allocation)
      self$fixed_amount <- fixed_amount
      self$status <- status
      self$due_date <- due_date
      self$amount_due <- fixed_amount
      self$account_type <- account_type
      self$freq <- freq
      self$num_periods <- 1
      self$priority <- priority
      self$Track_dues_and_balance <- data.frame(
        Date = as.POSIXct(character()),
        Amount_due = numeric(),
        Balance = numeric(),
        stringsAsFactors = FALSE
      )
    },

    # -------------- Getter for due_date-------------------------------------
    #' @description
    #' Retrieves the current due date of the account. This is typically used
    #' for periodic accounts such as bills or fixed savings that require funding
    #' on a recurring schedule.
    #'
    #' @return A POSIXct object representing the due date, or \code{NULL}
    #' if no due date is set.
    #'
    #' @examples
    #' rent <- GrandchildAccount$new(
    #'   name = "Rent",
    #'   fixed_amount = 75000,
    #'   due_date = Sys.Date() + 30
    #' )
    #' rent$get_due_date()

    get_due_date = function() {
      self$due_date
    },

    # ----------------- Setter for due_date ------------------------------
    #' @description
    #' Sets a new due date for the account. This is useful for accounts with
    #' periodic funding requirements, such as rent, bills, or fixed savings.
    #'
    #' @param due_date POSIXct. The new due date to assign to the account.
    #'
    #' @return None. Updates the account's \code{due_date} field and prints
    #' a confirmation message.
    #'
    #' @examples
    #' bill <- GrandchildAccount$new(name = "Electricity", fixed_amount = 5000)
    #' bill$set_due_date(Sys.Date() + 15)
    set_due_date = function(due_date) {
      self$due_date <- due_date
      cat("Due date for", self$name, "set to", due_date, "\n")
    },

    # -------------- Getter for fixed_amount--------------------------------
    #' @description
    #' Retrieves the fixed amount assigned to the account. This is typically
    #' used in accounts like bills, fixed savings, or loan payments where
    #' a specific amount is expected periodically.
    #'
    #' @return Numeric. The fixed amount required by the account.
    #'
    #' @examples
    #' rent <- GrandchildAccount$new(name = "Rent", fixed_amount = 75000)
    #' rent$get_fixed_amount()
    #' #> [1] 75000

    get_fixed_amount = function() {
      self$fixed_amount
    },

    #---------- Setter for fixed_amount-----------------------------------
    #' @description
    #' Sets a new fixed amount for the account. This value represents the
    #' expected periodic contribution or payment (e.g., monthly rent, loan
    #' installment).
    #' It also recalculates the current amount due based on the number of unpaid
    #' periods and the account balance.
    #'
    #' @param fixed_amount Numeric. The new fixed amount to be assigned to the
    #' account.
    #'
    #' @return None. Updates internal fields and prints a confirmation message.
    #'
    #' @examples
    #' rent <- GrandchildAccount$new(name = "Rent", fixed_amount = 50000)
    #' rent$set_fixed_amount(75000)
    #' #> Fixed amount for Rent set to 75000

    set_fixed_amount = function(fixed_amount) {
      self$fixed_amount <- fixed_amount
      self$amount_due <- fixed_amount * self$num_periods - self$balance
      cat("Fixed amount for", self$name, "set to", fixed_amount, "\n")
    },

    # --------------- Getter for account_type----------------------------------
    #' @description
    #' Sets a new fixed amount for the account. This value represents the
    #' expected periodic contribution or payment (e.g., monthly rent, loan
    #' installment).
    #' It also recalculates the current amount due based on the number of unpaid
    #' periods and the account balance.
    #'
    #' @param fixed_amount Numeric. The new fixed amount to be assigned to the
    #' account.
    #'
    #' @return None. Updates internal fields and prints a confirmation message.
    #'
    #' @examples
    #' rent <- GrandchildAccount$new(name = "Rent", fixed_amount = 50000)
    #' rent$set_fixed_amount(75000)
    #' #> Fixed amount for Rent set to 75000
    #'
    get_account_type = function() {
      self$account_type
    },

    # ----------- Setter for account_type -----------------------------------
    #' @description
    #' Sets the type of the account, which influences how it behaves with
    #' respect to funding, reactivation, and closure policies. This field
    #' is central to determining whether the account is recurring, fixed,
    #' or target-based.
    #'
    #' @param account_type Character. One of the supported types such as
    #'   \code{"Bill"}, \code{"Debt"}, \code{"FixedSaving"},
    #'   \code{"NonFixedSaving"}, or \code{"Expense"}.
    #'   Determines how due dates, funding limits, and surplus reallocation
    #'   are handled.
    #'
    #' @return None. Sets the \code{account_type} field and prints a
    #' confirmation.
    #'
    #' @details
    #' - \code{"Bill"} or \code{"FixedSaving"}: These accounts are period-based
    #'   and are reactivated upon due dates.
    #' - \code{"Debt"} or target savings: Once fully funded, they are closed and
    #'   not reopened.
    #' - \code{"Expense"} or \code{"NonFixedSaving"}: Do not enforce due dates
    #' or strict funding targets.
    #'
    #' @examples
    #' rent <- GrandchildAccount$new(name = "Rent", fixed_amount = 75000)
    #' rent$set_account_type("Bill")
    #' #> Account type for Rent set to Bill

    set_account_type = function(account_type) {
      self$account_type <- account_type
      cat("Account type for", self$name, "set to", account_type, "\n")
    },

    # ------------- Deposit method with period handling-------------------------
    #' @description
    #' Handles incoming deposits for grandchild accounts, including complex
    #' behavior for fixed-amount accounts such as Bills, FixedSavings,
    #' and Debts. It intelligently manages due dates, period increments,
    #' surplus reallocation, and account status updates.
    #'
    #' @param amount Numeric. The amount of money being deposited into the
    #' account.
    #' @param transaction_number Character or NULL. Optional unique identifier
    #' for the transaction. If NULL, a new one is generated.
    #' @param by Character. The party initiating the transaction
    #' (default is "User").
    #' @param channel Character or NULL. The method or channel used for the
    #' transaction (e.g., "Mobile Money").
    #' @param transaction_date POSIXct. The timestamp for the transaction (defaults to
    #' current system time).
    #'
    #' @details
    #' This method supports two core behaviors depending on the type of account:
    #'
    #' ## 1. Period-based Accounts (`Bill`, `FixedSaving`)
    #' - If the due date has passed, the system automatically increments the
    #' number of unpaid periods
    #'   and extends the due date.
    #' - If the deposited amount fully covers the required amount across all
    #' unpaid periods, the account is marked as `"inactive"` (temporarily
    #' closed).
    #' - Any surplus is redirected to the parent account for reallocation,
    #' ensuring no money is trapped in overfunded accounts.
    #' - The transaction is logged in `Track_dues_and_balance` to track
    #' financial health over time.
    #'
    #' ## 2. Non-period Accounts (`LongTermDebt`, `TargetSaving`)
    #' - When the required amount is met, the account is permanently closed,
    #' and will not reactivate.
    #'  This design ensures funds are focused on accounts that still need
    #'   attention.
    #'
    #' This mechanism encourages automatic reallocation of excess funds to
    #' critical needs without
    #' requiring users to micromanage their allocations — useful especially for
    #' users with fluctuating income.
    #'
    #' @return None. Internally updates the account status, balance,
    #' due amounts, and transaction history.
    #'
    #' @examples
    #' # main account
    #' main<- MainAccount$new("main")
    #'
    #' # child account
    #' child <- ChildAccount$new(
    #'   name = "Emergency Fund",
    #'   allocation = 0.3,
    #'   priority = 2
    #' )
    #'
    #' # Grand child account
    #' bill <- GrandchildAccount$new(
    #'   name = "Rent",
    #'   fixed_amount = 75000,
    #'   account_type = "Bill",
    #'   due_date = Sys.Date(),
    #'   freq = 30
    #' )
    #' # attach grand child to parent
    #' main$add_child_account(child)
    #' child$add_child_account(bill)
    #' bill$deposit(75000,channel="ABSA")
    #'
    #' # Example with surplus being returned to parent:
    #' bill$deposit(80000,channel="ABSA")
    #'
    #' # Example with underpayment:
    #' bill$deposit(20000,channel="ABSA")  # Remains active, shows updated due

    deposit = function(
      amount,
      transaction_number = NULL,
      by = "User",
      channel = NULL,
      transaction_date = Sys.time()
    ) {

      if (missing(channel) || is.null(channel)) {
        stop("`channel` is required and cannot be NULL.")
      }

      transaction_date <- tryCatch(as.POSIXct(transaction_date), error = function(e) as.POSIXct(NA))
      # Check if bill is overdue and update periods
      if (self$account_type == "Bill" & !is.null(self$due_date)) {
        if (transaction_date > self$due_date) {
          # Extend due date by frequency and increment periods
          self$due_date <- self$due_date + lubridate::days(self$freq)
          self$num_periods <- self$num_periods + 1
          cat(
            "Due date extended. Number of periods unpaid:",
            self$num_periods,
            "\n"
          )
          self$status <- "active"
          cat(
            self$name,
            "reactivated. Outstanding balance due:",
            self$num_periods * self$fixed_amount - self$balance,
            "\n"
          )
        }
      }

      # Handle fixed amount logic (Bill, Debt, FixedSaving)
      if (self$fixed_amount > 0) {
        if (amount >= (self$num_periods * self$fixed_amount - self$balance)) {
          deposit_amount <- self$num_periods * self$fixed_amount - self$balance
          super$deposit(
            amount = deposit_amount,
            transaction_number,
            by = by,
            channel,
            transaction_date
          )
          self$change_status("inactive") # Fully funded for the period

          # Move any extra amount to the parent account
          extra_amount <- amount - deposit_amount
          if (extra_amount > 0) {
            if(by=="User"){
              channel = "Returned Extra Allocation-up"
            } else {
              channel = "Returned Extra Allocation-down"
            }

            self$parent$deposit(
              amount = extra_amount,
              by = "System",
              channel = channel
            )
            cat(
              "Extra amount of", extra_amount,
              "moved to", self$parent$name,
              "\n"
            )
          }
          self$amount_due <- 0  # No amount due
          self$Track_dues_and_balance <- rbind(
            self$Track_dues_and_balance,
            data.frame(
              Date = Sys.time(),
              Amount_due = self$amount_due,
              Balance = self$balance,
              stringsAsFactors = FALSE
            )
          )
          cat(self$name, "fully funded for", self$num_periods, "period(s)\n")
          return(invisible(NULL))
        }
      }

      # Regular deposit if no overflow
      # you might think this double deposits but its not
      super$deposit(
        amount = amount,
        transaction_number = transaction_number,
        by = by,
        channel = channel,
        transaction_date = transaction_date
      )

      if (self$fixed_amount >  0) {
        self$amount_due <- self$num_periods * self$fixed_amount - self$balance
        self$Track_dues_and_balance <- rbind(
          self$Track_dues_and_balance,
          data.frame(
            Date = Sys.time(),
            Amount_due = self$amount_due,
            Balance = self$balance,
            stringsAsFactors = FALSE
          )
        )
      } else {
        self$Track_dues_and_balance <- rbind(
          self$Track_dues_and_balance,
          data.frame(
            Date = Sys.time(),
            Amount_due = self$amount_due,
            Balance = self$balance,
            stringsAsFactors = FALSE
          )
        )
      }
    },

    # ----------- Withdrawal method with borrowing logic ---------------------
    #' @description
    #' Handles withdrawal requests from a grandchild account. The method ensures
    #' sufficient balance is available, updates the internal transaction
    #' tracking, and compensates for
    #' partial withdrawals in fixed-amount accounts by adjusting the effective
    #' number of periods.
    #'
    #' @param amount Numeric. The amount to withdraw from the account.
    #' @param transaction_number Character or NULL. Optional transaction
    #' reference ID.
    #' @param by Character. The party initiating the withdrawal (default is
    #' "User").
    #' @param channel Character or NULL. Source or medium of the transaction
    #' (e.g., "Mobile Money").
    #' @param transaction_date POSIXct. The date and time of withdrawal (default is current
    #' system time).
    #'
    #' @details
    #' The method performs the following steps:
    #'
    #' - Checks whether the account balance is sufficient for the requested
    #' withdrawal.
    #' - If sufficient, it processes the withdrawal via the parent class method.
    #' - It then logs the updated balance and remaining amount due into the
    #' `Track_dues_and_balance` history.
    #'
    #' For accounts with a `fixed_amount` (e.g., Bills, FixedSavings, Debts):
    #'
    #' - Partial withdrawals are treated as funding reversals and reduce the
    #' number of fulfilled periods.
    #' - This ensures that the system maintains accurate state about what's
    #' left to fulfill for the account,
    #'   without modifying the `fixed_amount` itself.
    #'
    #' This mechanism is crucial in dynamic environments where users may
    #' occasionally retrieve money
    #' from priority accounts — e.g., for emergencies — and helps the system
    #' readjust allocation logic
    #' accordingly.
    #'
    #' @return None. The internal state of the account (balance, period count,
    #' logs) is updated.
    #'
    #' @examples
    #' # Withdraw a partial amount from a fully funded rent account
    #' rent <- GrandchildAccount$new("Rent", fixed_amount = 75000,
    #' account_type = "Bill")
    #' rent$deposit(75000,channel="ABSA")
    #' # Now equivalent to 0.53 of the rent period remaining.
    #' rent$withdraw(35000,channel="ABSA")
    #'

    withdraw = function(
      amount,
      transaction_number = NULL,
      by = "User",
      channel = NULL,
      transaction_date = Sys.time()
    ) {
      if (amount > 0) {
        if (amount > self$balance) {
          cat("Insufficient balance in", self$name, "to withdraw", amount, "\n")
          return(NULL)
        }
        super$withdraw(amount = amount, by = by, channel = channel)
        self$Track_dues_and_balance <- rbind(
          self$Track_dues_and_balance,
          data.frame(
            Date = Sys.time(),
            Amount_due = self$amount_due,
            Balance = self$balance,
            stringsAsFactors = FALSE
          )
        )

        # for bills and fixed savings withdrawal reduce balance we need to
        # compensate for istance with a monthly rent of $75000 if you withdraw
        # 35000 the rent remains 40000 but fixed amount reads 75000, instead of
        # changing fixed amount we can adjust the period.

        if (self$fixed_amount > 0) {
          self$num_periods <- self$num_periods - (amount / self$fixed_amount)
        }
      }
    },

    # Getter and Setter for frequency and periods
    #' @description
    #' Retrieves the recurrence frequency of the account, typically used for
    #' accounts with periodic obligations such as bills, fixed savings, or
    #' loans.
    #'
    #' @return The frequency of recurrence, as stored in the account
    #' (e.g., number of days, or a character label like "monthly").
    #'
    #' @details
    #' This frequency determines how often the account expects funding. For
    #' instance, a rent account with a monthly cycle would have a frequency of
    #' 30 (days), while a weekly expense might have 7.
    #'
    #' This field is primarily used in due date updates and period tracking,
    #' especially for auto-reactivating accounts like Bills or Fixed Savings
    #' after their due dates lapse.
    #'
    #' @examples
    #' acc <- GrandchildAccount$new(
    #'   "Rent",
    #'   fixed_amount = 1000,
    #'   freq = 30
    #')
    #' acc$get_account_freq()
    #' # [1] 30

    get_account_freq = function() {
      self$freq
    },

    # ------- Account frequency setter -----------------------------------------
    #' @description
    #' Sets the recurrence frequency of the account, which defines how often the
    #'account expects to be funded.
    #'
    #' @param account_freq Numeric or character value representing the
    #' recurrence frequency.
    #' For example, use `30` for a monthly bill or `"weekly"` if implementing a
    #' custom handler.
    #'
    #' @details
    #' This frequency value is crucial for managing due dates and determining
    #' when a new period starts (e.g., when a rent account should reactivate
    #' after a month).It is used in conjunction with the due date to trigger
    #' reactivation and allocation adjustments for fixed-type accounts such as
    #' Bills, Fixed Savings, and Loans. Changing the frequency may affect how
    #' missed or overdue periods are computed going forward.
    #'
    #' @examples
    #' acc <- GrandchildAccount$new("Water Bill", fixed_amount = 500)
    #' acc$set_account_freq(30)
    #' # Frequency for Water Bill set to 30

    set_account_freq = function(account_freq) {
      self$freq <- account_freq
      cat("Frequency for", self$name, "set to", account_freq, "\n")
    },

    # get account period
    #' @description
    #' Retrieves the number of unpaid or active periods associated with the
    #' account.
    #'
    #' @return Numeric value indicating how many periods are currently pending
    #' or tracked for the account.
    #'
    #' @details
    #' This is particularly relevant for fixed-type accounts like Bills,
    #' Fixed Savings, or Debts with a defined frequency.
    #' The number of periods (`num_periods`) represents how many cycles have
    #' passed without full funding.
    #' It increases when due dates pass without adequate deposits and decreases
    #' when partial withdrawals are made from already-funded periods.
    #'
    #' For example, if a rent account expects funding every 30 days and misses
    #' two cycles, `num_periods` will be 3 (including the current one), and the
    #' system will attempt to fund all missed cycles.
    #'
    #' @examples
    #' acc <- GrandchildAccount$new(
    #'   "Internet Bill",
    #'   fixed_amount = 2500,
    #'   freq = 30
    #'  )
    #' acc$get_account_periods()
    #' # [1] 1

    get_account_periods = function() {
      self$num_periods
    },

    #' @description
    #' Manually sets the number of unpaid or active periods for the account.
    #'
    #' @param periods Numeric value representing the number of periods to
    #' assign.
    #'
    #' @details
    #' This method allows manual control of how many cycles (e.g., months or
    #' days) are currently due or tracked for the account. It can be used in
    #' administrative corrections or simulations of time passage in budgeting
    #' models.
    #'
    #' Use with caution: setting `num_periods` directly may desynchronize with
    #' the actual due date logic unless adjustments are consistently maintained.
    #'
    #' @examples
    #' acc <- GrandchildAccount$new("Loan Payment", fixed_amount = 10000,
    #' freq = 30)
    #' acc$set_account_periods(3)
    #' # Output: Loan Payment has 3 period(s)


    set_account_periods = function(periods) {
      self$num_periods <- periods
      cat(self$name, "has", periods, "period(s)\n")
    }
  )
)
