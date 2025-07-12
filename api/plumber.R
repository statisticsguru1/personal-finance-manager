
library(plumber)
library(future)
library(promises)
library(tidyverse)
library(rlang)
plan(sequential)

#* @apiTitle Personal Finance Manager API
#* @apiDescription Secure API for managing user accounts, transactions, and balances.
#* @apiVersion 1.0.0
#* @apiContact list(
#*   name = "Festus Nzuma",
#*   url = "https://github.com/statisticsguru1/personal-finance-manager",
#*   email = "mutindafestus27@gmail.com"
#* )
#* @apiLicense list(
#*   name = "Proprietary License",
#*   url = "https://github.com/statisticsguru1/personal-finance-manager/LICENSE"
#* )


# -------------- Auth Filters --------------------------------------------------

# Initialize rate limit tracker
if (!exists("rate_limiter")) {
  rate_limiter <- new.env()
}

# ─── Initialize request tracker for exponential backoff ───
if (!exists("request_tracker", inherits = FALSE)) {
  request_tracker <- new.env()
}

MAX_REQUESTS <- as.numeric(Sys.getenv("MAX_REQUESTS", unset = 1000))
WINDOW_SIZE <- as.numeric(Sys.getenv("WINDOW_SIZE", unset = 3600))


#' Check Rate Limit for a Given User
#'
#' @description Internal helper that implements a sliding window rate limit.
#' Limits the number of API calls per user based on `MAX_REQUESTS` and `WINDOW_SIZE`.
#'
#' @param user_id A character string uniquely identifying the user.
#' @return TRUE if the user is allowed to proceed; FALSE if the rate limit is
#'  exceeded.
check_rate_limit <- function(user_id) {
  now <- Sys.time()
  rl <- rate_limiter[[user_id]]

  if (is.null(rl)) {
    rate_limiter[[user_id]] <- list(
      window_start = now,
      count = 1
    )
    return(TRUE)
  }

  elapsed <- as.numeric(difftime(now, rl$window_start, units = "secs"))

  if (elapsed > WINDOW_SIZE) {
    # Reset window
    rate_limiter[[user_id]] <- list(
      window_start = now,
      count = 1
    )
    return(TRUE)
  }

  if (rl$count < MAX_REQUESTS) {
    rl$count <- rl$count + 1
    rate_limiter[[user_id]] <- rl
    return(TRUE)
  }

  return(FALSE)
}

#* @tag auth
#* @filter auth
#* @apiTag auth "Authentication and Rate Limiting"
#* Authenticates requests using JWT tokens.
#* Adds user ID and role to `req` on success.
#* Applies exponential backoff and request rate limiting per user.
#*
#* @response 401 Invalid or missing token
#* @response 429 Rate limit exceeded or backoff triggered
#* @response 500 Missing or invalid server secret
function(req, res) {
  if (req$PATH_INFO == "/__ping__") return(forward())

  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
    res$status <- 401
    return(list(error = "Missing or invalid token"))
  }

  secret_key <- Sys.getenv("JWT_SECRET")
  if (secret_key == "") {
    res$status <- 500
    return(list(error = "Server misconfigured (missing JWT_SECRET)"))
  }

  token <- sub("Bearer ", "", auth_header)
  decoded <- verify_token(token, secret = secret_key)

  # ─────── Failed Auth: Apply Exponential Backoff ───────
  if (is.null(decoded)) {
    user_id <- token  # fallback identifier
    now <- Sys.time()
    tracker <- request_tracker[[user_id]]

    if (is.null(tracker)) {
      request_tracker[[user_id]] <- list(last_try = now, attempt = 0)
    } else {
      elapsed <- as.numeric(difftime(now, tracker$last_try, units = "secs"))
      expected_wait <- 2^tracker$attempt

      if (elapsed < expected_wait) {
        res$status <- 429
        return(
          list(
            error = paste(
              "Too many failed attempts. Wait",
              round(expected_wait - elapsed, 2),
              "seconds."
            )
          )
        )
      } else {
        request_tracker[[user_id]]$last_try <- now
        request_tracker[[user_id]]$attempt <- tracker$attempt + 1
      }
    }

    res$status <- 401
    return(list(error = "Invalid or expired token"))
  }

  # ─────── Successful Auth: Enforce Rate Limit ───────
  user_id <- decoded$user_id
  req$user_id <- user_id
  req$role <- decoded$role

  if (!check_rate_limit(user_id)) {
    res$status <- 429
    return(list(error = "Rate limit exceeded. Try later."))
  }

  forward()
}




# =============================================================================
# General end points
# These apply to all account types (Main, Child, Grand child)
# =============================================================================


# -------------- Health check(ping) endpoint -----------------------------------

#* Health check endpoint
#* @get /__ping__
ping <- function() {
  list(status = "ok")
}

# -------------- Deposit endpoint ----------------------------------------------

#* @tag accounts
#* @post /deposit
#* @summary Deposit money into an account
#* @description
#* Allows a user or admin to deposit a specified amount into a specific account
#*  identified by its UUID.
#* Performs permission checks and updates the account tree in persistent storage.
#*
#* @param uuid:str* Account UUID (required)
#* @param amount:float* Deposit amount (required)
#* @param channel:str* Deposit channel (required)
#* @param transaction_number:str Optional transaction number
#* @param by:str Who performed the deposit (default = "User")
#* @param date:str Timestamp of deposit (default = now)
deposit <- function(req, res,
                    uuid,
                    amount,
                    channel,
                    transaction_number = NULL,
                    by = "User",
                    date = Sys.time()) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")
      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "Account not found or unauthorized"
        )
      } else {
        account$deposit(
          amount = as.numeric(amount),
          transaction_number = transaction_number,
          by = by,
          channel = channel,
          date = date
        )
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          account_uuid = uuid,
          amount = as.numeric(amount),
          balance = account$balance
        )
      }
    })
  }) %...>% (function(result) {
    end_time <- Sys.time()
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(end_time, start_time, units = "secs")
    )

    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    end_time <- Sys.time()
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(end_time, start_time, units = "secs")
      )
    )
  })
}

# --------------- Withdraw endpoint --------------------------------------------

#* @tag accounts
#* @post /withdraw
#* @summary Withdraw money from an account
#* @description
#* Allows a user or admin to withdraw a specified amount from a target account (via UUID),
#* with appropriate checks and persistence to disk.
#*
#* @param uuid:str* Account UUID (required)
#* @param amount:float* Withdrawal amount (required)
#* @param channel:str* Withdrawal channel (required)
#* @param transaction_number:str Optional transaction number
#* @param by:str Who performed the withdrawal (default = "User")
#* @param date:str Timestamp of withdrawal (default = now)
withdraw <- function(req, res,
                     uuid,
                     amount,
                     channel,
                     transaction_number = NULL,
                     by = "User",
                     date = Sys.time()) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  amount <- as.numeric(amount)
  if (is.na(amount) || amount <= 0) {
    res$status <- 400
    return(list(success = FALSE, error = "Invalid withdrawal amount"))
  }

  future({
    library(magrittr)
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")
      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "Account not found or unauthorized"
        )
      } else {
        account$withdraw(
          amount = amount,
          transaction_number = transaction_number,
          by = by,
          channel = channel,
          date = date
        )
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          account_uuid = uuid,
          balance = account$balance
        )
      }
    })
  }) %...>% (function(result) {
    end_time <- Sys.time()
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(end_time, start_time, units = "secs")
    )

    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    end_time <- Sys.time()
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(end_time, start_time, units = "secs")
      )
    )
  })
}

# --------------- Distribute endpoint ------------------------------------------

#* @tag accounts
#* @post /distribute
#* @summary Distribute funds to child accounts
#* @description
#* Distributes a given amount from a parent account to all of its active
#* children based on their allocation.
#* Requires the parent account UUID and amount. Optional metadata like
#* `transaction` and `by` can be provided.
#*
#* @param uuid:str* UUID of the parent account
#* @param amount:float* Amount to distribute (must be > 0)
#* @param transaction:str Optional transaction reference
#* @param by:str Initiator of the distribution (default: "System")
#* @json

distribute <- function(req, res,
                       uuid,
                       amount = NULL,
                       transaction = NULL,
                       by = "System") {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")
      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "Account not found or unauthorized"
        ))
      }

      if (is.null(amount)) {
        return(list(
          success = FALSE,
          status = 500,
          error = "Missing amount"
        ))
      }

      if (is.na(amount) || length(amount) != 1 || !is.numeric(amount)) {
        return(list(
          success = FALSE,
          status = 500,
          error = "Amount should be numeric"
        ))
      }

      if (amount < 0) {
        return(list(
          success = FALSE,
          status = 500,
          error = "Negative amount is not allowed"
        ))
      }
      if (amount == 0) {
        return(list(
          success = FALSE,
          status = 500,
          error = "zero|amount"
        ))
      }

      # Perform the distribution
      account$distribute_to_children(
        amount = as.numeric(amount),
        transaction = transaction %||% paste0("dist-", Sys.time()),
        by = by
      )

      save_user_file(user_id, tree, "account_tree.Rds")
      list(
        success = TRUE,
        status = 200,
        message = paste("Distributed", amount, "from", account$name),
        uuid = uuid
      )
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}



# --------------- Add child acc endpoint --------------------------------------

#* @tag accounts
#* @post /add_sub_account
#* @summary Add a child or grandchild account under a parent account
#* @description
#* Adds a new child or grandchild account to an existing parent account.
#* - If the parent is a main account, a `ChildAccount` is created.
#* - If the parent is a child account, a `GrandchildAccount` is created.
#* Automatically persists changes to disk after update.
#*
#* @param parent_uuid:str* UUID of the parent account
#* @param name:str* Name of the new account (must be unique under this parent)
#* @param allocation:float* Allocation fraction between 0 and 1
#* @param priority:int Optional priority weight (default: 0)
#* @param fixed_amount:float Optional fixed amount (used only for grandchild)
#* @param due_date:str Optional ISO-formatted due date
#* @param account_type:str Optional account type (used for grandchild)
#* @param freq:int Optional frequency (used for grandchild)
#* @json

add_child_account <- function(req, res,
                              parent_uuid,
                              name,
                              allocation,
                              priority = 0,
                              fixed_amount = 0,
                              due_date = NULL,
                              account_type = NULL,
                              freq = NULL,
                              status = "active") {
  user_id <- req$user_id
  start_time <- Sys.time()

  future({
    library(tidyverse)
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")
      parent <- tree$find_account_by_uuid(parent_uuid)

      if (is.null(parent)) {
        return(
          list(
            success = FALSE,
            status = 404,
            error = "Parent account not found"
          )
        )
      }

      # 🔍 Validate name does not already exist under this parent
      existing_names <- parent$list_child_accounts()
      if (name %in% existing_names) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Account name already exists under this parent"
          )
        )
      }
      allocation <- suppressWarnings(as.numeric(allocation))

      if (is.na(allocation) || allocation < 0 || allocation > 1) {
        return(list(
          success = FALSE,
          status = 400,
          error = "Invalid allocation: must be a number between 0 and 1"
        ))
      }

      # Convert and instantiate
      allocation <- as.numeric(allocation)
      priority <- as.numeric(priority)
      fixed_amount <- as.numeric(fixed_amount)
      freq <- if (!is.null(freq)) as.numeric(freq) else NULL
      due_date <- if (!is.null(due_date)) as.POSIXct(due_date) else NULL

      # Determine which account type to create based on the parent
      if (
        inherits(parent, "MainAccount") && !inherits(parent, "ChildAccount")
      ) {
        child <- ChildAccount$new(
          name = name,
          allocation = allocation,
          priority = priority,
          status = status
        )
      } else {
        child <- GrandchildAccount$new(
          name = name,
          allocation = allocation,
          priority = priority,
          fixed_amount = fixed_amount,
          due_date = due_date,
          account_type = account_type,
          freq = freq,
          status = status
        )
      }

      parent$add_child_account(child)
      save_user_file(user_id, tree, "account_tree.Rds")

      list(
        success = TRUE,
        status = 200,
        message = paste(name, "added under", parent$name),
        child_type = class(child)[1],
        allocation = child$allocation
      )
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# -------------- Set_child_allocation endpoint ---------------------------------

#* @tag accounts
#* @post /set_child_allocation
#* @summary Update a child account's allocation
#* @description
#* Updates the allocation percentage for a specific child account under a given
#* parent account.
#* The allocation must be a numeric value between 0 and 1.
#*
#* @param parent_uuid:str* UUID of the parent account
#* @param child_name:str* Name of the child account
#* @param allocation:float* New allocation value (between 0 and 1)
#* @json


set_child_allocation <- function(req, res,
                                 parent_uuid,
                                 child_name,
                                 allocation = NULL) {
  user_id <- req$user_id
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")
      parent <- tree$find_account_by_uuid(parent_uuid)

      if (is.null(parent)) {
        return(
          list(
            success = FALSE,
            status = 404,
            error = "Parent account not found"
          )
        )
      }

      if (is.null(allocation) || allocation == "") {
        return(
          list(success = FALSE, status = 400, error = "Allocation is required")
        )
      }

      if (!(child_name %in% names(parent$child_accounts))) {
        return(
          list(
            success = FALSE,
            status = 404,
            error = "Child account not found under parent"
          )
        )
      }

      # Validate allocation
      allocation <- suppressWarnings(as.numeric(allocation))
      if (is.na(allocation) || allocation < 0 || allocation > 1) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Allocation must be a number between 0 and 1"
          )
        )
      }

      # Call method safely
      tryCatch({
        parent$set_child_allocation(child_name, allocation)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          message = paste(
            "Allocation for",
            child_name,
            "updated to",
            allocation
          ),
          total_allocation = parent$total_allocation
        )
      }, error = function(e) {
        list(success = FALSE, status = 400, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------- get_balance endpoint -------------------------------------

#* @tag accounts
#* @get /get_balance
#* @summary Get the current balance of an account
#* @description
#* Returns the current balance of an account specified by its UUID.
#* The account must belong to the authenticated user or be accessible by an admin.
#*
#* @param uuid:str* UUID of the account to fetch balance for
#* @json
get_balance <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # No lock needed for read-only operation
    tree <- load_user_file(user_id, "account_tree.Rds")
    account <- tree$find_account_by_uuid(uuid)

    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      balance = account$get_balance()
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------- get_transactions endpoint --------------------------------

#* @tag accounts
#* @get /get_transactions
#* @summary Get transaction history for an account
#* @description
#* Returns all past transactions (deposits, withdrawals, distributions, etc.)
#* for a given account UUID.
#* Only accessible by the account owner or an admin.
#*
#* @param uuid:str* UUID of the account whose transactions are requested
#* @json

get_transactions <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # No lock needed for read-only operation
    tree <- load_user_file(user_id, "account_tree.Rds")
    account <- tree$find_account_by_uuid(uuid)

    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    txns <- account$get_transactions()
    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      transaction_count = nrow(txns),
      transactions = txns
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------- list_child_accounts endpoint -----------------------------

#* @tag accounts
#* @get /list_child_accounts
#* @summary List child accounts under a parent
#* @description
#* Retrieves the names of all child accounts directly under the specified parent account.
#* Only accessible by the account owner or an admin.
#*
#* @param uuid:str* UUID of the parent account
#* @json

list_child_accounts <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # No lock needed (read-only)
    tree <- load_user_file(user_id, "account_tree.Rds")
    parent <- tree$find_account_by_uuid(uuid)

    if (is.null(parent)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Parent account not found or unauthorized"
      ))
    }

    children <- parent$list_child_accounts()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      account_name = parent$name,
      child_account_names = children,
      child_count = length(children)
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------- list_all_accounts endpoint -------------------------------
#* @tag accounts
#* @get /list_all_accounts
#* @summary List all accounts in the tree (upward and downward)
#* @description
#* Retrieves all accounts in the tree that are either children or ancestors
#* of the given UUID. Useful for complete hierarchical traversal starting at any node.
#* Only accessible by the account owner or an admin.
#*
#* @param uuid:str* UUID of the account (starting point for traversal)
#* @json
list_all_accounts <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Read-only operation — no lock needed
    tree <- load_user_file(user_id, "account_tree.Rds")
    account <- tree$find_account_by_uuid(uuid)

    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    all_names <- account$list_all_accounts()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      total_accounts = length(all_names),
      account_names = all_names
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------- find_account_by_name endpoint ----------------------------
#* @tag accounts
#* @get /find_account_by_name
#* @summary Search for accounts by name
#* @description
#* Returns a list of accounts matching a given name. Each result includes the UUID,
#*  account name, and its path in the account tree.
#* Accessible to the account owner or admin.
#*
#* @param name:str* Name of the account to search for
#* @json

find_account_by_name <- function(req, res, name = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account tree not found or unauthorized"
      ))
    }

    if (is.null(name)) {
      return(list(
        success = FALSE,
        status = 500,
        error = "Missing argument name"
      ))
    }

    matches <- tree$find_account(target_name = name)

    result_list <- lapply(matches, function(acct) {
      list(uuid = acct$uuid, name = acct$name, path = acct$path)
    })

    list(
      success = TRUE,
      status = 200,
      search_name = name,
      matches = result_list,
      total_matches = length(result_list)
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------- find_account_by_uuid endpoint ----------------------------
#* @tag accounts
#* @get /find_account_by_uuid
#* @summary Get detailed information of an account by UUID
#* @description
#* Returns detailed metadata about an account given its UUID. Includes account
#* name, path, balance, total balance, allocation, and parent UUID.
#* Accessible by the account owner or admin.
#*
#* @param uuid:str* UUID of the account to find
#* @json

find_account_by_uuid <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account tree not found or unauthorized"
      ))
    }

    # Ensure UUID provided
    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    acct <- tree$find_account_by_uuid(uuid)

    if (is.null(acct)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = acct$uuid,
      name = acct$name,
      path = acct$path,
      type = acct$type,
      balance = acct$balance,
      total_balance = acct$total_balance,
      amount_due = acct$amount_due,
      total_amount_due = acct$total_amount_due,
      allocation = acct$allocation,
      parent_uuid = if (!is.null(acct$parent)) acct$parent$uuid else NULL
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# --------------------- move_balance endpoint ----------------------------------

#* @tag accounts
#* @post /move_balance
#* @summary Move balance between accounts
#* @description
#* Transfers funds from one account to another. Both accounts must exist in the
#* tree and belong to the authenticated user (unless admin).
#* Ensures the amount is positive and the source account has sufficient balance.
#*
#* @param from_uuid:str* UUID of the source account
#* @param to_uuid:str* UUID of the destination account
#* @param amount:num* Amount to transfer (must be positive)
#* @json
move_balance <- function(
  req,
  res,
  from_uuid = NULL,
  to_uuid = NULL,
  amount = NULL
) {
  user_id <- req$user_id
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")

      # ✅ Validate presence of parameters
      if (is.null(from_uuid) || from_uuid == "") {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Source account (from_uuid) is required"
          )
        )
      }
      if (is.null(to_uuid) || to_uuid == "") {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Destination account (to_uuid) is required"
          )
        )
      }
      if (is.null(amount) || amount == "") {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Amount is required"
          )
        )
      }

      amount <- suppressWarnings(as.numeric(amount))
      if (is.na(amount) || amount <= 0) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Amount must be a positive number"
          )
        )
      }

      from_account <- tree$find_account_by_uuid(from_uuid)
      if (is.null(from_account)) {
        return(
          list(
            success = FALSE,
            status = 404,
            error = "Source account not found"
          )
        )
      }

      to_account <- tree$find_account_by_uuid(to_uuid)
      if (is.null(to_account)) {
        return(
          list(
            success = FALSE,
            status = 404,
            error = "Target account not found"
          )
        )
      }

      tryCatch({
        from_account$move_balance(
          target_account_uuid = to_uuid,
          amount = amount
        )
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          message = paste(
            "Moved",
            amount,
            "from",
            from_account$name,
            "to",
            to_account$name
          ),
          from_uuid = from_uuid,
          to_uuid = to_uuid,
          amount = amount
        )
      }, error = function(e) {
        list(success = FALSE, status = 400, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time, units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}

# --------------------- compute_total_balance endpoint -------------------------
#* @tag accounts
#* @get /compute_total_balance
#* @summary Compute total balance of an account
#* @description
#* Calculates the total balance of an account including its own balance and all
#* its descendants’ balances recursively.
#* Useful for checking the full balance under a top-level account or category.
#*
#* @param uuid:str* UUID of the account to compute total balance for
#* @json

compute_total_balance <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(uuid)) {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)

    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    total_balance <- account$compute_total_balance()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      total_balance = total_balance
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}



# --------------------- compute_total_due endpoint -----------------------------
#* @tag accounts
#* @get /compute_total_due
#* @summary Compute total amount due for an account
#* @description
#* Calculates the total amount due for a given account and all its descendant accounts.
#* Useful for understanding total obligations associated with a category or group.
#*
#* @param uuid:str* UUID of the account
#* @json
compute_total_due <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    library(tidyverse)
    if (is.null(uuid)) {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    tree <- load_user_file(user_id, "account_tree.Rds")
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account tree not found or unauthorized"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "Account not found or unauthorized"
      ))
    }

    total_due <- account$compute_total_due()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      total_due = total_due
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# --------------------- compute_total_due_within_days endpoint -----------------
#* @tag accounts
#* @get /compute_total_due_within_days
#* @summary Compute total due within the next N days
#* @description
#* Computes the total amount due for an account and its children that is expected
#* within the next `N` days. Useful for forecasting upcoming obligations.
#*
#* @param uuid:str* UUID of the account
#* @param days:int* Number of days to look ahead for due items
#* @json
compute_total_due_within_days <- function(req, res, uuid = NULL, days = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(success = FALSE, status = 400, error = "UUID is required"))
    }

    if (is.null(days) || days == "") {
      return(
        list(
          success = FALSE,
          status = 400,
          error = "Days parameter is required"
        )
      )
    }

    days <- suppressWarnings(as.numeric(days))
    if (is.na(days) || days < 0) {
      return(
        list(
          success = FALSE,
          status = 400,
          error = "Days must be a non-negative number"
        )
      )
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(success = FALSE, status = 404, error = "Account not found"))
    }

    total_due <- account$compute_total_due_within_n_days(days)

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      total_due = total_due,
      within_days = days
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}



# ------------------------ spending endpoint -----------------------------------
#* @tag accounts
#* @get /spending
#* @summary Compute total user spending (withdrawals) within a date range
#* @description
#* Computes the total amount withdrawn (spent) from a specified account and its
#* children
#* within a specified date range. If no date range is given, defaults to all time.
#*
#* @param uuid:str* UUID of the account to start the spending calculation
#* @param from:str  Start date (format: YYYY-MM-DD)
#* @param to:str    End date (format: YYYY-MM-DD)
#* @json

spending <- function(req, res, uuid = NULL, from = NULL, to = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    if (is.null(uuid)) {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    tree <- load_user_file(user_id, "account_tree.Rds")
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 404,
        error = "Account not found"
      ))
    }

    # Parse date range
    if (!is.null(from) && !is.null(to)) {
      daterange <- tryCatch({
        c(as.POSIXct(from), as.POSIXct(to))
      }, error = function(e) NULL)

      if (is.null(daterange)) {
        return(
          list(success = FALSE, status = 400, error = "Invalid date format")
        )
      }
    } else {
      daterange <- c(Sys.Date() - 365000, Sys.Date())
    }

    total_spending <- account$spending(daterange)

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      from = as.character(daterange[1]),
      to = as.character(daterange[2]),
      total_spending = total_spending
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------------ total_income endpoint -------------------------------
#* @tag accounts
#* @get /total_income
#* @summary Compute total income for an account and its children
#* @description
#* Calculates the total amount of income (deposits and credits) received by a
#* given account
#* and its children within a specified date range. Defaults to all time if no
#* range is provided.
#*
#* @param uuid:str* UUID of the account
#* @param from:str  Start date (format: YYYY-MM-DD)
#* @param to:str    End date (format: YYYY-MM-DD)
#* @json

total_income <- function(req, res, uuid = NULL, from = NULL, to = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    if (is.null(uuid)) {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    tree <- load_user_file(user_id, "account_tree.Rds")
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 404,
        error = "Account not found"
      ))
    }

    # Handle date range
    from_date <- tryCatch(as.POSIXct(from), error = function(e) NA)
    to_date <- tryCatch(as.POSIXct(to), error = function(e) NA)

    if (length(from_date) == 0 || is.na(from_date)) {
      from_date <- Sys.Date() - 365000
    }
    if (length(to_date) == 0 || is.na(to_date)) {
      to_date <- Sys.Date()
    }

    # Compute total income
    total_income <- account$total_income(c(from_date, to_date))

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      from = as.character(from_date),
      to = as.character(to_date),
      total_income = total_income
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------------ allocated_amount endpoint ---------------------------
#* @tag accounts
#* @get /allocated_amount
#* @summary Get total allocated amount for an account (including children)
#* @description
#* Returns the total allocated amount (deposits + user deposits in child accounts)
#* within a specified date range. If no dates are given, it defaults to all-time allocation.
#*
#* @param uuid:str* UUID of the account
#* @param from:str  Start date in format YYYY-MM-DD (optional)
#* @param to:str    End date in format YYYY-MM-DD (optional)
#* @json

allocated_amount <- function(req, res, uuid = NULL, from = NULL, to = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    # Find the account
    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 404,
        error = "Account not found"
      ))
    }

    # Parse and validate date range
    from_date <- tryCatch(as.POSIXct(from), error = function(e) NA)
    to_date <- tryCatch(as.POSIXct(to), error = function(e) NA)
    if (length(from_date) == 0 || is.na(from_date)) {
      from_date <- Sys.Date() - 365000
    }
    if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()

    # Call the method
    result_amount <- account$allocated_amount(c(from_date, to_date))

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      from = as.character(from_date),
      to = as.character(to_date),
      allocated_amount = result_amount
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------------ income_utilization endpoint -------------------------

#* @tag accounts
#* @get /income_utilization
#* @summary Compute income utilization of an account
#* @description
#* Returns the income utilization of an account and its children between the
#* specified date range.
#* Income utilization is computed as the ratio of total spending to total income.
#*
#* @param uuid:str* UUID of the account
#* @param from:str  Optional start date (`YYYY-MM-DD`)
#* @param to:str    Optional end date (`YYYY-MM-DD`)
#* @json

income_utilization <- function(req, res, uuid = NULL, from = NULL, to = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }
    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    # Find account
    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 404,
        error = "Account not found"
      ))
    }

    # Parse dates safely
    from_date <- tryCatch(as.POSIXct(from), error = function(e) NA)
    to_date <- tryCatch(as.POSIXct(to), error = function(e) NA)

    if (length(from_date) == 0 || is.na(from_date)) {
      from_date <- Sys.Date() - 365000
    }
    if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()

    daterange <- c(from_date, to_date)

    utilization <- account$income_utilization(daterange)

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      from = as.character(from_date),
      to = as.character(to_date),
      utilization = utilization
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------------ walking_amount endpoint -----------------------------

#* @tag accounts
#* @get /walking_amount
#* @summary Compute walking amount (amount_due or balance)
#* @description
#* Computes the "walking amount" (e.g. `amount_due` or `balance`) for an account
#*  and its children over a date range.
#* Commonly used for visualizing cumulative growth of liabilities or funds over time.
#*
#* @param uuid:str* UUID of the account
#* @param amt_type:str Type of amount to track: "amount_due" or "balance".
#* Default is "amount_due".
#* @param from:str Optional start date (`YYYY-MM-DD`)
#* @param to:str Optional end date (`YYYY-MM-DD`)
#* @json

walking_amount <- function(req, res) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  # Extract query parameters
  uuid <- req$argsQuery$uuid %||% NULL
  amt_type <- req$argsQuery$amt_type %||% "amount_due"
  from <- req$argsQuery$from %||% NULL
  to <- req$argsQuery$to %||% NULL

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")
    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    # Parse and validate dates
    from_date <- tryCatch(as.POSIXct(from), error = function(e) NA)
    to_date <- tryCatch(as.POSIXct(to), error = function(e) NA)
    if (length(from_date) == 0 || is.na(from_date)) {
      from_date <- Sys.Date() - 365000
    }
    if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()
    daterange <- c(from_date, to_date)

    # Compute result
    value <- account$walking_amount(amt_type = amt_type, daterange = daterange)

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      amt_type = amt_type,
      from = as.character(from_date),
      to = as.character(to_date),
      walking_amount = value
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# =============================================================================
# second  tier end points
# These apply to second tier account types ( Children, Grand children)
# =============================================================================

# ------------------change_account_status endpoint -----------------------------

#* @tag accounts
#* @post /change_account_status
#* @summary Change account status (child and grandchild accounts only)
#* @description
#* Change the status of a specific child or grandchild account. Valid statuses are
#*  `"active"`, `"inactive"`, or `"closed"`.
#* Only applicable to `ChildAccount` instances.
#*
#* @param uuid:str* UUID of the account to update
#* @param status:str* New status to apply ("active", "inactive", "closed")
#* @json

change_account_status <- function(req, res, uuid = NULL, status = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      # Load the tree
      tree <- load_user_file(user_id, "account_tree.Rds")

      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      if (is.null(uuid) || uuid == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "UUID is required"
        ))
      }

      if (is.null(status) || status == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "Status is required"
        ))
      }

      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        return(list(
          success = FALSE,
          status = 404,
          error = "Account not found"
        ))
      }

      if (!inherits(account, "ChildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = paste(
            "This operation is only allowed on child or grandchild accounts"
          )
        ))
      }

      result <- tryCatch({
        account$change_status(status)
        save_user_file(user_id, tree, "account_tree.Rds")  # persist change

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          new_status = account$status,
          message = paste(
            "Account",
            account$name,
            "status updated to",
            account$status
          )
        )
      }, error = function(e) {
        list(
          success = FALSE,
          status = 400,
          error = conditionMessage(e),
          uuid = uuid
        )
      })

      result
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}


# ------------------get_account_status endpoint -----------------------------

#* @tag accounts
#* @get /get_account_status
#* @summary Get the status of a child or grandchild account
#* @description
#* Retrieves the current status of a specific child or grandchild account.
#* Only applicable to accounts that inherit from `ChildAccount`.
#*
#* @param uuid:str* UUID of the account
#* @json
get_account_status <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    # Check class inheritance
    if (!inherits(account, "ChildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "This operation is only allowed on child or grandchild accounts"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      account_status = account$get_account_status()
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}

# ------------------set_priority endpoint -----------------------------

#* @tag accounts
#* @post /set_priority
#* @summary Set priority for a child or grandchild account
#* @description
#* Updates the priority of a child or grandchild account. This can be used to
#* control allocation or ordering logic within parent accounts.
#* Only applicable to accounts that inherit from `ChildAccount`.
#*
#* @param uuid:str* The UUID of the account
#* @param priority:str* The new priority value (numeric or string label)
#* @json

set_priority <- function(req, res, uuid = NULL, priority = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      # Load account tree
      tree <- load_user_file(user_id, "account_tree.Rds")

      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      if (is.null(uuid) || uuid == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "UUID is required"
        ))
      }

      if (is.null(priority) || priority == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "Priority is required"
        ))
      }

      account <- tree$find_account_by_uuid(uuid)
      if (is.null(account)) {
        return(list(
          success = FALSE,
          status = 403,
          error = "Account not found"
        ))
      }

      if (!inherits(account, "ChildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = paste(
            "This operation is only allowed",
            "on child or grandchild accounts"
          )
        ))
      }

      tryCatch({
        account$set_priority(priority)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          message = paste("Priority set to", priority),
          uuid = uuid,
          priority = priority
        )
      }, error = function(e) {
        list(success = FALSE, status = 400, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}

# ------------------get_priority endpoint -------------------------------------

#* @tag accounts
#* @get /get_priority
#* @summary Get the priority of a child or grandchild account
#* @description
#* Returns the current priority value of a child or grandchild account. This is useful
#* for understanding ordering or allocation behavior within a parent account.
#* Only applicable to accounts that inherit from `ChildAccount`.
#*
#* @param uuid:str* UUID of the account
#* @json

get_priority <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    if (!inherits(account, "ChildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Priority only available for child or grandchild accounts"
      ))
    }

    priority <- account$get_priority()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      priority = priority
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# =============================================================================
# Third  tier end points
# These apply to third tier account types (Grand children)
# =============================================================================

# ------------------set_due_date endpoint -------------------------------------

#* @tag accounts
#* @post /set_due_date
#* @summary Set due date for a grandchild account
#* @description
#* Assigns or updates a due date for a grandchild account. The due date is typically used
#* to determine when a financial obligation or allocation is expected. This operation is only
#* valid for accounts inheriting from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @param due_date:str* The due date in ISO date-time format (e.g., "2025-08-15T00:00:00Z")
#* @json

set_due_date <- function(req, res, uuid = NULL, due_date = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      # Load the user's account tree
      tree <- load_user_file(user_id, "account_tree.Rds")

      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      if (is.null(uuid) || uuid == "") {
        return(list(success = FALSE, status = 400, error = "UUID is required"))
      }

      if (is.null(due_date) || due_date == "") {
        return(
          list(success = FALSE, status = 400, error = "Due date is required")
        )
      }

      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        return(list(success = FALSE, status = 403, error = "Account not found"))
      }

      # Check if it's a grandchild (inherits from GrandchildAccount)
      if (!inherits(account, "GrandchildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = "Due date only applicable to grandchild accounts"
        ))
      }

      # Parse due date
      parsed_date <- tryCatch(as.POSIXct(due_date), error = function(e) NA)
      if (length(parsed_date) == 0 || is.na(parsed_date)) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Invalid due date format"
          )
        )
      }

      # Call method
      account$set_due_date(parsed_date)

      # Save updated tree
      save_user_file(user_id, tree, "account_tree.Rds")

      list(
        success = TRUE,
        status = 200,
        message = paste("Due date set for", account$name),
        uuid = uuid,
        due_date = as.character(parsed_date)
      )
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(
        Sys.time(),
        start_time,
        units = "secs"
      )
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}


# ------------------get_due_date endpoint -------------------------------------

#* @tag accounts
#* @get /get_due_date
#* @summary Get due date of a grandchild account
#* @description
#* Retrieves the due date associated with a specific grandchild account.
#* This can be used to check when the account's payment is expected.
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @json
get_due_date <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    if (!inherits(account, "GrandchildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Due date only applicable to grandchild accounts"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      due_date = as.character(account$get_due_date())
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------set_fixed_amount endpoint ----------------------------------

#* @tag accounts
#* @post /set_fixed_amount
#* @summary Set fixed amount for a grandchild account
#* @description
#* Sets the fixed amount for a grandchild account. This value is used
#* to compute the expected `amount_due` per period.
#*
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @param fixed_amount:num* Numeric fixed amount to set per period
#* @json
set_fixed_amount <- function(req, res, uuid = NULL, fixed_amount = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      # Validate presence of UUID and amount
      if (is.null(uuid) || uuid == "") {
        return(list(success = FALSE, status = 400, error = "UUID is required"))
      }
      if (is.null(fixed_amount) || fixed_amount == "") {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Fixed amount is required"
          )
        )
      }

      # Parse amount safely
      fixed_amount <- suppressWarnings(as.numeric(fixed_amount))
      if (is.na(fixed_amount) || fixed_amount < 0) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Fixed amount must be a non-negative number"
          )
        )
      }

      # Load tree
      tree <- load_user_file(user_id, "account_tree.Rds")
      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      acct <- tree$find_account_by_uuid(uuid)
      if (is.null(acct)) {
        return(list(success = FALSE, status = 403, error = "Account not found"))
      }

      # Check that it's a grandchild (inherits from GrandchildAccount)
      if (!inherits(acct, "GrandchildAccount") || is.null(acct$parent)) {
        return(
          list(
            success = FALSE,
            status = 403,
            error = "Fixed amount only applicable to grandchild accounts"
          )
        )
      }

      # Call method
      tryCatch({
        acct$set_fixed_amount(fixed_amount)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          fixed_amount = fixed_amount,
          amount_due = acct$amount_due
        )
      }, error = function(e) {
        list(success = FALSE, status = 400, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------get_fixed_amount endpoint ----------------------------------

#* @tag accounts
#* @get /get_fixed_amount
#* @summary Get fixed amount for a grandchild account
#* @description
#* Returns the currently set fixed amount for a grandchild account.
#* This value determines the expected `amount_due` per period.
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @json
get_fixed_amount <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    # Validate class inheritance
    if (!inherits(account, "GrandchildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "This endpoint is only applicable to grandchild accounts"
      ))
    }

    fixed_amount <- account$get_fixed_amount()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      fixed_amount = fixed_amount
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}

# ------------------set_account_type endpoint ----------------------------------
#* @tag accounts
#* @post /set_account_type
#* @summary Set account type for a grandchild account
#* @description
#* Updates the type/category of a grandchild account.
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @param account_type:str* The new account type label or code
#* @json
set_account_type <- function(req, res, uuid = NULL, account_type = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      # Load tree
      tree <- load_user_file(user_id, "account_tree.Rds")

      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      # UUID must be provided
      if (is.null(uuid) || uuid == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "UUID is required"
        ))
      }

      # account_type must be provided
      if (is.null(account_type) || account_type == "") {
        return(list(
          success = FALSE,
          status = 400,
          error = "Account type is required"
        ))
      }

      # Find account
      account <- tree$find_account_by_uuid(uuid)
      if (is.null(account)) {
        return(list(
          success = FALSE,
          status = 403,
          error = "Account not found"
        ))
      }

      # Check class
      if (!inherits(account, "GrandchildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = "This endpoint is only applicable to grandchild accounts"
        ))
      }

      # Attempt update
      tryCatch({
        account$set_account_type(account_type)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          message = paste("Account type updated to", account_type),
          uuid = uuid,
          account_type = account$account_type
        )
      }, error = function(e) {
        list(success = FALSE, status = 500, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# -----------------get_account_type endpoint ----------------------------------
#* @tag accounts
#* @get /get_account_type
#* @summary Get the account type of a grandchild account
#* @description
#* Returns the account type of a grandchild account (e.g., "rent", "loan", "utility").
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @json
get_account_type <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    if (!inherits(account, "GrandchildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "This method is only applicable to grandchild accounts"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      account_type = account$get_account_type()
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# -----------------set_account_freq endpoint ----------------------------------
#* @tag accounts
#* @post /set_account_freq
#* @summary Set frequency for a grandchild account
#* @description
#* Updates the frequency of a grandchild account — useful for budgeting intervals like
#* "Weekly", "Monthly", "Quarterly", etc. Only applies to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* UUID of the grandchild account
#* @param account_freq:str* New frequency (e.g., "Monthly", "Weekly")
#* @json
function(req, res, uuid = NULL, account_freq = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")

      # Validate UUID
      if (is.null(uuid) || uuid == "") {
        return(list(success = FALSE, status = 400, error = "UUID is required"))
      }

      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        return(list(success = FALSE, status = 403, error = "Account not found"))
      }

      # Validate class
      if (!inherits(account, "GrandchildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = "This endpoint is only applicable to grandchild accounts"
        ))
      }

      # Validate frequency
      if (is.null(account_freq) || account_freq == "") {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "account_freq is required"
          )
        )
      }

      # Apply method
      tryCatch({
        account$set_account_freq(account_freq)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          message = paste(
            "Frequency for",
            account$name,
            "set to",
            account_freq
          ),
          uuid = uuid,
          freq = account_freq
        )
      }, error = function(e) {
        list(success = FALSE, status = 500, error = conditionMessage(e))
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# -----------------get_account_freq endpoint ----------------------------------
#* @tag accounts
#* @get /get_account_freq
#* @summary Get frequency of a grandchild account
#* @description
#* Retrieves the frequency (e.g., "Monthly", "Weekly") set for a grandchild account.
#* Only works for accounts inheriting from `GrandchildAccount`.
#*
#* @param uuid:str* The UUID of the grandchild account
#* @json
get_account_freq <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      ))
    }

    account <- tree$find_account_by_uuid(uuid)

    if (is.null(account)) {
      return(list(
        success = FALSE,
        status = 403,
        error = "Account not found"
      ))
    }

    # Check class is GrandchildAccount
    if (!inherits(account, "GrandchildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "This endpoint is only applicable to grandchild accounts"
      ))
    }

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      freq = account$get_account_freq()
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}



# ------------------set_account_periods endpoint -------------------------------
#* @tag accounts
#* @post /set_account_periods
#* @summary Set number of periods for a grandchild account
#* @description
#* Sets the number of budgeting or billing periods for a grandchild account.
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* The UUID of the grandchild account
#* @param periods:int* Number of periods (positive integer)
#* @json

set_account_periods <- function(req, res, uuid = NULL, periods = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    with_account_lock(user_id, {
      tree <- load_user_file(user_id, "account_tree.Rds")

      if (is.null(tree)) {
        return(list(
          success = FALSE,
          status = if (role == "admin") 404 else 403,
          error = "User not found or unauthorized access"
        ))
      }

      if (is.null(uuid) || uuid == "") {
        return(list(success = FALSE, status = 400, error = "UUID is required"))
      }

      account <- tree$find_account_by_uuid(uuid)

      if (is.null(account)) {
        return(list(success = FALSE, status = 403, error = "Account not found"))
      }

      if (!inherits(account, "GrandchildAccount")) {
        return(list(
          success = FALSE,
          status = 403,
          error = "This endpoint is only applicable to grandchild accounts"
        ))
      }

      # Validate periods
      periods <- suppressWarnings(as.integer(periods))
      if (is.na(periods) || periods <= 0) {
        return(
          list(
            success = FALSE,
            status = 400,
            error = "Invalid number of periods"
          )
        )
      }

      tryCatch({
        account$set_account_periods(periods)
        save_user_file(user_id, tree, "account_tree.Rds")

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          periods = periods,
          message = paste("Number of periods set to", periods)
        )
      }, error = function(e) {
        list(success = FALSE, status = 400, error = e$message)
      })
    })
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
    )
  })
}


# ------------------get_account_periods endpoint -------------------------------
#* @tag accounts
#* @get /get_account_periods
#* @summary Get number of periods for a grandchild account
#* @description
#* Retrieves the number of budgeting or billing periods defined for a grandchild account.
#* Only applicable to accounts that inherit from `GrandchildAccount`.
#*
#* @param uuid:str* The UUID of the grandchild account
#* @json
get_account_periods <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  future({
    # Load account tree
    tree <- load_user_file(user_id, "account_tree.Rds")

    if (is.null(tree)) {
      return(list(
        success = FALSE,
        status = if (role == "admin") 404 else 403,
        error = "User not found or unauthorized access"
      ))
    }

    if (is.null(uuid) || uuid == "") {
      return(list(success = FALSE, status = 400, error = "UUID is required"))
    }

    account <- tree$find_account_by_uuid(uuid)
    if (is.null(account)) {
      return(list(success = FALSE, status = 403, error = "Account not found"))
    }

    if (!inherits(account, "GrandchildAccount")) {
      return(list(
        success = FALSE,
        status = 403,
        error = "This method is only applicable to grandchild accounts"
      ))
    }

    periods <- account$get_account_periods()

    list(
      success = TRUE,
      status = 200,
      uuid = uuid,
      periods = periods
    )
  }) %...>% (function(result) {
    result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
    result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
    result$execution_time <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    res$status <- result$status %||% 200
    result
  }) %...!% (function(err) {
    res$status <- 500
    list(
      success = FALSE,
      error = conditionMessage(err),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(
        difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )
      )
    )
  })
}
