library(plumber)
library(future)
library(promises)
library(tidyverse)
library(rlang)
plan(multisession)


# -------------- Auth Filters --------------------------------------------------

#* @filter auth
function(req, res) {
  # Skip auth for the health check
  if (req$PATH_INFO == "/__ping__") {
    return(forward())
  }

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

  if (is.null(decoded)) {
    res$status <- 401
    return(list(error = "Invalid or expired token"))
  }

  req$user_id <- decoded$user_id
  req$role <- decoded$role
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

#* endpoints/deposit.R
#* @post /deposit
#* @param uuid Account UUID (required)
#* @param amount Deposit amount (required)
#* @param channel Deposit channel (required)
#* @param transaction_number Optional transaction number
#* @param by Who performed the deposit (default = "User")
#* @param date Timestamp of deposit (default = now)
#* endpoints/deposit.R
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

#* endpoints/withdraw.R
#* @post /withdraw
#* @param uuid Account UUID (required)
#* @param amount Withdrawal amount (required)
#* @param channel Withdrawal channel (required)
#* @param transaction_number Optional transaction number
#* @param by Who performed the withdrawal (default = "User")
#* @param date Timestamp of withdrawal (default = now)
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

#* @post /distribute
#* @param uuid UUID of the parent account
#* @param amount Amount to distribute
#* @param transaction Transaction reference (optional)
#* @param by Initiator (default: "System")
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

#* Add a child or grandchild account under a parent
#* @post /add_sub_account
#* @param parent_uuid The UUID of the parent account
#* @param name The name of the child account
#* @param allocation The allocation fraction (e.g., 0.3)
#* @param priority Optional priority weight (default: 0)
#* @param fixed_amount Optional (used only for grandchild)
#* @param due_date Optional (ISO string)
#* @param account_type Optional (used only for grandchild)
#* @param freq Optional (used only for grandchild)
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

#* Update a child account's allocation under a parent
#* @post /set_child_allocation
#* @param parent_uuid UUID of the parent account
#* @param child_name Name of the child account
#* @param allocation New allocation value (between 0 and 1)
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

#* Get balance of an account
#* @get /get_balance
#* @param uuid UUID of the account
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

#* Get transaction history of an account
#* @get /get_transactions
#* @param uuid UUID of the account
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

#* List child accounts under a parent
#* @get /list_child_accounts
#* @param uuid UUID of the parent account
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
#* List all accounts in the tree (both up and down)
#* @get /list_all_accounts
#* @param uuid UUID of the account (start point for traversal)
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
#* Find accounts by name
#* @get /find_account_by_name
#* @param name Name of the account to search for
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
#* Find account by UUID
#* @get /find_account_by_uuid
#* @param uuid UUID of the account to find
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

#* Move balance from one account to another
#* @post /move_balance
#* @param from_uuid UUID of the source account
#* @param to_uuid UUID of the destination account
#* @param amount Amount to move
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
#* Compute total balance of an account (self + children)
#* @get /compute_total_balance
#* @param uuid UUID of the account to compute total balance for
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
#* Compute total amount due for an account (including all children)
#* @get /compute_total_due
#* @param uuid UUID of the account
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
#* Compute total due within next N days
#* @get /compute_total_due_within_days
#* @param uuid UUID of the account
#* @param days Number of days to check due amounts within
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
#* Compute total user spending (withdrawals) within date range
#* @get /spending
#* @param uuid UUID of the account to start the spending calculation
#* @param from Start date (format: YYYY-MM-DD)
#* @param to End date (format: YYYY-MM-DD)
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
#* Compute total income for an account and its children
#* @get /total_income
#* @param uuid UUID of the account
#* @param from Start date (YYYY-MM-DD)
#* @param to End date (YYYY-MM-DD)
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
#* Get total allocated amount (deposits + user deposits in children)
#* @get /allocated_amount
#* @param uuid UUID of the account
#* @param from Start date (YYYY-MM-DD)
#* @param to End date (YYYY-MM-DD)
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

#* Compute income utilization of an account
#* @get /income_utilization
#* @param uuid UUID of the account
#* @param from Optional start date (e.g. 2024-01-01)
#* @param to Optional end date (e.g. 2024-12-31)
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

#* Compute walking amount (amount_due or balance)
#* @get /walking_amount
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
# These apply to second tier account types ( Child, Grand child)
# =============================================================================

# ------------------change_account_status endpoint -----------------------------

#* Change account status (child and grandchild accounts only)
#* @post /change_account_status
#* @param uuid UUID of the account to update
#* @param status New status to apply ("active", "inactive", "closed")
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

#* Get the status of a child or grandchild account
#* @get /get_account_status
#* @param uuid Account UUID
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

#* Set priority for a child or grandchild account
#* @post /set_priority
#* @param uuid The UUID of the account
#* @param priority The new priority value (can be string or number)
#* @form
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

#* Get the priority of a child or grandchild account
#* @get /get_priority
#* @param uuid UUID of the account
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
