library(plumber)
library(future)
library(promises)
library(rlang)
plan(multisession)


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


#* Health check endpoint
#* @get /__ping__
ping<-function() {
  list(status = "ok")
}


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






#* @post /distribute
#* @param uuid UUID of the parent account
#* @param amount Amount to distribute
#* @param transaction Transaction reference (optional)
#* @param by Initiator (default: "System")
#* @json
distribute <- function(req, res,
                       uuid,
                       amount,
                       transaction = NULL,
                       by = "System"
                       ) {
  user_id <- req$user_id
  role <- req$role
  
  # Load the account tree
  tree <- load_user_account(user_id)
  account <- tree$find_account_by_uuid(uuid)
  
  if (is.null(account)) {
    res$status <- if (role == "admin") 404 else 403
    return(list(error = "Account not found or unauthorized"))
  }
  
  # Proceed with distribution
  tryCatch({
    account$distribute_to_children(
      amount = as.numeric(amount),
      transaction = if (is.null(transaction)) paste0(
        "dist-", Sys.time()
      ) else transaction,
      by = by
    )
    
    save_user_account(user_id, tree)
    
    list(
      success = TRUE,
      message = paste("Distributed", amount, "from", account$name),
      uuid = uuid
    )
  }, error = function(e) {
    res$status <- 500
    list(success = FALSE, error = e$message)
  })
}


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
                              status="active"
                              ) {
  
  user_id <- req$user_id
  tree <- load_user_account(user_id)
  parent <- tree$find_account_by_uuid(parent_uuid)
  
  if (is.null(parent)) {
    res$status <- 404
    return(list(success = FALSE, error = "Parent account not found"))
  }
  
  tryCatch({
    # Choose correct account class
    child_class <- if (inherits(parent, "MainAccount")) {
      ChildAccount
    } else {
      GrandchildAccount
    }
    
    # Convert inputs to proper types
    allocation <- as.numeric(allocation)
    priority <- as.numeric(priority)
    fixed_amount <- as.numeric(fixed_amount)
    freq <- if (!is.null(freq)) as.numeric(freq) else NULL
    due_date <- if (!is.null(due_date)) as.POSIXct(due_date) else NULL
    
    # Instantiate account
    child <- if (identical(child_class, ChildAccount)) {
      ChildAccount$new(
        name = name,
        allocation = allocation,
        priority = priority,
        status=status
      )
    } else {
      GrandchildAccount$new(
        name = name,
        allocation = allocation,
        priority = priority,
        fixed_amount = fixed_amount,
        due_date = due_date,
        account_type = account_type,
        freq = freq,
        status=status
      )
    }
    
    parent$add_child_account(child)
    save_user_account(user_id, tree)
    
    list(
      success = TRUE,
      message = paste(name, "added under", parent$name),
      child_type = class(child)[1],
      allocation = child$allocation
    )
    
  }, error = function(e) {
    res$status <- 400
    list(success = FALSE, error = e$message)
  })
}

