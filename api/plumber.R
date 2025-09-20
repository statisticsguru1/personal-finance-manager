library(plumber)
library(future)
library(promises)
library(tidyverse)
library(rlang)
plan(multisession)
pkgs_required <- c("tidyverse", "rlang","finman")

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

MAX_REQUESTS <- as.numeric(Sys.getenv("MAX_REQUESTS", unset = 180000))
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
  ip <- req$REMOTE_ADDR %||% ""
  if (!nzchar(ip)) ip <- "unknown"

  if (req$PATH_INFO %in% c("/__ping__","/login","/generate_access_token")) {
    # Apply rate limit based on IP even if auth is skipped
    if (!check_rate_limit(ip)) {
      res$status <- 429
      return(
        list(
          success=FALSE,
          status =429,
          error = "Rate limit exceeded. Try again later."
        )
      )
    }
    return(forward())
  }

  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
    res$status <- 401
    return(
      list(
        success=FALSE,
        status=401,
        error = "Missing or invalid token"
      )
    )
  }

  secret_key <- Sys.getenv("JWT_SECRET")
  if (secret_key == "") {
    res$status <- 500
    return(
      list(
        success=FALSE,
        status=500,
        error = "Server misconfigured (missing JWT_SECRET)"
      )
    )
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
            success=FALSE,
            status = 429,
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
    return(
      list(
        success=FALSE,
        status=401,
        error = "Invalid or expired token"
      )
    )
  }

  # ─────── Successful Auth: Enforce Rate Limit ───────
  user_id <- decoded$user_id
  req$user_id <- user_id
  req$role <- decoded$role

  if (!check_rate_limit(user_id)) {
    res$status <- 429
    return(
      list(
        success=FALSE,
        status=429,
        error = "Rate limit exceeded. Try later."
      )
    )
  }

  forward()
}

# -------------- Auth token --------------------------------------------------
#' @tag Authorization #'
#' @post /generate_access_token
#' @summary Generate a new access token
#' @description
#' Issues a JWT token (session-based or expiry-based).
#' Server enforces secret from env. Both user_id and role are required.
#'
#' @param req,res Plumber request/response objects
#' @param user_id The subject/user for whom the token is issued (required)
#' @param role The role assigned to the token (required)
#' @param type Either "session" or "expiry" (default "session")
#' @param session_id Required if type = "session"
#' @param exp Expiration time (POSIXct or numeric), required if type = "expiry"
#' @param ... Additional custom claims (forwarded to [issue_token()])
#' @return JSON response containing token and metadata
#'
token_endpoint <- function(req, res,
                           user_id = NULL,
                           role = NULL,
                           type = "Expiration",
                           size = 256,
                           header = NULL,
                           iss = NULL,
                           sub = NULL,
                           aud = NULL,
                           exp = NULL,
                           nbf = NULL,
                           iat = Sys.time(),
                           jti = NULL,
                           session_id = NULL,
                           ...) {
  timestamp <- Sys.time()
  secret <- Sys.getenv("JWT_SECRET")

  # --- Input validation ---------------------------------------------------
  if (missing(user_id) || is.null(user_id) || user_id == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "user_id is required",
      timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
    ))
  }

  if (missing(role) || is.null(role) || role == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "role is required",
      timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
    ))
  }

  if (type == "session" && is.null(session_id)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "session_id is required for session tokens",
      timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
    ))
  }

  if (type != "session" && is.null(exp)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "exp is required for expiry tokens",
      timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
    ))
  }

  if (secret == "") {
    res$status <- 500
    return(list(
      success = FALSE,
      status = 500,
      error = "Server misconfigured (missing JWT_SECRET)",
      timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
    ))
  }

  # --- Core handler -------------------------------------------------------
  tryCatch({

    # --- Normalize NumericDate claims ---------------------------------------
    if (!is.null(exp)) exp <- coerce_numeric_date(exp)
    if (!is.null(nbf)) nbf <- coerce_numeric_date(nbf)
    if (!is.null(iat)) iat <- coerce_numeric_date(iat)

    future_promise({
      tryCatch({
        token <- issue_token(
          type = type,
          secret=secret,
          size = size,
          header = header,
          iss = iss,
          sub = sub,
          aud = aud,
          exp = exp,
          nbf = nbf,
          iat = iat,
          jti = jti,
          session_id = session_id,
          role = role,
          user_id =user_id,
          ...
        )
        res$status <- 200
        list(
          success = TRUE,
          status = 200,
          user_id = user_id,
          token = token,
          timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
        )
      }, error = function(e) {
        res$status <- 500
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to issue token:", conditionMessage(e)),
          timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z")
        )
      }
      )
    }, packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      res$status <- result$status %||% 500
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(
        success = FALSE,
        status=503,
        error = paste("Token generation Failed:", conditionMessage(err))
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      error = paste("Failed to connect:", conditionMessage(e))
    )
  }
  )
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


# -------------- Register endpoint ----------------------------------------------

#' @tag accounts
#' @post /register
#' @summary Register a new user account
#' @description
#' Initializes a new user main account with optional initial balance.
#' Returns the account's UUID upon successful creation.
#'
#' @param user_id:str* Unique user identifier (letters, digits, underscores)
#' @param initial_balance:float Optional starting balance (default = 0)
#* @json
register <- function(req, res, user_id = NULL, initial_balance = 0) {
  start_time <- Sys.time()
  # --- Input validation BEFORE future --------------------------------------

  if (missing(user_id) || is.null(user_id) || user_id == "") {
    res$status<-400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "user_id is required"
      )
    )
  }

  # ---- core handler ------------------------------------------------------
  tryCatch({
    future_promise({
      result <- tryCatch({
        uuid <- create_user_account_base(
          user_id = user_id,
          initial_balance = as.numeric(initial_balance)
        )
        list(
          success = TRUE,
          status = 200,
          user_id = user_id,
          uuid = uuid
        )
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Account creation Failed:", conditionMessage(e))
        )
      })

      result
    }, packages=pkgs_required, seed=T) %...>% (function(result){
      end_time <- Sys.time()
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      res$status <- result$status %||% 500
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to connect:", conditionMessage(err)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(difftime(end_time, start_time, units = "secs"))
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(success = FALSE,status=500, error = paste("Failed to Connect:", conditionMessage(e)))
  })
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
#* @param initiated_by:str Who performed the deposit (default = "User")
#* @param transaction_date:str Optional timestamp of deposit
#* @json
deposit <- function(req, res,
                    uuid,
                    amount,
                    channel,
                    transaction_number = NULL,
                    initiated_by = "User",
                    transaction_date = NULL) {

  start_time <- Sys.time()

  user_id <- req$user_id
  role    <- req$role

  # --- Input validation BEFORE future --------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(success = FALSE, error = "uuid is required"))
  }

  if (missing(amount)) {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "amount is required"))
  }

  amt <- suppressWarnings(as.numeric(amount))
  if (is.na(amt)) {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "amount must be numeric"))
  }

  if (amt <= 0) {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "amount must be > 0"))
  }

  if (missing(channel) ||is.null(channel) || channel == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "channel is required"))
  }

   # Parse dates safely
  transaction_date <- tryCatch(
    safe_parse_date(transaction_date),
    error = function(e) Sys.time()  # fallback to now
  )

  # --- Future starts after inputs validated -------------------------------
  tryCatch({
    future_promise({
      result <- tryCatch({
        with_account_lock(user_id, {
          tree <- load_user_file(user_id, "account_tree.Rds")
          account <- tree$find_account_by_uuid(uuid)

          if (is.null(account)) {
            list(
              success = FALSE,
              status  = if (role == "admin") 404 else 403,
              error   = "Account not found or unauthorized"
            )
          } else {
            account$deposit(
              amount = amt,
              transaction_number = transaction_number,
              by = initiated_by,
              channel = channel,
              transaction_date = transaction_date
            )

            save_user_file(user_id, tree, "account_tree.Rds")

            list(
              success      = TRUE,
              status       = 200,
              account_uuid = uuid,
              amount       = amt,
              balance      = account$balance
            )
          }
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status  = 500,
          error   = paste("Deposit failed:", conditionMessage(e)),
          debug   = list(
            date_class = class(transaction_date),
            date_value = transaction_date,
            data_value1 = as.POSIXct(transaction_date)
          )
        )
      })

      result
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      end_time <- Sys.time()
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(end_time,   "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      res$status <- result$status %||% 500
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(
        success = FALSE,
        error   = paste("Deposit Failed:", conditionMessage(err)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time   = format(end_time,   "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(difftime(end_time, start_time, units = "secs"))
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
#* @param initiated_by:str Who performed the withdrawal (default = "User")
#* @param transaction_date:str Timestamp of withdrawal (default = now)
withdraw <- function(req, res,
                     uuid,
                     amount,
                     channel,
                     transaction_number = NULL,
                     initiated_by = "User",
                     transaction_date = Sys.time()) {
  start_time <- Sys.time()
  user_id <- req$user_id
  role <- req$role

  # ----- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "uuid is required"
      )
    )
  }
  if (missing(amount)) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "amount is required"
      )
    )
  }
  amount <- suppressWarnings(as.numeric(amount))
  if (is.na(amount) || amount <= 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid withdrawal amount",
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  }

  if (missing(channel) || is.null(channel) || channel == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "channel is required",
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  }
  # Safe date parsing
  #  Date parsing
  transaction_date <- tryCatch(
    safe_parse_date(transaction_date),
    error = function(e) Sys.time()  # fallback to now
  )

  # --- Core Handler ---------------------------------------------------

  tryCatch({
    future_promise({
      tryCatch({
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
              by = initiated_by,
              channel = channel,
              transaction_date = transaction_date
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
      }, error = function(e) {
        error_message <- conditionMessage(e)
        if (
          grepl(
            "Total allocation exceeds 100%", error_message,
            ignore.case = TRUE
          )|
          grepl(
            "Insufficient balance! Your current balance",
            error_message,
            ignore.case = TRUE
          )|
          grepl(
            "Insufficient balance in",
            error_message,
            ignore.case = TRUE
          )
        ) {
          return(list(
            success = FALSE,
            status = 400,
            error = paste("Failed:", error_message)
          ))
        }
        return(list(
          success = FALSE,
          status = 500,
          error = paste("Failed:", error_message)
        ))
      })
    }, packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      end_time <- Sys.time()
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(
        success = FALSE,
        status = 503,
        error = conditionMessage(err),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(difftime(end_time, start_time, units = "secs"))
      )
    })

  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e)),
      start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
      end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
      execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
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
#* @param initiated_by:str Initiator of the distribution (default: "System")
#* @json
distribute <- function(req, res,
                       uuid,
                       amount = NULL,
                       transaction = NULL,
                       initiated_by = "System") {
  start_time <- Sys.time()

  user_id <- req$user_id
  role    <- req$role

  # --- Input Validation ---------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(success = FALSE, error = "uuid is required"))
  }

  if (missing(amount)) {
    res$status <- 400
    return(list(success = FALSE, error = "amount is required"))
  }

  amt <- suppressWarnings(as.numeric(amount))
  if (is.na(amt)) {
    res$status <- 400
    return(list(success = FALSE, error = "amount must be numeric"))
  }

  if (amt <= 0) {
    res$status <- 400
    return(list(success = FALSE, error = "amount must be > 0"))
  }

  # --- Core Handler -------------------------------------------
  tryCatch({
    future_promise({
      tryCatch({
        with_account_lock(user_id, {
          tree    <- load_user_file(user_id, "account_tree.Rds")
          account <- tree$find_account_by_uuid(uuid)

          if (is.null(account)) {
            list(
              success = FALSE,
              status  = if (role == "admin") 404 else 403,
              error   = "Account not found or unauthorized"
            )
          } else {
            account$distribute_to_children(
              amount = amt,
              transaction = transaction,
              by = initiated_by
            )
            save_user_file(user_id, tree, "account_tree.Rds")

            list(
              success = TRUE,
              status = 200,
              account_uuid = uuid,
              message = "Amount successifully distributed to children",
              amount = amt,
              balance = account$balance
            )
          }
        })
      }, error = function(e) {
        error_message <- conditionMessage(e)

        if (grepl("Insufficient balance!", error_message, ignore.case = TRUE)) {
          return(list(
            success = FALSE,
            status = 400,
            error = paste("Distribution failed:", error_message)
          ))
        } else {
          return(list(
            success = FALSE,
            status = 500,
            error = paste("Distribution failed:", error_message)
          ))
        }
      })
    }, packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      end_time <- Sys.time()
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      res$status <- result$status %||% 500
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(
        success = FALSE,
        status=503,
        error = paste("Distribute Failed:", conditionMessage(err)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(difftime(end_time, start_time, units = "secs"))
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      error = paste("Failed to connect:", conditionMessage(e))
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
                              allocation = 0,
                              priority = 0,
                              fixed_amount = 0,
                              due_date = NULL,
                              account_type = NULL,
                              freq = NULL,
                              status = "active") {

  start_time <- Sys.time()
  user_id <- req$user_id
  role    <- req$role
  # --- Input Validation ---------------------------------------

  if (missing(parent_uuid) ||is.null(parent_uuid) || parent_uuid == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "uuid is required"))
  }

  if (missing(name) ||is.null(name) || name == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "name is required"))
  }

  allocation <- suppressWarnings(as.numeric(allocation))

  if (is.na(allocation) || allocation < 0 || allocation > 1) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid allocation: must be a number between 0 and 1"
    ))
  }

  # coecion
  priority <- suppressWarnings(as.numeric(priority))
  if (is.na(priority) || priority < 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid priority: must be a non-negative integer"
    ))
  }
  fixed_amount <- suppressWarnings(as.numeric(fixed_amount))

  if (is.na(fixed_amount) || fixed_amount < 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid fixed amount: must be a non-negative number"
    ))
  }

  freq <- if (!is.null(freq)) as.numeric(freq) else NULL
  if (!is.null(freq) && (is.na(freq) || freq < 0)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid frequency: must be a non-negative integer"
    ))
  }
  due_date <-  if (!is.null(due_date)) tryCatch(
    safe_parse_date(due_date), error = function(e)
      NA
    ) else NULL

  if (!is.null(due_date) && is.na(due_date)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid due date: must be a valid date"
    ))
  }

  # --- Core Handler -------------------------------------------
  tryCatch({
    future_promise({
      tryCatch({
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
            uuid = child$uuid,
            child_type = class(child)[1],
            allocation = child$allocation
          )
        })
      }, error = function(e) {
        error_message <- conditionMessage(e)

        if (grepl("Total allocation exceeds 100%", error_message, ignore.case = TRUE)) {
          return(list(
            success = FALSE,
            status = 400,
            error = paste("Child account not added:", error_message)
          ))
        }
        return(list(
          success = FALSE,
          status = 500,
          error = paste("Child account not added:", error_message)
        ))
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        status = 503,
        error = paste("Child account not added:", conditionMessage(err)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status=500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  start_time <- Sys.time()
  user_id <- req$user_id
  role    <- req$role

  # --- Input Validation ---------------------------------------

  if (missing(parent_uuid) ||is.null(parent_uuid) || parent_uuid == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "parent uuid is required"))
  }

  if (missing(child_name) ||is.null(child_name) || child_name == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "child_name is required"))
  }

  if (is.null(allocation) || allocation == "") {
    res$status <- 400
    return(
      list(success = FALSE, status = 400, error = "Allocation is required")
    )
  }

  allocation <- suppressWarnings(as.numeric(allocation))
  if (is.na(allocation) || allocation < 0 || allocation > 1) {
    res$status <-400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Allocation must be a number between 0 and 1"
      )
    )
  }

  # --- Core Handler -------------------------------------------
  tryCatch({
    future_promise({

      # Call method safely
      tryCatch({
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

          if (!(child_name %in% names(parent$child_accounts))) {
            return(
              list(
                success = FALSE,
                status = 404,
                error = "Child account not found under parent"
              )
            )
          }

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
        })
      }, error = function(e) {
        error_message <- conditionMessage(e)

        if (grepl("Total allocation exceeds 100%", error_message, ignore.case = TRUE)) {
          return(list(
            success = FALSE,
            status = 400,
            error = paste("Allocation not set:", error_message)
          ))
        }
        return(list(
          success = FALSE,
          status = 500,
          error = paste("Allocation not set:", error_message)
        ))
      })

    },packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        status = 503,
        error = paste("Allocation not set:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      status = 500,
      success = FALSE,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # --- Input Validation ---------------------------------------
  if (missing(uuid) ||is.null(uuid) || uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status=400,
        error = "uuid is required"
      )
    )
  }

  # --- Core Handler -------------------------------------------
  tryCatch({
    future_promise({
      tryCatch({
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

        balance = account$get_balance()
        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          balance=balance
        )

      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Balance not retrieved:", conditionMessage(e))
        )

      })

    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        status = 503,
        error = paste("Balance not retrieved:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # --- Input Validation ---------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(success = FALSE,status=400, error = "uuid is required"))
  }

  # --- Core Handler -------------------------------------------
  tryCatch({
    future_promise({
      tryCatch({
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

      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Transaction retrieval failed:", conditionMessage(e))
        )
      })

    }, packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Transaction retrieval failed:", conditionMessage(err)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })

  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # --- Input Validation ---------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status=400,
        error = "uuid is required")
    )
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to list child accounts:", conditionMessage(e))
        )
      })

    },packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to list child accounts:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # --- Input Validation ---------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status=400,
        error = "uuid is required")
    )
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to list accounts:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T
    ) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to list accounts:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # --- Input Validation ---------------------------------------
  if (missing(name) || is.null(name) || name == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status=400,
        error = "name is required")
    )
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      tryCatch({
        tree <- load_user_file(user_id, "account_tree.Rds")

        if (is.null(tree)) {
          return(list(
            success = FALSE,
            status = if (role == "admin") 404 else 403,
            error = "Account tree not found or unauthorized"
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to retrieve accounts:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to retrieve accounts:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # --- Input Validation ---------------------------------------
  if (is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      # Load account tree

      tryCatch({
        tree <- load_user_file(user_id, "account_tree.Rds")

        if (is.null(tree)) {
          return(list(
            success = FALSE,
            status = if (role == "admin") 404 else 403,
            error = "Account tree not found or unauthorized"
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to retrieve account:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to retrieve account:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # --- Input validation BEFORE future --------------------------------------
  if (is.null(from_uuid) || from_uuid == "") {
    res$status<-400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Source account (from_uuid) is required"
      )
    )
  }

  if (is.null(to_uuid) || to_uuid == "") {
    res$status<-400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Destination account (to_uuid) is required"
      )
    )
  }


  if (is.null(amount) || amount == "") {
    res$status<-400
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
    res$status<-400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Amount must be a positive number"
      )
    )
  }
  # ---- core logic---------------------------------------------

  tryCatch({
    future({
      tryCatch({
        with_account_lock(user_id, {
          tree <- load_user_file(user_id, "account_tree.Rds")

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
        })
      }, error = function(e) {
        error_message <- conditionMessage(e)
        if (
          grepl(
            "Total allocation exceeds 100%", error_message,
            ignore.case = TRUE
          )|
          grepl(
            "Insufficient balance! Your current balance",
            error_message,
            ignore.case = TRUE
          )
        ) {
          return(list(
            success = FALSE,
            status = 400,
            error = paste("Failed:", error_message)
          ))
        }
        return(list(
          success = FALSE,
          status = 500,
          error = paste("Failed:", error_message)
        ))
      })


    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # --- Input Validation ---------------------------------------
  if (missing(uuid)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      tryCatch({
        tree <- load_user_file(user_id, "account_tree.Rds")

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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute balance:", conditionMessage(e))
        )
      })


    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # --- Input Validation ---------------------------------------
  if (missing(uuid)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # --- Core Handler -------------------------------------------
  tryCatch({
    future({
      tryCatch({

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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute amount due:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = paste("Failed to compute amount due:", conditionMessage(e)),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(
          difftime(Sys.time(), start_time, units = "secs")
        )
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------

  if (missing(uuid)|| uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "UUID is required"
      )
    )
  }

  if (is.null(days) || days == "") {
    res$status <- 400
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
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Days must be a non-negative number"
      )
    )
  }

  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
          return(
            list(
              success = FALSE,
              status = 404,
              error = "Account not found"
            )
          )
        }

        total_due <- account$compute_total_due_within_n_days(days)

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          total_due = total_due,
          within_days = days
        )
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute total due:", conditionMessage(e))
        )
      })

    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------
  if (missing(uuid)||is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # Parse date range
  if (!is.null(from) && !is.null(to)) {
    daterange <- tryCatch({
      c(
        safe_parse_date(from),
        safe_parse_date(to)
      )
    }, error = function(e) NULL)  # catch parsing errors and return NULL

    if (is.null(daterange)) {
      res$status <- 400
      return(
        list(success = FALSE, status = 400, error = "Invalid date format")
      )
    }
  } else {
    # If from/to are missing, use a very wide default range
    daterange <- c(Sys.Date() - 365000, Sys.Date())
  }

  # avoid to_date earlier than from_date
  if (daterange[2] < daterange[1]) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }
  # -----------------------core logic------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
            status = if (role == "admin") 404 else 403,
            error = "Account not found"
          ))
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
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute spending:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # Handle date range
  from_date <- tryCatch(
    safe_parse_date(from),
    error = function(e) NA
  )

  to_date <- tryCatch(
    safe_parse_date(to),
    error = function(e) NA
  )

  if (length(from_date) == 0 || is.na(from_date)) {
    from_date <- Sys.Date() - 365000
  }
  if (length(to_date) == 0 || is.na(to_date)) {
    to_date <- Sys.Date()
  }
  # avoid to_date earlier than from_date
  if (to_date < from_date) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }
  # -------- core logic  ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
            status = if (role == "admin") 404 else 403,
            error = "Account not found"
          ))
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
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute total income:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # Parse and validate date range
  from_date <- tryCatch(
    safe_parse_date(from),
    error = function(e) NA
  )

  to_date <- tryCatch(
    safe_parse_date(to),
    error = function(e) NA
  )



  if (length(from_date) == 0 || is.na(from_date)) {
    from_date <- Sys.Date() - 365000
  }
  if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()

  # avoid to_date earlier than from_date
  if (to_date < from_date) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }

  # -------- corelogic ------------------------------------------------
  tryCatch({
    future({
      # Load account tree
      tryCatch({
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
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute allocated amount:", conditionMessage(e))
        )
      })
    },, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }


  # Parse dates safely
  from_date <- tryCatch(
    safe_parse_date(from),
    error = function(e) NA
  )

  to_date <- tryCatch(
    safe_parse_date(to),
    error = function(e) NA
  )

  if (length(from_date) == 0 || is.na(from_date)) {
    from_date <- Sys.Date() - 365000
  }
  if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()

  daterange <- c(from_date, to_date)

  # avoid to_date earlier than from_date
  if (to_date < from_date) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }

  # -------- core logic ------------------------------------------------

  tryCatch({
    future({
      tryCatch({
        # Load account tree

        tree <- load_user_file(user_id, "account_tree.Rds")

        if (is.null(tree)) {
          return(list(
            success = FALSE,
            status = if (role == "admin") 404 else 403,
            error = "User not found or unauthorized access"
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

        utilization <- account$income_utilization(daterange)

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          from = as.character(from_date),
          to = as.character(to_date),
          utilization = utilization
        )
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to compute utilization:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
walking_amount <- function(
    uuid=NULL,
    amt_type = "amount_due",
    from = NULL,
    to = NULL,
    req,
    res
) {


  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # Parse and validate dates
  from_date <- tryCatch(
    safe_parse_date(from),
    error = function(e) NA
  )

  to_date <- tryCatch(
    safe_parse_date(to),
    error = function(e) NA
  )
  if (length(from_date) == 0 || is.na(from_date)) {
    from_date <- Sys.Date() - 365000
  }
  if (length(to_date) == 0 || is.na(to_date)) to_date <- Sys.Date()

  daterange <- c(from_date, to_date)

  # avoid to_date earlier than from_date
  if (to_date < from_date) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }

  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Load account tree
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
            status = 403,
            error = "Account not found"
          ))
        }

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
      },error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to walking amount:", conditionMessage(e))
        )
      })

    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  if (missing(status) || is.null(status) || status == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Status is required"
    ))
  }
  # for base status corce to match
  if(tolower(status) %in% c("active", "inactive", "closed")) {
    status<-tolower(status)
  }


  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
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

          if (account$get_account_status() == "closed" && status == "closed") {
            res$status <- 400
            return(list(
              success = FALSE,
              status = 400,
              error = "This account is already closed"
            ))
          }


          result <- tryCatch({
            account$change_status(status)
            save_user_file(user_id, tree, "account_tree.Rds")  # persist change

            new_status<-account$get_account_status()
            list(
              success = TRUE,
              status = 200,
              uuid = uuid,
              new_status = new_status,
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to change account status:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # -------- core logic------------------------------------------------
  tryCatch({
    future({

      tryCatch({
        # Load account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get account status:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }


  if (missing(priority) || is.null(priority) || priority == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Priority is required"
    ))
  }

  priority <- suppressWarnings(as.numeric(priority))

  if (is.na(priority)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Priority must be numeric"
    ))
  }

  if (priority<0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Priority must be positive"
    ))
  }

  # --------------------------- core logic ------------------------------------
  tryCatch({
    future({
      tryCatch({
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
          account$set_priority(priority)
          save_user_file(user_id, tree, "account_tree.Rds")

          list(
            success = TRUE,
            status = 200,
            message = paste("Priority set to", priority),
            uuid = uuid,
            priority = priority
          )
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set priority:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # --------------------------- core logic ------------------------------------
  tryCatch({
    future({
      tryCatch({
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get priority:", conditionMessage(e))
        )
      })

    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # Parse due date
  parsed_date <- if(!is.null(due_date)){
    tryCatch(safe_parse_date(due_date), error = function(e) NA)
  } else {
    NULL
  }

  if (!is.null(parsed_date)&&(length(parsed_date) == 0 || is.na(parsed_date))) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Invalid due date format"
      )
    )
  }
  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set due date:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
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
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Load  account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get due date:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  if (missing(fixed_amount) || is.null(fixed_amount) || fixed_amount == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Fixed amount is required"
    ))

  }
  # Parse fixed amount safely
  fixed_amount <- suppressWarnings(as.numeric(fixed_amount))

  if (is.na(fixed_amount) || fixed_amount < 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Fixed amount must be a non-negative number"
    ))
  }

  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree

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
          acct$set_fixed_amount(fixed_amount)
          save_user_file(user_id, tree, "account_tree.Rds")

          list(
            success = TRUE,
            status = 200,
            uuid = uuid,
            fixed_amount = fixed_amount,
            amount_due = acct$amount_due
          )
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set fixed amount:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Load account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get fixed amount:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  if (missing(account_type)|| account_type == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Account type is required"
    ))
  }

  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree

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
          # Set account type
          account$set_account_type(account_type)
          save_user_file(user_id, tree, "account_tree.Rds")

          list(
            success = TRUE,
            status = 200,
            message = paste("Account type updated to", account_type),
            uuid = uuid,
            account_type = account$account_type
          )
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set account type:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------
  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get account type:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
#* @param account_freq:str* New frequency (e.g., 30 = "Monthly", 7= "Weekly",90 = "Quarterly")
#* @json
function(req, res, uuid = NULL, account_freq) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  if (missing(account_freq) ||account_freq == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "account_freq is required"
    ))
  }

  # Parse account_freq safely
  if(!is.null(account_freq)){
    account_freq <- suppressWarnings(as.numeric(account_freq))

    if (is.na(account_freq) || account_freq <= 0) {
      res$status <- 400
      return(list(
        success = FALSE,
        status = 400,
        error = "account_freq must be a positive number"
      ))
    }

  }
  # -------- core logic ---------------------------------------------------

  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree
        with_account_lock(user_id, {
          tree <- load_user_file(user_id, "account_tree.Rds")

          account <- tree$find_account_by_uuid(uuid)

          if (is.null(account)) {
            return(
              list(
                success = FALSE,
                status = 403,
                error = "Account not found"
              )
            )
          }

          # Validate class
          if (!inherits(account, "GrandchildAccount")) {
            return(list(
              success = FALSE,
              status = 403,
              error = "This endpoint is only applicable to grandchild accounts"
            ))
          }

          # Apply method
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
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set frequency:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
    )
  })
}


# -----------------get_account_freq endpoint ----------------------------------
#* @tag accounts
#* @get /get_account_freq
#* @summary Get frequency of a grandchild account
#* @description
#* Retrieves the frequency (e.g., 30 ="Monthly", 7="Weekly") set for a grandchild account.
#* Only works for accounts inheriting from `GrandchildAccount`.
#*
#* @param uuid:str* The UUID of the grandchild account
#* @json
get_account_freq <- function(req, res, uuid = NULL) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  # -------- core logic ---------------------------------------------------

  tryCatch({
    future({
      tryCatch({
        # Load account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get frequency:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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
  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }
  if (missing(periods) || is.null(periods) || periods == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Periods is required"
    ))
  }

  # Parse periods safely
  periods <- suppressWarnings(as.integer(periods))

  if (is.na(periods) || periods <= 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Periods must be a positive integer"
    ))
  }

  # -------- core logic ------------------------------------------------

  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree
        with_account_lock(user_id, {
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
            return(list(success = FALSE, status = 403, error = "Account not found"))
          }

          if (!inherits(account, "GrandchildAccount")) {
            return(list(
              success = FALSE,
              status = 403,
              error = "This endpoint is only applicable to grandchild accounts"
            ))
          }
          # Set periods
          account$set_account_periods(periods)
          save_user_file(user_id, tree, "account_tree.Rds")

          list(
            success = TRUE,
            status = 200,
            uuid = uuid,
            periods = periods,
            message = paste("Number of periods set to", periods)
          )
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to set account periods:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
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

  # -------- validate inputs ------------------------------------------------

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }


  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree
        # Load account tree
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
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get account periods:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(
        difftime(Sys.time(), start_time, units = "secs")
      )
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
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
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
    )
  })
}

# -------------- Delete User Account ----------------------------------------------

#* @tag accounts
#* @delete /delete
#* @param user_id:str* The user ID whose account should be deleted.
#* @summary Delete a user account and its associated data file
#* @description
#* Deletes the user's account file (typically `"account_tree.Rds"`) from
#* persistent storage(for main account) or just the target account.
#* Performs file existence checks and wraps the operation in a file lock to
#* ensure safe deletion.
#*
delete <- function(req, res, uuid = NULL) {
  start_time <- Sys.time()
  user_id <- req$user_id
  role <- req$role

  if (missing(uuid) || is.null(uuid) || uuid == "") {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "Account UUID is required"
      )
    )
  }

  tryCatch({
    future({
      tryCatch({
        # Ensure the user has a lock on their account
        # This prevents concurrent modifications to the account tree
        with_account_lock(user_id, {
          if (!user_file_exists(user_id)) {
            return(list(success = FALSE,
                        status = if (role == "admin") 404 else 403,
                        error = sprintf(
                          "Account for user_id '%s' does not exist.",
                          user_id
                        )
            )
            )
          }

          tree <- load_user_file(user_id, "account_tree.Rds")
          if (is.null(tree)) {
            return(
              list(
                success = FALSE,
                status = if (role == "admin") 404 else 403,
                error = "User not found or unauthorized access")
            )
          }

          # core deletion
          remove_account(tree, user_id, uuid)

          message <- if (tree$uuid == uuid) {
            sprintf(
              "Account for user_id '%s' deleted successfully.",
              user_id
            )
          } else {
            sprintf(
              "Account with UUID '%s' deleted successfully.",
              uuid
            )
          }

          list(
            success = TRUE,
            status = 200,
            uuid = uuid,
            message = message
          )
        })
      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to delete account:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      end_time <- Sys.time()
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time   <- format(end_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      end_time <- Sys.time()
      res$status <- 503
      list(success = FALSE, error = conditionMessage(err),
           start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
           end_time   = format(end_time, "%Y-%m-%dT%H:%M:%OS3Z"),
           execution_time = as.numeric(difftime(end_time, start_time, units = "secs")))
    })
  }, error = function(e) {
    res$status <- 500
    list(success = FALSE, status = 500,
         error = paste("Failed to connect:", conditionMessage(e)))
  })
}




# ------------------ get_minimal_tree endpoint -------------------------------
#* @tag accounts
#* @get /get_minimal_tree
#* @summary Get minimal version of an account tree
#* @description
#* Returns a simplified account tree for a given account UUID.
#* This avoids sending the full account object, reducing payload size
#* and improving front-end startup speed.
#*
#* @param uuid:str* UUID of the account (can be main, child, or grandchild)
#* @param n:int Number of days to compute total due within. Default: 30.
#* @param start_date:str Start date for statistics (YYYY-MM-DD).
#*   Default: 1000 years ago.
#* @param end_date:str End date for statistics (YYYY-MM-DD).
#*   Default: Today
#* @param ts_data:logical whether to generate timeseries data.
#*   Default: FALSE
#* @json
get_minimal_tree <- function(req, res, uuid = NULL, n = 30,
                             start_date = NULL, end_date = NULL,ts_data=F) {
  user_id <- req$user_id
  role <- req$role
  start_time <- Sys.time()

  # -------- validate inputs ------------------------------------------------
  if (!is.null(uuid) && uuid == "") {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "UUID is required"
    ))
  }

  # Parse n safely
  n<-suppressWarnings(as.numeric(n))
  if (is.na(n) || n <= 0) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "`n` must be a positive number"
    ))
  }

  # Handle date range defaults
  if (is.null(start_date)) start_date <- Sys.Date() - 365000
  if (is.null(end_date)) end_date <- Sys.Date()

  # Convert to Date
  start_date <- tryCatch(safe_parse_date(start_date), error = function(e) NA)
  end_date <- tryCatch(safe_parse_date(end_date), error = function(e) NA)

  if (is.na(start_date) || is.na(end_date)) {
    res$status <- 400
    return(list(
      success = FALSE,
      status = 400,
      error = "Invalid date format"
    ))
  }

  daterange <- c(start_date, end_date)

  # avoid to_date earlier than from_date
  if (daterange[2] < daterange[1]) {
    res$status <- 400
    return(
      list(
        success = FALSE,
        status = 400,
        error = "End date cannot be earlier than start date"
      )
    )
  }
  # -------- core logic ------------------------------------------------
  tryCatch({
    future({
      tryCatch({
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

        if (is.null(uuid) || uuid == "") {
          account <- tree
        } else {
          account <- tree$find_account_by_uuid(uuid)
          if (is.null(account)) {
            return(
              list(
                success = FALSE,
                status = 403,
                error = "Account not found")
            )
          }
        }

        # Call minimal_tree
        result_data <- minimal_tree(account, n = as.numeric(n), daterange = daterange,ts_data=ts_data)

        if(is.null(uuid)){
          uuid<- tree$uuid
        }

        list(
          success = TRUE,
          status = 200,
          uuid = uuid,
          minimal_tree = result_data
        )

      }, error = function(e) {
        list(
          success = FALSE,
          status = 500,
          error = paste("Failed to get minimal tree:", conditionMessage(e))
        )
      })
    }, packages=pkgs_required, seed=T) %...>% (function(result) {
      # Add metadata
      result$start_time <- format(start_time, "%Y-%m-%dT%H:%M:%OS3Z")
      result$end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      result$execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      res$status <- result$status %||% 200
      result
    }) %...!% (function(err) {
      res$status <- 503
      list(
        success = FALSE,
        error = conditionMessage(err),
        start_time = format(start_time, "%Y-%m-%dT%H:%M:%OS3Z"),
        end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
        execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      )
    })
  }, error = function(e) {
    res$status <- 500
    list(
      success = FALSE,
      status = 500,
      error = paste("Failed to connect:", conditionMessage(e))
    )
  })
}
