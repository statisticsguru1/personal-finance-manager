library(jose)
library(filelock)
#--------------------- validates user id before creating base acc-------------
#' Validate a User ID Format
#'
#' Checks whether a given user ID is valid and safe for use in file paths.
#' A valid user ID consists of only alphanumeric characters and underscores.
#' This function is used to prevent unsafe input that could lead to directory
#' traversal attacks or file system misuse.
#'
#' @param user_id A character string representing the user ID.
#'
#' @return A logical value: `TRUE` if the user ID is valid, `FALSE` otherwise.
#' @examples
#' is_valid_user_id("user123")       # TRUE
#' is_valid_user_id("user-abc")      # FALSE
#' is_valid_user_id("../../etc/")    # FALSE
#'
#' @export
is_valid_user_id <- function(user_id) {
  grepl("^[a-zA-Z0-9_]+$", user_id)
}

#--------------- creates base account during reg----------------------------
#' Create a New User Account Directory and Main Account Tree
#'
#' Creates a new user directory, initializes a `MainAccount` object as the
#' root of their account tree, and saves it to disk. Optionally, the user can
#' be initialized with a balance, although this is discouraged unless needed
#' for administrative reasons (as it bypasses the transaction log).
#'
#' @param user_id A validated user ID (must contain only letters,
#'  digits, and underscores).
#' @param base_dir The root directory where user accounts are stored.
#' Default is `"user_accounts"`.
#' @param initial_balance Optional numeric value specisfying the
#'        starting balance for the main account. Default is `0`.
#'        This balance is **not** recorded as a transaction.
#'
#' @return Invisibly returns `TRUE` if the account was created successfully.
#'
#' @details The function assumes the `user_id` has already been checked or
#' authorized at a higher level (e.g., registration). It will fail if the
#' user directory already exists. This function creates no login credentials.
#'
#' @note It is **recommended** to always create accounts with a `0` balance and
#' record any starting amount using a proper deposit transaction. This ensures
#' that all balance-affecting operations are logged and traceable.
#'
#' @examples
#' create_user_account_base("user_demo")
#' create_user_account_base("admin_user", initial_balance = 1000)
#'
#' @seealso \code{\link{is_valid_user_id}}, \code{\link{MainAccount}}
#' @export
create_user_account_base <- function(
  user_id,
  base_dir = "user_accounts",
  initial_balance = 0
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }

  # Expand "~" or relative path if necessary
  base_dir <- normalizePath(base_dir, mustWork = FALSE)
  user_dir <- file.path(base_dir, user_id)

  # Create base directory if it doesn't exist
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

  # Prevent overwriting existing user data
  if (dir.exists(user_dir)) stop("User already exists")

  dir.create(user_dir)

  # Create and save main account tree
  main <- MainAccount$new(name = "Main", balance = initial_balance)
  saveRDS(main, file.path(user_dir, "account_tree.Rds"))

  invisible(TRUE)
}

# ------------ load account-------------------------------------------
#' Load a User's Account Tree from Disk
#'
#' Loads the account tree for a given user from a serialized `.Rds` file
#' located in the user's data directory.
#'
#' This function complements `create_user_account_base()`, and is intended
#' to retrieve the account structure created during account initialization.
#' It validates the user ID and ensures the required account file exists
#' before reading it.
#'
#' @param user_id A string representing the unique user ID. Must satisfy the
#'   format validated by `is_valid_user_id()`.
#' @param base_dir The base directory where user account data is stored.
#'   Defaults to `"user_accounts"` or can be overridden by setting
#'   `ACCOUNT_BASE_DIR` via `Sys.setenv()`.
#'
#' @return An object representing the user's account tree, typically of
#'   class `MainAccount` or a compatible structure.
#'
#' @examples
#' \dontrun{
#'   load_user_account("user123")
#'   load_user_account("user123", base_dir = "data/accounts")
#' }
#'
#' @export
load_user_account <- function(
    user_id,
    base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }
  
  user_dir <- file.path(base_dir, user_id)
  file_path <- file.path(user_dir, "account_tree.Rds")
  
  if (!file.exists(file_path)) {
    stop("Account data not found for user: ", user_id)
  }
  
  readRDS(file_path)
}



# ------------ save account-------------------------------------------
#' Save a User Account Tree to Disk
#'
#' Persists a user's account tree object to an `.Rds` file on disk. This ensures
#' updates (e.g., deposits or withdrawals) to the user's accounts are saved 
#' between sessions.
#'
#' @param user_id Character string representing the unique user ID.
#' @param tree A user account tree object (typically returned by 
#' `load_user_account()`).
#' @param base_dir Optional. Base directory where user account data is stored.
#'   Defaults to the `ACCOUNT_BASE_DIR` environment variable or 
#'   `"user_accounts"` if unset.
#'
#' @return No return value. This function is called for its side effect of 
#' saving the object to disk.
#'
#' @details The function checks if the `user_id` is valid and ensures the 
#' corresponding user
#' directory exists. The `tree` object is serialized and saved as 
#' `"account_tree.Rds"` under the user's directory.
#'
#' @seealso [load_user_account()] to read account data, and 
#' [create_user_account_base()] to initialize it.
#'
#' @examples
#' \dontrun{
#' tree <- load_user_account("user123")
#' # Modify the account tree (e.g., deposit)
#' save_user_account("user123", tree)
#' }
#'
#' @export
save_user_account <- function(
    user_id,
    tree,
    base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }
  
  if (is.null(tree)) {
    stop("Cannot save NULL tree")
  }
  
  if (base_dir == "") {
    stop("Invalid base_dir: cannot be empty")
  }
  
  user_dir <- file.path(base_dir, user_id)
  
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE)
  }
  
  file_path <- file.path(user_dir, "account_tree.Rds")
  
  saveRDS(tree, file_path)
}



# ----------------decodes JWT tokens --------------------------------
#' Decode and Verify a JWT Token
#'
#' Attempts to decode and verify a JSON Web Token (JWT) using an HMAC secret.
#' Returns the decoded payload if valid, or `NULL` if verification fails.
#'
#' @param token A character string representing the JWT token (e.g., from an
#'        HTTP header).
#' @param secret A raw or character vector used as the HMAC secret for
#'        verification.
#'        Defaults to the global `secret_key` variable, which should be securely
#'         set (e.g., via `Sys.getenv("JWT_SECRET")`).
#'
#' @return A list representing the decoded JWT payload if the token is valid;
#'         otherwise, `NULL` if decoding fails or the token is invalid/expired.
#'
#' @examples
#' \dontrun{
#' library(jose)
#' token <- jwt_encode_hmac(list(user_id = "abc123"),
#' secret = charToRaw("my-secret"))
#' verify_token(token, secret = charToRaw("my-secret"))
#' }
#'
#' @seealso [jose::jwt_decode_hmac()], [Sys.getenv()]
#' @export

verify_token <- function(token, secret = secret_key) {
  tryCatch({
    jose::jwt_decode_hmac(token, secret = secret)
  }, error = function(e) NULL)
}


# -------------------- file lock ---------------------------------------------
with_account_lock <- function(
    user_id,
    expr,
    base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts"),
    timeout = 10
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }
  
  user_dir <- file.path(base_dir, user_id)
  lockfile <- file.path(user_dir, "account_tree.lock")
  
  start_time <- Sys.time()
  
  while (file.exists(lockfile)) {
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Could not acquire lock for user [", user_id, "]: timeout reached")
    }
    Sys.sleep(0.1)
  }
  
  # Create the lock file with current process ID
  lock_con <- file(lockfile, open = "w")
  writeLines(as.character(Sys.getpid()), lock_con)
  close(lock_con)
  
  on.exit({
    if (file.exists(lockfile)) file.remove(lockfile)
  }, add = TRUE)
  
  force(expr)
}




