library(jose)
library(tidyverse)
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
  grepl("^[a-zA-Z0-9_-]+$", user_id)
}

#--------------- creates base account during reg----------------------------
#' Create a New User Account.
#'
#' Initializes a `MainAccount` object and saves it using the configured backend.
#'
#' @param user_id A validated user ID (must contain only letters,
#'  digits, and underscores).
#' @param base_dir Deprecated. Use ACCOUNT_BACKEND instead.
#' @param initial_balance Optional numeric value specifying the
#'        starting balance for the main account. Default is `0`.
#'
#' @return Invisibly returns `TRUE` if the account was created successfully.
#'
#' @export
create_user_account_base <- function(
  user_id,
  base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts"),
  initial_balance = 0
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }

  # Prevent overwriting existing user data
  if (user_file_exists(user_id, file_name = "account_tree.Rds")) {
    stop("User already exists")
  }

  # Create and save main account tree
  main <- MainAccount$new(name = "Main", balance = initial_balance)

  save_user_file(user_id, main, file_name = "account_tree.Rds")

  invisible(return(main$uuid))
}


# ------------ load user files -------------------------------------------
#' Load a User's File from the Storage Backend
#'
#' Loads a file associated with a user from the configured storage backend.
#' The backend is determined by the `ACCOUNT_BACKEND` environment variable
#' (default is `"file"` for local storage), and supports loading various file
#' types like `.Rds`, `.json`, and `.csv` via the appropriate plugin.
#'
#' This function provides a unified interface to load user-specific data,
#' regardless of where or how it is stored (e.g., local file system, cloud,
#' or database). The actual loading logic is delegated to a plugin based
#' on the backend, which is configured using environment variables.
#'
#' @param user_id A string representing the unique user ID.
#' @param file_name The name of the file to load (e.g., `"account_tree.Rds"`).
#'   This should be a base name relative to the user-specific storage root.
#'
#' @return The R object loaded from the specified file.
#'
#' @details
#' The function constructs the appropriate arguments for the selected
#' backend using `build_plugin_args()` and dispatches the call using
#' `do.call()`. The file type determines how the file is parsed (e.g.,
#' `.Rds` via `readRDS()`, `.json` via `jsonlite::fromJSON()`).
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ACCOUNT_BACKEND = "file")
#' load_user_file("user123", "account_tree.Rds")
#' load_user_file("user123", "transactions.csv")
#' }
#'
#' @seealso [save_user_file()], [build_plugin_args()], and
#' plugin implementations like `load_from_file()`.
#'
#' @export
load_user_file <- function(user_id, file_name = "account_tree.Rds") {
  backend <- Sys.getenv("ACCOUNT_BACKEND", "file")
  fn_name <- paste0("load_from_", backend)

  if (!exists(fn_name, mode = "function")) {
    stop("No loader plugin found for backend: ", backend)
  }

  args <- build_plugin_args(
    backend,
    "load",
    user_id = user_id,
    file_name = file_name
  )

  do.call(fn_name, args)
}




# ------------ save account/lockfiles ---------------------------------------
#' Save a User-Specific File via Plugin Backend
#'
#' Saves an R object to a user-specific location using the appropriate plugin
#' backend (e.g., `"file"`, `"mongo"`, `"gdrive"`). The format and destination
#' are determined by the backend and file extension.
#'
#' This function acts as a plugin launcher for saving user-related data,
#' including account trees (`.Rds`), transaction records (`.csv`),
#' metadata (`.json`), or lockfiles (`.lock`). It delegates the actual
#' save operation to the corresponding backend plugin function (e.g.,
#' `save_to_file()`).
#'
#' @param user_id A character string representing the unique user ID.
#' @param object The R object to save. This can be any R object appropriate
#'   for the file extension used (e.g., list, data.frame, custom class).
#' @param file_name The file name, including the extension (e.g.,
#'   `"account_tree.Rds"`, `"meta.json"`, `"transactions.csv"`,
#'   `"account_tree.lock"`).
#'
#' @return No return value. This function is invoked for its side effect of
#' persisting a file via the selected backend plugin.
#'
#' @details
#' The appropriate plugin is selected based on the `ACCOUNT_BACKEND`
#' environment
#' variable (default: `"file"`). The function builds the arguments required by
#' the plugin using [build_plugin_args()], then delegates the save operation.
#'
#' An error is raised if no suitable save plugin is found.
#'
#' @seealso [build_plugin_args()], [load_user_file()], [remove_user_file()],
#'   [user_file_exists()]
#'
#' @examples
#' \dontrun{
#'   # Save an account tree to .Rds
#'   save_user_file("user123", MainAccount$new(name = "Main"))
#'
#'   # Save a data frame to CSV
#'   save_user_file("user123", data.frame(a = 1:3), "transactions.csv")
#'
#'   # Save a lock file (PID as a number)
#'   save_user_file("user123", Sys.getpid(), "account_tree.lock")
#' }
#'
#' @export
save_user_file <- function(user_id, object, file_name = "account_tree.Rds") {
  backend <- Sys.getenv("ACCOUNT_BACKEND", "file")
  fn_name <- paste0("save_to_", backend)

  if (!exists(fn_name, mode = "function")) {
    stop("No save plugin found for backend: ", backend)
  }

  args <- build_plugin_args(
    backend, "save",
    user_id = user_id,
    object = object,
    file_name = file_name
  )

  do.call(fn_name, args)
}


# -----------------------remove account/lockfile ------------------------
#' Remove a User File via Configured Storage Backend
#'
#' Deletes a specific user file using the appropriate backend plugin, based
#' on the `ACCOUNT_BACKEND` environment variable. This function abstracts the
#' file removal logic, allowing different storage systems to handle file
#' deletion (e.g., local file system, MongoDB, Google Drive).
#'
#' @param user_id A character string representing the user ID.
#' @param file_name Name of the file to remove. Defaults to
#'   `"account_tree.Rds"`.
#'
#' @return Invisibly returns `NULL`. Issues a warning if the file does not
#'   exist, or throws an error if the backend plugin is not found.
#'
#' @details The actual removal behavior is implemented by the selected
#'   backend plugin (e.g., `remove_from_file`, `remove_from_gdrive`, etc.).
#'   Configuration is controlled via environment variables like
#'   `ACCOUNT_BACKEND` and `ACCOUNT_BASE_DIR`.
#'
#' @examples
#' \dontrun{
#'   remove_user_file("user123")  # Deletes "account_tree.Rds"
#'   remove_user_file("user123", file_name = "data.json")
#' }
#'
#' @seealso [save_user_file()], [load_user_file()], [build_plugin_args()]
#' @export
remove_user_file <- function(user_id, file_name = "account_tree.Rds") {
  backend <- Sys.getenv("ACCOUNT_BACKEND", "file")
  fn_name <- paste0("remove_from_", backend)

  if (!exists(fn_name, mode = "function")) {
    stop("No remove plugin found for backend: ", backend)
  }

  args <- build_plugin_args(
    backend,
    "remove",
    user_id = user_id,
    file_name = file_name
  )
  do.call(fn_name, args)
}


# ---------------------- for checking account/lockfile existence ---------------
#' Check If a User File Exists via Configured Storage Backend
#'
#' Checks whether a specific file (e.g., `account_tree.Rds`, lock file, etc.)
#' exists for a given user by delegating to a backend-specific plugin.
#'
#' This function supports checking files stored in different storage backends
#' such as local disk, MongoDB, Google Drive, etc., as configured via the
#' `ACCOUNT_BACKEND` environment variable.
#'
#' @param user_id A string representing the unique user ID.
#' @param file_name Name of the file to check, relative to the user's folder.
#'   Defaults to `"account_tree.Rds"`.
#'
#' @return A logical value: `TRUE` if the file exists, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'   user_file_exists("user123") # Checks account_tree.Rds by default
#'   user_file_exists("user123", file_name = "account_tree.lock")
#' }
#'
#' @export
user_file_exists <- function(user_id, file_name = "account_tree.Rds") {
  backend <- Sys.getenv("ACCOUNT_BACKEND", "file")
  fn_name <- paste0("file_exists_", backend)

  if (!exists(fn_name, mode = "function")) {
    stop("No file-exists plugin found for backend: ", backend)
  }

  args <- build_plugin_args(
    backend,
    "file_exists",
    user_id = user_id,
    file_name = file_name
  )

  do.call(fn_name, args)
}


# --------------------- loading/saving plugins /checking existence--------------
####################### loading ####################################
#' Retry reading RDS file
#'
#' Attempts to read an RDS file multiple times before failing.
#'
#' @param file Path to the RDS file.
#' @param max_tries Number of retry attempts (default = 3).
#' @param delay Delay between retries in seconds (default = 0.3).
#'
#' @return The object read from the RDS file.
#' @export
retry_read_rds <- function(file, max_tries = 3, delay = 0.3) {
  for (i in seq_len(max_tries)) {
    tryCatch({
      return(readRDS(file))
    }, error = function(e) {
      if (i == max_tries) {
        message(sprintf("Failed to read RDS after %d attempts: %s", max_tries, e$message))
        stop(e)
      } else {
        Sys.sleep(delay)
      }
    })
  }
}


#' Load a User File from Local Filesystem
#'
#' Loads a user-specific file (e.g., account tree, lock file) from the
#'localfilesystem.The file format is determined automatically based on its
#'extension.
#'
#' @param user_id A character string representing the user's unique identifier.
#' @param file_name The name of the file to load (e.g., `"account_tree.Rds"`,
#'        `"data.json"`).
#' @param base_dir The base directory where user data is stored. This should
#'        point to the top-level folder containing all user subdirectories.
#'
#' @return The loaded object, parsed according to the file extension:
#' \itemize{
#'   \item `.Rds` files are loaded with \code{readRDS()}
#'   \item `.json` files are loaded with \code{jsonlite::fromJSON()}
#'   \item `.csv` files are loaded with \code{read.csv()}
#' }
#' some depedencies:
#' @importFrom utils read.csv write.csv
#'
#' @details
#' This function is intended to be called indirectly through a generic loader
#' such as \code{\link{load_user_file}}, which determines the appropriate
#' backend to use. The file extension determines the parser, and unsupported
#' file types will raise an error.
#'
#' @seealso \code{\link{load_user_file}}, \code{\link{readRDS}},
#' \code{\link[jsonlite]{fromJSON}}, \code{\link{read.csv}}
#'
#' @examples
#' \dontrun{
#'   load_from_file("user123", "account_tree.Rds", base_dir = "user_accounts")
#' }
#'
#' @export
load_from_file <- function(user_id, file_name, base_dir) {
  file_path <- file.path(base_dir, user_id, file_name)
  ext <- tools::file_ext(file_path)

  result <- tryCatch({
    switch(
      ext,
      "Rds" = tryCatch({
        retry_read_rds(file_path)
      }, error = function(e) {
        stop(
          sprintf(
            "Error loading RDS file: %s\nFile: %s",
            conditionMessage(e),
            file_path
        )
       )
      }),

      "json" = tryCatch({
        jsonlite::fromJSON(file_path)
      }, error = function(e) {
        stop(
          sprintf(
            "Error loading JSON file: %s\nFile: %s",
            conditionMessage(e),
            file_path
          )
        )
      }),

      "csv" = tryCatch({
        read.csv(file_path)
      }, error = function(e) {
        stop(
          sprintf(
            "Error loading CSV file: %s\nFile: %s",
            conditionMessage(e),
            file_path
          )
        )
      }),

      stop(
        sprintf(
          "Unsupported file type '.%s'.\nFile: %s",
          ext,
          file_path
        )
      )
    )
  }, error = function(e) {
    stop(
      sprintf(
        "Failed to load file '%s' for user '%s':\n%s",
        file_name,
        user_id,
        conditionMessage(e)
    )
   )
  })

  return(result)
}



####################### saving #####################################

## saving with retries
#' Retry saveRDS with Delay and Limited Attempts
#'
#' Attempts to save an R object to a file using `saveRDS()`, retrying on error up to a maximum number of tries.
#' This is useful in scenarios with high concurrency or potential file system contention, such as APIs or parallel processes.
#'
#' @param object An R object to save.
#' @param file A character string naming the file to save the R object to.
#' @param max_tries Maximum number of attempts before giving up. Default is 3.
#' @param delay Time in seconds to wait between attempts. Default is 0.3.
#'
#' @return `invisible(TRUE)` if successful. If all attempts fail, an error is raised.
#'
#' @examples
#' \dontrun{
#' retry_save_rds(my_data, "config.rds")
#' }
#'
#' @export
retry_save_rds <- function(object, file, max_tries = 3, delay = 0.3) {
  for (i in seq_len(max_tries)) {
    tryCatch({
      saveRDS(object, file)
      return(invisible(TRUE))
    }, error = function(e) {
      if (i == max_tries) {
        message(sprintf("Failed to save RDS after %d attempts: %s", max_tries, e$message))
        stop(e)
      } else {
        Sys.sleep(delay)
      }
    })
  }
}



## saving to local file

#' Save a User's File to the Local File System
#'
#' Saves an R object to a user-specific directory on the local file system
#' using a format inferred from the file extension. Supported formats include
#' `.Rds`, `.json`, `.csv`, and `.lock`.
#'
#' This function serves as the `"file"` backend plugin for saving user data.
#' It is usually called via `save_user_file()` and supports automatic directory
#' creation for the target user.
#'
#' @param user_id A character string specifying the unique user ID.
#' @param object The R object to save. This can be a list, data frame,
#'   atomic vector, or any serializable R object.
#' @param file_name The name of the file to save, including its extension
#'   (e.g., `"account_tree.Rds"`, `"transactions.json"`, `"session.lock"`).
#' @param base_dir The root directory where user-specific folders are stored.
#'   The object will be saved in `file.path(base_dir, user_id, file_name)`.
#'
#' @return Invisibly returns the path of the file after saving.
#'
#' @details
#' The file format is determined from the extension in `file_name`:
#' - **.Rds** — Saves the object using `saveRDS()`.
#' - **.json** — Saves the object using `jsonlite::write_json()` with
#'   `auto_unbox = TRUE`.
#' - **.csv** — Saves using `write.csv()` with `row.names = FALSE`.
#' - **.lock** — Treated as a text file; `writeLines()` is used with the
#'   assumption that `object` is a string or scalar.
#'
#' If the file extension is not recognized, the function falls back to
#' serializing the object with `serialize()` and writes it as binary.
#' A warning is issued to notify the developer and suggest extending the
#' plugin to handle custom file types explicitly.
#'
#' @note This function is intended for internal use within the plugin system.
#' For application-level usage, prefer calling `save_user_file()`.
#'
#' @examples
#' \dontrun{
#' save_to_file("user123", list(name = "Alice"), "profile.json",
#'              base_dir = "data")
#'
#' save_to_file("user123", data.frame(income = c(100, 200)), "income.csv",
#'              base_dir = "data")
#'
#' save_to_file("user123", MainAccount$new(name = "Main"), "account_tree.Rds",
#'              base_dir = "data")
#'
#' # Save a lock file
#' save_to_file("user123", Sys.getpid(), "account_tree.lock",
#'              base_dir = "data")
#'
#' # Save unknown file format (e.g., .yaml) — triggers warning
#' save_to_file("user123", list(config = TRUE), "custom.yaml",
#'              base_dir = "data")
#' }
#'
#' @seealso [save_user_file()], [load_from_file()], [build_plugin_args()]
#' @export

save_to_file <- function(user_id, object, file_name, base_dir) {
  user_dir <- file.path(base_dir, user_id)

  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE)
  }

  file_path <- file.path(user_dir, file_name)
  ext <- tools::file_ext(file_name)

  tryCatch({
    switch(
      ext,
      "Rds" = {
        tryCatch({
          retry_save_rds(object,file_path)
          #saveRDS(object,file_path)
        }, error = function(e) {
          stop(
            sprintf(
              "Error while saving RDS file: %s\nFile: %s",
              conditionMessage(e),
              file_path
            )
          )
        })
      },
      "json" = {
        tryCatch({
          jsonlite::write_json(object, file_path, auto_unbox = TRUE)
        }, error = function(e) {
          stop(
            sprintf(
              "Error while writing JSON file: %s\nFile: %s",
              conditionMessage(e),
              file_path
            )
          )
        })
      },
      "csv" = {
        tryCatch({
          write.csv(object, file_path, row.names = FALSE)
        }, error = function(e) {
          stop(
            sprintf(
              "Error while writing CSV file: %s\nFile: %s",
              conditionMessage(e),
              file_path)
          )
        })
      },
      "lock" = {
        tryCatch({
          if (!file.create(file_path)) {
            stop("Unable to create .lock file")
          }
        }, error = function(e) {
          stop(
            sprintf(
              "Error while creating lock file: %s\nFile: %s",
              conditionMessage(e),
              file_path
            )
          )
        })
      },
      {
        warning(
          sprintf(
            "Unrecognized file extension '.%s'. Saving using R's binary `serialize()`.\nFile: %s",
            ext, file_path
          )
        )
        tryCatch({
          con <- file(file_path, open = "wb")
          on.exit(close(con), add = TRUE)
          serialize(object, con)
        }, error = function(e) {
          stop(
            sprintf(
              "Error while serializing to file: %s\nFile: %s",
              conditionMessage(e), file_path)
          )
        })
      }
    )
  }, error = function(e) {
    stop(
      sprintf(
        "Failed to save file '%s' for user '%s':\n%s",
        file_name,
        user_id,
        conditionMessage(e))
    )
  })
  gc()
  invisible(file_path)
}



###################### Removing ##################################
## removing from local file
#' Remove a File from the Local File System
#'
#' Removes a specified file from a user's directory on the local file system.
#' This is a plugin for the `"file"` backend, used via the generic
#' interface `remove_user_file()`.
#'
#' If the file does not exist, a warning is issued but no error is thrown.
#'
#' @param user_id A string specifying the unique user ID.
#' @param file_name The name of the file to remove (e.g., `"account_tree.Rds"`).
#' @param base_dir The root directory containing all user folders. The full
#'   path is constructed as `file.path(base_dir, user_id, file_name)`.
#'
#' @return Returns `TRUE` if the file was successfully removed, or `FALSE` if
#'   the file did not exist or could not be removed. A warning is issued if the
#'   file is not found.
#'
#' @examples
#' \dontrun{
#' remove_from_file("user123", "account_tree.Rds", base_dir = "user_data")
#' }
#'
#' @seealso [save_to_file()], [load_from_file()], [remove_user_file()]
#'
#' @export
remove_from_file <- function(user_id, file_name, base_dir) {
  file_path <- file.path(base_dir, user_id, file_name)
  if (file.exists(file_path)) {
    file.remove(file_path)
  } else {
    warning("File does not exist: ", file_path)
  }
}

###################### checking file existence #######################
## checking file in local dir
#' Check if a user file exists on the local file system
#'
#' @param user_id The user ID
#' @param file_name The file name to check
#' @param base_dir The base directory for user data
#'
#' @return Logical TRUE/FALSE
file_exists_file <- function(user_id, file_name, base_dir) {
  file.exists(file.path(base_dir, user_id, file_name))
}


# -------------------- build plugin arg -----------------------
#' Build Plugin Arguments for Storage Backend Functions
#'
#' Constructs a list of arguments for storage plugin functions (e.g.,
#' `load_from_file`, `save_to_mongo`). It merges the base arguments passed
#' directly with plugin-specific configuration from environment variables.
#'
#' Supports backends such as local file system, MongoDB, and Google Drive.
#'
#' @param backend A string indicating the backend (e.g., "file", "mongo",
#' "gdrive").
#' @param mode The operation mode, one of "load", "save", "file_exists", or
#' "remove".
#' @param ... Additional arguments (e.g., `user_id`, `file_name`, etc.).
#'
#' @return A named list of arguments suitable for `do.call()` when calling
#' a plugin function.
#'
#' @examples
#' Sys.setenv(ACCOUNT_BASE_DIR = "user_accounts")
#' args <- build_plugin_args(
#'   "file", "load",
#'   user_id = "user1",
#'   file_name = "account_tree.Rds"
#' )
#' # Returns something like:
#' # list(user_id = "user1", file_name = "account_tree.Rds",
#' #      base_dir = "user_accounts")
#'
#' @export

build_plugin_args <- function(backend, mode = "load", ...) {
  base_args <- list(...)

  plugin_args <- switch(
    paste0(mode, "_", backend),

    # ==== File Backend ====
    "load_file" = list(
      base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
    ),
    "save_file" = list(
      base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
    ),
    "file_exists_file" = list(
      base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
    ),
    "remove_file" = list(
      base_dir = Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts")
    ),

    # ==== Mongo Backend ====
    "load_mongo" = list(
      uri = Sys.getenv("MONGO_URI"),
      db = Sys.getenv("MONGO_DB")
    ),
    "save_mongo" = list(
      uri = Sys.getenv("MONGO_URI"),
      db = Sys.getenv("MONGO_DB")
    ),
    "file_exists_mongo" = list(
      uri = Sys.getenv("MONGO_URI"),
      db = Sys.getenv("MONGO_DB")
    ),
    "remove_mongo" = list(
      uri = Sys.getenv("MONGO_URI"),
      db = Sys.getenv("MONGO_DB")
    ),

    # ==== Google Drive Backend ====
    "load_gdrive" = list(
      folder_id = Sys.getenv("GDRIVE_FOLDER_ID")
    ),
    "save_gdrive" = list(
      folder_id = Sys.getenv("GDRIVE_FOLDER_ID")
    ),
    "file_exists_gdrive" = list(
      folder_id = Sys.getenv("GDRIVE_FOLDER_ID")
    ),
    "remove_gdrive" = list(
      folder_id = Sys.getenv("GDRIVE_FOLDER_ID")
    ),

    stop("Unknown plugin configuration for backend: ", backend)
  )

  c(base_args, plugin_args)
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

verify_token <- function(token, secret) {
  tryCatch({
    jose::jwt_decode_hmac(token, secret = secret)
  }, error = function(e) NULL)
}


# -------------------- file lock ---------------------------------------------
#' Acquire a Lock for a User's Account Tree using Plugin-Based File Access
#'
#' Ensures exclusive access to a user's account tree by creating a lock file.
#' Waits until the lock is released or a timeout is reached. Uses the plugin
#' architecture to support multiple storage backends.
#'
#' @param user_id The user ID whose account tree should be locked.
#' @param expr An expression to evaluate within the lock.
#' @param timeout Maximum time in seconds to wait for the lock. Default is 1800.
#'
#' @return Returns the result of evaluating `expr`.
#' @export
with_account_lock <- function(
  user_id,
  expr,
  timeout = 1800
) {
  if (!is_valid_user_id(user_id)) {
    stop("Invalid user ID format")
  }

  lockfile <- "account_tree.lock"
  start_time <- Sys.time()

  while (user_file_exists(user_id, lockfile)) {
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Could not acquire lock for user [", user_id, "]: timeout reached")
    }
    Sys.sleep(0.1)
  }

  # Create the lock file using the plugin
  save_user_file(
    user_id, object = as.character(Sys.getpid()), file_name = lockfile
  )

  # Ensure lock is removed after execution
  on.exit({
    remove_user_file(user_id, file_name = lockfile)
  }, add = TRUE)

  force(expr)
}

###################### Get minimal tree ##################################

#' Get a Minimal Version of the Account Tree
#'
#' Retrieves a lightweight representation of an account object,
#' containing only essential details.
#' This is particularly useful for front-end initialization,
#' as it avoids fetching large volumes of data or making multiple
#' batch requests.
#'
#' @param account An R6 account object from the \code{finman} package.
#' @param n Integer. The number of days within which to compute
#'   the amount due. Default is 30.
#' @param daterange A length-2 \code{Date} vector specifying the
#'   date range for statistics (spending, total due, etc.).
#'   Default is from 1,000 years ago to today
#'   (\code{c(Sys.Date() - 365000, Sys.Date())}).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{name} – Account name.
#'     \item \code{account_uuid} – Account unique identifier.
#'     \item \code{balance} – Current account balance.
#'     \item \code{transactions} – Transaction history.
#'     \item \code{child_accounts_list} – Summary list of child accounts
#'       (name and UUID).
#'     \item \code{total_balance} – Total balance including child accounts.
#'     \item \code{total_due} – Total amount due including child accounts.
#'     \item \code{compute_total_due_within_n_days} – Amount due within the
#'       specified period.
#'     \item \code{spending}, \code{total_income}, \code{allocated_amount},
#'           \code{income_utilization}, \code{walking_amount_due},
#'           \code{walking_balance} – Computed statistics for the specified
#'           date range.
#'     \item \code{parent_uuid}, \code{allocation}, \code{account_status},
#'           \code{priority} – Additional details for \code{ChildAccount} objects.
#'     \item \code{due_date}, \code{fixed_amount}, \code{account_type},
#'           \code{account_freq}, \code{account_periods} – Additional details
#'           for \code{GrandchildAccount} objects.
#'     \item \code{child_accounts} – Recursively minimal child account trees.
#'   }
#'
#' @examples
#' \dontrun{
#' library(finman)
#'
#' # Create main account
#' main_account <- MainAccount$new("Main")
#'
#' # Add tier 2 accounts
#' main_account$add_child_account(
#'   ChildAccount$new("Needs", allocation = 0.55))
#' main_account$add_child_account(
#'   ChildAccount$new("Goals", allocation = 0.25))
#' main_account$add_child_account(
#'   ChildAccount$new("Debt Repayment", allocation = 0.20))
#'
#' # Add tier 3 accounts (example: Needs -> Rent)
#' main_account$child_accounts$`Needs`$add_child_account(
#'   GrandchildAccount$new(
#'     "Rent",
#'     allocation = 0.20,
#'     fixed_amount = 7500,
#'     account_type = "Bill",
#'     freq = 30,
#'     due_date = dmy("28-1-2025")
#'   )
#' )
#'
#' # Deposit and withdraw
#' main_account$deposit(10000, "TXN001", "Mpesa")
#' main_account$find_account("Rent")$withdraw(200, "TXN005", "Cash")
#'
#' # Get minimal account tree
#' tree <- minimal_tree(main_account)
#' }
#'
#' @export
minimal_tree<-function(account,n=30,daterange = c(Sys.Date() - 365000, Sys.Date())){
  details<- list(
    name = account$name,
    account_uuid = account$uuid,
    account_class = class(account)[1],
    balance = account$get_balance(),
    transactions =account$get_transactions(),
    child_accounts_list = sapply(
      account$child_accounts,
      function(acc){
        list(
        name = acc$name,
        uuid =acc$uuid
        )
      },
      simplify = F
    ),
    total_balance = account$compute_total_balance(),
    total_due = account$compute_total_due(),
    #construct name based on provided n
    compute_total_due_within_n_days = account$compute_total_due_within_n_days(n),
    spending = account$spending(daterange=daterange),
    total_income =account$total_income(daterange=daterange),
    allocated_amount = account$allocated_amount(daterange=daterange),
    income_utilization = account$income_utilization(daterange=daterange),
    walking_amount_due = account$walking_amount(daterange=daterange),
    walking_balance = account$walking_amount(amt_type = "Balance", daterange=daterange)
  )

    #add tier 2 details
    if(inherits(account,"ChildAccount")){
      details$parent_uuid = account$parent$uuid
      details$parent_name = account$parent$name
      details$allocation = account$allocation
      details$account_status = account$get_account_status()
      details$priority = account$get_priority()
    }

    # tier 3
    if(inherits(account,"GrandchildAccount")){
      details$due_date = account$get_due_date()
      details$fixed_amount= account$get_fixed_amount()
      details$account_type = account$get_account_type()
      details$account_freq = account$get_account_freq()
      details$account_periods = account$get_account_periods()
    }
  if(length(account$child_accounts)==0 || is.null(account$child_accounts)){
    details$child_accounts<-list()
  } else{
    details$child_accounts = sapply(
      account$child_accounts,
      minimal_tree,
      simplify = F
    )
  }

  return(invisible(details))
}

###################### remove_account ##################################
###################### remove_account ##################################

#' Remove an Account (Main or Sub-Account)
#'
#' Deletes an account from the account tree.
#' If the \code{uuid} matches the main/root account, the user's entire
#' account file is removed. Otherwise, the function locates the target
#' account within the tree and removes it from its parent.
#'
#' @param tree An R6 account tree object (e.g., created via
#'   \code{MainAccount$new()}).
#' @param user_id Character string. The unique identifier of the user who
#'   owns the account tree. Used to locate and delete the user's
#'   persisted file.
#' @param uuid Character string. The UUID of the account to be removed.
#'
#' @return Logical \code{TRUE} if the account was successfully removed.
#'   Throws an error if the account or its parent cannot be found.
#'
#' @details
#' - If the UUID corresponds to the main/root account:
#'   \itemize{
#'     \item The user's \code{"account_tree.Rds"} file is deleted from
#'           persistent storage.
#'   }
#' - If the UUID corresponds to a sub-account:
#'   \itemize{
#'     \item The target account is located via
#'           \code{tree$find_account_by_uuid(uuid)}.
#'     \item Its parent is identified, and the child is removed from
#'           \code{parent$child_accounts}.
#'     \item The updated tree is saved back to persistent storage with
#'           \code{save_user_file()}.
#'   }
#'
#' @seealso [find_account_by_uuid()], [save_user_file()]
#'
#' @examples
#' \dontrun{
#' library(finman)
#'
#' # Create a main account
#' main <- MainAccount$new("Main")
#'
#' # Add a sub-account
#' needs <- ChildAccount$new("Needs", allocation = 0.5)
#' main$add_child_account(needs)
#'
#' # Add a grandchild account
#' rent <- GrandchildAccount$new(
#'   "Rent", allocation = 0.3, fixed_amount = 7500,
#'   account_type = "Bill", freq = 30, due_date = Sys.Date() + 30
#' )
#' needs$add_child_account(rent)
#'
#' # Persist the account tree
#' save_user_file("user123", main, "account_tree.Rds")
#'
#' # Remove the grandchild account by uuid
#' remove_account(main, "user123", rent$uuid)
#'
#' # Remove the main account (deletes the file)
#' remove_account(main, "user123", main$uuid)
#' }
#' @export
remove_account <- function(tree,user_id, uuid) {
  # If uuid matches main account
  if (tree$uuid == uuid) {
    remove_user_file(user_id)
    return(TRUE)
  }

  # Otherwise, find the account and its parent
  account <- tree$find_account_by_uuid(uuid)
  if (is.null(account)) {
    stop(sprintf("Account with uuid %s not found", uuid))
  }
  parent <- account$parent
  if (is.null(parent)) {
    stop(sprintf("Parent for account %s not found", uuid))
  }

  # Remove the child from parent's list
  parent$child_accounts <- Filter(
    function(acc) acc$uuid != uuid,
    parent$child_accounts
  )

  save_user_file(user_id, tree)
  TRUE
}

