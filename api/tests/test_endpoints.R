library(testthat)
library(httr)
library(jose)
library(withr)
library(here)
library(jsonlite)
library(finman)
library(tidyverse)
library(mockery)

# =========================================================
# wait_for_server_ready function
# pings server every 0.1 sec until we receive a TRUE
# Receiving True means server is Ready or it times out
# it helps sending requests when server isnt ready
# =========================================================

wait_for_server_ready <- function(
    url = "http://127.0.0.1:8000/__ping__",
    timeout = 80
) {
  start_time <- Sys.time()
  while (as.numeric(Sys.time() - start_time, units = "secs") < timeout) {
    res <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (!is.null(res) && httr::status_code(res) == 200) return(TRUE)
    Sys.sleep(0.10)
  }
  stop("Server did not become ready within timeout.")
}

# =========================================================
# Setting up test user account (testuser)
# =========================================================
# Setup: start server, create user, etc.
tmp_dir <- tempfile("test-api-deposit-")
dir.create(tmp_dir, recursive = TRUE)
Sys.setenv(ACCOUNT_BASE_DIR = tmp_dir)
Sys.setenv(ACCOUNT_BACKEND = "file")
Sys.setenv(MAX_REQUESTS = 100000)
Sys.setenv(WINDOW_SIZE = 3600)
Sys.setenv(JWT_SECRET = "test-secret")
secret_key <- Sys.getenv("JWT_SECRET")


uuid <- create_user_account_base(
  user_id = "testuser",
  base_dir = tmp_dir,
  initial_balance = 500
)
#load_user_file("testuser", "account_tree.Rds")$uuid

# ============================================================================
# Setting up server background call function
# this helps avoid blocking main R session which will be used to interact with
# the server during testing(sending requests and getting responses)
# ===========================================================================

log_out <- tempfile("server-out-", fileext = ".log")
log_err <- tempfile("server-err-", fileext = ".log")

server <- callr::r_bg(
  function(main_file, jwt, base_dir, project_dir) {
    setwd(project_dir)
    Sys.setenv(JWT_SECRET = jwt)
    Sys.setenv(ACCOUNT_BASE_DIR = base_dir)
    Sys.setenv(ACCOUNT_BACKEND = "file")
    source(main_file)
  },
  args = list(
    main_file = here("api", "main.R"),
    jwt = secret_key,
    base_dir = tmp_dir,
    project_dir = here()
  ),
  stdout = log_out,
  stderr = log_err
)


withr::defer({
  if (server$is_alive()) server$kill()
  cat("📤 Server stdout:\n")
  cat(readLines(log_out, warn = FALSE), sep = "\n")
  cat("\n📥 Server stderr:\n")
  cat(readLines(log_err, warn = FALSE), sep = "\n")
}, envir = parent.frame())

# waiting server to get ready
wait_for_server_ready("http://127.0.0.1:8000/__ping__")

# ============================================================================
# Generating dummy auth token for user account (testuser)
# ============================================================================
# Helper: auth token
token <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser", role = "user"),
  secret = secret_key
)

# ============================================================================
# Tier one endpoints
# ============================================================================

# ============================================================================
# Testing the /register endpoint
# ============================================================================
test_that("Register a new user with default balance", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )
  # ✅ Register a new user
  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id),
    encode = "json"
  )
  parsed <- jsonlite::fromJSON(rawToChar(res$content))

  expect_equal(status_code(res), 200)
  expect_true(parsed$success)
  expect_equal(parsed$user_id, user_id)
  expect_match(parsed$uuid, "^[a-f0-9\\-]+$")

  # ✅ Execution metadata checks (covers async result enrichment)
  expect_true(!is.null(parsed$start_time))
  expect_true(!is.null(parsed$end_time))
  expect_true(!is.null(parsed$execution_time))
  expect_match(parsed$start_time, "^\\d{4}-\\d{2}-\\d{2}T")
})


test_that("Register with initial balance", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id, initial_balance = 150.75),
    encode = "json"
  )
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)
  expect_equal(parsed$user_id, user_id)
  expect_match(parsed$uuid, "^[a-f0-9\\-]+$")
})

test_that("Register handles NULL,NA,INf,-Inf, initial_balance", {
  #✅ Case 1: with  NULL initial_balance
  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(jwt_claim(user_id = user_id, role = "user"), secret = secret_key)

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id, initial_balance = NULL),
    encode = "json"
  )
  parsed <- fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)

  #✅ C Case 2: with NA initial_balance
  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(jwt_claim(user_id = user_id, role = "user"), secret = secret_key)


  res <- POST(
    url = "http://127.0.0.1:8000/register",
    add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id, initial_balance = NA),
    encode = "json"
  )
  parsed <- fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)

  #✅ C Case 3: with Inf initial_balance
  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(jwt_claim(user_id = user_id, role = "user"), secret = secret_key)


  res <- POST(
    url = "http://127.0.0.1:8000/register",
    add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id, initial_balance = Inf),
    encode = "json"
  )
  parsed <- fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)


  #✅ Case 4: with -Inf initial_balance

  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(jwt_claim(user_id = user_id, role = "user"), secret = secret_key)


  res <- POST(
    url = "http://127.0.0.1:8000/register",
    add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id, initial_balance = -Inf),
    encode = "json"
  )
  parsed <- fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)

})


test_that("🚫 Register fails with missing user_id", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(initial_balance = 100),
    encode = "json"
  )

  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 400)
  expect_false(parsed$success)
  expect_match(parsed$error, "user_id is required")
})

test_that("🚫 Register fails with empty user_id", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = ""),
    encode = "json"
  )

  expect_equal(status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "user_id is required")
})

test_that("🚫 Register fails with invalid user_id format", {
  user_id <- "!!!invalid@#"
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id),
    encode = "json"
  )
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 500)
  expect_false(parsed$success)
  expect_match(parsed$error, "Invalid user ID format")
})


test_that("🚫 Register with invalid initial_balance (non-numeric)", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id, initial_balance = "invalid"),
    encode = "json"
  )

  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 200)
  expect_true(parsed$success)
  expect_equal(parsed$user_id, user_id)
  # Should still assign default 0 or NA based on coercion behavior
})


test_that(" 🚫 Registering an already existing user returns 409 or succeeds based on backend", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  # First registration
  res1 <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id),
    encode = "json"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(status_code(res1), 200)

  # Second registration (expect conflict or overwrite depending on system design)
  res2 <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id),
    encode = "json"
  )

  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(status_code(res2), 500)
  expect_match(parsed2$error, "User already exists")
})

test_that("🚫 Register fails without Authorization header", {
  res <- POST(
    url = "http://127.0.0.1:8000/register",
    body = list(user_id = uuid::UUIDgenerate()),
    encode = "json"
  )
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 401)
  expect_match(parsed$error, "Missing or invalid token")
})

test_that("✅  Register returns execution timing metadata", {
  user_id <- uuid::UUIDgenerate()
  tokenregister <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  res <- POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", tokenregister)),
    body = list(user_id = user_id),
    encode = "json"
  )
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_true(!is.null(parsed$execution_time))
  expect_match(parsed$start_time, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_match(parsed$end_time, "^\\d{4}-\\d{2}-\\d{2}T")
})


# with_mocked_bindings(
#   my_future_promise = function(expr) {
#     promise(function(resolve, reject) {
#       reject(simpleError("🔥 async crash simulated"))
#     })
#   },
#   .package = 'promises',  # 👈 force it to use the global environment
#   {
#     test_that("register hits %...!% handler and returns 503", {
#       user_id <- uuid::UUIDgenerate()
#       token <- jwt_encode_hmac(jwt_claim(user_id = user_id, role = "user"), secret = secret_key)
#
#       res <- httr::POST(
#         url = "http://127.0.0.1:8000/register",
#         httr::add_headers(Authorization = paste("Bearer", token)),
#         body = list(user_id = user_id),
#         encode = "json"
#       )
#
#       parsed <- jsonlite::fromJSON(rawToChar(res$content))
#       expect_equal(httr::status_code(res), 503)
#       expect_false(parsed$success)
#       expect_match(parsed$error, "Failed to connect")
#     })
#   }
# )

# ============================================================================
# Testing the /deposit endpoint
# ============================================================================

test_that("other POST /deposit endpoint edge cases", {

  ## testing deposits
  # ✅ Case 1: Successful deposit
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = 1000, channel = "bank"),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$account_uuid, uuid)
  expect_equal(parsed1$balance, 1500)

  # 🚫 Case 2: Missing token
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    body = list(uuid = uuid, amount = 100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res2), 401)
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Missing or invalid token", ignore.case = TRUE)

  # 🚫 Case 3: Invalid UUID
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent-uuid", amount = 100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res3), 403)
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_false(parsed3$success)
  expect_match(
    parsed3$error, "Account not found or unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 4: Missing uuid

  res <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(amount = 100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 5: Missing amount

  res <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "amount is required", ignore.case = TRUE)

  # 🚫 Case 6:  Negative amount
  res <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = -100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "amount must be > 0", ignore.case = TRUE)


  # 🚫  Case 7:  Non-numeric amount

  res <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = "ten", channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "amount must be numeric", ignore.case = TRUE)

  #🚫 Case 8: Missing channel
  res <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = 100),
    encode = "form"
  )
  expect_equal(httr::status_code(res), 400)
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_false(parsed$success)
  expect_match(parsed$error, "channel is required", ignore.case = TRUE)
})

# create a new user
create_user_account_base(
  user_id = "testuser21",
  base_dir = tmp_dir,
  initial_balance = 1300
)

token11 <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser21", role = "user"),
  secret = secret_key
)
to_corrupt_uuid <- load_user_file("testuser21", "account_tree.Rds")$uuid

path_to_corrupt<-file.path(
  Sys.getenv("ACCOUNT_BASE_DIR", "user_accounts"),
  "testuser21",
  "account_tree.Rds"
)
saveRDS(
  list(
    invalid = "data"
  ),
  file = path_to_corrupt
)
test_that("POST /deposit with corrupt account", {
  # 🚫 Case 9: Wrong user_id
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token11)),
    body = list(uuid = to_corrupt_uuid, amount = 1000, channel = "bank"),
    encode = "form"
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 500)
  expect_false(parsed9$success)
  expect_match(parsed9$error, "Deposit failed:")
})




# ============================================================================
# Testing the /withdraw endpoint
# ============================================================================

test_that("POST /withdraw works", {
  ## testing withdrawals
  # ✅ Case 4: Successful withdrawal
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = 200, channel = "mpesa"),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 200)
  expect_true(parsed4$success)
  expect_equal(parsed4$account_uuid, uuid)
  expect_equal(parsed4$balance, 1300)  # 1500 - 200

  # 🚫 Case 5: Withdraw more than balance
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = 999999, channel = "bank"),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "insufficient|balance", ignore.case = TRUE)

  # 🚫 Case 6: Withdraw with invalid token
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = "Bearer invalidtoken"),
    body = list(uuid = uuid, amount = 50, channel = "bank"),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 401)

  # 🚫 Case 7: Withdraw with negative amount
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = -100, channel = "bank"),
    encode = "form"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "invalid.*amount", ignore.case = TRUE)

  # 🚫 Case 8: Missing uuid in body
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(amount = 100, channel = "bank"),
    encode = "form"
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_false(parsed8$success)
  expect_match(parsed8$error, "uuid.*required", ignore.case = TRUE)

  # 🚫 Case 9: Missing amount in body
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, channel = "bank"),
    encode = "form"
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 400)
  expect_false(parsed9$success)
  expect_match(parsed9$error, "amount.*required", ignore.case = TRUE)

  # 🚫 Case 10: Invalid UUID format
  res10 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "not-a-uuid", amount = 100, channel = "bank"),
    encode = "form"
  )
  parsed10 <- jsonlite::fromJSON(rawToChar(res10$content))
  expect_equal(httr::status_code(res10), 403)
  expect_false(parsed10$success)
  expect_match(
    parsed10$error,
    "Account not found or unauthorized",
    ignore.case = TRUE
  )

})


#🚫 Assuming another_uuid is a real UUID but belongs to someone else
other_uuid<-uuid::UUIDgenerate()
res11 <- httr::POST(
  url = "http://127.0.0.1:8000/withdraw",
  httr::add_headers(Authorization = paste("Bearer", token)),
  body = list(uuid = other_uuid, amount = 100, channel = "bank"),
  encode = "form"
)
parsed11 <- jsonlite::fromJSON(rawToChar(res11$content))
expect_equal(httr::status_code(res11), 403)
expect_false(parsed11$success)
expect_match(
  parsed11$error,
  "Account not found or unauthorized",
  ignore.case = TRUE
)


# ============================================================================
# Testing the /add_sub_account endpoint
# ============================================================================
test_that("POST /add_sub_account endpoint edge cases", {
  #✅  Case 8: Successful child account creation
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Needs",
      allocation = 0.4
    )
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 200)
  expect_true(parsed8$success)
  expect_equal(parsed8$child_type, "ChildAccount")
  expect_equal(parsed8$allocation, 0.4)

  # 🚫 Case 9: Duplicate name under same parent
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Needs",
      allocation = 0.3
    )
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 400)
  expect_false(parsed9$success)
  expect_match(parsed9$error, "already exists", ignore.case = TRUE)

  #🚫 Case 10: Parent UUID not found
  res10 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = "invalid-parent-uuid",
      name = "Savings",
      allocation = 0.2
    )
  )
  parsed10 <- jsonlite::fromJSON(rawToChar(res10$content))
  expect_equal(httr::status_code(res10), 404)
  expect_false(parsed10$success)

  #🚫 Case 11: Missing token
  res11 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Investments",
      allocation = 0.2
    )
  )
  expect_equal(httr::status_code(res11), 401)

  #🚫 Case 12: Invalid allocation
  res12 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Emergency",
      allocation = "not-a-number"
    )
  )
  parsed12 <- jsonlite::fromJSON(rawToChar(res12$content))
  expect_equal(httr::status_code(res12), 400)
  expect_false(parsed12$success)
  expect_match(parsed12$error, "allocation.*number", ignore.case = TRUE)

  #✅  Case 13: Successful grandchild account creation
  # First, add a child account
  res13a <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Goals",
      allocation = 0.2
    )
  )
  parsed13a <- jsonlite::fromJSON(rawToChar(res13a$content))
  child_uuid <- load_user_file("testuser", "account_tree.Rds")$
    find_account("Goals")[[1]]$uuid

  res13b <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = child_uuid,
      name = "Farming",
      allocation = 0.5,
      fixed_amount = 500,
      due_date = Sys.Date(),
      freq = 30,
      account_type = "Bill"
    )
  )
  parsed13b <- jsonlite::fromJSON(rawToChar(res13b$content))
  expect_equal(httr::status_code(res13b), 200)
  expect_true(parsed13b$success)
  expect_equal(parsed13b$child_type, "GrandchildAccount")

  #✅  Case 14: anything past grand child is grandchild
  grandchild_uuid <- load_user_file("testuser", "account_tree.Rds")$
    find_account("Farming")[[1]]$uuid

  res14 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = grandchild_uuid,
      name = "SubFarming",
      allocation = 0.1
    )
  )
  parsed14 <- jsonlite::fromJSON(rawToChar(res14$content))
  expect_equal(httr::status_code(res14), 200)
  expect_true(parsed14$success)
  expect_equal(parsed14$child_type, "GrandchildAccount")
})


#🚫 Case 15: Missing parent_uuid

res15 <- httr::POST(
  url = "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "form",
  body = list(
    name = "ForgotUUID",
    allocation = 0.1
  )
)
parsed15 <- jsonlite::fromJSON(rawToChar(res15$content))
expect_equal(httr::status_code(res15), 400)
expect_false(parsed15$success)
expect_match(parsed15$error, "uuid.*required", ignore.case = TRUE)


#🚫 Case 16: Allocation > 1

res16 <- httr::POST(
  url = "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "form",
  body = list(
    parent_uuid = uuid,
    name = "Overflow",
    allocation = 1.2
  )
)
parsed16 <- jsonlite::fromJSON(rawToChar(res16$content))
expect_equal(httr::status_code(res16), 400)
expect_false(parsed16$success)
expect_match(parsed16$error, "allocation", ignore.case = TRUE)


#🚫 Case 17: Allocation < 0

res17 <- httr::POST(
  url = "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "form",
  body = list(
    parent_uuid = uuid,
    name = "NegativeAlloc",
    allocation = -0.1
  )
)
parsed17 <- jsonlite::fromJSON(rawToChar(res17$content))
expect_equal(httr::status_code(res17), 400)
expect_false(parsed17$success)
expect_match(parsed17$error, "allocation", ignore.case = TRUE)

#🚫 Case 19: Invalid due date format
child_uuid <- load_user_file("testuser", "account_tree.Rds")$
  find_account("Goals")[[1]]$uuid

res19 <- httr::POST(
  url = "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "form",
  body = list(
    parent_uuid = child_uuid,
    name = "BadDate",
    allocation = 0.1,
    due_date = "not-a-date"
  )
)

parsed19 <- jsonlite::fromJSON(rawToChar(res19$content))
expect_equal(httr::status_code(res19), 400)
expect_false(parsed19$success)
expect_match(parsed19$error, "Invalid due date: must be a valid date", ignore.case = TRUE)

#🚫 Case 20: Coercion edge case for priority or freq

res20 <- httr::POST(
  url = "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "form",
  body = list(
    parent_uuid = uuid,
    name = "BadPriority",
    allocation = 0.1,
    priority = "low"
  )
)
parsed20 <- jsonlite::fromJSON(rawToChar(res20$content))
expect_equal(httr::status_code(res20), 400)
expect_false(parsed20$success)
expect_match(parsed20$error,
             "Invalid priority: must be a non-negative integer",
             ignore.case = TRUE)

# ============================================================================
# Testing the /distribute endpoint
# ============================================================================

# create a new user
create_user_account_base(
  user_id = "testuser1",
  base_dir = tmp_dir,
  initial_balance = 1300
)
# Generate a new token for the new user
token <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser1", role = "user"),
  secret = secret_key
)

# refresh uuid of the main account
main_uuid <- load_user_file("testuser1", "account_tree.Rds")$uuid


test_that("POST /distribute endpoint handles edge cases", {
  # Setup: Create account tree with Main > Child1 + Child2
  res1 <- httr::POST(
    "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      name = "DistributionChild1",
      allocation = 0.5,
      priority = 1
    ),
    encode = "json"
  )
  uuid1 <- jsonlite::fromJSON(rawToChar(res1$content))$uuid

  res2 <- httr::POST(
    "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      name = "DistributionChild2",
      allocation = 0.5,
      priority = 2
    ),
    encode = "json"
  )
  uuid2 <- jsonlite::fromJSON(rawToChar(res2$content))$uuid

  expect_equal(
    length(load_user_file("testuser", "account_tree.Rds")$child_accounts),
    2
  )

  # ✅ Case  1: Successful distribution
  res15 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid, amount = 1000),
    encode = "json"
  )
  parsed15 <- jsonlite::fromJSON(rawToChar(res15$content))
  expect_equal(httr::status_code(res15), 200)
  expect_true(parsed15$success)
  expect_match(parsed15$message, "Amount successifully distributed to children")
  expect_equal(load_user_file("testuser1", "account_tree.Rds")$balance, 300)
  expect_equal(
    load_user_file("testuser1", "account_tree.Rds")$child_accounts[[1]]$balance,
    500
  )

  expect_equal(
    load_user_file("testuser1", "account_tree.Rds")$child_accounts[[2]]$balance,
    500
  )

  # ✅ Case 2:Distribution with custom transaction
  res16 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid, amount = 200, transaction = "custom-txn"),
    encode = "json"
  )
  parsed16 <- jsonlite::fromJSON(rawToChar(res16$content))
  expect_equal(httr::status_code(res16), 200)
  expect_true(parsed16$success)
  expect_match(parsed16$message, "Amount successifully distributed to children")

  # 🚫 Case 3:Missing  uuid
  res_missing_uuid <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(amount = 100),
    encode = "json"
  )
  parsed_missing_uuid <- jsonlite::fromJSON(rawToChar(res_missing_uuid$content))
  expect_equal(httr::status_code(res_missing_uuid), 400)
  expect_false(parsed_missing_uuid$success)
  expect_match(parsed_missing_uuid$error, "uuid.*required", ignore.case = TRUE)

  # 🚫 Case 4: Distribution to nonexistent UUID
  res17 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent-uuid", amount = 100),
    encode = "json"
  )
  parsed17 <- jsonlite::fromJSON(rawToChar(res17$content))
  expect_equal(httr::status_code(res17), 403)
  expect_false(parsed17$success %||% FALSE)
  expect_match(parsed17$error, "not found|unauthorized", ignore.case = TRUE)

  # 🚫 Case 5: Negative amount
  res18 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid, amount = -100),
    encode = "json"
  )
  parsed18 <- jsonlite::fromJSON(rawToChar(res18$content))
  expect_equal(httr::status_code(res18), 400)
  expect_false(parsed18$success)
  expect_match(parsed18$error, "amount must be > 0", ignore.case = TRUE)

  # 🚫 Case 6: Non-numeric amount
  res19 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid, amount = "abc"),
    encode = "json"
  )
  parsed19 <- jsonlite::fromJSON(rawToChar(res19$content))
  expect_equal(httr::status_code(res19), 400)
  expect_false(parsed19$success)
  expect_match(parsed19$error, "amount must be numeric", ignore.case = TRUE)

  # 🚫 Case 7: Missing amount
  res20 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid),
    encode = "json"
  )
  parsed20 <- jsonlite::fromJSON(rawToChar(res20$content))
  expect_equal(httr::status_code(res20), 400)
  expect_false(parsed20$success)
  expect_match(parsed20$error, "amount is required", ignore.case = TRUE)

  # 🚫 Case 8: Unauthorized token
  res21 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = "Bearer badtoken"),
    body = list(uuid = main_uuid, amount = 100),
    encode = "json"
  )
  expect_equal(httr::status_code(res21), 401)

  # 🚫 Case 9: Zero amount
  res22 <- httr::POST(
    "http://127.0.0.1:8000/distribute",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = main_uuid, amount = 0),
    encode = "json"
  )
  parsed22 <- jsonlite::fromJSON(rawToChar(res22$content))
  expect_equal(httr::status_code(res22), 400)
  expect_false(parsed22$success)
  expect_match(parsed22$error, "zero|amount", ignore.case = TRUE)
})
# 🚫 Case 10: Insufficient balance

res_insufficient <- httr::POST(
  "http://127.0.0.1:8000/distribute",
  httr::add_headers(Authorization = paste("Bearer", token)),
  body = list(uuid = main_uuid, amount = 99999),
  encode = "json"
)
parsed_insufficient <- jsonlite::fromJSON(rawToChar(res_insufficient$content))
expect_equal(httr::status_code(res_insufficient), 400)
expect_false(parsed_insufficient$success)
expect_match(parsed_insufficient$error, "insufficient balance", ignore.case = TRUE)



# ============================================================================
# Testing the /set_child_allocation endpoint
# ============================================================================
main_uuid <- load_user_file("testuser", "account_tree.Rds")$uuid
token <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser", role = "user"),
  secret = secret_key
)

test_that("POST /set_child_allocation works", {
  ## Testing /set_child_allocation endpoint

  # ✅ Case 1: Successful allocation update
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 0.4
    ),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_match(parsed1$message, "updated to 0.4")

  # 🚫Case 2:  fake-id
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = "fake-id",
      child_name = "Needs",
      allocation = 0.3
    ),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 404)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Parent account not found", ignore.case = TRUE)

  # 🚫 Case 3: Missing child account
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "NotAChild",
      allocation = 0.2
    ),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 404)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Child account not found", ignore.case = TRUE)

  # 🚫 Case 4: Invalid allocation (> 1)
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 1.5
    ),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "between 0 and 1", ignore.case = TRUE)

  # 🚫 Case 5: Missing allocation
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs"
    ),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "Allocation is required", ignore.case = TRUE)

  # 🚫 Case 6: Allocation causes total to exceed 1
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 0.99  # Assuming others already take ~0.2
    ),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Total allocation exceeds", ignore.case = TRUE)

  # ✅ Case 7: Allocation to 0 deactivates child
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 0
    ),
    encode = "form"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 200)
  expect_true(parsed7$success)
  expect_match(parsed7$message, "updated to 0", ignore.case = TRUE)

  # (optional) confirm deactivation if using a helper
  tree <- load_user_file("testuser", "account_tree.Rds")
  account <- tree$find_account_by_uuid(main_uuid)
  expect_equal(account$child_accounts[["Needs"]]$status, "inactive")

  # 🚫 Case 8: Allocation is non-numeric
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = "forty"
    ),
    encode = "form"
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_false(parsed8$success)
  expect_match(parsed8$error, "between 0 and 1", ignore.case = TRUE)

  # 🚫 ase 9: Missing Authorization token
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 0.2
    ),
    encode = "form"
  )
  expect_equal(httr::status_code(res9), 401)


  # 🚫 Case 10: Edge allocation = 1.0
  res10 <- httr::POST(
    url = "http://127.0.0.1:8000/set_child_allocation",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      parent_uuid = main_uuid,
      child_name = "Needs",
      allocation = 1
    ),
    encode = "form"
  )
  parsed10 <- jsonlite::fromJSON(rawToChar(res10$content))
  expect_equal(httr::status_code(res10), 400)
  expect_false(parsed10$success)

})


# ============================================================================
# Testing the /get_balance endpoint
# ============================================================================
test_that("GET /get_balance works", {
  ## Testing /get_balance endpoint

  # ✅ Case 1: Valid UUID returns balance
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = main_uuid),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$balance))
  expect_true(parsed1$balance >= 0)

  # 🚫 Case 2: Invalid UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = "non-existent-uuid"),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(
    parsed2$error,
    "account not found|unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 3: Missing UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    httr::add_headers(Authorization = paste("Bearer", token))
    # No query param
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 4: No Authorization token
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res4), 401)


  # 🚫 Case 5: Empty UUID
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = ""),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 6: Unauthorized access to another user's account
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = "1234"),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "account not found|unauthorized", ignore.case = TRUE)

  # 🚫 Case 7: Unauthorized access to another user's account with a valid UUID

  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/get_balance",
    query = list(uuid = uuid::UUIDgenerate()),  # belongs to another user
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 403)
  expect_false(parsed7$success)


})


# ============================================================================
# Testing the /get_transactions endpoint
# ============================================================================
# create a new user for case 2
create_user_account_base(
  user_id = "testuser2",
  base_dir = tmp_dir,
  initial_balance = 1300
)

token1 <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser2", role = "user"),
  secret = secret_key
)
empty_account_uuid <- load_user_file("testuser2", "account_tree.Rds")$uuid

test_that("GET /get_transactions works", {
  ## Testing /get_transactions endpoint

  # ✅ Case 1: Valid UUID with transactions
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_type(parsed1$transactions, "list")
  expect_true(parsed1$transaction_count >= 0)
  expect_true("start_time" %in% names(parsed1))
  expect_true("end_time" %in% names(parsed1))
  expect_true("execution_time" %in% names(parsed1))


  # ✅ Case 2: Valid UUID with no transactions
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    httr::add_headers(Authorization = paste("Bearer", token1)),
    query = list(uuid = empty_account_uuid)  # this account has no transactions
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, empty_account_uuid)
  expect_equal(parsed2$transaction_count, 0)
  expect_type(parsed2$transactions, "list")

  # 🚫 Case 3: Invalid UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(
    parsed3$error, "account not found|unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 4: Missing UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no query param
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 5: No Authorization Token
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res5), 401)

  # 🚫 Case 6: empty string UUID
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_transactions",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "uuid is required", ignore.case = TRUE)
})

# ============================================================================
# Testing the /list_child_accounts endpoint
# ============================================================================

test_that("GET /list_child_accounts works", {
  ## Testing /list_child_accounts endpoint

  # ✅ Case 1: Valid parent with child accounts
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.character(parsed1$child_account_names))
  expect_type(parsed1$child_count, "integer")
  expect_true(parsed1$child_count >= 1)

  # ✅ Case 2: Valid parent with no child accounts
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    httr::add_headers(Authorization = paste("Bearer", token1)),
    query = list(
      uuid = empty_account_uuid
    )  # account exists but has no children
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, empty_account_uuid)
  expect_length(parsed2$child_account_names, 0)
  expect_equal(parsed2$child_count, 0)

  # 🚫 Case 3: Invalid parent UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(
    parsed3$error, "parent account not found|unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 4: Missing UUID param
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no query param
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 5: No Authorization token
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    query = list(uuid = main_uuid)
  )
  expect_equal(
    httr::status_code(res5),
    401
  )

  # 🚫 Case 6: empty parent UUID
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/list_child_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(
    parsed6$error, "uuid is required",
    ignore.case = TRUE
  )
})

# ============================================================================
# Testing the /list_all_accounts endpoint
# ============================================================================

leaf_account_uuid <- load_user_file("testuser2", "account_tree.Rds")$uuid
test_that("GET /list_all_accounts works", {
  ## Testing /list_all_accounts endpoint

  # ✅ Case 1: Valid UUID returns full account list
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_type(parsed1$account_names, "character")
  expect_true(parsed1$total_accounts >= 1)
  expect_true("Main" %in% parsed1$account_names)

  # 🚫 Case 2: Invalid UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(
    parsed2$error, "account not found|unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 3: UUID of account with no children (returns at least 1, itself)
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    httr::add_headers(Authorization = paste("Bearer", token1)),
    query = list(uuid = leaf_account_uuid)
  )

  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 200)
  expect_true(parsed3$success)
  expect_equal(parsed3$uuid, leaf_account_uuid)
  expect_type(parsed3$account_names, "character")
  expect_true(
    load_user_file(
      "testuser2",
      "account_tree.Rds"
    )$name %in% parsed3$account_names
  )

  # 🚫 Case 4: Missing UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()  # no UUID
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)  # plumber will throw
  expect_false(parsed4$success)
  expect_match(parsed4$error, "uuid is required", ignore.case = TRUE)

  # 🚫 Case 5: No Authorization header
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res5), 401)

  # 🚫 Case 6: empty parent UUID
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/list_all_accounts",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(
    parsed6$error, "uuid is required",
    ignore.case = TRUE
  )
})


# ============================================================================
# Testing the /find_account_by_name endpoint
# ============================================================================

test_that("GET /find_account_by_name works", {
  ## Testing /find_account_by_name endpoint

  # ✅ Case 1: Match on known name
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_name",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(name = "Needs")
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_match(parsed1$search_name, "Needs")
  expect_true(parsed1$total_matches >= 1)
  expect_true(any(grepl("Needs", parsed1$matches$name)))
  expect_true(is.data.frame(parsed1$matches))
  expect_true(all(c("uuid", "name", "path") %in% names(parsed1$matches)))

  # ✅ Case 2: Name that does not exist
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_name",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(name = "NonExistentAccount")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$total_matches, 0)
  expect_equal(length(parsed2$matches), 0)
  expect_true(is.list(parsed2$matches))

  # 🚫 Case 3: Missing name parameter
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_name",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()  # No name
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(
    httr::status_code(res3),
    400
  )  # Plumber throws due to missing required param
  expect_true(httr::status_code(res3) %in% c(400))
  expect_false(parsed3$success)
  expect_match(parsed3$error, "name is required", ignore.case = TRUE)

  # 🚫 Case 4: No Authorization token
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_name",
    query = list(name = "Needs")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 401)
  expect_match(parsed4$error, "Missing or invalid token", ignore.case = TRUE)
})

# ============================================================================
# Testing the /find_account_by_uuid endpoint
# ============================================================================

test_that("GET /find_account_by_uuid works", {
  ## Testing /find_account_by_uuid endpoint

  # ✅ Case 1: Valid UUID returns account info
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_uuid",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$balance))
  expect_true(parsed1$balance >= 0)
  expect_true(all(c("name", "type", "path") %in% names(parsed1)))

  # 🚫 Case 2: Invalid UUID (not found)
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_uuid",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(
    parsed2$error,
    "account not found|unauthorized",
    ignore.case = TRUE
  )

  # 🚫 Case 3: Missing UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_uuid",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()  # No UUID
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "uuid.*required", ignore.case = TRUE)

  # 🚫 Case 4: No Authorization token
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/find_account_by_uuid",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res4), 401)
})


# ============================================================================
# Testing the /move_balance endpoint
# ============================================================================
tokenz <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser1", role = "user"),
  secret = secret_key
)

main_cc <- load_user_file("testuser1", "account_tree.Rds")
uuid <- main_cc$uuid
source_uuid <- main_cc$find_account("DistributionChild1")[[1]]$uuid
destination_uuid <- main_cc$find_account("DistributionChild2")[[1]]$uuid
source_balance <- main_cc$find_account_by_uuid(source_uuid)$balance
destination_balance <- main_cc$find_account_by_uuid(destination_uuid)$balance

test_that("POST /move_balance works", {
  ## Testing /move_balance endpoint

  # ✅ Setup: deposit money into source account
  res0 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(
      uuid = source_uuid,
      amount = 1000,
      channel = "test-init"
    ),
    encode = "form"
  )
  parsed0 <- jsonlite::fromJSON(rawToChar(res0$content))
  expect_equal(httr::status_code(res0), 200)
  expect_equal(parsed0$balance, source_balance + 1000)

  # ✅ Case 1: Successful transfer
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(
      from_uuid = source_uuid,
      to_uuid = destination_uuid,
      amount = 200
    ),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$amount, 200)
  expect_equal(parsed1$from_uuid, source_uuid)
  expect_equal(parsed1$to_uuid, destination_uuid)
  expect_equal(
    load_user_file(
      "testuser1",
      "account_tree.Rds"
    )$find_account_by_uuid(
      parsed1$from_uuid
    )$balance,
    parsed0$amount + destination_balance - 200
  )

  expect_equal(
    load_user_file(
      "testuser1",
      "account_tree.Rds"
    )$find_account_by_uuid(
      parsed1$to_uuid
    )$balance,
    destination_balance + 200
  )
  expect_match(parsed1$message, "Moved 200")

  # 🚫 Case 2: Source account not found
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(
      from_uuid = "fake-uuid",
      to_uuid = destination_uuid,
      amount = 100
    ),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 404)
  expect_false(parsed2$success)
  expect_match(
    parsed2$error,
    "Source account not found",
    ignore.case = TRUE
  )

  # 🚫 Case 3: Target account not found
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(
      from_uuid = source_uuid,
      to_uuid = "non-existent-uuid",
      amount = 100
    ),
    encode = "form"
  )

  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 404)
  expect_false(parsed3$success)
  expect_match(
    parsed3$error,
    "Target account not found",
    ignore.case = TRUE
  )

  # 🚫 Case 4: Invalid amount
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(
      from_uuid = source_uuid,
      to_uuid = destination_uuid,
      amount = "invalid"
    ),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(
    parsed4$error, "Amount must be a positive number",
    ignore.case = TRUE
  )

  # 🚫 Case 5: Missing fields
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    httr::add_headers(Authorization = paste("Bearer", tokenz)),
    body = list(),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "from_uuid|to_uuid|amount", ignore.case = TRUE)

  # 🚫 Case 6: No token (unauthorized)
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/move_balance",
    body = list(
      from_uuid = source_uuid,
      to_uuid = destination_uuid,
      amount = 100
    ),
    encode = "form"
  )
  expect_equal(httr::status_code(res6), 401)

  # 🚫 Case 7: Zero or negative amount
  res <- POST(url = "http://127.0.0.1:8000/move_balance",
              httr::add_headers(Authorization = paste("Bearer", tokenz)),
              body = list(
                from_uuid = source_uuid,
                to_uuid = destination_uuid,
                amount = 0
              ),
              encode = "json")
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 400)
  expect_match(parsed$error, "Amount must be a positive number")


  # 🚫 Case 8: Insufficient balance
  res <- POST(url = "http://127.0.0.1:8000/move_balance",
              httr::add_headers(Authorization = paste("Bearer", tokenz)),
              body = list(
                from_uuid = source_uuid,
                to_uuid = destination_uuid,
                amount = 9999999
              ),
              encode = "json")
  parsed <- jsonlite::fromJSON(rawToChar(res$content))
  expect_equal(status_code(res), 400)
  expect_match(parsed$error, "Insufficient balance")
})


# ============================================================================
# Testing the /compute_total_balance endpoint
# ============================================================================

test_that("GET /compute_total_balance works", {
  ## Testing /compute_total_balance endpoint

  # ✅ Case 1: Compute total balance of an account with children
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_balance",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$total_balance))
  expect_true(parsed1$total_balance >= 0)

  # ✅ Case 2: Compute balance of an account with no children
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_balance",
    httr::add_headers(Authorization = paste("Bearer", token1)),
    query = list(uuid = empty_account_uuid)  # this account has no children
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, empty_account_uuid)
  expect_true(is.numeric(parsed2$total_balance))

  # 🚫 case 3: Invalid UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_balance",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "not found|unauthorized", ignore.case = TRUE)

  # 🚫 case 4: Missing UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_balance",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no query
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required", ignore.case = TRUE)

  # 🚫 case 5: No Authorization token
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_balance",
    query = list(uuid = main_uuid)
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 401)
  expect_match(parsed5$error, "Missing or invalid token", ignore.case = TRUE)
})

# ============================================================================
# Testing the /compute_total_due endpoint
# ============================================================================
test_that("GET /compute_total_due works", {
  ## Testing /compute_total_due endpoint

  # ✅ Case 1: Valid UUID with due amount
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$total_due))
  expect_true(length(parsed1$total_due) == 1)
  expect_true(parsed1$total_due >= 0)

  # ✅ Case 2: Valid UUID with zero due (account + children have no amount_due)
  token <- jwt_encode_hmac(
    jwt_claim(user_id = "testuser1", role = "user"),
    secret = secret_key
  )

  # refresh uuid of the main account
  main_uuid <- load_user_file("testuser1", "account_tree.Rds")$uuid

  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$total_due, 0)

  # 🚫 Case 3: Invalid UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "not found|unauthorized", ignore.case = TRUE)

  # 🚫 Case 4: Missing UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required", ignore.case = TRUE)

  # 🚫 Case 5: Missing Authorization header
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due",
    query = list(uuid = main_uuid)
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 401)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "unauthorized|token", ignore.case = TRUE)

})

# ============================================================================
# Testing the /compute_total_due_within_days
# ============================================================================

uuid <- load_user_file("testuser", "account_tree.Rds")$uuid
token <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser", role = "user"),
  secret = secret_key
)

test_that("GET /compute_total_due_within_days works", {
  ## Testing /compute_total_due_within_days endpoint

  # ✅ Case 1: Valid UUID and days returns total due
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, days = 30)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_equal(parsed1$within_days, 30)
  expect_true(is.numeric(parsed1$total_due))
  expect_true(parsed1$total_due >= 0)

  # 🚫 Case 2: Missing UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(days = 15)
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID.*required", ignore.case = TRUE)

  # 🚫 Case 3: Missing days
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Days.*required", ignore.case = TRUE)

  # 🚫 case 4: Invalid UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid", days = 30)
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 404)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "account.*not.*found", ignore.case = TRUE)

  # 🚫 Case 5: Invalid days value
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, days = "invalid")
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "days.*number", ignore.case = TRUE)

  # 🚫 Case 6: Negative days value
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, days = -10)
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "non-negative", ignore.case = TRUE)

  # 🚫 Case 7: Missing token
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/compute_total_due_within_days",
    query = list(uuid = uuid, days = 30)
  )
  expect_equal(httr::status_code(res7), 401)
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_match(parsed7$error, "missing|invalid", ignore.case = TRUE)
})




# ============================================================================
# Testing the /spending
# ============================================================================

test_that("GET /spending works", {
  ## Testing /spending endpoint

  # ✅ Case 1: Valid UUID and date range
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "2024-01-01",
      to = "2025-12-31"
    )
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$total_spending))
  expect_true(parsed1$total_spending >= 0)
  expect_true(!is.null(parsed1$start_time))
  expect_true(!is.null(parsed1$end_time))
  expect_true(is.numeric(parsed1$execution_time))


  # ✅ Case 2: Valid UUID, no date range (uses default)
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_true(is.numeric(parsed2$total_spending))

  # 🚫 Case 3: Invalid date format
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, from = "not-a-date", to = "2025-01-01")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Invalid date format", ignore.case = TRUE)

  # 🚫 Case 4: Invalid UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "account not found", ignore.case = TRUE)

  # 🚫 Case 5: Missing UUID
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "UUID is required", ignore.case = TRUE)

  # 🚫 Case 6: Unauthorized (no token)
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res6), 401)

  # 🚫 Case 7: to_date earlier than from_date returns 400
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/spending",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "2025-12-31",
      to = "2024-01-01"
    )
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "End date cannot be earlier than start date", ignore.case = TRUE)

})


# ============================================================================
# Testing the /total_income
# ============================================================================
test_that("GET /total_income works", {
  ## Testing /total_income endpoint

  # ✅ Case 1: Valid UUID with default date range
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, main_uuid)
  expect_true(is.numeric(parsed1$total_income))
  expect_true(parsed1$total_income >= 0)

  # ✅ Case 2: Valid UUID with specific date range (edge: narrow date window)
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "2025-01-01",
      to = "2025-01-02"
    )
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, main_uuid)
  expect_true(is.numeric(parsed2$total_income))

  # 🚫 Case 3: Missing UUID (should error)
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no query param
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required", ignore.case = TRUE)

  # 🚫 Case 4: Invalid UUID (account not found)
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found", ignore.case = TRUE)

  # 🚫 Case 5: No Authorization header
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    query = list(uuid = uuid)
  )
  expect_equal(
    httr::status_code(res5), 401
  )

  # 🚫 Case 6: Invalid date format
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "not-a-date",
      to = "2025-01-01"
    )
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 200)
  expect_true(parsed6$success)

  # 🚫 Case 7: End date earlier than start date
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/total_income",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "2025-12-31",
      to = "2025-01-01"
    )
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(
    parsed7$error,
    "End date cannot be earlier than start date",
    ignore.case = TRUE
  )

})



# ============================================================================
# Testing the /allocated_amount
# ============================================================================

test_that("GET /allocated_amount works", {
  ## Testing /allocated_amount endpoint

  # ✅ Case 1: Valid UUID with default date range
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, uuid)
  expect_true(is.numeric(parsed1$allocated_amount))
  expect_true(parsed1$allocated_amount >= 0)

  # ✅ Case 2: Valid UUID with custom date range
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = main_uuid,
      from = "2020-01-01",
      to = "2025-12-31"
    )
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, main_uuid)
  expect_match(parsed2$from, "2020-01-01")
  expect_match(parsed2$to, "2025-12-31")
  expect_true(is.numeric(parsed2$allocated_amount))

  # 🚫 Case 3: Invalid UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid-123")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 404)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "account not found", ignore.case = TRUE)

  # 🚫 Case 4: Missing UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required", ignore.case = TRUE)

  # 🚫 Case 5: Invalid date formats
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, from = "bad-date", to = "also-bad")
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 200)  # defaults should kick in
  expect_true(parsed5$success)
  expect_true(is.numeric(parsed5$allocated_amount))

  # 🚫 Case 6: No auth token
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    query = list(uuid = main_uuid)
  )
  expect_equal(httr::status_code(res6), 401)

  # 🚫 Case 7: to < from
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/allocated_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, from = "2025-12-31", to = "2020-01-01")
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "end date cannot be earlier", ignore.case = TRUE)
})

# ============================================================================
# Testing the /income_utilization
# ============================================================================

test_that("GET /income_utilization works", {
  ## Testing /income_utilization endpoint

  # ✅ Case 1: Valid UUID with data (default date range)
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, uuid)
  expect_true(is.numeric(parsed1$utilization))
  expect_true(parsed1$utilization >= 0)
  expect_true(!is.null(parsed1$start_time))
  expect_true(!is.null(parsed1$end_time))
  expect_true(is.numeric(parsed1$execution_time))


  # ✅ Case 2: Valid UUID with explicit date range (with data)
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "2024-01-01",
      to = "2025-01-01"
    )
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, uuid)
  expect_true(is.numeric(parsed2$utilization))

  # ✅ Case 3: Valid UUID, but no transactions in date range
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      from = "1900-01-01",
      to = "1900-12-31"
    )
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 200)
  expect_true(parsed3$success)
  expect_equal(parsed3$uuid, uuid)
  expect_true(is.numeric(parsed3$utilization))
  expect_true(parsed3$utilization >= 0)


  # 🚫 Case 4: Invalid UUID
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "fake-uuid")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 404)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "account not found", ignore.case = TRUE)

  # 🚫 Case 5: Missing UUID
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_match(parsed5$error, "UUID is required", ignore.case = TRUE)

  # 🚫 Case 6: No Authorization
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    query = list(uuid = uuid)
  )
  expect_equal(httr::status_code(res6), 401) # depends on your auth middleware

  # ✅ Case 7: Invalid dates (non-parseable)
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, from = "invalid", to = "invalid")
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 200)
  expect_true(parsed7$success)
  expect_true(is.numeric(parsed7$utilization))

  # 🚫 Case 8: End date earlier than start date
  res8 <- httr::GET(
    url = "http://127.0.0.1:8000/income_utilization",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, from = "2025-01-01", to = "2020-01-01")
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_false(parsed8$success)
  expect_match(parsed8$error, "end date.*earlier", ignore.case = TRUE)
})



# ============================================================================
# Testing the /walking_amount
# ============================================================================

test_that("GET /walking_amount works", {

  # ✅  alid request: amount_due
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, amt_type = "amount_due")
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_true(is.numeric(parsed1$walking_amount))
  expect_match(parsed1$amt_type, "amount_due")

  # ✅ Valid request: balance
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, amt_type = "balance")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_true(is.numeric(parsed2$walking_amount))
  expect_match(parsed2$amt_type, "balance")

  # 🚫 Invalid uuid (not found)
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Account not found")

  # 🚫  Missing uuid
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no uuid
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required")

  # Invalid amt_type (should still return numeric)

  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid, amt_type = "nonsense")
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 200)
  expect_true(parsed5$success)
  expect_true(is.numeric(parsed5$walking_amount))  # fallback behavior


  # ✅ Explicit date range
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      amt_type = "amount_due",
      from = "2024-01-01",
      to = "2025-01-01"
    )
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 200)
  expect_true(parsed6$success)
  expect_true(is.numeric(parsed6$walking_amount))

  # ✅ Valid UUID but no transactions in date range
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      amt_type = "amount_due",
      from = "1900-01-01",
      to = "1900-12-31"
    )
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 200)
  expect_true(parsed7$success)
  expect_true(is.numeric(parsed7$walking_amount))

  # 🚫 Reversed date range
  res8 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = uuid,
      amt_type = "amount_due",
      from = "2025-01-01",
      to = "2024-01-01"
    )
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_match(parsed8$error, "End date cannot be earlier than start date")

  # 🚫 No Authorization header
  res9 <- httr::GET(
    url = "http://127.0.0.1:8000/walking_amount",
    query = list(uuid = uuid, amt_type = "amount_due")
  )
  expect_equal(httr::status_code(res9), 401)

})

# ============================================================================
# Tier two endpoints
# ============================================================================
## some set up
main_uuid <- load_user_file("testuser", "account_tree.Rds")$uuid

# Setup: Create account tree with Main > Child1 + Child2
rest <- httr::POST(
  "http://127.0.0.1:8000/add_sub_account",
  httr::add_headers(Authorization = paste("Bearer", token)),
  body = list(
    parent_uuid = main_uuid,
    name = "Debts",
    allocation = 0.1,
    priority = 1
  ),
  encode = "json"
)

debts_uuid <- load_user_file(
  "testuser",
  "account_tree.Rds"
)$find_account("Debts")[[1]]$uuid


# ============================================================================
# Testing the /change_account_status endpoint
# ============================================================================

test_that("POST /change_account_status works", {

  # ✅ Case 1: Valid change to 'inactive'
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = debts_uuid,
      status = "active"
    ),
    encode = "json"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$new_status, "active")

  # 🚫 Case 2: Attempt to close account with balance > 0

  ## deposit to 100 to make sure balance isnot 0
  res21 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, amount = 100, channel = "Barclays"),
    encode = "form"
  )

  ## now try to close
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, status = "closed"),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Withdraw from this account")

  # ✅  Case 3: Withdraw to 0 balance then close
  res22 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, amount = 100, channel = "Mpesa"),
    encode = "json"
  )

  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, status = "closed"),
    encode = "json"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 200)
  expect_true(parsed3$success)
  expect_equal(parsed3$new_status, "closed")

  # 🚫  Case 4: Missing UUID
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(status = "inactive"),
    encode = "json"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required")

  # 🚫 Case 5: Missing status
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid),
    encode = "json"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 400)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "Status is required")

  # 🚫 Case 6: Invalid UUID
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "fake-uuid", status = "inactive"),
    encode = "json"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 404)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Account not found")

  # 🚫 Case 7: UUID of main account (unauthorized)

  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, status = "inactive"),
    encode = "json"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 403)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "only allowed on child or grandchild")

  # ✅  Case 8: status is can be customized need to be careful
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = debts_uuid,
      status = "nonsense"
    ),
    encode = "json"
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 200)
  expect_true(parsed8$success)
  expect_match(parsed8$new_status, "nonsense")

  # ✅  Case 9: Case sensitivity on status for base statuses
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, status = "CLOSED"),  # uppercase
    encode = "json"
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 200)
  expect_true(parsed9$success)
  expect_match(parsed9$new_status, "closed")

  ## lets first restore status
  res9a <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, status = "active"),
    encode = "json"
  )
  parsed9a <- jsonlite::fromJSON(rawToChar(res9a$content))
  expect_equal(httr::status_code(res9a), 200)
  expect_true(parsed9a$success)
  expect_equal(parsed9a$new_status, "active")

  # 🚫 Case 10: Closing account twice
  res10a <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, status = "closed"),
    encode = "json"
  )
  parsed10a <- jsonlite::fromJSON(rawToChar(res10a$content))
  expect_equal(httr::status_code(res10a), 200)
  expect_true(parsed10a$success)

  # try closing again
  res10b <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = debts_uuid, status = "closed"
    ),
    encode = "json"
  )
  parsed10b <- jsonlite::fromJSON(rawToChar(res10b$content))
  expect_true(httr::status_code(res10b) %in% c(400))
  expect_false(parsed10b$success)
  expect_match(parsed10b$error,"This account is already closed")


  # 🚫 Case 11: Unauthorized role (simulate with different token)
  res11 <- httr::POST(
    url = "http://127.0.0.1:8000/change_account_status",
    httr::add_headers(Authorization = paste("Bearer", "user_token")),
    body = list(uuid = debts_uuid, status = "inactive"),
    encode = "json"
  )
  parsed11 <- jsonlite::fromJSON(rawToChar(res11$content))
  expect_true(httr::status_code(res11) %in% c(401, 403))
  expect_false(parsed11$success)

})

# ============================================================================
# Testing the /get_account_status
# ============================================================================

test_that("GET /get_account_status works", {

  # ✅  Valid request: should return status for a valid child/grandchild account
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = debts_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_match(parsed1$account_status, "closed")

  # 🚫 Invalid uuid: should return 403 with error message
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account not found")

  # 🚫 Missing uuid: should return 400
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_status",
    httr::add_headers(Authorization = paste("Bearer", token))
    # No uuid param
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 Wrong class: trying to call on main account should return 403
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_status",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "only allowed on child or grandchild accounts")
})



# ============================================================================
# Testing the /set_priority endpoint
# ============================================================================

test_that("POST /set_priority works", {

  # ✅ Valid request with integer priority
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, priority = 1),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$priority, 1)
  expect_equal(parsed1$uuid, debts_uuid)
  expect_match(parsed1$message, "Priority set to")

  # 🚫 Missing UUID
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(priority = 2),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required")

  # 🚫 Missing priority
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Priority is required")

  # 🚫 Invalid UUID (not found)
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent", priority = 5),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found")

  # 🚫 Invalid account type (e.g. MainAccount)
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, priority = 3),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only allowed on child or grandchild")

  # 🚫 Non-numeric priority (should fail validation / coercion)
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, priority = "High"),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Priority must be numeric")

  # 🚫 Negative priority
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, priority = -1),
    encode = "form"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "Priority must be positive")

  # ✅ Decimal priority (should be allowed if coerced)
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/set_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = debts_uuid, priority = 2.5),
    encode = "form"
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 200)
  expect_true(parsed8$success)
  expect_equal(parsed8$priority, 2.5)
  expect_match(parsed8$message, "Priority set to")
})



# ============================================================================
# Testing the /get_priority endpoint
# ============================================================================

test_that("GET /get_priority works", {
  # ✅  1 Valid request for child account
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = debts_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_true(is.numeric(parsed1$priority) || is.character(parsed1$priority))

  # 🚫 2. Missing UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required", ignore.case = TRUE)

  # 🚫  3. Invalid UUID (not found)
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Account not found", ignore.case = TRUE)

  # 🚫  4. UUID of a main account (not a child or grandchild)
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(
    parsed4$error, "Priority only available for child or grandchild",
    ignore.case = TRUE
  )

  # 🚫  5. Missing Authorization header
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    query = list(uuid = debts_uuid)
  )
  expect_equal(httr::status_code(res5), 401)

  # 🚫  6. Invalid Authorization token
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_priority",
    httr::add_headers(Authorization = "Bearer invalidtoken"),
    query = list(uuid = debts_uuid)
  )
  expect_equal(httr::status_code(res6), 401)

})


# ============================================================================
# Tier three endpoints
# ============================================================================
## some set up
main_uuid <- load_user_file("testuser", "account_tree.Rds")$uuid

Debts_uuid <- load_user_file(
  "testuser",
  "account_tree.Rds"
)$find_account(
  "Debts"
)[[1]]$uuid



SubFarming_uuid <- load_user_file(
  "testuser",
  "account_tree.Rds"
)$find_account(
  "SubFarming"
)[[1]]$uuid


main<-load_user_file("testuser", "account_tree.Rds")
# ============================================================================
# Testing the /set_due_date endpoint
# ============================================================================

test_that("POST /set_due_date works as expected", {

  # ✅ 1. Valid request: set due date for a grandchild
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = "2025-08-30"),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, SubFarming_uuid)
  expect_match(parsed1$due_date, "2025-08-30")

  # 🚫 2. Invalid: due_date missing
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 200)
  expect_true(parsed2$success)
  expect_match(parsed2$message, "Due date set for")

  # 🚫 3. Invalid: uuid missing
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(due_date = "2025-09-10"),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 4. Invalid: due_date format
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = "invalid-date"),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Invalid due date format")

  # 🚫 5. Invalid: account is child, not grandchild
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = Debts_uuid, due_date = "2025-12-12"),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "Due date only applicable to grandchild accounts")

  # 🚫 6. Invalid: UUID does not exist
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent-uuid", due_date = "2025-10-01"),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Account not found")

  # 🚫 7. Invalid: applying to main account
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, due_date = "2025-10-01"),
    encode = "form"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 403)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "Due date only applicable to grandchild accounts")

  # 🚫 8. Unauthorized: missing token
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    body = list(uuid = SubFarming_uuid, due_date = "2025-08-30"),
    encode = "form"
  )
  expect_equal(httr::status_code(res8), 401)

  # ✅ 9. Leap-year date
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = "2024-02-29"),
    encode = "form"
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 200)
  expect_true(parsed9$success)
  expect_match(parsed9$due_date, "2024-02-29")

  # ✅ 10. Far future date
  res10 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = "2100-01-01"),
    encode = "form"
  )
  parsed10 <- jsonlite::fromJSON(rawToChar(res10$content))
  expect_equal(httr::status_code(res10), 200)
  expect_true(parsed10$success)
  expect_match(parsed10$due_date, "2100-01-01")

  # 🚫 11. Invalid type for due_date
  res11 <- httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = 12345),
    encode = "form"
  )
  parsed11 <- jsonlite::fromJSON(rawToChar(res11$content))
  expect_equal(httr::status_code(res11), 400)
  expect_false(parsed11$success)
  expect_match(parsed11$error, "Invalid due date format")
})



# ============================================================================
# Testing the /get_due_date endpoint
# ============================================================================
test_that("GET /get_due_date works correctly", {

  # ✅ 1. Valid grandchild account
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, SubFarming_uuid)
  expect_true(is.character(parsed1$due_date))

  # 🚫 2. Invalid UUID (not found)
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account not found")

  # 🚫 3. Missing UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token))
    # No uuid
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 4. Child account (not allowed)
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = Debts_uuid)
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Due date only applicable to grandchild accounts")

  # 🚫 5. Main account (not allowed)
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "Due date only applicable to grandchild accounts")

  # ✅ 6. Consistency test: set then get
  httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = "2025-11-20"),
    encode = "form"
  )

  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 200)
  expect_true(parsed6$success)
  expect_equal(parsed6$due_date, "2025-11-20")

  # ✅ 7. Grandchild without due date
  # we want to reuse the above acc so we first reset duedate to NULL
  res7a1<-httr::POST(
    url = "http://127.0.0.1:8000/set_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, due_date = NULL),
    encode = "form"
  )

  # now get due_date should be NULL or empty list
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/get_due_date",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )

  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 200)
  expect_true(parsed7$success)
  expect_equal(parsed7$uuid, SubFarming_uuid)
  expect_true(is.null(parsed7$due_date) || length(parsed7$due_date) == 0)
})


# ============================================================================
# Testing the /set_fixed_amount endpoint
# ============================================================================

test_that("POST /set_fixed_amount works for grandchild accounts only", {

  # ✅ 1. Valid grandchild request
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid,
      fixed_amount = 1200
    ),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$fixed_amount, 1200)
  expect_true(is.numeric(parsed1$amount_due))

  # 🚫 2. Missing fixed_amount
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid
    ),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Fixed amount is required", ignore.case = TRUE)

  # 🚫 3. Invalid fixed_amount (not numeric)
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid,
      fixed_amount = "nonsense"
    ),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "non-negative number", ignore.case = TRUE)

  # 🚫 4. Invalid: child account (Debts_uuid)
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = Debts_uuid,
      fixed_amount = 500
    ),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "grandchild accounts", ignore.case = TRUE)

  # 🚫 5. Invalid: main account
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = uuid,
      fixed_amount = 500
    ),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "grandchild accounts", ignore.case = TRUE)

  # 🚫 6. Missing UUID
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(fixed_amount = 500),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "UUID is required", ignore.case = TRUE)


})

# ============================================================================
# Testing the /get_fixed_amount endpoint
# ============================================================================

test_that("GET /get_fixed_amount works for grandchild accounts", {

  # ✅ 1. Valid: Grandchild account
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_true(is.numeric(parsed1$fixed_amount))
  expect_equal(parsed1$uuid, SubFarming_uuid)

  # 🚫 2. Missing UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token))
    # No query param
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required", ignore.case = TRUE)

  # 🚫 3. Non-existent UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "non-existent-uuid-999")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 403)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Account not found", ignore.case = TRUE)

  # 🚫 4. Main account (not allowed)
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "only applicable to grandchild accounts", ignore.case = TRUE)

  # 🚫 5. Child account (not allowed)
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = Debts_uuid)
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild accounts", ignore.case = TRUE)

  # ✅ 6. Consistency test: set then get fixed amount
  httr::POST(
    url = "http://127.0.0.1:8000/set_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, fixed_amount = 2000),
    encode = "form"
  )

  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_fixed_amount",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 200)
  expect_true(parsed6$success)
  expect_equal(parsed6$fixed_amount, 2000)
})


# ============================================================================
# Testing the /set_account_types endpoint
# ============================================================================
test_that("POST /set_account_type works as expected", {

  # ✅ 1. Valid case: grandchild account
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, account_type = "Bill"),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$account_type, "Bill")
  expect_match(parsed1$message, "Account type updated to")

  # 🚫 2. Missing UUID
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(account_type = "Fixed"),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required")

  # 🚫 3. Missing account_type
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Account type is required")

  # 🚫 4. Invalid UUID
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent-uuid", account_type = "loan"),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found")

  # 🚫 5. Wrong class: main account
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, account_type = "savings"),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild")

  # 🚫 6. Wrong class: child account
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = Debts_uuid, account_type = "monthly"),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "only applicable to grandchild")
})


# ============================================================================
# Testing the /get_account_type endpoint
# ============================================================================

test_that("GET /get_account_type works correctly", {

  # ✅ 1. Valid request (GrandchildAccount)
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)  # grandchild uuid
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, SubFarming_uuid)
  expect_true(is.character(parsed1$account_type))

  # 🚫 2. Invalid UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid-123")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account not found")

  # 🚫 3. Missing UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no uuid param
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 4. UUID for child account
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = Debts_uuid)  # child account
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "only applicable to grandchild accounts")

  # 🚫 5. UUID for main account
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)  # main account
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild accounts")

  # ✅ 6. Consistency test: set then get account type
  httr::POST(
    url = "http://127.0.0.1:8000/set_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = SubFarming_uuid, account_type = "Bill"),
    encode = "form"
  )

  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_type",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 200)
  expect_true(parsed6$success)
  expect_equal(parsed6$account_type, "Bill")
})


# ============================================================================
# Testing the /set_account_freq endpoint
# ============================================================================

test_that("POST /set_account_freq works", {

  # ✅ 1. Valid request (GrandchildAccount)
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid,
      account_freq = 30
    ),
    encode = "form"
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$freq, 30)
  expect_equal(parsed1$uuid, SubFarming_uuid)

  # 🚫 2. Missing UUID
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      account_freq = 30
    ),
    encode = "form"
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required")

  # 🚫 3. Missing account_freq
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid
    ),
    encode = "form"
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "account_freq is required")

  # 🚫 4. Invalid UUID
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = "invalid-uuid",
      account_freq = 7
    ),
    encode = "form"
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found")

  # 🚫 5. Not a grandchild (use child uuid)
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = Debts_uuid,  # child uuid
      account_freq = 90
    ),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild")

  # 🚫 6. Empty UUID string
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = "",
      account_freq = 7
    ),
    encode = "form"
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 400)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "UUID is required")

  # 🚫 7. Empty account_freq string
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid,
      account_freq = ""
    ),
    encode = "form"
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "account_freq is required")

  # 🚫 8. Invalid Frequency Format (non-numeric string)
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(
      uuid = SubFarming_uuid,
      account_freq = "366N" # not for numeric characters like "366" coercion works
    ),
    encode = "form"
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_false(parsed8$success)
  expect_match(parsed8$error,
               "account_freq must be a positive number",
               ignore.case = TRUE)

  # 🚫 9. Unauthorized (no token)
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_freq",
    body = list(
      uuid = SubFarming_uuid,
      account_freq = 30
    ),
    encode = "form"
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_true(httr::status_code(res9) %in% c(401, 403))
  expect_false(parsed9$success)
})


# ============================================================================
# Testing the /get_account_freq endpoint
# ============================================================================

test_that("GET /get_account_freq works", {

  # ✅ 1. Valid request (GrandchildAccount)
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)  # grandchild
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_true(is.numeric(parsed1$freq) || is.null(parsed1$freq))

  # 🚫 2. Invalid UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "invalid-uuid")
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account not found", ignore.case = TRUE)

  # 🚫 3. Missing UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token))
    # no uuid
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required", ignore.case = TRUE)

  # 🚫 4. Empty UUID string
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "UUID is required", ignore.case = TRUE)

  # 🚫 5. Not applicable to main account
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)  # main
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild", ignore.case = TRUE)

  # 🚫 6. Not applicable to child account
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = Debts_uuid)  # child
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "only applicable to grandchild", ignore.case = TRUE)

  # 🚫 7. Unauthorized (no token)
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_freq",
    query = list(uuid = SubFarming_uuid)
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_true(httr::status_code(res7) %in% c(401, 403))
  expect_false(parsed7$success)
})


# ============================================================================
# Testing the /set_account_periods endpoint
# ============================================================================

test_that("POST /set_account_periods works for valid and invalid cases", {

  # ✅ 1. Valid request (GrandchildAccount)
  res1 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = SubFarming_uuid, periods = 6),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$periods, 6)
  expect_match(parsed1$message, "Number of periods set to 6")

  # 🚫 2. Invalid UUID
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = "invalid-uuid", periods = 3),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 403)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account not found")

  # 🚫 3. Missing UUID
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(periods = 3),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 4. Missing periods
  res4 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = SubFarming_uuid),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 400)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Periods is required")

  # 🚫 5. Invalid account type (Main Account)
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = uuid, periods = 4),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild accounts")

  # 🚫 6. Invalid account type (Child Account)
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = Debts_uuid, periods = 4),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "only applicable to grandchild accounts")

  # 🚫 7. Invalid periods (non-numeric)
  res7 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = SubFarming_uuid, periods = "nonsense"),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_equal(httr::status_code(res7), 400)
  expect_false(parsed7$success)
  expect_match(parsed7$error, "Periods must be a positive integer")

  # 🚫 8. Invalid periods (negative value)
  res8 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = SubFarming_uuid, periods = -3),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_equal(httr::status_code(res8), 400)
  expect_false(parsed8$success)
  expect_match(parsed8$error, "Periods must be a positive integer")

  # 🚫 9. Invalid periods (zero value)
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/set_account_periods",
    body = list(uuid = SubFarming_uuid, periods = 0),
    encode = "form",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 400)
  expect_false(parsed9$success)
  expect_match(parsed9$error, "Periods must be a positive integer")

})


# ============================================================================
# Testing the /get_account_periods
# ============================================================================

test_that("GET /get_account_periods works correctly", {
  # ✅ 1. Valid request (GrandchildAccount)
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = SubFarming_uuid)
  )
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_equal(httr::status_code(res1), 200)
  expect_true(parsed1$success)
  expect_equal(parsed1$uuid, SubFarming_uuid)
  expect_true(is.numeric(parsed1$periods))

  # 🚫 2. Missing UUID
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token))
    # No UUID
  )
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_equal(httr::status_code(res2), 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "UUID is required")

  # 🚫 3. Empty UUID
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_equal(httr::status_code(res3), 400)
  expect_false(parsed3$success)
  expect_match(parsed3$error, "UUID is required")

  # 🚫 4. UUID does not exist
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "nonexistent-uuid-1234")
  )
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_equal(httr::status_code(res4), 403)
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found")

  # 🚫 5. Child account (invalid class)
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = Debts_uuid)
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 403)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "only applicable to grandchild accounts")

  # 🚫 6. Main account (invalid class)
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_account_periods",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = uuid)
  )
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_equal(httr::status_code(res6), 403)
  expect_false(parsed6$success)
  expect_match(parsed6$error, "only applicable to grandchild accounts")
})

# ============================================================================
# Testing the /delete
# ============================================================================

test_that("DELETE /delete works correctly with uuid", {
  # Setup: create a new user and main account
  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )
  # Register account
  POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id),
    encode = "json"
  )

  # Load tree to get its UUID
  tree <- finman:::load_user_file(user_id, "account_tree.Rds")
  main_uuid <- tree$uuid

  # ➕ Register a sub-account under main
  res_sub<- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = main_uuid,
      name = "Needs",
      allocation = 0.4
    )
  )
  parsed_sub <- fromJSON(rawToChar(res_sub$content))
  expect_equal(res_sub$status_code, 200)
  expect_true(parsed_sub$success)
  sub_uuid <- parsed_sub$uuid
  # 🚫 1. Missing uuid
  res1 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  parsed1 <- fromJSON(rawToChar(res1$content))
  expect_equal(res1$status_code, 400)
  expect_false(parsed1$success)
  expect_match(parsed1$error, "Account UUID is required")

  # 🚫 2. Empty uuid
  res2 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "")
  )
  parsed2 <- fromJSON(rawToChar(res2$content))
  expect_equal(res2$status_code, 400)
  expect_false(parsed2$success)
  expect_match(parsed2$error, "Account UUID is required")

  # ✅ 3. Successfully deletes sub-account (before main)
  res3a <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = sub_uuid)
  )
  parsed3a <- fromJSON(rawToChar(res3a$content))
  expect_equal(res3a$status_code, 200)
  expect_true(parsed3a$success)
  expect_match(
    parsed3a$message,
    sprintf("Account with UUID '%s' deleted successfully",
            sub_uuid)
    )

  # ✅ 4. Successfully deletes main account
  res3 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid)
  )
  parsed3 <- fromJSON(rawToChar(res3$content))
  expect_equal(res3$status_code, 200)
  expect_true(parsed3$success)
  expect_match(parsed3$message, sprintf(
    "Account for user_id '%s' deleted successfully",
    user_id)
    )
  expect_false(finman:::user_file_exists(user_id))

  # 🚫 5. Non-existent uuid
  fake_uuid <- "nonexistent-uuid-1234"
  res4 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = fake_uuid)
  )
  parsed4 <- fromJSON(rawToChar(res4$content))
  expect_true(res4$status_code%in%c(404, 403))
  expect_false(parsed4$success)
  expect_match(parsed4$error, sprintf(
    "Account for user_id"
    )
    )

  # 🚫 6. Invalid uuid (malicious)
  bad_uuid <- "../etc/passwd"
  res5 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = bad_uuid)
  )
  parsed5 <- fromJSON(rawToChar(res5$content))
  expect_true(res5$status_code %in% c(403, 404))
  expect_false(parsed5$success)

  # 🚫 7. Deleting same account twice
  temp_user_id <- uuid::UUIDgenerate()
  token_temp <- jwt_encode_hmac(
    jwt_claim(user_id = temp_user_id, role = "user"),
    secret = secret_key
  )
  POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", token_temp)),
    body = list(user_id = temp_user_id),
    encode = "json"
  )
  tree_temp <- finman:::load_user_file(temp_user_id, "account_tree.Rds")
  temp_uuid <- tree_temp$uuid

  DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token_temp)),
    query = list(uuid = temp_uuid)
  )

  res6 <- httr::DELETE(
    url = "http://127.0.0.1:8000/delete",
    httr::add_headers(Authorization = paste("Bearer", token_temp)),
    query = list(uuid = temp_uuid)
  )
  parsed6 <- fromJSON(rawToChar(res6$content))
  expect_true(res6$status_code %in% c(403, 404))
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Account for user_id")
})




# ============================================================================
# Testing the /minimal_tree endpoint
# ============================================================================


test_that("GET /minimal_tree endpoint edge cases", {
  user_id <- uuid::UUIDgenerate()
  token <- jwt_encode_hmac(
    jwt_claim(user_id = user_id, role = "user"),
    secret = secret_key
  )

  # Register account
  POST(
    url = "http://127.0.0.1:8000/register",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id),
    encode = "json"
  )

  # Load tree to get its UUID
  tree <- finman:::load_user_file(user_id, "account_tree.Rds")
  main_uuid <- tree$uuid

  # ➕ Register a sub-account under main
  res_sub<- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = main_uuid,
      name = "Needs",
      allocation = 0.4
    )
  )
  parsed_sub <- fromJSON(rawToChar(res_sub$content))
  sub_uuid <- parsed_sub$uuid
  # Test cases:
  # ✅ 1. Successful full tree retrieval (uuid = "")
  res1 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid, n = 30)
  )
  #expect_equal(httr::status_code(res1), 200)
  parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
  expect_true(parsed1$success)
  expect_true(!is.null(parsed1$minimal_tree))
  expect_equal(parsed1$uuid, main_uuid)

  # ✅ 2. Successful subaccount retrieval
  res2 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = sub_uuid, n = 10)
  )
  expect_equal(httr::status_code(res2), 200)
  parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
  expect_true(parsed2$success)
  expect_equal(parsed2$uuid, sub_uuid)
  expect_true(!is.null(parsed2$minimal_tree))

  # 🚫 3. Missing token
  res3 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    query = list(uuid = main_uuid, n = 30)
  )
  expect_equal(httr::status_code(res3), 401)
  parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
  expect_false(parsed3$success)
  expect_match(parsed3$error, "Missing or invalid token", ignore.case = TRUE)

  # 🚫 4. Invalid uuid (non-existent account)
  res4 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = "fake-uuid", n = 30)
  )
  expect_equal(httr::status_code(res4), 403)
  parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
  expect_false(parsed4$success)
  expect_match(parsed4$error, "Account not found", ignore.case = TRUE)

  # 🚫 5. Non-numeric n
  res5 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(uuid = main_uuid, n = "ten")
  )
  expect_equal(httr::status_code(res5), 400)
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_false(parsed5$success)
  expect_match(parsed5$error, "`n` must be a positive number", ignore.case = TRUE)

  # 🚫 6. Invalid date format
  res6 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = main_uuid,
      n = 30,
      start_date = "2020-13-01",
      end_date = "2020-12-01"
      )
  )
  expect_equal(httr::status_code(res6), 400)
  parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
  expect_false(parsed6$success)
  expect_match(parsed6$error, "Invalid date format", ignore.case = TRUE)

  # 🚫 7. Start date later than end date
  res7 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(
      uuid = main_uuid,
      n = 30,
      start_date = "2025-12-01",
      end_date = "2025-01-01"
      )
  )
  expect_equal(httr::status_code(res7), 400)
  parsed7 <- jsonlite::fromJSON(rawToChar(res7$content))
  expect_false(parsed7$success)
  expect_match(
    parsed7$error,
    "End date cannot be earlier than start date",
    ignore.case = TRUE
    )

  # ✅ 8. Missing uuid parameter entirely
  res8 <- httr::GET(
    url = "http://127.0.0.1:8000/get_minimal_tree",
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list(n = 30)
  )
  expect_equal(httr::status_code(res8), 200)
  parsed8 <- jsonlite::fromJSON(rawToChar(res8$content))
  expect_true(parsed8$success)

})


