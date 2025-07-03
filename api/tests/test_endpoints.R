library(testthat)
library(httr)
library(jose)
library(withr)
library(here)
library(jsonlite)
library(finman)
library(tidyverse)


wait_for_server_ready <- function(
    url = "http://127.0.0.1:8000/__ping__",
    timeout = 20
) {
  start_time <- Sys.time()
  while (as.numeric(Sys.time() - start_time, units = "secs") < timeout) {
    res <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (!is.null(res) && httr::status_code(res) == 200) return(TRUE)
    Sys.sleep(0.10)
  }
  stop("Server did not become ready within timeout.")
}


# Setup: start server, create user, etc.
tmp_dir <- tempfile("test-api-deposit-")
dir.create(tmp_dir, recursive = TRUE)
Sys.setenv(ACCOUNT_BASE_DIR = tmp_dir)
Sys.setenv(ACCOUNT_BACKEND = "file")
Sys.setenv(JWT_SECRET = "test-secret")
secret_key <- Sys.getenv("JWT_SECRET")

create_user_account_base(
  user_id = "testuser",
  base_dir = tmp_dir,
  initial_balance = 500
)
uuid <- load_user_file("testuser", "account_tree.Rds")$uuid
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

wait_for_server_ready("http://127.0.0.1:8000/__ping__")

# Helper: auth token
token <- jwt_encode_hmac(
  jwt_claim(user_id = "testuser", role = "user"),
  secret = secret_key
)

test_that("POST /deposit endpoint edge cases", {

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
  
  # ✅ Case 2: Missing token
  res2 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    body = list(uuid = uuid, amount = 100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res2), 401)
  
  # ✅ Case 3: Invalid UUID
  res3 <- httr::POST(
    url = "http://127.0.0.1:8000/deposit",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = "non-existent-uuid", amount = 100, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res3), 403)
})


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
  
  # ✅ Case 5: Withdraw more than balance
  res5 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(uuid = uuid, amount = 999999, channel = "bank"),
    encode = "form"
  )
  parsed5 <- jsonlite::fromJSON(rawToChar(res5$content))
  expect_equal(httr::status_code(res5), 500)
  expect_false(parsed5$success)
  expect_match(parsed5$error, "insufficient|balance", ignore.case = TRUE)
  
  # ✅ Case 6: Withdraw with invalid token
  res6 <- httr::POST(
    url = "http://127.0.0.1:8000/withdraw",
    httr::add_headers(Authorization = "Bearer invalidtoken"),
    body = list(uuid = uuid, amount = 50, channel = "bank"),
    encode = "form"
  )
  expect_equal(httr::status_code(res6), 401)
  
  # ✅ Case 7: Withdraw with negative amount
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
  
})


test_that("POST /add_sub_account endpoint edge cases", {
  # Case 8: Successful child account creation
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
  
  # Case 9: Duplicate name under same parent
  res9 <- httr::POST(
    url = "http://127.0.0.1:8000/add_sub_account",
    httr::add_headers(Authorization = paste("Bearer", token)),
    encode = "form",
    body = list(
      parent_uuid = uuid,
      name = "Needs",  # Duplicate name
      allocation = 0.3
    )
  )
  parsed9 <- jsonlite::fromJSON(rawToChar(res9$content))
  expect_equal(httr::status_code(res9), 400)
  expect_false(parsed9$success)
  expect_match(parsed9$error, "already exists", ignore.case = TRUE)
  
  # Case 10: Parent UUID not found
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
  
  # Case 11: Missing token
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
  
  # Case 12: Invalid allocation
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
  
  # Case 13: Successful grandchild account creation
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
  print(parsed13b)
  expect_equal(httr::status_code(res13b), 200)
  expect_true(parsed13b$success)
  expect_equal(parsed13b$child_type, "GrandchildAccount")
  
  # Case 14: anything past grand child is grandchild
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
