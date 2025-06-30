library(testthat)
library(httr)
library(jose)
library(withr)
library(here)
library(jsonlite)

devtools::load_all()  # Load your package (e.g., finman)

wait_for_server_ready <- function(
    url = "http://127.0.0.1:8000/__ping__",
    timeout = 20
) {
  start_time <- Sys.time()
  while (as.numeric(Sys.time() - start_time, units = "secs") < timeout) {
    res <- tryCatch(httr::GET(url), error = function(e) NULL)
    if (!is.null(res) && httr::status_code(res) == 200) return(TRUE)
    Sys.sleep(0.25)
  }
  stop("Server did not become ready within timeout.")
}

test_that("POST /deposit endpoint edge cases", {
  # Setup: start server, create user, etc.
  tmp_dir <- tempfile("test-api-deposit-")
  dir.create(tmp_dir, recursive = TRUE)
  Sys.setenv(ACCOUNT_BASE_DIR = tmp_dir)
  Sys.setenv(JWT_SECRET = "test-secret")
  secret_key <- Sys.getenv("JWT_SECRET")
  
  create_user_account_base("testuser", tmp_dir, initial_balance = 500)
  uuid <- load_user_account("testuser", tmp_dir)$uuid
  
  log_out <- tempfile("server-out-", fileext = ".log")
  log_err <- tempfile("server-err-", fileext = ".log")
  
  server <- callr::r_bg(
    function(main_file, jwt, base_dir, project_dir) {
      setwd(project_dir)
      Sys.setenv(JWT_SECRET = jwt)
      Sys.setenv(ACCOUNT_BASE_DIR = base_dir)
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
