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
host_url = "http://127.0.0.1:8000/"

wait_for_server_ready <- function(
    url = paste0(host_url,"__ping__"),
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
Sys.setenv(HOST_URL = "http://127.0.0.1:8000/")
secret_key <- Sys.getenv("JWT_SECRET")


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
wait_for_server_ready(paste0(host_url,"__ping__"),timeout = 30)

# ============================================================================
# Generating dummy auth token for user account (testuser)
# ============================================================================
# Helper: auth token
user_id <- uuid::UUIDgenerate()
token <- jwt_encode_hmac(
  jwt_claim(user_id = user_id , role = "user"),
  secret = secret_key
)

res <- POST(
    url = paste0(host_url,"register"),
    httr::add_headers(Authorization = paste("Bearer", token)),
    body = list(user_id = user_id),
    encode = "json"
)

parsed <- jsonlite::fromJSON(rawToChar(res$content))
print(parsed)
main_uuid<-parsed$uuid

res1 <- httr::POST(
  url = paste0(host_url,"add_sub_account"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "json",
  body = list(
    parent_uuid = main_uuid,
    name = "Goals",
    allocation = 0.2
  )
)

parsed1 <- jsonlite::fromJSON(rawToChar(res1$content))
print(parsed1)

res2 <- httr::POST(
  url = paste0(host_url,"add_sub_account"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "json",
  body = list(
    parent_uuid = main_uuid,
    name = "Needs",
    allocation = 0.55
  )
)
parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
print(parsed2)

res3 <- httr::POST(
  url = paste0(host_url,"add_sub_account"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "json",
  body = list(
    parent_uuid = main_uuid,
    name = "Debt Repayment",
    allocation = 0.2
  )
)
parsed3 <- jsonlite::fromJSON(rawToChar(res3$content))
print(parsed3)

res4 <- httr::POST(
  url = paste0(host_url,"add_sub_account"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  encode = "json",
  body = list(
    parent_uuid = parsed2$uuid,
    name = "Rent",
    allocation = 0.2,
    fixed_amount = 7500,
    account_type = "Bill",
    freq = 30,
    due_date = dmy("28-08-2025")
  )
)

parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
print(parsed4)
res6 <- httr::GET(
    url = paste0(host_url, "get_minimal_tree"),
    httr::add_headers(Authorization = paste("Bearer", token)),
    query = list()
  )
parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
print(parsed6$minimal_tree)
