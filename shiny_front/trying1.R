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

user_id <- uuid::UUIDgenerate()

res2 <- httr::POST(
  url = "http://127.0.0.1:8000/generate_access_token",
  query = list(
    user_id = user_id,
    role = "user",
    type ="Testing",
    exp = as.integer(Sys.time()) + 9000
  )
)
parsed2 <- jsonlite::fromJSON(rawToChar(res2$content))
print(parsed2)
token<-parsed2$token

# register account

res <- POST(
  url = paste0(host_url,"register"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  body = list(user_id = user_id),
  encode = "json"
)

# add subaccounts
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

# get tree 
parsed4 <- jsonlite::fromJSON(rawToChar(res4$content))
print(parsed4)
res6 <- httr::GET(
  url = paste0(host_url, "get_minimal_tree"),
  httr::add_headers(Authorization = paste("Bearer", token)),
  query = list()
)
parsed6 <- jsonlite::fromJSON(rawToChar(res6$content))
print(parsed6$minimal_tree)
