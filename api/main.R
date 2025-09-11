library(plumber)
library(httr)
library(jsonlite)
library(sodium)
library(jose)
library(finman)
library(tidyverse)

# =============================================================================
 # Env variables these should be removed during hosting
 #tmp_dir <- tempfile("test-api-deposit-")
 Sys.setenv("ACCOUNT_BASE_DIR"="user_accounts")
 #dir.create(tmp_dir, recursive = TRUE)
 Sys.setenv(ACCOUNT_BACKEND = "mongo")
 Sys.setenv(MAX_REQUESTS = 1000000)
 Sys.setenv(WINDOW_SIZE = 3600)
 Sys.setenv(JWT_SECRET = "test-secret")
 secret_key <- Sys.getenv("JWT_SECRET")
 #Sys.setenv(HOST_URL = "http://127.0.0.1:8000/")
Sys.setenv("MONGO_URI"=uri)
Sys.setenv("MONGO_DB"="Finman")


# =============================================================================

pr <- plumb(file = "api/plumber.R")
pr$run(
  host = "0.0.0.0",
  port = as.numeric(Sys.getenv("PORT", unset = 8000)),
  swagger = FALSE
)
