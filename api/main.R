library(plumber)
library(httr)
library(jsonlite)
library(sodium)
library(jose)
library(finman)
library(tidyverse)

# =============================================================================
 # Env variables these should be removed during hosting
 secret_key <- Sys.getenv("JWT_SECRET")
 uri<-Sys.getenv("MONGO_URI")
# =============================================================================

pr <- plumb(file = "api/plumber.R")
pr$run(
  host = "0.0.0.0",
  port = as.numeric(Sys.getenv("PORT", unset = 8000)),
  swagger = FALSE
)
