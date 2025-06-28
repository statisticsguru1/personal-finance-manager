library(plumber)
library(httr)
library(jsonlite)
library(sodium)
library(jose)
devtools::load_all()

pr <- plumb(file = "api/plumber.R")
pr$run(
  host = "0.0.0.0",
  port = as.numeric(Sys.getenv("PORT", unset = 8000)),
  swagger = FALSE
)

