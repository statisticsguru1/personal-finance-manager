for (i in 1:20) {
  res <- tryCatch(
    httr::GET("http://127.0.0.1:8000/__ping__"),
    error = function(e) NULL
  )
  if (!is.null(res) && httr::status_code(res) == 200) break
  Sys.sleep(0.25)
}
print(res$status_code)