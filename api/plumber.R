library(plumber)

#* @filter auth
function(req, res) {
  # Skip auth for the health check
  if (req$PATH_INFO == "/__ping__") {
    return(forward())
  }
  
  auth_header <- req$HTTP_AUTHORIZATION
  
  if (is.null(auth_header) || !startsWith(auth_header, "Bearer ")) {
    res$status <- 401
    return(list(error = "Missing or invalid token"))
  }
  
  secret_key <- Sys.getenv("JWT_SECRET")
  
  if (secret_key == "") {
    res$status <- 500
    return(list(error = "Server misconfigured (missing JWT_SECRET)"))
  }
  
  token <- sub("Bearer ", "", auth_header)
  
  decoded <- verify_token(token, secret = secret_key)
  
  if (is.null(decoded)) {
    res$status <- 401
    return(list(error = "Invalid or expired token"))
  }
  
  req$user_id <- decoded$user_id
  req$role <- decoded$role
  forward()
}


#* Health check endpoint
#* @get /__ping__
ping<-function() {
  list(status = "ok")
}


#* endpoints/deposit.R
#* @post /deposit
#* @param uuid Account UUID (required)
#* @param amount Deposit amount (required)
#* @param channel Deposit channel (required)
#* @param transaction_number Optional transaction number
#* @param by Who performed the deposit (default = "User")
#* @param date Timestamp of deposit (default = now)
deposit<- function(req, res,
         uuid,
         amount,
         channel,
         transaction_number = NULL,
         by = "User",
         date = Sys.time()) {
  user_id <- req$user_id
  role <- req$role
  # Load full tree
  tree <- load_user_account(user_id)

  # Try to find account
  account <- tree$find_account_by_uuid(uuid)

  # Role-based access control
  if (is.null(account)) {
    res$status <- if (role == "admin") 404 else 403
    return(list(error = "Account not found or unauthorized"))
  }

  # Proceed with deposit
  account$deposit(
    amount = as.numeric(amount),
    transaction_number = transaction_number,
    by = by,
    channel = channel,
    date = date
  )

  # Save updated tree
  save_user_account(user_id, tree)

  list(
    success = TRUE,
    account_uuid = uuid,
    balance = account$balance
  )
}