# fetch the whole account tree
call_api <- function(endpoint,
                     method = "GET",
                     body = NULL,
                     params = NULL,
                     token = NULL) {

  headers <- c()
  if (!is.null(token) && nzchar(token)) {
    headers <- c(Authorization = paste("Bearer", token))
  }

  httr::VERB(
    verb   = method,
    url    = paste0(Sys.getenv("HOST_URL"), endpoint),
    httr::add_headers(.headers = headers),
    body   = body,
    query  = params,
    encode = "json"
  )
}

# deposit
post_deposit <- function(uuid, amount, channel, transaction_number = NULL,token) {
  res <- POST(
    url = paste0(host_url, "deposit"),
    add_headers(Authorization = paste("Bearer", token)),
    encode = "json",
    body = list(
      uuid = uuid,
      amount = amount,
      channel = channel,
      transaction_number = transaction_number,
      by = "User",
      date = as.character(Sys.time())
    )
  )
  fromJSON(rawToChar(res$content))
}


# Recursively search the account tree for a node with the given account_uuid
get_account_by_uuid <- function(node, account_uuid) {
  if (is.null(node)) return(NULL)

  # Check current node
  if (!is.null(node$account_uuid) && node$account_uuid == account_uuid) {
    return(node)
  }

  # If this node has children, search them
  if (!is.null(node$child_accounts)) {
    for (child in node$child_accounts) {
      result <- get_account_by_uuid(child, account_uuid)
      if (!is.null(result)) {
        return(result)
      }
    }
  }

  return(NULL)  # not found in this branch
}


# Traverse recursively and return all account uuids + names
get_all_accounts <- function(node) {
  if (is.null(node)) return(list())

  accounts <- list(
    list(
      account_uuid = node$account_uuid,
      name = node$name
    )
  )

  # If children exist, collect theirs too
  if (!is.null(node$child_accounts)) {
    for (child in node$child_accounts) {
      accounts <- c(accounts, get_all_accounts(child))
    }
  }

  return(accounts)
}

get_all_transactions <- function(node) {
  all_txns <- data.frame()  # empty collector

  # only bind if transactions exist and are a data.frame with rows
  if (!is.null(node$transactions) &&
      is.data.frame(node$transactions) &&
      nrow(node$transactions) > 0) {
    all_txns <- bind_rows(all_txns, node$transactions)
  }

  # recurse if children exist
  if (!is.null(node$child_accounts) && length(node$child_accounts) > 0) {
    for (child in node$child_accounts) {
      child_txns <- get_all_transactions(child)
      if (!is.null(child_txns) && nrow(child_txns) > 0) {
        all_txns <- bind_rows(all_txns, child_txns)
      }
    }
  }

  return(all_txns)
}


is_duplicate_tran<-function(node,uuid,transaction_number) {
  if (is.null(transaction_number) || length(transaction_number) != 1) {
    return(FALSE)
  }
  transaction_number %in% get_account_by_uuid(node,uuid)$transactions$TransactionID
}

# deposit modal  dialog
data_deposit <- function(failed = FALSE,selected_uuid, main_account,user_input = list()) {

  account_details<-get_account_by_uuid(main_account,selected_uuid)
    modalDialog(
      title = div(
        sprintf("💰 Deposit Funds to %s account", account_details$name),
        style="font-weight:bold; font-size:1.3rem;"
        ),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_deposit", "Submit Deposit", class = "btn btn-primary")
      ),
      div(class = "modal-body",
          helpText("Enter the details for your deposit. Mandatory fields are marked with an asterisk (*)."),
          helpText(sprintf("the account is now %s",account_details$name)),
          helpText(sprintf("the account iid now %s",account_details$account_uuid)),
          helpText(sprintf("recorded account %s",selected_uuid)),
          numericInput("dep_amount", "Amount *", value = NULL),
          if (failed & is.null(user_input$amount)) div(
            style = "color: red; margin-top:-0.5rem; font-size:0.9rem;",
            "Deposit amount is required."
          ),
          textInput("dep_txn", "Transaction Number", placeholder="e.g TAP8I3JK2G"),
          textInput("dep_channel", "Transaction Channel *", placeholder="e.g ABC BANK"),
          if (failed & is.null(user_input$channel)) div(
            style = "color: red; margin-top:-0.5rem; font-size:0.9rem;",
            "Transaction channel is required."
          ),
          dateInput("dep_date", "Date", value = Sys.Date()),
          if (failed && !is.null(user_input$transaction_date) &&
              user_input$transaction_date > Sys.Date()) div(
            style = "color: red; margin-top:-0.5rem; font-size:0.9rem;",
            "Transaction date cannot be in the future."
          ),
          hr(),
          tags$p("⚠️ Ensure transaction details are correct before submitting. Incorrect entries may delay processing.",
                 style = "color:#f87171; font-size:0.9rem;")
      )
    )
}

