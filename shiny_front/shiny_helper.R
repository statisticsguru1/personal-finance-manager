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

# Deposit modal dialog
data_deposit <- function(account, failed = FALSE, user_input = list()) {
  modalDialog(
    title = div(
      sprintf("💰 Deposit Funds to %s account", account$name),
      class = "modal-title"
    ),
    easyClose = TRUE,
    size = "l",
    footer = tagList(
      tags$button(
        type = "button",
        class = "btn btn-secondary",
        `data-bs-dismiss` = "modal",
        "Cancel"
      ),
      actionButton("submit_deposit", "Submit Deposit", class = "btn btn-primary")
    ),
    div(
      class = "modal-body",
      helpText(HTML("Enter the details for your deposit. Mandatory fields are marked with an asterisk <span class='asterisk'>*</span>.")),

      # Amount (required)
      numericInput(
        "dep_amount",
        label = tags$label("Amount", class = "required"),
        value = NULL,
        min = 0
      ),
      if (failed && is.null(user_input$amount)) {
        div("Deposit amount is required.", class = "form-error")
      },

      # Transaction number (optional)
      textInput(
        "dep_txn",
        label = "Transaction Number",
        placeholder = "e.g TAP8I3JK2G"
      ),
      if (failed && !is.null(user_input$transaction_number) &&
          is_duplicate_tran(account, account$account_uuid, user_input$transaction_number)) {
        div("This transaction number already exists for this account. Please enter a unique number.",
            class = "form-error")
      },

      # Transaction channel (required)
      textInput(
        "dep_channel",
        label = tags$label("Transaction Channel", class = "required"),
        placeholder = "e.g ABC BANK"
      ),
      if (failed && is.null(user_input$channel)) {
        div("Transaction channel is required.", class = "form-error")
      },

      # Date (validated: not future)
      dateInput("dep_date","Date", value = Sys.Date()),
      if (failed && !is.null(user_input$transaction_date) &&
          user_input$transaction_date > Sys.Date()) {
        div("Transaction date cannot be in the future.", class = "form-error")
      }
    )
  )
}


# Withdrawal modal dialog
data_withdrawal <- function(account,currency, failed = FALSE, user_input = list()) {
  modalDialog(
    title = div(
      sprintf("🏧 Withdraw Funds from %s account", account$name),
      class = "modal-title"
    ),
    easyClose = TRUE,
    size = "l",
    footer = tagList(
      tags$button(
        type = "button",
        class = "btn btn-secondary",
        `data-bs-dismiss` = "modal",
        "Cancel"
      ),
      actionButton("submit_withdrawal", "Submit Withdrawal", class = "btn btn-primary")
    ),
    div(
      class = "modal-body",

      # Show current balance
      div(
        class = "current-balance mb-3",
        strong("Current Balance: "),
        sprintf("%s %.2f",currency, account$balance)
      ),

      helpText(HTML(
        "Enter the details for your withdrawal. Mandatory fields are marked with an asterisk <span class='asterisk'>*</span>."
      )),

      # Amount (required, must be >= 0, <= balance)
      numericInput(
        "wd_amount",
        label = tags$label("Amount", class = "required"),
        value = NULL,
        min = 0,
        max = account$balance
      ),
      if (failed && is.null(user_input$amount)) {
        div("Withdrawal amount is required.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount < 0) {
        div("Withdrawal amount cannot be negative.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount > account$balance) {
        div("Withdrawal amount cannot exceed current balance.", class = "form-error")
      },

      # Transaction number (optional, but must not duplicate)
      textInput(
        "wd_txn",
        label = "Transaction Number",
        placeholder = "e.g WDR4K8JX1P"
      ),
      if (failed && !is.null(user_input$transaction_number) &&
          is_duplicate_tran(account, account$account_uuid, user_input$transaction_number)) {
        div("This transaction number already exists for this account. Please enter a unique number.",
            class = "form-error")
      },

      # Transaction channel (required)
      textInput(
        "wd_channel",
        label = tags$label("Transaction Channel", class = "required"),
        placeholder = "e.g ATM / Bank Transfer"
      ),
      if (failed && is.null(user_input$channel)) {
        div("Transaction channel is required.", class = "form-error")
      },

      # Date (validated: not future)
      dateInput("wd_date", "Date", value = Sys.Date()),
      if (failed && !is.null(user_input$transaction_date) &&
          user_input$transaction_date > Sys.Date()) {
        div("Transaction date cannot be in the future.", class = "form-error")
      }
    )
  )
}

# Transfer modal dialog (cleaned version)
data_transfer <- function(main_account,selected_uuid,currency, failed = FALSE, user_input = list()) {
  from_account<- get_account_by_uuid(main_account,selected_uuid)
  modalDialog(
    title = div(
      sprintf("🔄 Transfer Funds from %s account", from_account$name),
      class = "modal-title"
    ),
    easyClose = TRUE,
    size = "l",
    footer = tagList(
      tags$button(
        type = "button",
        class = "btn btn-secondary",
        `data-bs-dismiss` = "modal",
        "Cancel"
      ),
      actionButton("submit_transfer", "Submit Transfer", class = "btn btn-primary")
    ),
    div(
      class = "modal-body",

      helpText(HTML("Select the destination account and enter the transfer amount.")),

      # --- FROM SECTION ---
      h5("From Account"),
      div(
        strong("Name: "),span(from_account$name, style = "color: gray; font-size: 0.9rem;"), br(),
        strong("UUID: "), span(from_account$account_uuid, style = "color: gray; font-size: 0.9rem;"), br(),
        strong("Current Balance: "),span(sprintf("%s %.2f", "$", from_account$balance), style = "color: gray; font-size: 0.9rem;")

      ),
      tags$hr(),

      # --- TO SECTION ---
      h5("To Account"),
      selectInput(
        "trf_to",
        label = tags$label("Select Destination Account", class = "required"),
        choices = sapply(
          get_all_accounts(main_account),
          function(account){setNames(account$account_uuid,account$name)}
        ),
        selected = if (!is.null(user_input$to_uuid)) user_input$to_uuid else NULL
      ),

      uiOutput("to_account_details"),
      if (failed && is.null(user_input$to_uuid)) {
        div("Destination account is required.", class = "form-error")
      },

      tags$hr(),

      # --- AMOUNT ---
      numericInput(
        "trf_amount",
        label = tags$label("Amount", class = "required"),
        value = if (!is.null(user_input$amount)) user_input$amount else NULL,
        min = 0,
        max = from_account$balance
      ),
      if (failed && is.null(user_input$amount)) {
        div("Transfer amount is required.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount <= 0) {
        div("Transfer amount must be greater than zero.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount > from_account$balance) {
        div("Transfer amount cannot exceed current balance.", class = "form-error")
      }
    )
  )
}

# distribute modal
data_distribute <- function(account, currency, failed = FALSE, user_input = list()) {
  modalDialog(
    title = div(
      sprintf("📤 Distribute Funds from %s account", account$name),
      class = "modal-title"
    ),
    easyClose = TRUE,
    size = "l",
    footer = tagList(
      tags$button(
        type = "button",
        class = "btn btn-secondary",
        `data-bs-dismiss` = "modal",
        "Cancel"
      ),
      actionButton("submit_distribute", "Submit Distribution", class = "btn btn-primary")
    ),
    div(
      class = "modal-body",

      # Show current balance
      div(
        class = "current-balance mb-3",
        strong("Current Balance: "),
        sprintf("%s %.2f", currency, account$balance)
      ),

      helpText(HTML(
        "Enter the distribution details. Mandatory fields are marked with an asterisk <span class='asterisk'>*</span>."
      )),

      # Amount (required, must be > 0 and <= balance)
      numericInput(
        "dist_amount",
        label = tags$label("Amount", class = "required"),
        value = if (!is.null(user_input$amount)) user_input$amount else NULL,
        min = 0,
        max = account$balance
      ),
      if (failed && is.null(user_input$amount)) {
        div("Distribution amount is required.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount <= 0) {
        div("Distribution amount must be greater than zero.", class = "form-error")
      },
      if (failed && !is.null(user_input$amount) && user_input$amount > account$balance) {
        div("Distribution amount cannot exceed current balance.", class = "form-error")
      },

      # Transaction (optional, no uniqueness check needed)
      textInput(
        "dist_txn",
        label = "Transaction Reference (optional)",
        placeholder = "e.g DIST12345"
      )
    )
  )
}


processing_modal <- function(
    context = NULL,
    message = "Processing, please wait..."
) {
  modalDialog(
    div(class = "processing-modal", `data-context` = context,
        div(style = "text-align:center; padding: 20px;",
            div(class = "spinner-border text-primary", role = "status"),
            p(message)
        )
    ),
    easyClose = FALSE,
    footer = NULL,
    size = "s"
  )
}



status_modal <- function(type = c("success", "error"), title = NULL, message = NULL) {
  type <- match.arg(type)

  icon_tag <- if (type == "success") {
    icon("circle-check", class = "fa-3x status-icon success-icon")
  } else {
    icon("circle-exclamation", class = "fa-3x status-icon error-icon")
  }

  # Custom footer button
  footer_btn <- tags$button(
    type = "button",
    class = "status-btn btn",  # scoped class
    `data-bs-dismiss` = "modal",
    "Close"
  )

  modalDialog(
    title = NULL,
    easyClose = TRUE,
    footer = footer_btn,
    div(
      style = "text-align:center; padding: 20px;",
      icon_tag,
      tags$h3(
        if (is.null(title)) ifelse(type == "success", "Success!", "Error!") else title
      ),
      tags$p(
        if (is.null(message)) {
          if (type == "success") {
            "Your request was successfully processed."
          } else {
            "There was an issue processing your request. Please try again."
          }
        } else message
      )
    ),
    size = "m",
    class = paste("status-modal", type),  # scoped class
  )
}




# Helper to refresh main_account() asynchronously
refresh_main_account <- function(token, main_account) {
  future({
    res <- tryCatch({
      call_api("/get_minimal_tree", token = token)
    }, error = function(e) {
      return(list(error = sprintf("Failed to fetch account tree: %s", e$message)))
    })

    if (is.null(res) || httr::status_code(res) != 200) {
      return(list(error = "Account tree request failed."))
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(rawToChar(res$content)),
      error = function(e) {
        return(list(error = "Failed to parse account tree response."))
      }
    )

    if (!is.null(parsed$minimal_tree)) {
      list(tree = parsed$minimal_tree)
    } else {
      list(error = "No account tree returned.")
    }
  }) %...>% (function(result) {
    if (!is.null(result$error)) {
      showNotification(result$error, type = "error")

    } else if (!is.null(result$tree)) {
      oldtree <- isolate(main_account())

      # Update only if different
      if (!identical(oldtree, result$tree)) {
        main_account(result$tree)
      }
    }
  }) %...!% (function(e) {
    showNotification(
      paste("Unexpected error refreshing account:", e$message),
      type = "error"
    )
  })
}



