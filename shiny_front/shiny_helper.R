# fetch the whole account tree
get_main_account_from_api <- function(token) {
  res <- GET(
    url = paste0(host_url, "get_minimal_tree"),
    add_headers(Authorization = paste("Bearer", token))
  )
  parsed<-fromJSON(rawToChar(res$content))
  parsed$minimal_tree
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
