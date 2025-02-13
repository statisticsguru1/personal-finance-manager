#greetings
generate_greeting <- function(name) {
  current_time <- as.POSIXlt(Sys.time()) # Get the current time
  hour <- current_time$hour            # Extract the hour
  
  greeting <- if (hour >= 5 && hour < 12) {
    paste("Good Morning", name, "!")
  } else if (hour >= 12 && hour < 18) {
    paste("Good Afternoon", name, "!")
  } else {
    paste("Good Evening", name, "!")
  }
  return(tags$span(greeting, class = "welcome-text"))
}

# Define the custom function for nested tabs
nav_tab_nested <- function(..., ids = NULL, tabsetid = "tabSet1", is_parent = FALSE) {
  dots <- list(...)
  
  if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))
  
  shiny::tags$ul(
    class = if (is_parent) "nav nav-pills nav-stacked parent-tab" else "nav nav-pills nav-stacked child-tab",
    lapply(seq_along(dots), function(i) {
      id <- dots[[i]]$attribs$id
      if (is.null(id)) id <- ids[[i]]
      
      cl <- paste(tabsetid, if (is_parent) "parent" else "child", if (i == 1 && is_parent) "active")
      onclick <- if (is_parent) {
        sprintf("toggleChildren('%s'); opentab('%s', '%s'); notifyServerselected_tab('%s');", id, tabsetid, id, id)
      } else {
        sprintf("opentab('%s', '%s'); notifyServerselected_tab('%s');", tabsetid, id, id)
      }
      
      # Create the `li` tag for each tab
      tags$li(
        class = cl, id = id,
        onclick = onclick,
        style = "cursor: pointer;", # Mouse pointer
        tags$a(dots[[i]])
      )
    })
  )
}

# Define the custom function for nested content
nav_content_nested <- function(..., ids = NULL, tabsetid = "tabSet1") {
  dots <- list(...)
  
  if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))
  
  shiny::div(
    lapply(seq_along(dots), function(i) {
      id <- dots[[i]]$attribs$id
      if (is.null(id)) id <- ids[[i]]
      
      idc <- strsplit(id, "-")[[1]]
      if (idc[length(idc)] != "content") id <- paste0(id, "-content")
      
      shiny::div(
        class = paste0(tabsetid, "-content"),
        style = if (i == 1) "display: block;" else "display: none;",
        id = id,
        dots[[i]]
      )
    })
  )
}

# this input binding actually doesnt interact with users
# its just a stupid way to make sure input bindings are discovered 
# on server side

selectedaccountInput <- function(id, value = NULL) {
  tags$input(
    id = id,
    value = value,
    type = "internalinput",
    class = "selectedaccountInput",
    style = "display: none;" # This hides the element from the UI
  )
}


build_sidebar <- function(account) {
  children <- account$child_accounts
  account_id <- account$uuid
  account_label <- span(icon("wallet", class = "black-wallet"), span(account$name))
  is_parent<-ifelse(length(children)>0,T,F)
  
  sidebar_item <- nav_tab_nested(account_label, 
                                 is_parent = is_parent, 
                                 ids = c(account_id)
  )
  
  # If this account has children, create the children list and recurse for each child
  if (length(children) > 0) {
    child_list <- tags$ul(
      class = paste0(account_id, "-children"),
      style = "display: none;"
    )
    
    for (child in children) {
      child_list <- tagAppendChild(child_list, build_sidebar(child))
    }
    
    # Wrap child list in the parent tab
    sidebar_item <- tagAppendChild(sidebar_item, child_list)
  }
  
  return(sidebar_item)
}

# Function to dynamically construct content pages for all accounts
generate_nav_content <- function(main_account, content_generator) {
  all_accounts <- main_account$list_all_accounts()
  
  # Generate content divs for each account
  content_divs <- lapply(all_accounts, function(account) {
    
    account_data <- main_account$find_account(account)
    div(
      id = paste0(account_data$uuid, "-content"), # Dynamic ID
      content_generator(account_data)            # Dynamically generated content
    )
  })
  

  do.call(nav_content_nested, content_divs)
}

generate_child_accounts_section <- function(account) {
  # Ensure account$child_accounts is not NULL or empty
  if (is.null(account$child_accounts) || length(account$child_accounts) == 0) {
    return(NULL) # Return nothing if there are no child accounts
  }
  
  # Dynamically generate the child account cards
  child_account_cards <- lapply(account$child_accounts, function(child_account) {
    card(
      class = "child-account-card",
      card_header(
        tags$h4(child_account$name),
        class = "custom-card-header11"
      ),
      card_body(
        p(strong("Balance:"), paste(round(child_account$balance,2))),
        div(
          class = "status-line",
          tags$strong("Status: "),
          tags$span(
            class = if (tolower(child_account$status) == "active") "badge-success" else "badge-danger",
            child_account$status
          )
        ),
        actionLink(paste("navigate_",child_account$uuid),"more details",icon=icon("external-link-alt"))
      )
    )
  })
  
  # Remove names from the list to avoid layout issues(!!! refused to work with a named list)
  names(child_account_cards) <- NULL
  
  # Return the complete section
  return(
    card(
      class = "section child-accounts-section",
      fill=T,
      card_header(
        tags$h3("Child Accounts"),
        tags$hr(class = 'tags-hr'),
        class = "card-title",
      ),
      tagList(
      layout_column_wrap(
        heights_equal = "row",
        width = ifelse(length(account$child_accounts)<=2,1/2,ifelse(length(account$child_accounts)<4,1/3,1/4)),#control how cards span(2 columns for 1 or children, 3 for 3 children and 4 for 4 or more)
        gap = "15px",            # Space between the cards
        !!!child_account_cards
      )
    )
    )
    )
}

default_content_generator <- function(account) {
  tagList(
    # Account Details Section
    card(
      class = "section account-details card",
      fill=T,
      card_header(tags$h3("Account Details"),
                  tags$hr(class = 'tags-hr'),
                  class = "card-title"),
      uiOutput(paste0("account_summary_section",account$uuid))
    ),

    # Actions Section
    card(
      class = "section actions",
      fill=T,
      card_header(tags$h3("Actions"),
                  tags$hr(class = 'tags-hr'),
                  class = "card-title"
                  ),
    uiOutput(paste0("actions_section",account$uuid))
    ),
    
    # Transactions Section
    card(
      class = "section",
      fill = TRUE,
      card_header(
        tags$h3("Transactions"),
        tags$hr(class = 'tags-hr'),
        class = "card-title"
      ),
      card_body(
        layout_column_wrap(
          fill = TRUE,
          heights_equal = "row",
          DT::dataTableOutput(paste0("transaction_table_", account$uuid))
        )
      )
    ),
    
    # Child Accounts Section
    uiOutput(paste0("children_section_",account$uuid)),
    
    # Visualizations Section
    card(
      class = "section visualizations",
      card_header(tags$h3("Visualizations"),
                  tags$hr(class = 'tags-hr'),
                  class = "card-title"
      ),
      layout_column_wrap(
       fill = T,
       width = 1/2,
      highchartOutput(paste0("balance_to_debt_", account$uuid)),         # Unique ID
      highchartOutput(paste0("transaction_trend_chart_", account$uuid))   # Unique ID
    )
    )
  )
}


