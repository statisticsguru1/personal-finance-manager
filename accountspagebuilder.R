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
  account_label <- span(bs_icon("database"), span(account$name))
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
        p(strong("Balance:"), paste(child_account$balance)),
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
  return(tags$div(
    class = "section child-accounts-section",
    tags$h3("Child Accounts"),
    tags$hr(class = 'tags-hr'),
    tagList(
      layout_column_wrap(
        heights_equal = "row",
        width = ifelse(length(account$child_accounts)<=2,1/2,ifelse(length(account$child_accounts)<4,1/3,1/4)),#control how cards span(2 columns for 1 or children, 3 for 3 children and 4 for 4 or more)
        gap = "15px",            # Space between the cards
        !!!child_account_cards
      )
    )
  ))
}

default_content_generator <- function(account) {
  tags$div(
    class="custom-main-panel",
  tagList(
    # Account Details Section
    tags$div(
      class = "section account-details card",
      tags$h3("Account Details", class = "card-title"),
      tags$hr(class = 'tags-hr'),
      tags$div(
        class = "details-grid",
        tags$div(
          class = "detail-item",
          tags$strong("Account ID:"),
          tags$span(account$uuid)
        ),
        tags$div(
          class = "detail-item",
          tags$strong("Name:"),
          tags$span(account$name)
        ),
        tags$div(
          class = "detail-item",
          tags$strong("Type:"),
          tags$span(class(account)[1])
        ),
        tags$div(
          class = "detail-item",
          tags$strong("Balance:"),
          tags$span(class = "balance", paste(format(account$balance, big.mark = ",")))
        ),
        tags$div(
          class = "detail-item",
          tags$strong("Balance in Children:"),
          tags$span(paste(format(account$compute_total_balance(), big.mark = ",")))
        ),
        if (!is.null(account$status)) {
          tags$div(
            class = "detail-item",
            tags$strong("Status:"),
            tags$span(class = if (tolower(account$status) == "active") "badge-success" else "badge-danger", account$status)
          )
        },
        if (!is.null(account$allocation)) {
          tags$div(
            class = "detail-item",
            tags$strong("Allocation:"),
            tags$span(paste(scales::percent(account$allocation, accuracy = 0.01)))
          )
        },
        if (!is.null(account$parent)) {
          tags$div(
            class = "detail-item",
            tags$strong("Parent Account:"),
            tags$a(href = paste0("/account/", account$parent$uuid), account$parent$name, class = "parent-link")
          )
        }
      )
    ),
    
    # Actions Section
    tags$div(
      class = "section actions",
      tags$h3("Actions"),
      tags$hr(class = 'tags-hr'),
      layout_column_wrap(
        width=1/4,
        heights_equal = "row",
        gap="10px",
        fill = T,
        card(
          class = "hidden-cards",
          card_body(
            layout_column_wrap(
              width=1,
              fill=T,
              heights_equal = "row",
              gap="10px",
              numericInput(paste0("deposit_amount_", account$uuid), "Deposit Amount:", value = NA, min = 0),
              textInput(paste0("deposit_transaction_",account$uuid),"Transaction number",value="",placeholder="e.g TAP8I3JK2G"),
              textInput(paste0("deposit_transaction_channel_",account$uuid),"Transaction Channel",value="",placeholder="e.g ABC BANK"),
              actionButton(paste0("deposit_btn_", account$uuid), "Deposit",class = "btn-normal")
            )
          )
        ),
        card(
          class = "hidden-cards",
          card_body(
            layout_column_wrap(
              width=1,
              fill=T,
              heights_equal = "row",
              gap="10px",
              numericInput(paste0("withdraw_amount_", account$uuid), "Withdraw Amount:", value = NA, min = 0),
              actionButton(paste0("withdraw_btn_", account$uuid), "Withdraw",class = "btn-normal")
            )
          )
        ),
        card(
          class = "hidden-cards",
          card_body(
            layout_column_wrap(
              width=1,
              fill=T,
              gap="10px",
              heights_equal = "row",
              numericInput(paste0("transfer_amount_", account$uuid), "Transfer Amount:", value = 0, min = 0),
              selectInput(paste0("transfer_target_", account$uuid), "Transfer To:", choices = main_account$list_all_accounts()),
              actionButton(paste0("transfer_btn_", account$uuid), "Transfer",, class = "btn-normal")
            )
          )
        ),
        card(
          class = "hidden-cards",
        card_body(
          layout_column_wrap(
            width=1,
            fill=T,
            gap="10px",
            heights_equal = "row",
            numericInput(paste0("pay_", account$uuid),
                         tagList(paste("Pay",ifelse(grepl("main",account$name, ignore.case = TRUE),"all dues",account$name)),
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("Pay amount due in accounts below"),
                                   placement = "right")),
                         value =min(account$compute_total_balance(),account$compute_total_due()), min = 0),
            actionButton(paste0("transfer_btn_", account$uuid), "Pay",class = "btn-normal")
          )
        )
      )
        ),
      br(),
      tags$hr(class = 'tags-hr'),
      selectInput(paste0("more_actions_", account$uuid),"more actions",choices=c("Add account","Edit account","Close Account")),
      conditionalPanel(
        condition = sprintf("input['%s'] =='Add account'",paste0("more_actions_",account$uuid)),
      layout_column_wrap(
        width=1/3,
        gap="6px",
        textInput(paste0("account_name_add",account$uuid),"Account name",value=""),
        numericInput(paste0("allocation_add",account$uuid),
                     tagList("Allocation",
                     tooltip(
                       bs_icon("info-circle"),
                       paste("what percentage of",main_account$find_account_by_uuid(account$uuid)$name,
                       "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                             main_account$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
                       placement = "right")),value = (1-account$total_allocation)),
        numericInput(paste0("priority_add",account$uuid),
                     tagList("Priority",
                     tooltip(
                       bs_icon("info-circle"),
                       paste("A numeric value representing priority, high values gives this account high priority over other accounts from the same parent"),
                       placement = "right")),value=0),
                     )),
      conditionalPanel(
        condition = sprintf("input['%s'] =='Edit account'",paste0("more_actions_",account$uuid)),
        
        if(class(account)[1]== "MainAccount"){

        }
        else if(class(account)[1]== "ChildAccount"){
          layout_column_wrap(
            width=1/4,
            gap="6px",
            textInput(paste0("account_name_edit_child",account$uuid),"Account name",value=account$name),
            numericInput(paste0("allocation_edit_child",account$uuid),
                         tagList("Allocation",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("what percentage of",main_account$find_account_by_uuid(account$uuid)$name,
                                         "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                                         main_account$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
                                   placement = "right")),value =account$allocation),
            textInput(paste0("account_status_edit_child",account$uuid),"Status",value=account$get_account_status()),
            numericInput(paste0("priority_edit_child",account$uuid),
                         tagList("Priority",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("A numeric value representing priority, high values gives this account high priority over other accounts from the same parent"),
                                   placement = "right")),value=account$get_priority())
          )
          
        }
        else if(class(account)[1]== "GrandchildAccount"){
          layout_column_wrap(
            width=1/4,
            gap="6px",
            textInput(paste0("account_name_edit_grandchild",account$uuid),"Account name",value=account$name),
            numericInput(paste0("allocation_edit_grandchild",account$uuid),
                         tagList("Allocation",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("what percentage of",main_account$find_account_by_uuid(account$uuid)$name,
                                         "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                                         main_account$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
                                   placement = "right")),value =account$allocation),
            textInput(paste0("account_status_edit_grandchild",account$uuid),"Status",value=account$get_account_status()),
            numericInput(paste0("priority_edit_grandchild",account$uuid),
                         tagList("Priority",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("A numeric value representing priority, high values gives this account high priority over other accounts from the same parent"),
                                   placement = "right")),value=account$get_priority()),
            dateInput(paste0("due_date_edit_grandchild",account$uuid),
                      "Due date",
                       value =account$get_due_date(),
                      min=account$get_due_date()-100,
                      max=account$get_due_date()+1000000
                      ),
            numericInput(paste0("fixed_amount_edit_grandchild",account$uuid),
                         tagList("Fixed amount",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("The fixed amount for the account eg monthly bills have a monthly payment amount"),
                                   placement = "right")),value=account$get_fixed_amount()),
            selectInput(paste0("account_type_edit_grandchild",account$uuid),"Account type",
                        choice=c("Bill", "Debt", "Expense", "FixedSaving", "NonFixedSaving"),
                        selected=account$get_account_type()),
            numericInput(paste0("period_edit_grandchild",account$uuid),
                         tagList("Period",
                                 tooltip(
                                   bs_icon("info-circle"),
                                   paste("how frequent do you pay this account monthly(30),Quarterly(90) etc,this only applies for fixed payments like bills,loans and fixed savings"),
                                   placement = "right")),
                         value=account$get_account_periods())
          )
        } 
        else {
          
        },
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] =='Close Account'",paste0("more_actions_",account$uuid)),
        # some modals if the account has money you need to move it.
      ),
      br(),
      actionButton(paste0("save", account$uuid), "Save", class = "btn-danger",width="200px")
      ),
    
    # Transactions Section
    tags$div(
      class = "section",
      tags$h3("Transactions"),
      tags$hr(class = 'tags-hr'),
      DT::dataTableOutput(paste0("transaction_table_", account$uuid))  # Unique ID
    ),
    
    # Child Accounts Section
    generate_child_accounts_section(account),
    
    # Visualizations Section
    tags$div(
      class = "section visualizations",
      tags$h3("Visualizations"),
      tags$hr(class = 'tags-hr'),
      layout_column_wrap(
       fill = T,
       width = 1/2,
      highchartOutput(paste0("balance_to_debt_", account$uuid)),         # Unique ID
      highchartOutput(paste0("transaction_trend_chart_", account$uuid))   # Unique ID
    )
    )
  )
  )
}
