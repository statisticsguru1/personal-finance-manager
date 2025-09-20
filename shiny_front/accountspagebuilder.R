#source("shiny_helper.R")
#source("here::here("shiny_front/"shiny_helper.R"))
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
# nav_content_nested <- function(..., ids = NULL, tabsetid = "tabSet1") {
#   dots <- list(...)
#
#   if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))
#
#   shiny::div(
#     lapply(seq_along(dots), function(i) {
#       id <- dots[[i]]$attribs$id
#       if (is.null(id)) id <- ids[[i]]
#
#       idc <- strsplit(id, "-")[[1]]
#       if (idc[length(idc)] != "content") id <- paste0(id, "-content")
#
#       shiny::div(
#         class = paste0(tabsetid, "-content"),
#         style = if (i == 1) "display: block;" else "display: none;",
#         id = id,
#         dots[[i]]
#       )
#     })
#   )
# }



nav_content_nested <- function(..., ids = NULL, tabsetid = "tabSet1") {
  dots <- list(...)
  if (is.null(ids)) ids <- paste0("bsTab-", seq_along(dots))

  # Find which IDs are children of the first item (based on naming)
  first_id <- dots[[1]]$attribs$id %||% ids[[1]]
  first_parent_prefix <- sub("-content$", "", first_id)

  shiny::div(
    lapply(seq_along(dots), function(i) {
      id <- dots[[i]]$attribs$id %||% ids[[i]]
      if (!grepl("-content$", id)) id <- paste0(id, "-content")

      # Check if this is the first parent or its direct children
      is_first_or_child <- startsWith(
        id,
        paste0(first_parent_prefix, "-")) || id == paste0(first_parent_prefix, "-content")

      shiny::div(
        class = paste0(tabsetid, "-content"),
        style = if (i == 1 || is_first_or_child) "display: block;" else "display: none;",
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
  account_id <- account$account_uuid
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

  all_accounts <- get_all_accounts(main_account)

  # Generate content divs for each account
  content_divs <- lapply(all_accounts, function(account) {
    account_data <- get_account_by_uuid(main_account,account$account_uuid)
    div(
      id = paste0(account_data$account_uuid, "-content"), # Dynamic ID
      content_generator(account_data)            # Dynamically generated content
    )
  })


  do.call(nav_content_nested, content_divs)
}

generate_child_accounts_section <- function(account,currency="$") {
  # Ensure account$child_accounts is not NULL or empty
  if (is.null(account$child_accounts) || length(account$child_accounts) == 0) {
    return(NULL) # Return nothing if there are no child accounts
  }


  # theme colors generator
  generate_random_color_pair <- function() {
    repeat {
      # Generate a random color
      random_hue <- runif(1, 0, 360)
      random_sat <- runif(1, 0.7, 1)
      random_light <- runif(1, 0.3, 0.5)

      random_color <- hex(HLS(random_hue, random_light, random_sat))

      generate_fade_color <- function(hex_color, fade_factor = 0.7) {
        rgb_color <- hex2RGB(hex_color)
        hls_color <- as(rgb_color, "HLS")
        hls_color@coords[, "L"] <- pmin(1, hls_color@coords[, "L"] + fade_factor * (1 - hls_color@coords[, "L"]))
        faded_hex <- hex(as(hls_color, "RGB"))
        return(faded_hex)
      }
      faded_color <- generate_fade_color(random_color)

      # function for computing color distance

      color_distance <- function(color1, color2) {
        rgb1 <- hex2RGB(color1)@coords
        rgb2 <- hex2RGB(color2)@coords
        sqrt(sum((rgb1 - rgb2)^2)) * 255
      }

      # Ensure the random color is not white and is distinct from its faded version
      if (random_color != "#FFFFFF") {
        return(list(color = random_color, faded = faded_color))
      }
    }
  }


  # Define theme colors and icons for dynamic selection

  icon_names <- c("dollar-sign", "chart-line", "money-bill","money-bill",
                  "credit-card","piggy-bank", "wallet","briefcase","coins",
                  "money-check","hand-holding-dollar","sack-dollar","chart-pie",
                  "circle-dollar-to-slot"
                  )

  # Dynamically generate the child account cards
  child_account_cards <- lapply(account$child_accounts, function(child_account) {

    # Randomly select a theme color and an icon
    theme_colors<-generate_random_color_pair()
    selected_color <- theme_colors$color
    background<-theme_colors$faded
    selected_icon <- sample(icon_names, 1)

    card(
      class = "child-account-custom-card",
      card_header(
        class = "child-account-header",
        span(
          icon(
            selected_icon,
            class = "child-account-icon",
            style = paste("color:", selected_color, ";")
          ),
          span(
            child_account$name,
            class = "child-account-title")
          ),
        span(
          paste(child_account$account_status),
          class = "child-account-status-badge",
          style=sprintf(
            "color:%s;background-color:%s",
            ifelse(
              tolower(child_account$account_status)=="active",
              "#28a745",
              "#dc3545"
            ),
            ifelse(
              tolower(child_account$account_status)=="active",
              "#d4edda",
              "#f8d7da"
            )
          )
        )
      ),
      card_body(
        span("Balance:", style = "color: gray; font-size: 0.9rem;"),
        div(
          paste0(
            currency,
            format(round(child_account$balance, 2), nsmall = 2)
            ),
          class = "child-account-balance"
        ),
        div(
          class = "child-account-progress-container",
          span(
            paste0(100*child_account$allocation, "%"),
            style = paste("color:", selected_color, ";")
          ),
          span(
            "Allocation",
            style = "color: gray; font-size: 0.85rem;"
          )
        ),
        div(
          class = "child-account-progress-bar",
          div(
            class = "child-account-progress-fill",
            style = sprintf(
              "background:%s; width:%s%%;",
              selected_color, 100 * child_account$allocation
            )
          )
        ),

        a(
          "more details",
          href = "#",
          class = "child-account-more-details",
          style=sprintf(
            "color:%s",
            selected_color
          ),
          HTML(
            sprintf(
              "<svg width='12' height='12'><path d='M0 6h8M6 4l4 2-4 2' stroke='%s' stroke-width='2' fill='none'/></svg>",
              selected_color
            )
         )
        )
      )
    )
  })

  # Remove names from the list to avoid layout issues
  names(child_account_cards) <- NULL

  # Return the complete section
  return(
    card(
      class = "section child-accounts-section",
      fill=T,
      card_header(
        tags$h3("Child Accounts"),
        tags$hr(class = 'tags-hr'),
        class = "card-title"
      ),
      tagList(
        layout_column_wrap(
          heights_equal = "row",
          width = ifelse(
            length(account$child_accounts) <= 2, 1/2,
            ifelse(
              length(account$child_accounts) < 4, 1/3, 1/4
            )
          ),
          gap = "15px",
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
      card_header(
        tags$h3(
          tags$i(class = "fas fa-chart-simple icon-header"),
          "Account Details"
          ),
        tags$hr(class = 'tags-hr'),
        class = "card-title"
      ),
      uiOutput(paste0("account_summary_section",account$account_uuid))
    ),

    # Actions Section
    card(
      class = "section actions",
      fill=T,
      card_header(
        tags$h3("Actions"),
        tags$hr(class = 'tags-hr'),
        class = "card-title"
      ),
      uiOutput(paste0("actions_section",account$account_uuid))
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
          DT::dataTableOutput(paste0("transaction_table_", account$account_uuid))
        )
      )
    ),

    # Child Accounts Section
    uiOutput(paste0("children_section_",account$account_uuid)),

    # Visualizations Section
    card(
      class = "section visualizations",
      card_header(
        tags$h3("Visualizations"),
        tags$hr(class = 'tags-hr'),
        class = "card-title"
      ),
      layout_column_wrap(
        fill = T,
        width = 1/2,
        highchartOutput(paste0("balance_to_debt_", account$account_uuid)),
        highchartOutput(paste0("transaction_trend_chart_",account$account_uuid))
      )
    )
  )
}


