
server <- function(input, output,session) {

  # observeEvent(input$dark_mode,{
  #   if(input$dark_mode=="dark"){
  #     showNotification("Welcome to dark mode!!!")
  #   }
  # })

  #logging
  track_usage(
  # storage_mode = store_json(),
  storage_mode = store_rds(".")
  )

  user <- reactiveVal(
    list(
      user_id = "ceecfcc5-d4f5-4889-a4a8-fdc2776129e0",
      currency = "$",
      role = "user",
      First_name = "Festus",
      Middle_name ="Mutinda",
      Last_name = "Nzuma",
      user_email = "mutindafestus27@gmail.com",
      token = NULL
    )
  )

 output$greetings<-renderUI({
   req(user())
   generate_greeting(user()$First_name)
 }
 )


 output$user_email<-renderText({
   req(user())
   paste(user()$user_email)
 }
 )

 output$fullnames<-renderText({
   req(user())
   paste(user()$First_name,user()$Last_name)
 }
 )




 host_url<-Sys.getenv("HOST_URL")
  observe({
    # Run only once on session start
    isolate({
      res <- tryCatch({
        call_api(
          "/generate_access_token",
          method = "POST",
          body = list(
            user_id = user()$user_id,
            role = user()$role,
            type = "session",
            session_id = Sys.getpid()
          )
        )
      }, error = function(e) {
        showNotification(paste("Failed to call API:", e$message), type = "error")
        return(NULL)
      })

      if (is.null(res)) return()

      if (httr::status_code(res) != 200) {
        showNotification("Token request failed", type = "error")
        return()
      }

      parsed <- tryCatch(
        jsonlite::fromJSON(rawToChar(res$content)),
        error = function(e) {
          showNotification("Failed to parse token response", type = "error")
          return(NULL)
        }
      )

      if (!is.null(parsed$token)) {
        user(modifyList(user(), list(token = parsed$token)))
      }
    })
  })



  # keep whole account tree in memory
  main_account <- reactiveVal(NULL)

  # First immediate fetch
  observe({
    req(user()$token)  # wait until token is ready

    res <- tryCatch({
      call_api("/get_minimal_tree", token = user()$token)
    }, error = function(e) {
      showNotification(paste("Failed to fetch account tree:", e$message), type = "error")
      return(NULL)
    })

    if (is.null(res) || httr::status_code(res) != 200) return()

    parsed <- tryCatch(
      jsonlite::fromJSON(rawToChar(res$content)),
      error = function(e) {
        showNotification("Failed to parse account tree response", type = "error")
        return(NULL)
      }
    )

    if (!is.null(parsed$minimal_tree)) {
      main_account(parsed$minimal_tree)
    }
  })

  # Background refresher
  observe({
    invalidateLater(4*60*1000, session)
    req(user()$token)

    res <- tryCatch({
      call_api("/get_minimal_tree", token = user()$token)
    }, error = function(e) {
      showNotification(paste("Failed to fetch account tree:", e$message), type = "error")
      return(NULL)
    })

    if (is.null(res) || httr::status_code(res) != 200) return()

    parsed <- tryCatch(
      jsonlite::fromJSON(rawToChar(res$content)),
      error = function(e) {
        showNotification("Failed to parse account tree response", type = "error")
        return(NULL)
      }
    )

    if (!is.null(parsed$minimal_tree)) {
      main_account(parsed$minimal_tree)
    }
  })

  currency <- reactive({
    user()$currency
  })

  #### Dashboard content ===================================================================================================
  savings_goal <- reactive({
    req(main_account())
    main_account()$total_balance/(main_account()$total_balance+main_account()$total_due)
  })

  observe({
    # saving goal value

    output$savings_progress <- renderUI({
      div(
        class = "highlight",
        paste(
          "Your Savings Goal:",
          if(is.na(savings_goal())){
            ""
          } else{
            scales::percent(savings_goal(),accuracy=0.1)
          }),
        div(
          class = "progress-bar",
          div(
            class = "progress-fill",
            style = paste("width:", scales::percent(savings_goal(),accuracy=0.1))
          )
        )
      )
    })

    output$overall_bal<-renderUI({
      req(main_account())
      HTML(paste(h1(paste0(currency(),format(main_account()$total_balance, big.mark = ",", scientific = FALSE)), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })

    output$amount_due<-renderUI({
      req(main_account())
      HTML(paste(h1(paste0(currency(),format(main_account()$total_due, big.mark = ",", scientific = FALSE)), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })

    output$due_in_n_days<-renderUI({
      req(main_account())
      HTML(paste(h1(paste0(currency(),format(main_account()$compute_total_due_within_n_days, big.mark = ",", scientific = FALSE)), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })

    output$number_of_accounts<-renderUI({
      req(main_account())
      HTML(paste(h1(format(length(main_account()$child_accounts_list), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })

    # Income allocation
    output$overall_alloc<-renderHighchart({
      req(main_account())
      accounts<-list()
      for (child in main_account()$child_accounts){
        accounts[[length(accounts)+1]]<-list(name = child$name, y = child$allocation)
      }

      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Income Allocation") %>%
        hc_xAxis(categories = accounts %>% map(~ .x$name) %>% unlist()) %>%
        hc_add_series(
          name = "Allocation",
          data = 100 * (accounts %>% map(~ .x$y) %>% unlist()),
          colorByPoint = TRUE
        ) %>%
        hc_plotOptions(
          bar = list(
            dataLabels = list(
              enabled = TRUE,
              format = '{point.y:.1f} %'
            )
          )
        ) %>%
        hc_tooltip(
          pointFormat = '{point.y:.1f} %'
        )
    })

    # Total Balance vs Amount Due
    output$overall_pie_chart <- renderHighchart({
      req(main_account()$total_balance, main_account()$total_due)

      currency_symbol <- currency()

      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Income vs. Obligations") %>%
        hc_add_series(
          data = list(
            list(name = "Balance", y = main_account()$total_balance),
            list(name = "Liabilities", y = main_account()$total_due)
          )
        ) %>%
        hc_plotOptions(
          pie = list(
            dataLabels = list(
              enabled = TRUE,
              format = paste0("{point.name}: ", currency_symbol, "{point.y:.2f}"),
              style = list(
                color = "contrast",
                fontSize = "10px"
              )
            )
          )
        ) %>%
        hc_tooltip(
          pointFormat = paste0("<b>{point.name}</b>: ", currency_symbol, "{point.y:.2f}")
        )
    })

    # Transactions


    output$transtable <- renderUI({
      req(main_account())

      # collect transactions from all accounts
      all_txs <- get_all_transactions(main_account())

      if (nrow(all_txs) == 0) {
        return(HTML("<p>No transactions available</p>"))
      }

      all_txs %>%
        arrange(desc(Date)) %>%
        filter(By == "User") %>%
        select(-c(By, amount_due, overall_balance)) %>%
        mutate(across(where(is.numeric),round,2))%>%
        head(3) %>%
        mutate(
          Amount = sprintf(
            '<span class="amount-%s">%s</span>',
            tolower(Type),
            format(Amount, big.mark = ",", scientific = FALSE)
          ),
          Type = sprintf(
            '<span class="transaction-type type-%s">%s</span>',
            tolower(Type), Type
          ),
          Balance = format(Balance, big.mark = ",", scientific = FALSE)
        ) %>%
        knitr::kable(
          align = 'l', row.names = F,
          format = "html", escape = F,
          table.attr = 'class="custom-table2"'
        ) %>%
        HTML()
    })

    # popover

    output$info_message <- renderUI({
      req(main_account())
      # Extracting allocations for each child account
      allocations <- purrr::map(main_account()$child_accounts, ~ .x$allocation)

      # Formatting allocations into a list of percentages
      allocation_text <- purrr::map2(names(allocations), allocations, ~paste0("- <strong>", .x, ":</strong> ", scales::percent(.y)))

      # Render UI with income allocations
      div(
        h1("Income vs. Obligations:"),
        p(HTML(paste(
          "Your total account balance is:",
          strong(paste0(currency(),format(main_account()$total_balance, big.mark = ",", scientific = FALSE))),
          ". This is the aggregated account balances from all accounts."
        ))),
        p(HTML(paste(
          "The total amount due is:",
          strong(paste0(currency(),format(main_account()$total_due, big.mark = ",", scientific = FALSE))),
          ". This amount is the aggregated arrears from all accounts, with fixed target amounts such as bills, fixed saving goals, debts such as loans, loan arrears, etc. Accounts with no fixed targets, such as non-fixed savings, don't contribute towards this.
        It is the Debt you would remain with if you used your account balance to pay your liabilities"
        ))),
        tags$hr(),
        h1("Income Allocation:"),
        p("This is how your new income will get distributed to different accounts."),
        tags$ul(
          # Create a list of allocations
          lapply(allocation_text, function(text) {
            tags$li(HTML(text))  # Use HTML() to render the bold tags correctly
          })
        ),
        tags$hr(),
        h1("Transactions:"),
        p("This is a snapshot of your income transactions."),
        tags$hr(),
        p(
          "More information is available on the ",
          tags$a(href = "#accounts-page", "accounts page."),  # Link to the accounts page
          " Click here to view."
        ),
        class = "popover-content info-message"  # Apply popover-content class here
      )
    })


    # Distribution of amount due

    output$tier2_allocation_chart2 <- renderHighchart({
      req(main_account())
      # Retrieve the currency symbol dynamically
      currency_symbol <- currency()

      accounts <- list()
      for (child in main_account()$child_accounts) {
        accounts[[length(accounts) + 1]] <- list(name = child$name, y = child$total_due)
      }

      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Distribution of Liabilities (Amount Due)") %>%
        hc_xAxis(categories = accounts %>% map(~ .x$name) %>% unlist()) %>%
        hc_add_series(
          name = "Amt. Due",
          data = accounts %>% map(~ .x$y) %>% unlist(),
          colorByPoint = TRUE  # Ensures each bar gets a different color
        ) %>%
        hc_plotOptions(
          bar = list(
            dataLabels = list(
              enabled = TRUE,
              format = sprintf('%s{point.y:.2f}', currency_symbol)  # Display currency in value labels
            )
          )
        ) %>%
        hc_tooltip(
          pointFormat = sprintf('<b>{point.name}</b>: %s{point.y:.2f}', currency_symbol)  # Display currency in tooltip
        )
    })


    # Distribution of account balance
    output$tier2_allocation_chart <- renderHighchart({
      req(main_account())
      # Retrieve the currency symbol dynamically
      currency_symbol <- currency()
      total_balance <- main_account()$total_balance

      accounts <- list()
      for (child in main_account()$child_accounts) {
        accounts[[length(accounts) + 1]] <- list(name = child$name, y = child$total_balance)
      }

      highchart() %>%
        hc_chart(
          type = "pie",
          events = list(
            render = JS(sprintf("
          function() {
            var chart = this,
                series = chart.series[0];

            var totalBalance = '%s%s';  // Embed the currency and total balance
            var customLabel = chart.options.chart.customLabel;

            if (!customLabel) {
              customLabel = chart.options.chart.customLabel = chart.renderer.label(
                'Total<br/><strong>' + totalBalance + '</strong>'
              )
              .css({
                color: 'black',
                textAnchor: 'middle'
              })
              .add();
            }

            var x = series.center[0] + chart.plotLeft,
                y = series.center[1] + chart.plotTop - (customLabel.attr('height') / 2);

            customLabel.attr({ x: x, y: y });

            // Adjust font size based on chart diameter
            customLabel.css({ fontSize: (series.center[2] / 10) + 'px' });
          }
        ", currency_symbol, total_balance))  # Inject currency symbol + balance into JS
          )
        ) %>%
        hc_title(text = "Distribution of Account Balance") %>%
        hc_plotOptions(
          pie = list(
            innerSize = '60%',
            dataLabels = list(
              enabled = FALSE,
              format = '{point.name}: {point.percentage:.1f} %'
            )
          )
        ) %>%
        hc_add_series(
          name = "Balance Distribution",
          data = accounts
        ) %>%
        hc_tooltip(
          pointFormat = sprintf('{point.name}: %s{point.y:.2f}', currency_symbol)  # Add currency in tooltip
        )
    })


    #popopper message for the card
    output$info_message1 <- renderUI({
      req(main_account())
      currency_symbol <- currency()  # Retrieve the currency symbol dynamically

      # Extracting account balance distribution
      balance_distribution <- purrr::map(main_account()$child_accounts, ~ .x$total_balance)
      total_balance <- sum(unlist(balance_distribution))  # Total balance across all child accounts

      balance_distribution_text <- purrr::map2(
        names(balance_distribution), balance_distribution,
        ~paste0("- <strong>", .x, ":</strong> ", currency_symbol, format(.y, big.mark = ",", scientific = FALSE),
                " (", round(.y / total_balance * 100, 2), "%)")
      )

      # Extracting liabilities distribution
      liabilities_distribution <- purrr::map(main_account()$child_accounts, ~ .x$total_due)
      total_liabilities <- sum(unlist(liabilities_distribution))  # Total liabilities across all child accounts

      liabilities_distribution_text <- purrr::map2(
        names(liabilities_distribution), liabilities_distribution,
        ~paste0("- <strong>", .x, ":</strong> ", currency_symbol, format(.y, big.mark = ",", scientific = FALSE),
                " (", round(.y / total_liabilities * 100, 2), "%)")
      )

      # Render UI with the relevant sections and data
      div(
        # Distribution of Account Balances Section
        h1("Distribution of Account Balances:"),
        p("Your total account balance funds the following accounts:"),
        tags$ul(
          lapply(balance_distribution_text, function(text) {
            tags$li(HTML(text))  # Render bold text correctly
          })
        ),
        tags$hr(),

        # Distribution of Liabilities Section
        h1("Distribution of Liabilities:"),
        p("The total amount due is distributed across the following accounts:"),
        tags$ul(
          lapply(liabilities_distribution_text, function(text) {
            tags$li(HTML(text))
          })
        ),
        tags$hr(),

        # More information link
        p(
          "More information is available on the ",
          tags$a(href = "#accounts-page", "accounts page."),  # Link to the accounts page
          " Click here to view."
        ),
        class = "popover-content info-message"  # Apply popover-content class here
      )
    })



  })

  # tier 2 accounts

  accounts <- reactive({
    req(main_account())
    accounts_data <- list()
    acc_child<-list()
    account_names <- c()
    for (child in main_account()$child_accounts) {
      accounts_data[[length(accounts_data) + 1]] <- list(
        list(name = "Balance", y = child$total_balance),
        list(name = "Amount Due", y = child$total_due)
      )
      account_names[length(accounts_data)] <- child$name

      # grand children
      acc_child[[length(acc_child)+1]]<-list(
        categories=names(child$child_accounts),
        balance =unlist(child$child_accounts%>%purrr::map(~.x$total_balance)),
        due =unlist(child$child_accounts%>%purrr::map(~.x$total_due))
      )
    }
    names(accounts_data) <- account_names
    names(acc_child)<-account_names
    list(accounts_data=accounts_data,acc_child=acc_child)
  })

  output$dynamic_cards <- renderUI({
    # Dynamically create cards for each account
    card_list<-lapply(names(accounts()$accounts_data), function(account_name) {
      card(
        card_header(
          paste(account_name, "Account"),
          popover(
            bs_icon("info-circle"),
            uiOutput(paste0(gsub(" ", "_", account_name), "_poppover")),
            title = "Detailed Information",
            placement = "top"
          ),
          class = "custom-card-header"),
        highchartOutput(paste0(gsub(" ", "_", account_name), "_chart")),  # Pie chart for Account
        tags$hr(class = "tags-hr"),
        highchartOutput(paste0(gsub(" ", "_", account_name), "_distribution_chart"))  # Bar chart for distribution across grandchild accounts
      )
    })

    layout_column_wrap(
      heights_equal = "row",
      width = 1/3,
      gap = "3px",
      !!!card_list
    )

  })

  observe({
    lapply(names(accounts()$accounts_data), function(account_name) {

      # Pie chart for Balance vs Amount Due
      output[[paste0(gsub(" ", "_", account_name), "_chart")]] <- renderHighchart({
        currency_symbol <- currency()  # Get the dynamic currency symbol

        highchart() %>%
          hc_chart(type = "pie") %>%
          hc_title(text = paste(account_name, "Account Balance")) %>%
          hc_add_series(
            data = lapply(accounts()$accounts_data[[account_name]], function(account) {
              list(name = account$name, y = account$y)
            })
          ) %>%
          hc_plotOptions(
            pie = list(
              dataLabels = list(
                enabled = TRUE,
                format = paste0("{point.name}: ", currency_symbol, "{point.y:,.2f}")  # Format with currency
              )
            )
          ) %>%
          hc_tooltip(
            pointFormat = paste0("{point.name}: <b>", currency_symbol, "{point.y:,.2f}</b>")  # Tooltip formatting
          )
      })


      # Bar chart for distribution across grandchild accounts
      output[[paste0(gsub(" ", "_", account_name), "_distribution_chart")]] <- renderHighchart({
        currency_symbol <- currency()  # Get the dynamic currency symbol

        highchart() %>%
          hc_chart(type = "bar") %>%
          hc_title(text = paste(account_name, "Account Balance Distribution")) %>%
          hc_xAxis(categories = accounts()$acc_child[[account_name]]$categories) %>%
          hc_add_series(
            name = "Balance",
            data = as.numeric(accounts()$acc_child[[account_name]]$balance),
            dataLabels = list(enabled = TRUE, format = paste0(currency_symbol, " {point.y:,.2f}"))
          ) %>%
          hc_add_series(
            name = "Amount Due",
            data = as.numeric(accounts()$acc_child[[account_name]]$due),
            dataLabels = list(enabled = TRUE, format = paste0(currency_symbol, " {point.y:,.2f}"))
          ) %>%
          hc_tooltip(
            pointFormat = paste0("{series.name}: <b>", currency_symbol, "{point.y:,.2f}</b>")
          )
      })


      output[[paste0(gsub(" ", "_", account_name), "_poppover")]] <- renderUI({
        currency_symbol <- currency()  # Get the dynamic currency symbol

        total_balance_distribution <- accounts()$accounts_data[[account_name]]  # Data for this account
        child_liabilities_distribution <- accounts()$acc_child[[account_name]]$due  # Liabilities data
        child_balance_distribution <- accounts()$acc_child[[account_name]]$balance

        div(
          h1(paste(account_name, "Detailed Account Information:")),
          p(paste("Your", account_name, "account has:")),
          tags$ul(
            lapply(seq_along(total_balance_distribution), function(i) {
              tags$li(HTML(paste0(
                "<strong>", total_balance_distribution[[i]]$name, ":</strong> ",
                currency_symbol, format(total_balance_distribution[[i]]$y, big.mark = ",", scientific = FALSE)
              )))
            })
          ),
          tags$hr(),
          h1("Distribution of Child Acc Bal. & Liabilities:"),
          p("The total amount due and balance are distributed across the following child accounts:"),
          tags$table(
            class = "styled-table",
            tags$thead(
              tags$tr(
                tags$th("Account Name"),
                tags$th("Acc Balance"),
                tags$th("Amount Due")
              )
            ),
            tags$tbody(
              lapply(seq_along(child_liabilities_distribution), function(i) {
                tags$tr(
                  tags$td(names(child_liabilities_distribution[i])),  # Account Name
                  tags$td(paste0(currency_symbol, format(child_balance_distribution[i], big.mark = ",", scientific = FALSE))),  # Balance
                  tags$td(paste0(currency_symbol, format(child_liabilities_distribution[i], big.mark = ",", scientific = FALSE)))  # Amount Due
                )
              })
            )
          ),
          tags$hr(),
          p(
            "More information is available on the ",
            tags$a(href = "#accounts-page", "accounts page."),  # Link to the accounts page
            " Click here to view."
          ),
          class = "popover-content info-message"  # Apply popover-content class here
        )
      })

    })

    output$alerts_reminders <- renderUI({
      alerts <- list()
      reminders <- list()  # Placeholder for future reminder logic

      for (acc in get_all_accounts(main_account())) {
        account<-get_account_by_uuid(main_account(),acc$account_uuid)

        if (!is.null(account$due_date)) {
          due_date <- as.Date(account$due_date)
          current_date <- Sys.Date()
          days_remaining <- as.numeric(due_date - current_date)

          if (days_remaining <= 5 && days_remaining > 0) {
            alerts <- append(alerts, list(
              div(class = "alert alert-warning", paste("Your", account$name, "account is due in", days_remaining, "days."))
            ))
          }

          if (days_remaining < 0) {
            alerts <- append(alerts, list(
              div(class = "alert alert-danger", paste("Your", account$name, "account's due date has passed!"))
            ))
          }
        }
      }

      # If both alerts and reminders are empty, show "No alerts at the moment"
      if (length(alerts) == 0 && length(reminders) == 0) {
        return(div(class = "no-alerts", "No alerts or reminders at the moment"))
      }

      tagList(alerts, reminders)
    })


  })


  ### Accounts page===========================================================================================================

  output$build_sidebar<-renderUI({
    build_sidebar(main_account())
  })

  output$dummy<-renderUI({
    req(main_account())
    selectedaccountInput("selected_tab",value=main_account()$account_uuid)
  })

  output$nav_content<-renderUI({
    req(main_account())
    generate_nav_content(main_account(), default_content_generator)
  })



  observeEvent(input$selected_tab, {
    req(input$selected_tab)

    # details section
    output[[paste0("account_summary_section", input$selected_tab)]] <- renderUI({
      currency_symbol <- currency()  # Get dynamic currency symbol

      account <- get_account_by_uuid(main_account(),input$selected_tab)

      tags$div(
        class = "details-grid",
        tags$div(
          class = "detail-item",
          tags$i(class = "fas fa-id-badge"),
          tags$strong("Account ID:"),
          tags$span(account$account_uuid)
        ),
        tags$div(
          class = "detail-item",
          tags$i(class = "fas fa-signature"),
          tags$strong("Name:"),
          tags$span(account$name)
        ),
        if (!is.null(account$account_type)) {
        tags$div(
          class = "detail-item",
          tags$i(class = "fas fa-layer-group"),
          tags$strong("Type:"),
          tags$span(account$account_type)
        )
          },
        tags$div(
          class = "detail-item",
          tags$i(class = "fas fa-wallet"),
          tags$strong("Balance:"),
          tags$span(
            class = "balance",
            paste0(currency_symbol, format(round(account$balance, 2), big.mark = ","))
          )
        ),
        tags$div(
          class = "detail-item",
          tags$i(class = "fas fa-piggy-bank"),
          tags$strong("Balance in Children:"),
          tags$span(
            paste0(
              currency_symbol,
              format(round(account$total_balance, 2), big.mark = ",")
            )
          )
        ),
        if (!is.null(account$account_status)) {
          tags$div(
            class = "detail-item",
            tags$i(
              class = if (
                tolower(account$account_status) == "active"
                ) "fas fa-circle-check" else "fas fa-circle-xmark"
              ),
            tags$strong("Status:"),
            tags$span(
              class = if (tolower(account$account_status) == "active") "badge-success" else "badge-danger",
              account$account_status
            )
          )
        },
        if (!is.null(account$allocation)) {
          tags$div(
            class = "detail-item",
            tags$i(class = "fas fa-chart-pie"),
            tags$strong("Allocation:"),
            tags$span(paste(scales::percent(account$allocation, accuracy = 0.01)))
          )
        },
        if (!is.null(account$parent_name)) {
          tags$div(
            class = "detail-item",
            tags$i(class = "fas fa-sitemap"),
            tags$strong("Parent Account:"),
            tags$a(
              href = paste0("/account/",account$parent_uuid),
              account$parent_name,
              class = "parent-link"
            )
          )
        }
      )
    })


    # actions section
    output[[paste0("actions_section",input$selected_tab)]]<-renderUI({
      req(main_account())
      account <- get_account_by_uuid(main_account(),input$selected_tab)
      tagList(
        layout_column_wrap(
          width=1/3,
          heights_equal = "row",
          gap="3px",
          fill = T,
          card(
            class = "action-card",
            bg=NULL,
            card_header(
              span(
                span(
                  icon("arrow-circle-down", class = "action-icon"),
                  class = "action-icon-container",
                  style = "background-color: #F1FFF1; color: #99FF9;" # Green example
                ),
                "Deposit"
              ),
              class = "action-title"
            ),
            card_body(
              layout_column_wrap(
                width=1,
                fill=T,
                heights_equal = "row",
                gap="10px",
                numericInput(
                  paste0("deposit_amount_", account$account_uuid),
                  "Deposit Amount:",
                  value = NA,
                  min = 0
                ),
                textInput(
                  paste0("deposit_transaction_",account$account_uuid),
                  "Transaction number",
                  value="",
                  placeholder="e.g TAP8I3JK2G"
                ),
                textInput(
                  paste0("deposit_transaction_channel_",account$account_uuid),
                  "Transaction Channel",
                  value="",
                  placeholder="e.g ABC BANK"
                ),
                actionButton(
                  paste0("deposit_btn_",account$account_uuid),
                  "Deposit",
                  class = "btn-normal"
                )
              )
            )
          ),
          card(
            class = "action-card",
            card_header(
              span(
                span(
                  icon("arrow-circle-up", class = "action-icon"),
                  class = "action-icon-container",
                  style = "background-color:#FFFCF3; color: #ffeaa7;"
                ),
                "Withdrawal"
              ),
              class = "action-title"
            ),
            card_body(
              layout_column_wrap(
                width=1,
                fill=T,
                heights_equal = "row",
                gap="1px",
                numericInput(
                  paste0("withdraw_amount_", account$account_uuid),
                  sprintf(
                    "Withdraw Amount/Pay %s",
                    ifelse(
                      grepl("main",account$name, ignore.case = TRUE),
                      "dues",
                      account$name
                    )
                  ), value = NA, min = 0),
                actionButton(
                  paste0("withdraw_btn_", account$account_uuid),
                  "Withdraw",
                  class = "btn-normal"
                )
              )
            )
          ),
          card(
            class = "action-card",
            card_header(
              span(
                span(
                  icon("paper-plane", class = "action-icon"),
                  class = "action-icon-container",
                  style = "background-color:#EEF8FC; color: #87CEEB;"
                ),
                "Transfer"
              ),
              class = "action-title"
            ),
            card_body(
              layout_column_wrap(
                width=1,
                fill=T,
                gap="3px",
                heights_equal = "row",
                numericInput(
                  paste0("transfer_amount_", account$account_uuid),
                  "Transfer Amount:", value = 0,
                  min = 0
                ),
                selectInput(
                  paste0("transfer_target_", account$account_uuid),
                  "Transfer To:",
                  choices = sapply(
                    get_all_accounts(main_account()),
                    function(x){setNames(x$account_uuid,x$name)}
                    )
                ),
                actionButton(
                  paste0("transfer_btn_", account$account_uuid),
                  "Transfer",
                  class = "btn-normal"
                )
              )
            )
          )
        ),
        #tags$hr(class = 'tags-hr'),
        card(
          fill=T,
          class = "more-actions-card",
          card_header(
            span(
              span(
                icon("bars", class = "action-icon"),
                class = "action-icon-container",
                style = "background-color: rgba(108, 117, 255, 0.15); color: #6c75ff;" # Blue (Transfer)
              ),
              tags$strong("More Actions")
            ),
            class = "action-title"
          ),
          selectInput(
            paste0("more_actions_", account$account_uuid),
            "more actions",
            choices=c("Add account","Edit account","Close Account")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] =='Add account'",paste0("more_actions_",account$account_uuid)),
            layout_column_wrap(
              width=1/3,
              gap="25px",
              textInput(
                paste0("account_name_add",account$account_uuid),
                "Account name",
                value=""
              ),
              numericInput(
                paste0("allocation_add",account$account_uuid),
                tagList("Allocation",
                        tooltip(
                          bs_icon("info-circle"),
                          paste(
                            "what percentage of",
                            account$name,
                            "do you wish to allocate this account,
                             this should be a value between 0-1.note total allocations for",
                            account$name,"should not exceed 1 "
                          ),
                          placement = "right"
                        )
                ),
                value = round((1-account$total_allocation),2)),
              numericInput(
                paste0("priority_add",account$account_uuid),
                tagList(
                  "Priority",
                  tooltip(
                    bs_icon("info-circle"),
                    paste(
                      "A numeric value representing priority,
                     high values gives this account high priority over
                    other accounts from the same parent"
                    ),
                    placement = "right"
                  )
                ),value=0
              ),
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] =='Edit account'",paste0("more_actions_",account$account_uuid)),

            if(account$account_class== "MainAccount"){

            }
            else if(account$account_class== "ChildAccount"){
              layout_column_wrap(
                width=1/4,
                gap="25px",
                textInput(
                  paste0("account_name_edit_child",account$account_uuid),
                  "Account name",
                  value=account$name
                ),
                numericInput(
                  paste0("allocation_edit_child",account$account_uuid),
                  tagList(
                    "Allocation",
                    tooltip(
                      bs_icon("info-circle"),
                      paste(
                        "what percentage of",
                        account$parent_name,
                        "do you wish to allocate",account$name,",
                                   this should be a value between 0-1.note
                                   total allocations for",
                        account$parent_name,"should not exceed 1 "
                      ),
                      placement = "right")
                  ),
                  value =account$allocation
                ),
                textInput(
                  paste0("account_status_edit_child",account$account_uuid),
                  "Status",
                  value=account$account_status
                ),
                numericInput(
                  paste0("priority_edit_child",account$account_uuid),
                  tagList(
                    "Priority",
                    tooltip(
                      bs_icon("info-circle"),
                      paste(
                        "A numeric value representing priority, high values
                      gives this account high priority over other accounts
                      from the same parent"
                      ),
                      placement = "right"
                    )
                  ),
                  value=account$priority
                )
              )

            }
            else if(account$account_class== "GrandchildAccount"){
              layout_column_wrap(
                width=1/4,
                gap="25px",
                textInput(
                  paste0("account_name_edit_grandchild",account$account_uuid),
                  "Account name",
                  value=account$name
                ),
                numericInput(
                  paste0("allocation_edit_grandchild",account$account_uuid),
                  tagList(
                    "Allocation",
                    tooltip(
                      bs_icon("info-circle"),
                      paste(
                        "what percentage of",
                        account$parent_name,
                        "do you wish to allocate",account$name,",
                                   this should be a value between 0-1.note
                                   total allocations for",
                        account$parent_name,"should not exceed 1 "
                      ),
                      placement = "right"
                    )
                  ),
                  value =account$allocation
                ),
                textInput(
                  paste0("account_status_edit_grandchild",account$account_uuid),
                  "Status",
                  value=account$account_status
                ),
                numericInput(
                  paste0("priority_edit_grandchild",account$account_uuid),
                  tagList(
                    "Priority",
                    tooltip(
                      bs_icon("info-circle"),
                      paste(
                        "A numeric value representing priority, high values gives
                        this account high priority over other accounts from the
                        same parent"
                      ),
                      placement = "right"
                    )
                  ),
                  value=account$priority
                ),
                dateInput(
                  paste0("due_date_edit_grandchild",account$account_uuid),
                  "Due date",
                  value = safe_parse_date(account$due_date),
                  min = safe_parse_date(account$due_date)-lubridate::years(100),
                  max = safe_parse_date(account$due_date)+lubridate::years(1000)
                ),
                numericInput(
                  paste0("fixed_amount_edit_grandchild",account$account_uuid),
                  tagList(
                    "Fixed amount",
                    tooltip(
                      bs_icon("info-circle"),
                      paste("The fixed amount for the account eg monthly bills have a monthly payment amount"),
                      placement = "right"
                    )
                  ),
                  value=account$fixed_amount),
                selectInput(
                  paste0("account_type_edit_grandchild",account$account_uuid),
                  "Account type",
                  choice=c("Bill", "Debt", "Expense", "FixedSaving", "NonFixedSaving"),
                  selected=account$account_type
                ),
                numericInput(
                  paste0("period_edit_grandchild",account$account_uuid),
                  tagList(
                    "Period",
                    tooltip(
                      bs_icon("info-circle"),
                      paste(
                        "how frequent do you pay this account monthly(30),
                      Quarterly(90) etc,this only applies for fixed
                      payments like bills,loans and fixed savings"
                      ),
                      placement = "right"
                    )
                  ),
                  value=account$account_periods
                )
              )
            }
            else {

            },
          ),
          conditionalPanel(
            condition = sprintf(
              "input['%s'] =='Close Account'",
              paste0("more_actions_",account$account_uuid)
            ),
            # some modals if the account has money you need to move it.
          ),
          br(),
          actionButton(
            paste0("save", account$account_uuid),
            "Save",
            class = "btn-danger",width="100px"
          )
        )
      )
    })

    # transaction table
    output[[paste0("transaction_table_", input$selected_tab)]] <- DT::renderDataTable({
      req(main_account())

      trans<-get_account_by_uuid(main_account(), input$selected_tab)$transactions
      if(length(trans)==0){
        trans<-data.frame(
          Type = character(),
          By = character(),
          TransactionID = character(),
          Channel = character(),
          Amount = numeric(),
          Balance = numeric(),
          amount_due = numeric(),
          overall_balance = numeric(),
          Date = as.POSIXct(character()),
          stringsAsFactors = FALSE
        )
      }

      datatable(
        trans%>%
          select(-c(amount_due, overall_balance)) %>%
          mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
          mutate(
            Amount = format(Amount, big.mark = ",", scientific = FALSE),
            Balance = format(Balance, big.mark = ",", scientific = FALSE)
          ),
        options = list(
          scrollY = "auto",  # Allows the table height to adjust dynamically
          scrollX = TRUE, # Optional: ensures horizontal scrolling if needed
          pageLength = 6
        )
      )
    })


    # children
    output[[paste0("children_section_",input$selected_tab)]]<-renderUI({
      req(main_account())
      # Child Accounts Section
      account<-get_account_by_uuid(main_account(), input$selected_tab)
      generate_child_accounts_section(account,currency())
    })

    # Pie chart for Balance vs Amount Due
    output[[paste0("balance_to_debt_", input$selected_tab)]] <- renderHighchart({
      req(main_account())
      currency_symbol <- currency()  # Get dynamic currency symbol
      account<-get_account_by_uuid(main_account(), input$selected_tab)

      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Income vs. Obligations") %>%
        hc_add_series(
          data = list(
            list(
              name = "Balance",
              y = account$total_balance,
              formatted = paste0(currency_symbol, format(round(account$total_balance, 2), big.mark = ","))
            ),
            list(
              name = "Liabilities",
              y = account$total_due,
              formatted = paste0(currency_symbol, format(round(account$total_due, 2), big.mark = ","))
            )
          )
        ) %>%
        hc_plotOptions(
          pie = list(
            dataLabels = list(
              enabled = TRUE,  # Enable data labels
              format = '{point.name}: {point.formatted}',  # Show formatted value with currency
              style = list(
                color = 'contrast',  # Contrast with the background color
                fontSize = '10px'  # Set font size for better visibility
              )
            )
          )
        )
    })


    output[[paste0("transaction_trend_chart_", input$selected_tab)]] <- renderHighchart({
      req(main_account())
      currency_symbol <- currency()  # Get dynamic currency symbol

      trans<-get_account_by_uuid(main_account(), input$selected_tab)$transactions
      if(length(trans)==0){
        trans<-data.frame(
          Type = character(),
          By = character(),
          TransactionID = character(),
          Channel = character(),
          Amount = numeric(),
          Balance = numeric(),
          amount_due = numeric(),
          overall_balance = numeric(),
          Date = as.POSIXct(character()),
          stringsAsFactors = FALSE
        )
      }

      plot_data <- trans%>%
        group_by(Date) %>%
        summarise(
          Total_Balance = max(overall_balance),
          Total_Amount = sum(amount_due)
        ) %>%
        ungroup() %>%
        mutate(Date = as.character(Date))  # Ensure proper date handling

      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Transaction Trends Over Time") %>%
        hc_xAxis(categories = plot_data$Date, title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = paste0("Amount (", currency_symbol, ")"))) %>%
        hc_add_series(
          name = "Total Balance",
          data = plot_data$Total_Balance,
          tooltip = list(
            pointFormat = paste0("<b>Total Balance</b>: ", currency_symbol, "{point.y:,.2f}<br/>")
          )
        ) %>%
        hc_add_series(
          name = "Total Amount Due",
          data = plot_data$Total_Amount,
          tooltip = list(
            pointFormat = paste0("<b>Total Amount Due</b>: ", currency_symbol, "{point.y:,.2f}<br/>")
          )
        ) %>%
        hc_tooltip(shared = TRUE) %>%
        hc_plotOptions(
          line = list(
            dataLabels = list(
              enabled = TRUE,
              format = paste0(currency_symbol, "{y:,.2f}")  # Show formatted currency values
            ),
            enableMouseTracking = TRUE
          )
        )
    })


  })


  # processing actions

  #observe deposit
  observeEvent(input[[paste0("deposit_btn_", input$selected_tab)]], {
    req(
      input[[paste0("deposit_amount_", input$selected_tab)]],
      input[[paste0("deposit_transaction_", input$selected_tab)]],
      input[[paste0("deposit_transaction_", input$selected_tab)]]!="",
      input[[paste0("deposit_transaction_channel_", input$selected_tab)]],
      input[[paste0("deposit_transaction_", input$selected_tab)]]!=""
    )

    if(
      !is_duplicate_tran(
        get_account_by_uuid(account,input$selected_tab),
        input$selected_tab,
        input[[paste0("deposit_transaction_", input$selected_tab)]]
      )
    ){ #check if is a duplicate
      amount <- input[[paste0("deposit_amount_", input$selected_tab)]]
      transaction_number <- input[[paste0("deposit_transaction_", input$selected_tab)]]
      transaction_channel <- input[[paste0("deposit_transaction_channel_", input$selected_tab)]]

      # Execute deposit
      tryCatch({
        res <- post_deposit(input$selected_tab,amount,channel)

        main_account()$find_account_by_uuid(input$selected_tab)$deposit(
          amount = amount,
          transaction_number = ifelse(transaction_number == "", NULL, transaction_number),
          channel = transaction_channel,
          date = Sys.Date()
        )

        main_account(main_account())
        showNotification("Deposit successfully processed!", type = "message")
        # Clear form inputs after successful deposit
        updateTextInput(session, paste0("deposit_transaction_", input$selected_tab), value = "")
        updateNumericInput(session, paste0("deposit_amount_", input$selected_tab), value = NA,min=0)
        updateTextInput(session, paste0("deposit_transaction_channel_", input$selected_tab),value = "")

        # save the updated main account #persistence

        save(main_account, file = "main_account.RData")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    }

  })

  observeEvent(input[[paste0("withdraw_btn_",input$selected_tab)]], {
    req(
      input[[paste0("withdraw_amount_",input$selected_tab)]]
    )

    isolate({
      amount <- input[[paste0("withdraw_amount_", input$selected_tab)]]
      tryCatch({
        main_account()$find_account_by_uuid(input$selected_tab)$withdraw(
          amount = amount,
          transaction_number = NULL,
          channel = "User",
          date = Sys.Date()
        )

        main_account(main_account())

        showNotification("Withdrawal successfully processed!", type = "message")
        # Clear form inputs after successful deposit
        updateNumericInput(session, paste0("withdraw_amount_", input$selected_tab), value = 0,min=0)
        # save the updated main account #persistence

        save(main_account, file = "main_account.RData")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

  })

  # Reports ===========================================================================
  main_account_reports <- reactiveVal(NULL)

  # Reports need more customization so this works like
  # main_account() but will be called more oftenly
  observe({
    req(user()$token)  # wait until token is ready
    req(input$custom_range)
    res <- tryCatch({
      call_api(
        "/get_minimal_tree",
        params=list(
          start_date=input$custom_range[1],
          end_date=input$custom_range[2],
          ts_data = T
          ),
        token = user()$token
        )
    }, error = function(e) {
      showNotification(paste("Failed to fetch account tree:", e$message), type = "error")
      return(NULL)
    })

    if (is.null(res) || httr::status_code(res) != 200) return()

    parsed <- tryCatch(
      jsonlite::fromJSON(rawToChar(res$content)),
      error = function(e) {
        showNotification("Failed to parse account tree response", type = "error")
        return(NULL)
      }
    )

    if (!is.null(parsed$minimal_tree)) {
      main_account_reports(parsed$minimal_tree)
    }
  })
  observe({
    invalidateLater(6*60*1000, session)
    req(user()$token)  # wait until token is ready
    req(input$custom_range)
    res <- tryCatch({
      call_api(
        "/get_minimal_tree",
        params=list(
          start_date=input$custom_range[1],
          end_date=input$custom_range[2],
          ts_data = T
        ),
        token = user()$token
      )
    }, error = function(e) {
      showNotification(paste("Failed to fetch account tree:", e$message), type = "error")
      return(NULL)
    })

    if (is.null(res) || httr::status_code(res) != 200) return()

    parsed <- tryCatch(
      jsonlite::fromJSON(rawToChar(res$content)),
      error = function(e) {
        showNotification("Failed to parse account tree response", type = "error")
        return(NULL)
      }
    )

    if (!is.null(parsed$minimal_tree)) {
      main_account_reports(parsed$minimal_tree)
    }
  })
  # Total income
  output$total_income <- renderText({
    paste0(currency(),format(main_account_reports()$total_income,
                             big.mark = ",",
                             scientific = FALSE))
  })

  output$income_change <- renderUI({
    current_income<-main_account_reports()$total_income
    previous_income<-main_account_reports()$previous_total_income

    if (previous_income == 0) {
      change <- if (current_income > 0) 100 else 0
    } else
      {
      change <- ((current_income - previous_income) / previous_income) * 100
    }

    color_class <- ifelse(change >= 0, "vb-text-green", "vb-text-red")
    arrow_icon <- ifelse(change >= 0, "arrow-up", "arrow-down")
    span(icon(arrow_icon), paste0(abs(change), "% vs last month"), class = color_class)
  })

  # spending
  output$total_spending <- renderText({
    paste0(currency(),format(main_account_reports()$spending,
                             big.mark = ",",
                             scientific = FALSE))
  })
  output$spending_change <- renderUI({
    current_spending<-main_account_reports()$spending
    previous_spending<-main_account_reports()$previous_spending

    if (previous_spending == 0) {
      change <- if (current_spending > 0) 100 else 0
    } else
      {
      change <- ((current_spending - previous_spending) / previous_spending) * 100
    }
    color_class <- ifelse(change >= 0, "vb-text-green", "vb-text-red")
    arrow_icon <- ifelse(change >= 0, "arrow-up", "arrow-down")
    span(icon(arrow_icon), paste0(abs(change), "% vs last month"), class = color_class)
  })

  # income utilization
  output$utilization <- renderText({
    paste0(round(100*main_account_reports()$income_utilization,2),"%")
  })

  output$utilization_change <- renderUI({
    current_utilization <- main_account_reports()$income_utilization
    previous_utilization <- main_account_reports()$previous_income_utilization

    if (previous_utilization == 0) {
      change <- if (current_utilization > 0) 100 else 0
    } else
      {
      change <- ((current_utilization - previous_utilization) / previous_utilization) * 100
    }

    # Determine color and icon based on your rules
    if (previous_utilization > 1.3) {
      # Previous was overutilization
      if (change > 0) {
        color_class <- "vb-text-red"  # Increase, bad (overutilization getting worse)
        arrow_icon <- "arrow-up"
      } else {
        color_class <- "vb-text-green"  # Decrease, good (overutilization improving)
        arrow_icon <- "arrow-down"
      }
    } else if (previous_utilization < 0.7) {
      # Previous was underutilization
      if (change > 0) {
        color_class <- "vb-text-green"  # Increase, good (underutilization improving)
        arrow_icon <- "arrow-up"
      } else {
        color_class <- "vb-text-red"  # Decrease, bad (underutilization getting worse)
        arrow_icon <- "arrow-down"
      }
    } else {
      # Utilization is within the acceptable range (around 1)
      color_class <- ifelse(change > 0, "vb-text-red", "vb-text-green")
      arrow_icon <- ifelse(change > 0, "arrow-up", "arrow-down")
    }

    span(icon(arrow_icon), paste0(abs(round(change, 2)), "% vs last month"), class = color_class)
  })

  # debt ratio

  output$debt_ratio <- renderText({
    debt_amount<-main_account_reports()$walking_amount_due
    balance_amount<-main_account_reports()$walking_balance
    paste0(round(100*debt_amount/((debt_amount+balance_amount)+.Machine$double.eps),2),"%")
  })


  output$debt_change <- renderUI({
    current_debt_amount<-main_account_reports()$walking_amount_due
    current_balance_amount<-main_account_reports()$walking_balance
    current_debt_ratio<-current_debt_amount/((current_debt_amount+current_balance_amount)+.Machine$double.eps)

    previous_debt_amount<-main_account_reports()$previous_walking_amount_due
    previous_balance_amount<-main_account_reports()$previous_walking_balance
    previous_debt_ratio<-current_debt_amount/((current_debt_amount+current_balance_amount)+.Machine$double.eps)

    if (previous_debt_ratio== 0) {
      change <- if (current_debt_ratio > 0) 100 else 0
    } else
      {
      change <- ((current_debt_ratio- previous_debt_ratio) / previous_debt_ratio) * 100
    }

    color_class <- ifelse(change >= 0, "vb-text-red", "vb-text-green")
    arrow_icon <- ifelse(change >= 0, "arrow-up", "arrow-down")
    span(icon(arrow_icon), paste0(abs(change), "% vs last month"), class = color_class)
  })

  income_spending <- reactive({
    req(main_account_reports())
    repos<<-main_account_reports()
    df <- main_account_reports()$time_series%>%select(Date, Income, Spending)

    first_nonzero_row <- which(df$Income != 0 | df$Spending != 0)

    if (length(first_nonzero_row) > 0) {
      df <- df %>% slice(min(first_nonzero_row):n())
    } else {
      df <- df %>% slice(n())
    }
    df
  })


  output$income_vs_spending <- renderHighchart({
    df<-income_spending()
    ddf<<-df
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = as.character(df$Date)) %>%
      hc_yAxis(title = list(text = paste0("Amount (", currency(), ")"))) %>%
      hc_add_series(
        name = "Income",
        data = df$Income,
        tooltip = list(pointFormat = paste0("<b>Income:</b> ", currency(), "{point.y:,.2f}<br>"))
      ) %>%
      hc_add_series(
        name = "Spending",
        data = df$Spending,
        tooltip = list(pointFormat = paste0("<b>Spending:</b> ", currency(), "{point.y:,.2f}<br>"))
      )

  })

  utilization <- reactive({
    req(main_account_reports())
    df <- main_account_reports()$time_series %>% select(Date, Allocated, Spend=Spending)

    first_nonzero_row <- which(df$Allocated != 0 | df$Spend != 0)

    if (length(first_nonzero_row) > 0) {
      df <- df %>% slice(min(first_nonzero_row):n())
    } else {
      df <- df %>% slice(n())
    }

    df
  })

  output$utilized <- renderHighchart({
    df<-utilization()
    df <- df %>% mutate(utilization = round(100 * Spend / Allocated, 2))

    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = as.character(df$Date)) %>%
      hc_yAxis(title = list(text = paste0("Amount (", currency(), ")"))) %>%
      hc_add_series(
        name = "Allocated",
        data = df$Allocated,
        tooltip = list(pointFormat = paste0("<b>Allocated:</b> ", currency(), "{point.y:,.2f}<br>"))
      ) %>%
      hc_add_series(
        name = "Spend",
        data = df$Spend,
        tooltip = list(pointFormat = paste0(
          "<b>Spend:</b> ", currency(), "{point.y:,.2f}<br>",
          "<b>Utilization:</b> {point.utilization}%"
        ))
      ) %>%
      hc_plotOptions(series = list(
        point = list(events = list(
          mouseOver = JS("
        function() {
          var index = this.index;
          var chart = this.series.chart;
          var spend = chart.series[1].data[index].y;
          var allocated = chart.series[0].data[index].y;
          var utilization = allocated > 0 ? (spend / allocated * 100).toFixed(2) : 0;
          chart.series[1].data[index].update({ utilization: utilization }, true, false);
        }
      ")
        ))
      ))

  })
  # spending
  output$spending_drill<-renderHighchart({
    prepare_drilldown_data <- function(account, drilldown_series = list()) {
      children <- account$child_accounts

      # Base case: If no children, return only the account data
      if (length(children) == 0) {
        return(list(
          name = account$name,
          y = account$spending
        ))
      }

      # Create an empty data list for this account
      data_points <- list()

      for (child in children) {
        child_data <- prepare_drilldown_data(child, drilldown_series)

        # Store drilldown data separately
        if (!is.null(child_data$drilldown)) {
          drilldown_series[[length(drilldown_series) + 1]] <- child_data$drilldown
        }

        # Add child data to parent level
        data_points <- append(data_points, list(
          list(
            name = child$name,
            y = child$spending,
            drilldown = if (length(child$child_accounts) > 0) child$name else NULL
          )
        ))
      }

      # Return structured data
      return(list(
        name = account$name,
        y = account$spending,
        drilldown = list(
          id = account$name,
          data = data_points
        ),
        drilldown_series = drilldown_series
      ))
    }

    # Generate account data with properly structured drilldowns
    account_data <- prepare_drilldown_data(main_account_reports())

    # Extract top-level data (main account level)
    top_level_data <- lapply(account_data$drilldown$data, function(x) {
      list(name = x$name, y = x$y, drilldown = x$drilldown)
    })

    # Extract all drilldowns (flattened list)
    drilldown_series <- account_data$drilldown_series

    # Build Highcharts drilldown pie chart
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Spending") %>%
      hc_subtitle(text = "Click on a category to see details") %>%
      hc_series(
        list(
          name = "Accounts",
          colorByPoint = TRUE,
          data = top_level_data
        )
      ) %>%
      hc_drilldown(
        series = drilldown_series
      ) %>%hc_tooltip(
        pointFormat = paste0("<b>{point.name}:</b> ", currency(), "{point.y:,.2f}")
      )

  })

  debt_trend_data <- reactive({
    req(main_account_reports())
    df <- main_account_reports()$time_series %>% select(Date, Overall_Debt)

    if (any(df$overall_debt != 0)) {
      df <- df%>%
        filter(row_number() >= which(overall_debt != 0)[1])
    } else {
      df <- df%>% slice(n()) # Keep only the last row
    }
    df
  })

  output$debt<-renderHighchart({
    debt_data<-debt_trend_data()
    alala<<-debt_data
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = as.character(debt_data$Date)) %>%
      hc_yAxis(title = list(text = "Debt ($)")) %>%
      hc_add_series(name = "Debt", data = debt_data$overall_debt)
  })


}




