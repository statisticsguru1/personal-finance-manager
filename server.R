server <- function(input, output,session) {
  
  observeEvent(input$dark_mode,{
    if(input$dark_mode=="dark"){
      showNotification("Welcome to dark mode!!!")
    }
  })
  
  #logging
  #track_usage(
    #storage_mode = store_json()
    #storage_mode = store_rds(".")
  #)
  
  main_account <- reactiveVal(main_account)
  
  #### Dashboard content ===================================================================================================
  savings_goal <- reactive({
      main_account()$compute_total_balance()/(main_account()$compute_total_balance()+main_account()$compute_total_due())
    })
  
  observe({
    # saving goal value
    
    output$savings_progress <- renderUI({
      div(
        class = "highlight", paste("Your Savings Goal:",scales::percent(savings_goal(),accuracy=0.1)),
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
      HTML(paste(h1(format(main_account()$compute_total_balance(), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })
    
    output$amount_due<-renderUI({
      HTML(paste(h1(format(main_account()$compute_total_due(), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })
    
    output$due_in_n_days<-renderUI({
      HTML(paste(h1(format(main_account()$compute_total_due_within_n_days(30), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
    })
    
    output$number_of_accounts<-renderUI({
      HTML(paste(h1(format(length(main_account()$list_all_accounts()), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
      })
    
    # Income allocation
    output$overall_alloc<-renderHighchart({
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
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Income vs. Obligations") %>%
        hc_size(width = 100, height = 80)%>%
        hc_add_series(
          data = list(
            list(name = "Balance", y = main_account()$compute_total_balance()),
            list(name = "Liabilities", y = main_account()$compute_total_due())
          )
        ) %>%
        hc_plotOptions(
          pie = list(
            dataLabels = list(
              enabled = TRUE,  # Enable data labels
              format = '{point.name}: {point.y}',  # Format: name and value
              style = list(
                color = 'contrast',  # Contrast with the background color
                fontSize = '10px'  # Set font size for better visibility
              )
            )
          )
        )
    })
    
    # Transactions
    
    output$transtable<-renderUI({
      main_account()$transactions%>%
        filter(By=="User")%>%
        select(-c(By,amount_due,overall_balance))%>%
        tail(3)%>%
        mutate(
          Amount=sprintf('<span class="amount-%s">%s</span>',tolower(Type), Amount),
          Type =sprintf('<span class="transaction-type type-%s">%s</span>', 
                        tolower(Type), Amount),
        )%>%knitr::kable(align = 'l', row.names = F,format="html" ,escape = F, table.attr = 'class="custom-table2"')%>%
        HTML()
    })
    
    # popover 
    
    output$info_message <- renderUI({
      # Extracting allocations for each child account
      allocations <- purrr::map(main_account()$child_accounts, ~ .x$allocation)
      
      # Formatting allocations into a list of percentages
      allocation_text <- purrr::map2(names(allocations), allocations, ~paste0("- <strong>", .x, ":</strong> ", scales::percent(.y)))
      
      # Render UI with income allocations
      div(
        h1("Income vs. Obligations:"),
        p(HTML(paste(
          "Your total account balance is:", 
          strong(format(main_account()$compute_total_balance(), big.mark = ",", scientific = FALSE)), 
          ". This is the aggregated account balances from all accounts."
        ))),
        p(HTML(paste(
          "The total amount due is:", 
          strong(format(main_account()$compute_total_due(), big.mark = ",", scientific = FALSE)), 
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
    
    output$tier2_allocation_chart2<-renderHighchart({
      accounts<-list()
      for (child in main_account()$child_accounts){
        accounts[[length(accounts)+1]]<-list(name = child$name, y = child$compute_total_due())
      }
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Distribution of liabilities(Amount due)") %>%
        hc_xAxis(categories = accounts %>% map(~ .x$name) %>% unlist()) %>%
        hc_add_series(
          name = "Amt. Due", 
          data = accounts %>% map(~ .x$y) %>% unlist(),
          colorByPoint = TRUE  # Ensures each bar gets a different color
        ) %>%
        hc_plotOptions(
          bar = list(
            dataLabels = list(
              enabled = TRUE
            )
          )
        ) %>%
        hc_tooltip(
          #pointFormat = '{point.y:.1f} %'  # Display percentage in tooltip
        )
      
      
    })
    
    # Distribution of account balance
    output$tier2_allocation_chart <- renderHighchart({
      accounts<-list()
      for (child in main_account()$child_accounts){
        accounts[[length(accounts)+1]]<-list(name = child$name, y = child$compute_total_balance())
      }
      highchart() %>%
        hc_chart(
          type = "pie",
          events = list(
            render = JS(sprintf("
        function() {
          var chart = this,
              series = chart.series[0];

          var totalBalance = '%s';  // Embed the total balance directly from R
          var customLabel = chart.options.chart.customLabel;// Embed the total balance directly from R

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
      ", main_account()$compute_total_balance()))  # Inject balance directly into the JS code
          )
        ) %>%
        hc_title(
          text = "Distribution of Account Balance"  # Title handled in the JS render function
        ) %>%
        hc_plotOptions(
          pie = list(
            innerSize = '60%',
            dataLabels = list(
              enabled = F,
              format = '{point.name}: {point.percentage:.1f} %'
            )
          )
        ) %>%
        hc_add_series(
          name = "Balance Distribution",
          data = accounts
        )%>%
        hc_tooltip(
          pointFormat = '{point.y:.f}'  # Display percentage in tooltip
        )
      
    })
    
    #popopper message for the card
    output$info_message1 <- renderUI({
      # Extracting account balance distribution (account balance across child accounts)
      balance_distribution <- purrr::map(main_account()$child_accounts, ~ .x$compute_total_balance())
      total_balance <- sum(unlist(balance_distribution))  # Total balance across all child accounts
      balance_distribution_text <- purrr::map2(names(balance_distribution), balance_distribution, 
                                               ~paste0("- <strong>", .x, ":</strong>", format(.y, big.mark = ",", scientific = FALSE), 
                                                       " (", round(.y / total_balance * 100, 2), "%)")
      )
      
      # Extracting liabilities distribution (distribution of liabilities across child accounts)
      liabilities_distribution <- purrr::map(main_account()$child_accounts, ~ .x$compute_total_due())
      total_liabilities <- sum(unlist(liabilities_distribution))  # Total liabilities across all child accounts
      
      # Formatting liabilities distribution into a list of amounts and percentages
      liabilities_distribution_text <- purrr::map2(names(liabilities_distribution), liabilities_distribution, 
                                                   ~paste0("- <strong>", .x, ":</strong>", format(.y, big.mark = ",", scientific = FALSE), 
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
    accounts_data <- list()
    acc_child<-list()
    account_names <- c()
    for (child in main_account()$child_accounts) {
      accounts_data[[length(accounts_data) + 1]] <- list(
        list(name = "Balance", y = child$compute_total_balance()),
        list(name = "Amount Due", y = child$compute_total_due())
      )
      account_names[length(accounts_data)] <- child$name
     
       # grand children
      acc_child[[length(acc_child)+1]]<-list(
        categories=names(child$child_accounts),
        balance =unlist(child$child_accounts%>%purrr::map(~.x$compute_total_balance())),
        due =unlist(child$child_accounts%>%purrr::map(~.x$compute_total_due()))
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
        highchart() %>%
          hc_chart(type = "pie") %>%
          hc_title(text = paste(account_name, "Account Balance")) %>%
          hc_add_series(data = accounts()$accounts_data[[account_name]])
        
        
      })
      
      # Bar chart for distribution across grandchild accounts
      output[[paste0(gsub(" ", "_", account_name), "_distribution_chart")]] <- renderHighchart({
        highchart() %>%
          hc_chart(type = "bar") %>%
          hc_title(text = paste(account_name, "Account Balance Distribution")) %>%
          hc_xAxis(categories = accounts()$acc_child[[account_name]]$categories) %>%
          hc_add_series(name = "Balance", data = as.numeric(accounts()$acc_child[[account_name]]$balance)) %>%
          hc_add_series(name = "Amount Due", data = as.numeric(accounts()$acc_child[[account_name]]$due))
      })
      
      output[[paste0(gsub(" ", "_", account_name), "_poppover")]] <- renderUI({
        
        total_balance_distribution <- accounts()$accounts_data[[account_name]]  # Data for this account
        child_liabilities_distribution <- accounts()$acc_child[[account_name]]$due  # Liabilities data
        child_balance_distribution<- accounts()$acc_child[[account_name]]$balance
        
        div(
          h1(paste(account_name, "Detailed Account Information:")),
          p(paste("Your",account_name, "account has:")),
          tags$ul(
            lapply(seq_along(total_balance_distribution), function(i) {
              tags$li(HTML(paste0(
                "<strong>", total_balance_distribution[[i]]$name, ":</strong>",
                format(balance_distribution[[i]]$y, big.mark = ",", scientific = FALSE)
              )))
            })
          ),
          tags$hr(),
          h1("Distribution of child acc Bal. & Liabilities:"),
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
                  tags$td(format(child_balance_distribution[i], big.mark = ",", scientific = FALSE)),  # Balance
                  tags$td(format(child_liabilities_distribution[i], big.mark = ",", scientific = FALSE))  # Amount Due
                )
              })
            )
          )
          ,
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
    
    output$alerts_reminders <- renderUI({
      alerts <- list()
      reminders <- list()  # Placeholder for future reminder logic
      
      for (acc in main_account()$list_all_accounts()) {
        account <- main_account()$find_account(acc)  # Find the account
        
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
    selectedaccountInput("selected_tab",value=main_account()$uuid) #this is a dump custom binding it actually dispays nothing
  })
  
  output$nav_content<-renderUI({
    generate_nav_content(main_account(), default_content_generator)
  })
  
  
    
  observeEvent(input$selected_tab, {
    req(input$selected_tab)
    
    # details section
    output[[paste0("account_summary_section",input$selected_tab)]]<-renderUI({
      account<-main_account()$find_account_by_uuid(input$selected_tab)
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
            tags$span(class = "balance", paste(format(round(account$balance,2), big.mark = ",")))
          ),
          tags$div(
            class = "detail-item",
            tags$strong("Balance in Children:"),
            tags$span(paste(format(round(account$compute_total_balance(),2), big.mark = ",")))
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
    })
    
    # actions section
    output[[paste0("actions_section",input$selected_tab)]]<-renderUI({
      account<-main_account()$find_account_by_uuid(input$selected_tab)
      tagList(
        layout_column_wrap(
          width=1/3,
          heights_equal = "row",
          gap="3px",
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
                numericInput(paste0("withdraw_amount_", account$uuid),sprintf("Withdraw Amount/Pay %s",ifelse(grepl("main",account$name, ignore.case = TRUE),"dues",account$name)), value = NA, min = 0),
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
                selectInput(paste0("transfer_target_", account$uuid), "Transfer To:", choices = main_account()$list_all_accounts()),
                actionButton(paste0("transfer_btn_", account$uuid), "Transfer", class = "btn-normal")
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
                                   paste("what percentage of",main_account()$find_account_by_uuid(account$uuid)$name,
                                         "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                                         main_account()$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
                                   placement = "right")),value = round((1-account$total_allocation),2)),
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
                                     paste("what percentage of",main_account()$find_account_by_uuid(account$uuid)$name,
                                           "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                                           main_account()$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
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
                                     paste("what percentage of",main_account()$find_account_by_uuid(account$uuid)$name,
                                           "do you wish to allocate this account, 
                             this should be a value between 0-1.note total allocations for",
                                           main_account()$find_account_by_uuid(account$uuid)$name,"should not exceed 1 "),
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
      )
   })
    
    # transaction table
    output[[paste0("transaction_table_", input$selected_tab)]] <- DT::renderDataTable({
      datatable(
        main_account()$find_account_by_uuid(input$selected_tab)$transactions %>%
          select(-c(amount_due, overall_balance)) %>%
          mutate(across(where(is.numeric), round, 2)),
        options = list(
          scrollY = "auto",  # Allows the table height to adjust dynamically
          scrollX = TRUE, # Optional: ensures horizontal scrolling if needed
          pageLength = 6
        )
      )
    })
    
    
    # children
     output[[paste0("children_section_",input$selected_tab)]]<-renderUI({
       # Child Accounts Section
       account<-main_account()$find_account_by_uuid(input$selected_tab)
       generate_child_accounts_section(account)
    })
    

    
    # Pie chart for Balance vs Amount Due
    output[[paste0("balance_to_debt_", input$selected_tab)]] <- renderHighchart({
      highchart() %>%
        hc_chart(type = "pie") %>%
        hc_title(text = "Income vs. Obligations") %>%
        hc_add_series(
          data = list(
            list(name = "Balance", y = main_account()$find_account_by_uuid(input$selected_tab)$compute_total_balance()),
            list(name = "Liabilities", y = main_account()$find_account_by_uuid(input$selected_tab)$compute_total_due())
          )
        ) %>%
        hc_plotOptions(
          pie = list(
            dataLabels = list(
              enabled = TRUE,  # Enable data labels
              format = '{point.name}: {point.y:.2f}',  # Format: name and value
              style = list(
                color = 'contrast',  # Contrast with the background color
                fontSize = '10px'  # Set font size for better visibility
              )
            )
          )
        )
    })
    
    output[[paste0("transaction_trend_chart_",input$selected_tab)]] <- renderHighchart({
      plot_data <- main_account()$find_account_by_uuid(input$selected_tab)$transactions %>%
        group_by(Date) %>%
        summarise(
          Total_Balance = max(overall_balance), 
          Total_Amount = sum(amount_due) 
        )
      
      # Create line chart
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Transaction Trends Over Time") %>%
        hc_xAxis(categories = as.character(plot_data$Date), title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Amount/Balances")) %>%
        hc_add_series(name = "Total Balance", data = plot_data$Total_Balance) %>%
        hc_add_series(name = "Total Amount due", data = plot_data$Total_Amount) %>%
        hc_tooltip(
          shared = TRUE,
          pointFormat = '<b>{series.name}</b>: {point.y:.2f}<br/>'
        ) %>%
        hc_plotOptions(
          line = list(
            dataLabels = list(
              enabled = TRUE,
              format = '{y:.2f}'  # Round the data label values to 2 decimal places
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
    
    if(!main_account()$find_account_by_uuid(input$selected_tab)$is_duplicate_transaction(input[[paste0("deposit_transaction_", input$selected_tab)]])){ #check if is a duplicate
      amount <- input[[paste0("deposit_amount_", input$selected_tab)]]
      transaction_number <- input[[paste0("deposit_transaction_", input$selected_tab)]]
      transaction_channel <- input[[paste0("deposit_transaction_channel_", input$selected_tab)]]   
      # Execute deposit
      tryCatch({
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
  
  
  # Reports 
  
  observe({
    if(!is.null(main_account())){
    updatePickerInput(session = session, inputId = "selaccount",
                      choices = main_account()$list_all_accounts())
    }
  })
  
  output$total_income <- renderUI({
    ss<<-input$date
    HTML(paste(h1(format(main_account()$total_income(as.POSIXct(input$date)),
                         big.mark = ",",
                         scientific = FALSE), class = "card-body-value")))
    })
  output$total_spending <- renderUI({
    main_account()$spending()
    })
  output$utilization <- renderUI({ paste0(round(sum(financial_data$Amount[financial_data$Type == "Spending"]) / sum(financial_data$Amount[financial_data$Type == "Income"]) * 100, 1), "%") })
  output$debt_ratio <- renderUI({ paste0(round(tail(debt_data$Debt, 1) / tail(savings_data$Savings, 1) * 100, 1), "%") })
  

}

