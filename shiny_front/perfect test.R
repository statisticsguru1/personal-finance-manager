library(shiny)
library(httr)
library(jsonlite)
library(jose)

# ---- helpers ----
host_url <- "http://127.0.0.1:8000/"
user_id <- "86e99ab0-adae-46dc-84b1-851717ef6d44"
token <- jwt_encode_hmac(
  jwt_claim(user_id = user_id, role = "user"),
  secret = secret_key
)

source("shiny_helper.R")

# ---- app ----
ui <- fluidPage(
  titlePanel("Deposit + Balance Check"),
  fluidRow(
    column(6,
           textInput("uuid", "Account UUID:", ""),
           numericInput("amount", "Deposit Amount:", 100, min = 1),
           textInput("channel", "Channel:", "bank"),
           actionButton("deposit", "Make Deposit"),
           hr(),
           h3("Check Balance"),
           textInput("check_uuid", "Account UUID to check:", ""),
           actionButton("check_balance", "Check Balance"),
           verbatimTextOutput("balance")
    )
  )
)

server <- function(input, output, session) {
  # keep whole account tree in memory
  main_account <- reactiveVal(NULL)

  # initial fetch
  observe({
    main_account(get_main_account_from_api())
  })

  # background refresh every 2 min
  observe({
    invalidateLater(120000, session)
    main_account(get_main_account_from_api())
  })

  # deposit event
  observeEvent(input$deposit, {
    req(input$uuid, input$amount, input$channel)
    res <- post_deposit(input$uuid, input$amount, input$channel)
    if (!is.null(res$success) && res$success) {
      showNotification("Deposit successful", type = "message")
      main_account(get_main_account_from_api()) # refresh whole tree
    } else {
      showNotification("Deposit failed", type = "error")
    }
  })

  # balance check (no API call; just look into cached tree)
  observeEvent(input$check_balance, {
    req(main_account())

    acct <- get_account_by_uuid(main_account(), input$check_uuid)

    if (!is.null(acct)) {
      output$balance <- renderPrint({
        list(
          uuid          = acct$account_uuid,
          name          = acct$name,
          balance       = acct$balance,
          total_balance = acct$total_balance,
          total_due     = acct$total_due
        )
      })
    } else {
      output$balance <- renderPrint({"UUID not found in current account tree"})
    }
  })

}

shinyApp(ui, server)
