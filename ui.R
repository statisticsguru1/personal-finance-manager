library(shiny)
library(bslib)
library(highcharter)
library(bsicons)
library(knitr)
library(DT)
library(tidyverse)
library(shinylogs)
library(shinyWidgets)

options(knitr.kable.NA = '')

source("accountspagebuilder.R")
source("testfiles.R")

if (file.exists("main_account.RData")) {
 # load("main_account.RData")
} else {
 # main_account <- MainAccount$new("Main")
}


#load("main_account.RData")

myname<- "Festus"    #first name from log in info

ui <- page_navbar(
  title = "Finman",
  theme = bs_theme(version = 5,"bslib_spacer" = "0rem",preset="bootstrap"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = 'jsfuns.js')
  ),
  bg="#87CEEB",
  gap="4px",
  
  # Dashboard Tab
  
  nav_panel(
    "Dashboard",
    div(
      style = "display: flex; flex-direction: column; row-gap: 3px;",
      
      # Top Section: Hero Area
      card(
        class = "hero-section",
        card_body(
          div(
            class = "profile",
            img(src = "https://avatar.iran.liara.run/public/5", alt = "User Avatar"),
            div(
              generate_greeting(myname),
              div("Plan Your Finances, Secure Your Future.", class = "tagline")
            )
          ),
          uiOutput("savings_progress"),
          div(
            class = "btn-toolbar quick-actions",
            actionButton("add_account", "Add Account", class = "btn-success"),
            actionButton("transfer_money", "Transfer Money", class = "btn-primary"),
            actionButton("view_reports", "View Reports", class = "btn-warning")
          )
        )
      )
      ,
      # Total Financial Overview
      layout_column_wrap(
        width=1/4,
        fill = T,
        gap = "3px",
        card(
          fill=T,
          full_screen = T,
          card_header("Balance amount", class = 'custom-card-header'),
          card_body(
            div(
              class = 'card-body-div', 
              div(
                class='card-body-value-div',
                uiOutput("overall_bal")
              ),
              div(
                class = 'icon-div',
                icon("coins",class= "icon-val1")
              )
            ),
            tags$hr(class = "tags-hr"),
            p("sum from all accounts", class = "text-muted"),
            class = "card-content"
          )
        ),
        card(
          full_screen = T,
          card_header("Amount Due", class = 'custom-card-header'),
          card_body(
            div(
              class = 'card-body-div',
              div(
                class='card-body-value-div',
                uiOutput("amount_due")
                ),
              div(
                class = 'icon-div',  # Fixed size for the icon area
                icon("clock", class= "icon-val1")  # Relative font size for responsiveness
              )
            ),
            tags$hr(class = "tags-hr"),
            p("Pending payments", class = "text-muted"),
            class = "card-content"
          )
        ),
        card(
          full_screen = T,
          card_header("Due in 30 days", class = 'custom-card-header'),
          card_body(
            div(
              class = 'card-body-div',
              div(
                class='card-body-value-div',
                uiOutput("due_in_n_days")
              ),
              div(
                class = 'icon-div',  # Fixed size for the icon area
                icon("money-bill", class= "icon-val1")  # Relative font size for responsiveness
              )
            ),
            tags$hr(class = "tags-hr"),
            p("Short-term obligations", class = "text-muted"),
            class = "card-content"
          )
        ),
        card(
          full_screen = T,
          card_header("Number of accounts", class = 'custom-card-header'),
          card_body(
            div(
              class = 'card-body-div',
              div(
                class='card-body-value-div',
                uiOutput("number_of_accounts")
              ),
              div(
                class = 'icon-div',  # Fixed size for the icon area
                icon("briefcase", class= "icon-val1")  # Relative font size for responsiveness
              )
            ),
            tags$hr(class = "tags-hr"),  # Adjust the spacing around <hr>
            class = "card-content"
          )
        )
        
      ),
      
      # Distribution of amnt due and balance
      layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            fill = TRUE,
            gap = "3px",
            card(
              fill=T,
              full_screen = T,
              card_header(div(),
                          popover(
                            bs_icon("info-circle"),
                            uiOutput("info_message"),
                            title = "Detailed Information",
                            placement = "top"
                            ),
                          class = "custom-card-header1"),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "3px",
                  highchartOutput("overall_pie_chart"),   
                  highchartOutput("overall_alloc")
                ),
                
                tags$hr(class="tags-hr"),
                p("Recent Transactions"),
                DT::dataTableOutput("transtable"),
                actionLink("view_all", "View All Transactions")
              )
            ),
            card(
              card_header(
                div(),
                popover(
                  bs_icon("info-circle"),
                  uiOutput("info_message1"),
                  title = "Detailed Information",
                  placement = "top"
                ),
                class = "custom-card-header1"
              ),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "3px",
                  highchartOutput("tier2_allocation_chart"),  
                  highchartOutput("tier2_allocation_chart2")),
                  tags$hr(class="tags-hr"),
                card(
                  fill=T,
                  card_header("Alerts & Reminders",
                              class="custom-card-header1"),
                  card_body(uiOutput("alerts_reminders"))
                )
              )
            )
          ),
      
      # Tier-2 Account Breakdown (Needs, Goals, Debt)
      #uiOutput("dynamic_cards"),
    )
  ),
  
  # Accounts navbar
  nav_panel("Accounts", 
   # sidebar         
  layout_sidebar(
    sidebar=sidebar(
      title = tags$div(
        class = "custom-sidebar-title",
        icon("briefcase"), 
        "Accounts"
      ),
      width = "300px",
      tags$div(
        class = "custom-sidebar",
        #build_sidebar(main_account) 
        uiOutput("build_sidebar")
      )
    ),
    
    # navbar content 
    uiOutput("nav_content"),
    uiOutput("dummy")
  )
  ),

  nav_panel("Reports",
            layout_sidebar(
              sidebar=sidebar(
                title = tags$div(
                  class = "custom-sidebar-title",
                  icon("filter"), 
                  "Filters"
                ),
                width="300px",
                class = "custom-sidebar",
              pickerInput("selaccount", "Select Account", 
                          choices = list("Main","Needs", "Goals", "Savings"),
                          multiple = FALSE, options = list(`live-search` = TRUE)),
              dateRangeInput("date", "Select Date Range", start = Sys.Date() - 366, end = Sys.Date()),
              selectInput("transaction_type", "Transaction Type", 
                          choices = c("All", "Income", "Spending", "Transfers")),
              downloadButton("download_report", "Download Report")
            ),
            
            layout_column_wrap(
              width=1/4,
              gap = "3px",
              #fill = T,
              card(
                fill=T,
                full_screen = T,
                card_header("Total Income", class = 'custom-card-header'),
                card_body(
                  div(
                    class = 'card-body-div', 
                    div(
                      class='card-body-value-div',
                      uiOutput("total_income")
                    ),
                    div(
                      class = 'icon-div',
                      icon("coins",class= "icon-val11")
                    )
                  ),
                  tags$hr(class = "tags-hr"),
                  p("sum from all accounts", class = "text-muted"),
                  class = "card-content"
                )
              ),
              card(
                fill=T,
                full_screen = T,
                card_header("Total spending", class = 'custom-card-header'),
                card_body(
                  div(
                    class = 'card-body-div', 
                    div(
                      class='card-body-value-div',
                      uiOutput("total_spending")
                    ),
                    div(
                      class = 'icon-div',
                      icon("credit-card",class= "icon-val1")
                    )
                  ),
                  tags$hr(class = "tags-hr"),
                  p("sum from all accounts", class = "text-muted"),
                  class = "card-content"
                )
              ),
              
              card(
                fill=T,
                full_screen = T,
                card_header("Money Utilization", class = 'custom-card-header'),
                card_body(
                  div(
                    class = 'card-body-div', 
                    div(
                      class='card-body-value-div',
                      #uiOutput("total_spending")
                    ),
                    div(
                      class = 'icon-div',
                      icon("credit-card",class= "icon-val1")
                    )
                  ),
                  tags$hr(class = "tags-hr"),
                  p("sum from all accounts", class = "text-muted"),
                  class = "card-content"
                )
              ),
              card(
                fill=T,
                full_screen = T,
                card_header("Assets-to-Liabilities", class = 'custom-card-header'),
                card_body(
                  div(
                    class = 'card-body-div', 
                    div(
                      class='card-body-value-div',
                      uiOutput("total_spending")
                    ),
                    div(
                      class = 'icon-div',
                      icon("credit-card",class= "icon-val1")
                    )
                  ),
                  tags$hr(class = "tags-hr"),
                  p("sum from all accounts", class = "text-muted"),
                  class = "card-content"
                )
              )
            ),
            card()
            )
            ),
  nav_spacer(),
  nav_item(
    input_dark_mode(id="dark_mode",mode="light"),
  )
)

