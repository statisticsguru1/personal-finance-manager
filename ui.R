library(shiny)
library(bslib)
library(highcharter)
library(bsicons)
library(knitr)
library(DT)
library(tidyverse)

options(knitr.kable.NA = '')

source("accountspagebuilder.R")
#source("testfiles.R")
load("main_account.RData")

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
                #HTML(paste(h1(format(main_account$compute_total_balance(), big.mark = ",", scientific = FALSE), class = "card-body-value")))  # Relative font size (5% of viewport width)
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
        build_sidebar(main_account) 
      )
    ),
    
    # navbar content 
    generate_nav_content(main_account, default_content_generator),
    #uiOutput("accounts_page"),
    selectedaccountInput("selected_tab",value=main_account$uuid) #this is a dump custom binding it actually dispays nothing
  )
  ),

  nav_panel("Reports"),
  nav_spacer(),
  nav_item(
    input_dark_mode(id="dark_mode",mode="light"),
  )
)

