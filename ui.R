library(shiny)
library(bslib)
library(highcharter)
library(bsicons)
library(knitr)
library(DT)
library(tidyverse)
library(shinylogs)
library(shinyWidgets)
library(shinySearchbar)
library(colorspace)
library(future)
library(furrr)
library(promises)

options(knitr.kable.NA = '')

source("accountspagebuilder.R")
source("testfiles.R")

if (file.exists("main_account.RData")) {
  load("main_account.RData")
} else {
  main_account <- MainAccount$new("Main")
}


#load("main_account.RData")

Name<-"Festus Nzuma"
myname<- "Festus"    #first name from log in info

ui <- page_navbar(
  #navbar_options(underline = F),
  id="appId",
  title = span(
    tags$img(src = "Image 13.png", 
             style = "width: 30px; height: 30px; margin-right: 8px;"), 
    span("Finman",class = "navbar-brand")
  ),
  theme = bs_theme(version = 5,bg="white",fg="black",preset = "bootstrap"),
  #padding="25px",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = 'jsfuns.js')
  ),
  gap="4px",
  bg="white",

## Dashboard page =======================================================================
  nav_panel(
    span(icon("home"), "Dashboard",class = "custom-tab"),
    style = "padding: 25px;",
    div(
      style = "display: flex; flex-direction: column; row-gap: 3px;",
      
      # Top Section: Hero Area
      layout_column_wrap(
        width=1,
        gap="0px",
        class="hero-section",
        div(
          class = "profile",
          #img(src = "https://avatar.iran.liara.run/public/5", alt = "User Avatar"),
          img(src = "IMG_20180523_160151.jpg", alt = "User Avatar"),
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
      ,
      # Total Financial Overview
      
      layout_column_wrap(
        width = 1/4,
        fill = TRUE,
        gap = "10px",  # Adjust spacing
        
        # First Box - Balance Amount
        card(
          fill = TRUE,
          class = "value-box",
          card_body(
            div(class = "value-box-header d-flex justify-content-between align-items-center",
                span("Balance amount", class = "value-box-title"),
                icon("coins", class = "value-box-icon")
            ),
            div(class = "value-box-content", uiOutput("overall_bal")),
            p("Sum from all accounts", class = "value-box-footer")
          )
        ),
        
        # Second Box - Amount Due
        card(
          fill = TRUE,
          class = "value-box",
          card_body(
            div(class = "value-box-header d-flex justify-content-between align-items-center",
                span("Amount Due", class = "value-box-title"),
                icon("clock", class = "value-box-icon")
            ),
            div(class = "value-box-content", uiOutput("amount_due")),
            p("Pending payments", class = "value-box-footer")
          )
        ),
        
        # Third Box - Due in 30 days
        card(
          fill = TRUE,
          class = "value-box",
          card_body(
            div(class = "value-box-header d-flex justify-content-between align-items-center",
                span("Due in 30 days", class = "value-box-title"),
                icon("money-bill", class = "value-box-icon")
            ),
            div(class = "value-box-content", uiOutput("due_in_n_days")),
            p("Short-term obligations", class = "value-box-footer")
          )
        ),
        
        # Fourth Box - Number of Accounts
        card(
          fill = TRUE,
          class = "value-box",
          card_body(
            div(class = "value-box-header d-flex justify-content-between align-items-center",
                span("Number of accounts", class = "value-box-title"),
                icon("briefcase", class = "value-box-icon")
            ),
            div(class = "value-box-content", uiOutput("number_of_accounts")),
            p("Number of accounts", class = "value-box-footer")
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
                  width = 1,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "1px",
                
                layout_column_wrap(
                  width = 1/2,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "2px",
                  highchartOutput("overall_pie_chart"),   
                  highchartOutput("overall_alloc")
                ),
                tags$hr(class="tags-hr"),
                p("Recent Transactions",class="custom-card-header112"),
                uiOutput("transtable")
              )
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
                  width = 1,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "1px",
                layout_column_wrap(
                  width = 1/2,
                  heights_equal = "row",
                  fill = TRUE,
                  gap = "2px",
                  highchartOutput("tier2_allocation_chart"),  
                  highchartOutput("tier2_allocation_chart2")),
                  tags$hr(class="tags-hr"),
                card(
                  fill = TRUE,
                  class = "alerts-card",
                  card_header("Alerts & Reminders", class = "alerts-header"),
                  card_body(
                    div(class = "alerts-body", uiOutput("alerts_reminders"))
                  )
                )
                
              )
            )
            )
          ),
      
      # Tier-2 Account Breakdown (Needs, Goals, Debt)
      #uiOutput("dynamic_cards"),
    )
  ),
  
## Accounts page=======================================================================

  nav_panel(span(icon("wallet"), "Accounts",class = "custom-tab"),
  
  layout_sidebar(
    style = "background-color:#F6F6F6;",
    sidebar=sidebar(
      width = "310px",
      title = tags$div(
        class = "custom-sidebar-title",
        icon("briefcase"), 
        "Accounts"
      ),
      uiOutput("build_sidebar"),
      uiOutput("dummy")
      
    ),
    
    # navbar content 
    uiOutput("nav_content")
  )
  ),

## Reports page=======================================================================
  nav_panel(span(icon("chart-column"), "Reports",class = "custom-tab"),
            layout_sidebar(
              sidebar=sidebar(
                title = tags$div(
                  class = "custom-sidebar-title",
                  icon("filter"), 
                  "Filters"
                ),
                width="300px",
                class = "custom-sidebar",
              dateRangeInput("date", "Select Date Range", start = Sys.Date() - 366, end = Sys.Date()),
              downloadButton("download_report", "Download Report")
            ),
            
            layout_column_wrap(
              width = 1,
              fill = TRUE,
              heights_equal = "row",
            layout_column_wrap(
              width = 1/4,
              fill = TRUE,
              heights_equal = "row",
              # Value Boxes
              card(
                fill=T,
                class = "vb-value-box",
                div(class = "vb-value-box-header",
                    span(class = "vb-icon-container vb-icon-blue",
                         icon("wallet")),
                    "Total Income"),
                div(textOutput("total_income"), class = "vb-value-box-value"),
                div(uiOutput("income_change"), class = "vb-value-box-change")
              ),
              
              card(fill=T,
                class = "vb-value-box",
                div(class = "vb-value-box-header",
                    span(class = "vb-icon-container vb-icon-red",
                         icon("credit-card")),
                    "Total Spending"),
                div(textOutput("total_spending"), class = "vb-value-box-value"),
                div(uiOutput("spending_change"), class = "vb-value-box-change")
              ),
              
              card(
                fill=T,
                class = "vb-value-box",
                div(class = "vb-value-box-header",
                    span(class = "vb-icon-container vb-icon-green",
                         icon("chart-simple")),
                    "% Utilization"),
                div(textOutput("utilization"), class = "vb-value-box-value"),
                div(uiOutput("utilization_change"), class = "vb-value-box-change")
              ),
              
              card(
                fill=T,
                class = "vb-value-box",
                div(class = "vb-value-box-header",
                    span(class = "vb-icon-container vb-icon-purple",
                         icon("chart-line")),
                    "Debt Ratio"),
                div(textOutput("debt_ratio"), class = "vb-value-box-value"),
                div(uiOutput("debt_change"), class = "vb-value-box-change")
              )
            ),
            layout_column_wrap(
              fill=TRUE,
              width=1/2,
              heights_equal = "row",
              card(
                fill=T,
                card_header("Income vs. Spending Trend"),
                highchartOutput("income_vs_spending")
              ),
              card(
                fill=T,
                card_header("Money Utilization"),
                highchartOutput("utilized")
              )),
            
            layout_column_wrap(
              fill=TRUE,
              width=1/2,
              heights_equal = "row",
              card(
                fill=T,
                card_header("Distribution of spending"),
                highchartOutput("spending_drill")
              ),
              card(
                fill=T,
                card_header("Trend in Debt reduction"),
                highchartOutput("debt")
              )
            )
            )
            )
            ),
  nav_panel(span(icon("user"), "Profile",class = "custom-tab"), h1("profile Content")),
  nav_panel(span(icon("users"), "About us",class = "custom-tab"), h1("about Content")),
  nav_spacer(),
  #nav_item(
  #  shinySearchbar::searchbar("buscador", contextId = "appId",cycler=F, width = "200px",counter = F,placeholder = "search")
  #),
  nav_item(
    actionLink("notifications", "", icon("bell",class="custom-bell"),class = "notification-icon")
  ),
  nav_menu(
    title = span(tags$img(src = "IMG_20180523_160151.jpg", 
                          style = "width: 24px; height: 24px; border-radius: 50%; margin-right: 8px;"),
                 span(paste(Name), class = "nav-menu-title"
                 )
    ),
    align = "right",
    nav_item(
      tags$a(
        icon("user"), "Profile",
        href = "https://github.com/rstudio/shiny",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        icon("cog"), "Settings",
        href = "https://github.com/rstudio/shiny",
        target = "_blank"
      )
    ),
    nav_item(
      tags$a(
        icon("sign-out"), "Logout",
        href = "https://posit.co",
        target = "_blank"
      )
    )
  ),
  nav_item(
    input_dark_mode(id="dark_mode",mode="light"),
  )
)

