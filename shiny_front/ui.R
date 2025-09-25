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
library(finman)

options(knitr.kable.NA = '')

source("accountspagebuilder.R")
source("shiny_helper.R")

ui <- tagList(
  # Overlay at top-level (outside of page_navbar)
  div(
    id = "loading-overlay",
    style = "position: fixed;
             top: 0; left: 0;
             right: 0; bottom: 0;
             display: none; /* start hidden */
             justify-content: center;
             align-items: center;
             background: rgba(255, 255, 255, 0.95);
             z-index: 9999;
             flex-direction: column;",
    tags$div(id = "loading-spinner", role = "status"),
    tags$p("Loading your account...")
  ),



  # Your main navbar app

  page_navbar(
  id="appId",
  title = span(
    class = "navbar-brand",
    tags$img(src = "Image 13.png",
             style = "width: 30px; height: 30px; margin-right: 8px;"),
    "Finman"
  )
  ,
  theme = bs_theme(version = 5,bg="white",fg="black",preset = "bootstrap"),
  #padding="25px",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
    ),
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
            uiOutput("greetings"),
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



  ## Accounts page =======================================================================

  nav_panel(span(icon("wallet"), "Accounts",class = "custom-tab"),

            layout_sidebar(
              style = "background-color:#F6F6F6;",
              sidebar=sidebar(
                width = "310px",
                class = "custom-sidebar",
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
  ## Reports page =======================================================================
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
                dateRangeInput("custom_range", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
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
    tags$div(
      class = "notification-wrapper",
      actionLink(
        "notifications",
        NULL,
        icon("bell", class = "custom-bell"),
        class = "notification-icon"
      ),
      uiOutput("notification_count_badge")
    )
  ),

  nav_menu(
    # Avatar + user info block (header)
    title = tags$div(
      class = "nav-link-title",
      tags$img(
        src = "IMG_20180523_160151.jpg"),
      tags$div(
        tags$span(
          textOutput("fullnames"),
          class = "nav-menu-name"
        ),
        tags$span(
          textOutput("user_email"),
          class = "nav-menu-email"
        )
      )
    ),
    align = "right",

    # Dashboard
    nav_item(
      tags$a(
        icon("tachometer-alt"), "Dashboard",
        href = "#",
        class = "nav-link-item"
      )
    ),

    # Profile
    nav_item(
      tags$a(
        icon("user"), "Profile",
        href = "#",
        class = "nav-link-item"
      )
    ),

    # Settings
    nav_item(
      tags$a(
        icon("cog"), "Settings",
        href = "#",
        class = "nav-link-item"
      )
    ),

    # Billing
    nav_item(
      tags$a(
        icon("credit-card"), "Billing",
        href = "#",
        class = "nav-link-item"
      )
    ),

    # Sign out
    nav_item(
      tags$a(
        icon("sign-out-alt"), "Sign Out",
        href = "#",
        class = "nav-link-item"
      )
    ),
  )

  #,
  # nav_item(
  #   input_dark_mode(id="dark_mode",mode="light"),
  # )
)
)



