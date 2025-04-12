# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(leaflet)       # For map output
library(plotly)        # For interactive line chart
library(fmsb)          # For radar chart
library(dplyr)         # For data wrangling
library(tidyr)
library(readr)

# UI ----
ui <- dashboardPage(
  skin = "blue",
  
  # Header ----
  dashboardHeader(
    title = "Office Market Forecast System",
    titleWidth = 280,
    tags$li(class = "dropdown", actionLink("info", icon("info-circle"))),
    tags$li(class = "dropdown", boastUtils::surveyLink(name = "leasing_forecast_app")),
    tags$li(class = "dropdown", tags$a(id = "home", href = 'https://www.savills.com', icon("home")))
  ),
  
  # Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Project Intro & Methods", tabName = "intro", icon = icon("info-circle")),
      menuItem("Market Overview (Where?)", tabName = "overview", icon = icon("globe")),
      menuItem("Trend Forecast (When?)", tabName = "forecast", icon = icon("chart-line")),
      menuItem("References", tabName = "references", icon = icon("book"))
    ),
    tags$div(class = "sidebar-logo", boastUtils::sidebarFooter())
  ),
  
  # Body ----
  dashboardBody(
    tabItems(
      
      # Page 1 - Home ----
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("🏢 City Leasing Trend Recommendation System"),
                br(),
                h3("Project Overview"),
                p("This dashboard provides an interactive visualization system to support data-driven market entry strategies in commercial real estate."),
                h3("Team Info"),
                p("Michael Yun · Your University · April 2025"),
                br(),
                h3("Navigation"),
                tags$ul(
                  tags$li(actionLink("go_intro", "→ Project Intro & Methods")),
                  tags$li(actionLink("go_overview", "→ Market Overview (Where?)")),
                  tags$li(actionLink("go_forecast", "→ Trend Forecast (When?)"))
                )
              )
      ),
      
      # Page 2 - Intro ----
      tabItem(tabName = "intro",
              fluidPage(
                h2("🔍 Project Introduction & Methodology"),
                br(),
                h3("Research Question"),
                p("Which cities are worth entering now based on leasing competitiveness and projected growth?"),
                br(),
                h3("Data Files Used"),
                tags$ul(
                  tags$li("Leases.csv: Individual leasing transactions"),
                  tags$li("Price and Availability Data.csv: Market-level rent & space"),
                  tags$li("Major Market Occupancy Data.csv: Kastle swipe estimates"),
                  tags$li("Unemployment.csv: BLS quarterly unemployment")
                ),
                br(),
                h3("Analysis Workflow"),
                tags$p("→ Data → Aggregation → Trend Scoring → ML Forecast → Visualization"),
                tags$img(src = "workflow.png", width = "75%") # ensure image in www/
              )
      ),
      
      # Page 3 - Market Overview ----
      tabItem(tabName = "overview",
              fluidPage(
                h2("🌐 Market Overview (Where?)"),
                p("Click on a city in the map to explore trend score, radar chart, and recommendation."),
                fluidRow(
                  column(width = 8,
                         leafletOutput("map", height = 500),
                         br(),
                         h5("Bubble size: total leased SF · Color: trend label (Emerging / Stable / Declining)")
                  ),
                  column(width = 4,
                         h4("Selected City:"),
                         verbatimTextOutput("selected_city"),
                         br(),
                         h4("5-Dimension Radar Chart"),
                         plotOutput("radar_chart", height = 300),
                         br(),
                         h4("Recommendation:"),
                         textOutput("recommendation_text")
                  )
                )
              )
      ),
      
      # Page 4 - Forecast ----
      tabItem(tabName = "forecast",
              fluidPage(
                h2("📈 Leasing Trend Forecast (When?)"),
                p("Forecast next-quarter leasing activity and growth rate for selected cities."),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("forecast_city", "Select a City:", choices = NULL),
                    br(),
                    h4("Growth Prediction for Q1 2025:"),
                    verbatimTextOutput("predicted_growth_text"),
                    h4("Recommendation:"),
                    textOutput("forecast_recommendation")
                  ),
                  mainPanel(
                    plotlyOutput("forecast_plot", height = 500),
                    br(),
                    h5("Solid line: historical leasing volume · Red dashed point: forecasted Q1 2025")
                  )
                )
              )
      ),
      
      # Page 5 - References ----
      tabItem(tabName = "references",
              fluidPage(
                h2("📚 References"),
                br(),
                h3("Data Sources"),
                tags$ul(
                  tags$li("Savills Lease Transaction Data (2018–2024)"),
                  tags$li("Price & Availability Metrics"),
                  tags$li("Kastle Occupancy Reports"),
                  tags$li("U.S. Bureau of Labor Statistics (BLS)")
                ),
                h3("Packages"),
                tags$ul(
                  tags$li("shiny, shinydashboard, shinyWidgets, shinyBS, boastUtils"),
                  tags$li("leaflet, plotly, fmsb, dplyr, tidyr, readr")
                ),
                br(),
                boastUtils::copyrightInfo()
              )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
