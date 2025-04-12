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
library(DT)
# UI ----
ui <- dashboardPage(
  skin = "blue",
  
  # Header ----
  dashboardHeader(
    title = "Penn State F4 Team",
    titleWidth = 280
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
                h3("Team Members"),
                tags$ul(
                  tags$li("Michael Yun"),
                  tags$li("Runyi Zhang"),
                  tags$li("Jingchun Zhang"),
                  tags$li("Zhaoyu Hou")
                ),
                br(),
                h3("Navigation"),
                tags$p("This dashboard includes four main sections:"),
                tags$ul(
                  tags$li(tags$b("Project Introduction & Methods") , " — Learn about our data sources, research question, and analytical workflow."),
                  tags$li(tags$b("Market Overview (Where?)") , " — Explore city-level leasing competitiveness using maps and radar charts."),
                  tags$li(tags$b("Trend Forecast (When?)") , " — View next-quarter leasing predictions and strategic recommendations."),
                  tags$li(tags$b("References") , " — See all supporting data sources and R packages used.")
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
                h3("Workflow Diagram"),
                grVizOutput("workflow_diagram", height = "600px")
              )
      ),
      
      # Page 3 - Overview ----
      tabItem(tabName = "overview",
              fluidPage(
                h2("🌍 Market Overview (Where?)"),
                p("Explore market competitiveness across major U.S. cities. Based on trend scores calculated per quarter."),
                
                # Quarter selection
                fluidRow(
                  column(4,
                         selectInput("selected_year", "Select Year:", choices = c(2019:2024), selected = 2024)
                  ),
                  column(4,
                         selectInput("selected_quarter", "Select Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q4")
                  )
                ),
                
                br(),
                
                # Map + Radar Chart
                fluidRow(
                  column(width = 7,
                         h3("📍 National Leasing Heatmap"),
                         leafletOutput("heatmap", height = 500),
                         helpText("Bubble size = total leased SF · Color = trend label (Emerging = green, Stable = orange, Declining = red)")
                  ),
                  column(width = 5,
                         h3("📊 Radar Chart of Top 5 Cities"),
                         plotOutput("top5_radar", height = 500)
                  )
                ),
                
                br(), hr(),
                
                # City ranking table
                fluidRow(
                  column(12,
                         h3("🏙️ Top 5 Cities by Trend Score"),
                         DT::dataTableOutput("top5_table")
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

server <- function(input, output, session) {
  
  # === Workflow Diagram (Page 2) ===
  output$workflow_diagram <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz("
      digraph flowchart {
        graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.5]

        node [fontname = Helvetica, fontsize = 40, shape = box, style = filled, fillcolor = dodgerblue, fontcolor = white]
        A [label = 'Stage 1\\nFeature Construction']
        B [label = 'Stage 2\\nTrend Scoring']
        C [label = 'Stage 3\\nML Forecasting']
        D [label = 'Stage 4\\nVisualization']

        node [fontname = Helvetica, fontsize = 32, shape = box, style = filled, fillcolor = lightgoldenrod1, fontcolor = black]
        A1 [label = '• leasedSF ≥ 10,000\\n• Merge Rent, Occupancy, Unemployment']
        B1 [label = '• Z-score + Equal weights\\n• Label cities by trend']
        C1 [label = '• Lag features\\n• XGBoost → Q1 2025 prediction']
        D1 [label = '• Map + Radar + Plotly\\n• ENTER / WAIT / EXIT']

        node [shape = ellipse, fillcolor = crimson, fontcolor = white, fontsize = 36]
        START [label = 'START']
        END [label = 'END']

        START -> A -> B -> C -> D -> END
        A -> A1
        B -> B1
        C -> C1
        D -> D1
      }
    ")
  })
  
  
}




# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)