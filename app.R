# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(leaflet)
library(plotly)
library(fmsb)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(DiagrammeR)
library(ggplot2)

# UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Penn State F4 Team", titleWidth = 280),
  
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
                tags$ul(
                  tags$li(tags$b("Project Introduction & Methods")),
                  tags$li(tags$b("Market Overview (Where?)")),
                  tags$li(tags$b("Trend Forecast (When?)")),
                  tags$li(tags$b("References"))
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
                  tags$li("Leases.csv"),
                  tags$li("Price and Availability Data.csv"),
                  tags$li("Major Market Occupancy Data.csv"),
                  tags$li("Unemployment.csv")
                ),
                br(),
                h3("Feature Construction Workflow"),
                grVizOutput("workflow_feature", height = "auto")
              )
      ),
      
      # Page 3 - Market Overview ----
      tabItem(tabName = "overview",
              fluidPage(
                h2("🌍 Market Overview (2024 Q4 Only)"),
                p("Explore market competitiveness across major U.S. cities based on 2024 Q4 trend scores."),
                fluidRow(
                  column(6, h3("📍 Market Trend Heatmap"), leafletOutput("heatmap", height = 500)),
                  column(6, h3("📊 Trend Score Distribution"), plotOutput("score_hist", height = 500))
                ),
                br(), hr(),
                fluidRow(
                  column(12, h3("🏙️ Top 5 Markets by Trend Score"), DTOutput("top5_table"))
                ),
                br(),
                h3("Trend Scoring Workflow"),
                grVizOutput("workflow_scoring", height = "auto")
              )
      ),
      
      # Page 4 - Trend Forecast ----
      tabItem(tabName = "forecast",
              fluidPage(
                h2("📈 Trend Forecast (2025 Q1)"),
                p("This page shows projected leasing activity (2025 Q1) for top-performing markets."),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(
                      inputId = "selected_top5_cities",
                      label = p(strong("Top 5 Cities by Growth Rate (2025 Q1)")),
                      choices = NULL,
                      selected = NULL
                    )
                  ),
                  mainPanel(
                    plotOutput("forecast_plot", height = 450),
                    br(),
                    h4("📋 Comparison Table"),
                    DTOutput("forecast_comparison"),
                    br(),
                    h3("ML Forecasting Workflow"),
                    grVizOutput("workflow_forecasting", height = "auto")
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

# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Workflow: Feature Construction (Page 2) ----
  output$workflow_feature <- renderGrViz({
    grViz("
    digraph feature {
      graph [layout = dot, rankdir = LR]

      node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 13]

      A [label = 'Merge multi-source data\\n(leases, rent, unemployment, etc.)']
      B [label = 'Construct city-quarter level panel dataset']
      C [label = 'Create lag features\\n(e.g., prior leased SF, occupancy rate change)']
      D [label = 'Log-transform leased SF\\n(log1p(total_leased_sf))']

      A -> B -> C -> D
    }
    ")
  })
  
  # ---- Workflow: Trend Scoring (Page 3) ----
  output$workflow_scoring <- renderGrViz({
    grViz("
    digraph scoring {
      graph [layout = dot, rankdir = LR]

      node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 13]

      A [label = 'Filter leases\\n(SF ≥ 10,000)']
      B [label = 'Aggregate leases\\n(city & quarter)']
      C [label = 'Join rent, vacancy, occupancy, unemployment']
      D [label = 'Standardize metrics (Z-score)']
      E [label = 'Calculate trend score\\n(Equal-weight average)']

      A -> B -> C -> D -> E
    }
    ")
  })
  
  # ---- Workflow: ML Forecasting (Page 4) ----
  output$workflow_forecasting <- renderGrViz({
    grViz("
    digraph ml {
      graph [layout = dot, rankdir = LR]

      node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 13]

      A [label = 'Target: log(total_leased_sf)']
      B [label = 'Create lagged features\\n(log_rent, availability, etc.)']
      C [label = 'Split data: training & validation']
      D [label = 'Train Random Forest / XGBoost']
      E [label = 'Evaluate RMSE, MAE, R²']
      F [label = 'Predict leasing for 2025 Q1']

      A -> B -> C -> D -> E -> F
    }
    ")
  })
  
  # ---- Page 3: Market Overview ----
  trend_data <- read_csv("trend_scores_with_coords.csv")
  selected_data <- trend_data %>% filter(year == 2024, quarter == "Q4")
  
  output$heatmap <- renderLeaflet({
    pal <- colorNumeric("RdYlGn", selected_data$trend_score)
    leaflet(data = selected_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 8,
        fillColor = ~pal(trend_score),
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(market, ": ", round(trend_score, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~trend_score, title = "Trend Score")
  })
  
  output$score_hist <- renderPlot({
    hist(selected_data$trend_score, breaks = 15, col = "steelblue", border = "white",
         main = "Distribution of Trend Scores", xlab = "Trend Score")
  })
  
  output$top5_table <- renderDT({
    selected_data %>%
      arrange(desc(trend_score)) %>%
      select(market, trend_score) %>%
      mutate(trend_score = round(trend_score, 2)) %>%
      head(5)
  })
  
  # ---- Page 4: Forecast ----
  observe({
    forecast_data <- read_csv("features1_wide.csv") %>%
      mutate(growth_rate = (`2025_Q1` - `2024_Q4`) / `2024_Q4`) %>%
      arrange(desc(growth_rate))
    
    top5_cities <- forecast_data %>%
      slice_max(growth_rate, n = 5) %>%
      pull(market)
    
    updateCheckboxGroupInput(
      session,
      inputId = "selected_top5_cities",
      choices = top5_cities,
      selected = top5_cities
    )
    
    output$forecast_plot <- renderPlot({
      req(input$selected_top5_cities)
      
      plot_data <- forecast_data %>%
        filter(market %in% input$selected_top5_cities) %>%
        pivot_longer(cols = -c(market, growth_rate), names_to = "time", values_to = "leased_sf") %>%
        filter(!is.na(leased_sf)) %>%
        mutate(
          year = as.numeric(substr(time, 1, 4)),
          quarter = substr(time, 6, 7),
          quarter_num = case_when(
            quarter == "Q1" ~ 1,
            quarter == "Q2" ~ 2,
            quarter == "Q3" ~ 3,
            quarter == "Q4" ~ 4,
            TRUE ~ NA_real_
          ),
          time_numeric = year + (quarter_num - 1) / 4
        ) %>%
        arrange(market, time_numeric)
      
      dashed_segments <- forecast_data %>%
        filter(market %in% input$selected_top5_cities) %>%
        transmute(
          market,
          x = 2024 + 3/4,
          xend = 2025,
          y = `2024_Q4`,
          yend = `2025_Q1`
        )
      
      ggplot() +
        geom_line(data = plot_data %>% filter(time != "2025_Q1"),
                  aes(x = time_numeric, y = leased_sf, group = market, color = market),
                  size = 1) +
        geom_segment(data = dashed_segments,
                     aes(x = x, xend = xend, y = y, yend = yend, color = market),
                     linetype = "dashed", linewidth = 1.2) +
        geom_point(data = plot_data,
                   aes(x = time_numeric, y = leased_sf, color = market),
                   size = 2) +
        labs(
          title = "Total Leased SF by Quarter",
          x = "Time", y = "Leased SF", color = "Market"
        ) +
        theme_minimal()
    })
    
    output$forecast_comparison <- renderDT({
      req(input$selected_top5_cities)
      forecast_data %>%
        filter(market %in% input$selected_top5_cities) %>%
        transmute(
          Market = market,
          `2024 Q4 SF` = round(`2024_Q4`),
          `2025 Q1 Predicted` = round(`2025_Q1`),
          `Growth Rate` = paste0(round(growth_rate * 100, 1), "%")
        )
    })
  })
}

# Run App ----
boastUtils::boastApp(ui = ui, server = server)

