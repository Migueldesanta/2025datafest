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
library(plotly)
# UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Penn State F4 Team", titleWidth = 280),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Method", tabName = "intro", icon = icon("info-circle")),
      menuItem("Market Overview (Where?)", tabName = "overview", icon = icon("globe")),
      menuItem("Trend Forecast (When?)", tabName = "forecast", icon = icon("chart-line"))
    ),
    tags$div(class = "sidebar-logo", boastUtils::sidebarFooter())
  ),
  
  dashboardBody(
    tabItems(
      # Page 1 - Home ----
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("🏢  City Leasing Forecast & Market Explorer"),
                br(),
                h3("Overview"),
                p(tags$b("The COVID-19 pandemic"), " disrupted U.S. office leasing markets. To understand which cities are best positioned for market entry as the sector recovers, we focused on data from ", 
                  tags$b("2021 Q4 onward"), ". Using ", tags$b("large lease records"), " and ", tags$b("market indicators"), 
                  ", we developed ", tags$b("trend scores"), " to capture ", tags$b("city competitiveness"), 
                  " and applied ", tags$b("machine learning"), " to forecast ", tags$b("leasing activity for 2025 Q1"), "."
                ),
                h3("Research Question"),
                p("Based on ", tags$b("leasing trends"), " and ", tags$b("market fundamentals"), 
                  ", which ", tags$b("U.S. cities"), " are most ", tags$b("competitive"), 
                  " and ", tags$b("suitable for market entry in the next quarter"), "?"),
                h3("Team Members"),
                tags$ul(
                  tags$li("Michael Yun"),
                  tags$li("Runyi Zhang"),
                  tags$li("Jingchun Zhang"),
                  tags$li("Zhaoyu Hou")
                )
              )
      ),
      
      # Page 2 - Methodology ----
      tabItem(
        tabName = "intro",
        withMathJax(),
        
          h2("🛠️ Methodology"),
          p("We focused on commercial leasing recovery patterns after COVID-19. Using 2021 Q4 onward data, we constructed trend scores to evaluate market competitiveness and applied machine learning to forecast future leasing activity."),
          
          # ---- Box 1: Data Description ----
          box(
            title = strong("Data Description"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = '100%',
            tags$ul(
              tags$li("Time frame: 2021 Q4 – 2024 Q4"),
              tags$li("Leasing data filtered by area ≥ 10,000 SF"),
              tags$li("Quarterly panel data by city"),
              tags$li("Merged indicators: rent, vacancy, occupancy, unemployment")
            )
          ),
          
          # ---- Box 2: Feature Engineering ----
          box(
            title = strong("Feature Engineering"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = '100%',
            tags$ul(
              tags$li("Created lag variables for key indicators"),
              tags$li("Calculated rate of change (e.g., Δ vacancy, Δ occupancy)"),
              tags$li("Log-transformed leased SF to stabilize variance"),
              tags$li("Aligned features into city-quarter panel structure")
            )
          ),
          
          # ---- Box 3: Market Review (Trend Scoring) ----
          box(
            title = strong("Market Review (Trend Scoring)"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = '100%',
            tags$ul(
              tags$li("Standardized 5 market indicators using Z-scores"),
              tags$li("Included metrics: leased SF, rent (↓), vacancy (↓), occupancy (↑), unemployment (↓)"),
              tags$li("Computed composite trend score by equal weighting"),
              tags$li("Higher score implies greater leasing competitiveness")
            ),
            p("Mathematical formula for trend score:"),
            p("$$
      \\text{Trend Score}_{it} = \\frac{1}{5} \\left( Z_{leased\\_sf} - Z_{rent} - Z_{vacancy} + Z_{occupancy} - Z_{unemployment} \\right)
      $$")
          ),
          
          # ---- Box 4: Trend Forecasting ----
          box(
            title = strong("Trend Forecasting"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = '100%',
            tags$ul(
              tags$li("Target: log-transformed leased SF in 2025 Q1"),
              tags$li("Model: XGBoost regression"),
              tags$li("Features: lagged panel of 2021–2024 indicators"),
              tags$li("Train/validation split: 80% / 20%"),
              tags$li("Evaluation metrics: RMSE and \\(R^2\\)"),
              tags$li("Back-transform forecast for interpretability")
            )
          )
        )
      ,
      
      # ---- Page 3 - Market Overview ----
      tabItem(tabName = "overview",
              fluidPage(
                h2("🌍 Market Overview"),
                p("Explore market competitiveness based on trend scores since 2021 Q4."),
                br(),
                h3("Trend Scoring Workflow"),
                grVizOutput("workflow_scoring", height = "auto"),
                br(), hr(),
                
                fluidRow(
                  column(6,
                         h3("📍 Market Trend Heatmap"),
                         leafletOutput("heatmap", height = 550)
                  ),
                  column(6,
                         h3("📊 3D Trend Score Distribution (Since 2021 Q4)"),
                         sidebarPanel(
                           checkboxGroupInput("selected_3d_cities",
                                              label = "Select Cities for 3D View:",
                                              choices = NULL,
                                              selected = NULL),
                           width = 12
                         ),
                         plotlyOutput("trend_score_3d", height = 500)
                  )
                ),
                br(), hr(),
                fluidRow(
                  column(12,
                         h3("🏙️ Top 5 Markets by Average Trend Score"),
                         DTOutput("top5_table")
                  )
                )
              )),
      
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
                    DTOutput("forecast_comparison")
                  )
                )
              )
      )
      )
    )
  )


# ---- Server ----
server <- function(input, output, session) {
  

  
  # ---- Load and Filter Data ----
  trend_data <- read_csv("trend_scores_with_coords.csv")
  
  # Use only 2021 Q4 and later
  trend_filtered <- trend_data %>%
    filter(year > 2021 | (year == 2021 & quarter == "Q4"))
  
  # ---- Compute Top 5 Markets by Avg Score ----
  top5_summary <- trend_filtered %>%
    group_by(market) %>%
    summarise(
      avg_score = mean(trend_score, na.rm = TRUE),
      lat = first(lat),
      lon = first(lon),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_score)) %>%
    slice_head(n = 5)
  
  # ---- Heatmap with Labels ----
  output$heatmap <- renderLeaflet({
    pal <- colorNumeric("RdYlGn", domain = trend_filtered$trend_score)
    
    leaflet(data = trend_filtered) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        color = ~pal(trend_score),
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.7
      ) %>%
      addLabelOnlyMarkers(
        data = top5_summary,
        lng = ~lon, lat = ~lat,
        label = ~market,
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~trend_score,
                title = "Trend Score")
  })
  
  # ---- Top 5 Table ----
  output$top5_table <- renderDT({
    top5_summary %>%
      transmute(
        Market = market,
        `Avg Trend Score` = round(avg_score, 3)
      )
  })
  
  # ---- Update 3D Selector ----
  observe({
    updateCheckboxGroupInput(
      session, "selected_3d_cities",
      choices = top5_summary$market,
      selected = top5_summary$market
    )
  })
  
  # ---- 3D Plot with Actual Trend Scores ----
  output$trend_score_3d <- renderPlotly({
    req(input$selected_3d_cities)
    
    plot_data <- trend_filtered %>%
      filter(market %in% input$selected_3d_cities) %>%
      mutate(
        quarter_num = as.numeric(substr(quarter, 2, 2)),
        year_qtr = year + (quarter_num - 1) / 4
      )
    
    plot_ly(
      data = plot_data,
      x = ~market,
      y = ~trend_score,
      z = ~year_qtr,
      type = "scatter3d",
      mode = "lines+markers",
      color = ~market,
      line = list(width = 4),
      marker = list(size = 5)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Market"),
          yaxis = list(title = "Trend Score"),
          zaxis = list(title = "Time (Year.Quarter)")
        ),
        title = "3D Trend Score Trajectory (Top 5 Markets)"
      )
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

