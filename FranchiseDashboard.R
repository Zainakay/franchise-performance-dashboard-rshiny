library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(plotly)
library(DT)

# =====================================================
# LOAD DATA
# =====================================================

df <- read.csv("Franchise_Data.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

# =====================================================
# HELPER FUNCTIONS
# =====================================================

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, 0, x / y)
}

# =====================================================
# UI
# =====================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display:flex; align-items:center;",
      tags$img(src = "Logo.png", height = "45px", style = "margin-right:10px;"),
      tags$span("Franchise Dashboard Example", style = "font-size:18px;font-weight:bold;")
    ),
    titleWidth = 420
  ),
  
  
  dashboardSidebar(
    width = 280,
    
    sidebarMenu(
      menuItem("Executive Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Revenue & Growth", tabName = "revenue", icon = icon("pound-sign")),
      menuItem("Customer Experience", tabName = "customers", icon = icon("users")),
      menuItem("Operations", tabName = "operations", icon = icon("cogs")),
      menuItem("Franchise Ranking", tabName = "ranking", icon = icon("table"))
    ),
    
    br(),
    
    selectizeInput(
      "franchise",
      "Select Franchise(s)",
      choices = sort(unique(df$Franchise)),
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Choose franchise(s)",
        plugins = list("remove_button")
      )
    ),
    
    sliderInput(
      "month",
      "Month Range",
      min = 1,
      max = 12,
      value = c(1, 12),
      step = 1,
      ticks = FALSE
    ),
    
    sliderInput(
      "year",
      "Year Range",
      min = min(year(df$Date)),
      max = max(year(df$Date)),
      value = c(min(year(df$Date)), max(year(df$Date))),
      step = 1,
      sep = "", 
      ticks = FALSE
    ),
    
    br(),
    
    div(
      style = "text-align:center;",
      actionButton(
        "reset",
        "Reset Filters",
        icon = icon("undo"),
        width = "55%"
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color:#f4f8fc;
        }

        .skin-blue .main-header .logo {
          background:#87CEEB !important;
          color:white !important;
        }

        .skin-blue .main-header .navbar {
          background:#87CEEB !important;
        }

        .skin-blue .main-sidebar {
          background:#1f2937 !important;
        }

        .box, .small-box {
          border-radius:12px;
        }

        .selectize-input {
          border-radius:8px;
          border:1px solid #87CEEB;
        }

        .irs-bar,
        .irs-bar-edge {
          background:#00A6D6 !important;
          border-top:1px solid #00A6D6 !important;
          border-bottom:1px solid #00A6D6 !important;
        }

        .irs-from,
        .irs-to,
        .irs-single {
          background:#00A6D6 !important;
        }

        .irs-slider {
          width:18px !important;
          height:18px !important;
          top:28px !important;
        }

        .irs-line,
        .irs-bar {
          height:6px !important;
        }

        .btn-default {
          background:#00A6D6;
          color:white;
          border:none;
          border-radius:8px;
          font-weight:bold;
        }

        .btn-default:hover {
          background:#008DB3;
          color:white;
        }
      "))
    ),
    
    tabItems(
      
      # =====================================================
      # EXECUTIVE OVERVIEW
      # =====================================================
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("total_revenue", width = 3),
          valueBoxOutput("total_bookings", width = 3),
          valueBoxOutput("new_customers", width = 3),
          valueBoxOutput("complaint_rate", width = 3),
         ),
        
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Selected Period",
            div(
              style = "
                text-align:center;
                font-size:24px;
                font-weight:bold;
                padding:15px;
                color:#2C3E50;
              ",
              textOutput("period_display")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Executive Insights",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            htmlOutput("executive_insights")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Franchise Revenue (£)",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("revenue_bar", height = "320px"))
          ),
          
          box(
            title = "Revenue Trend",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("revenue_trend", height = "320px"))
          )
        )
      ),
      
      # =====================================================
      # REVENUE & GROWTH
      # =====================================================
      
      tabItem(
        tabName = "revenue",
        
        fluidRow(
          box(
            title = "Revenue vs Complaint Rate",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("revenue_vs_complaints", height = "330px"))
          )
        ),
        
        fluidRow(
          
          valueBoxOutput("revenue_growth_kpi", width = 4),
          valueBoxOutput("total_revenue_growth_kpi", width = 4),
          valueBoxOutput("best_revenue_franchise_kpi", width = 4)
          
        ),
        
        fluidRow(
          
          box(
            title = "Revenue Contribution by Franchise",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("revenue_contribution", height = "320px"))
          ),
          
          box(
            title = "Revenue per Booking (£)",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("revenue_per_booking", height = "320px"))
          )
          
        )
      ),
      
      # =====================================================
      # CUSTOMER EXPERIENCE
      # =====================================================
      
      tabItem(
        tabName = "customers",
        fluidRow(
          valueBoxOutput("avg_satisfaction", width = 3),
          valueBoxOutput("total_complaints_customer", width = 3),
          valueBoxOutput("best_franchise_customer", width = 3),
          valueBoxOutput("worst_franchise_customer", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Customer Satisfaction vs Complaint Rate",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            withSpinner(plotlyOutput("satisfaction_vs_complaints", height = "330px"))
          )
        ),
        fluidRow(
          
          box(
            title = "Customer Satisfaction Trend",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            withSpinner(
              plotlyOutput("satisfaction_trend", height = "300px")
            )
          ),
          
          box(
            title = "Top Complaint Franchises",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            height = "370px",
            DTOutput("complaint_table")
          )
          
        ),
      ),
      
      # =====================================================
      # OPERATIONS
      # =====================================================
      
      tabItem(
        tabName = "operations",
        fluidRow(
          valueBoxOutput("attendance_kpi", width = 3),
          valueBoxOutput("capacity_kpi", width = 3),
          valueBoxOutput("revenue_per_booking_ops", width = 3),
          valueBoxOutput("bookings_kpi", width = 3)
        ),
        fluidRow(
          
          box(
            title = "Capacity Utilisation by Franchise",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(
              plotlyOutput("capacity_bar", height = "300px")
            )
          ),
          
          box(
            title = "Operational Performance Summary",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            DTOutput("operations_table")
          )
          
        ),
  
        
        fluidRow(
          
          box(
            title = "Attendance vs Capacity Utilisation",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            withSpinner(
              plotlyOutput("attendance_vs_capacity", height = "350px")
            )
          )
          
        )
      ),
      
      # =====================================================
      # RANKING
      # =====================================================
      
      tabItem(
        tabName = "ranking",
        
        fluidRow(
          box(
            title = "Franchise Performance Ranking",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("ranking_table")
          )
        )
      )
    )
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectizeInput(session, "franchise", selected = character(0))
    updateSliderInput(session, "month", value = c(1, 12))
    updateSliderInput(
      session,
      "year",
      value = c(min(year(df$Date)), max(year(df$Date)))
    )
  })
  
  period_text <- reactive({
    paste0(
      month.abb[input$month[1]], " ", input$year[1],
      " - ",
      month.abb[input$month[2]], " ", input$year[2]
    )
  })
  
  output$period_display <- renderText({
    period_text()
  })
  
  selected_or_network <- reactive({
    if (is.null(input$franchise) || length(input$franchise) == 0) {
      "All Franchises"
    } else {
      paste(input$franchise, collapse = ", ")
    }
  })
  
  filtered_data <- reactive({
    temp <- df
    
    if (!is.null(input$franchise) && length(input$franchise) > 0) {
      temp <- temp %>%
        filter(Franchise %in% input$franchise)
    }
    
    temp %>%
      filter(
        month(Date) >= input$month[1],
        month(Date) <= input$month[2],
        year(Date) >= input$year[1],
        year(Date) <= input$year[2]
      )
  })
  
  franchise_summary <- reactive({
    filtered_data() %>%
      group_by(Franchise) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        Bookings = sum(Bookings, na.rm = TRUE),
        New_Customers = sum(New_Customers, na.rm = TRUE),
        Complaints = sum(Complaints, na.rm = TRUE),
        Complaint_Rate = safe_divide(Complaints, Bookings) * 100,
        Avg_Satisfaction = mean(Customer_Satisfaction, na.rm = TRUE),
        Avg_Attendance = mean(Attendance_Rate, na.rm = TRUE),
        Capacity_Utilisation = safe_divide(
          sum(Bookings, na.rm = TRUE),
          sum(Lesson_Capacity, na.rm = TRUE)
        ) * 100,
        Revenue_Per_Booking = safe_divide(Revenue, Bookings),
        .groups = "drop"
      )
  })
  
  # =====================================================
  # KPI BOXES
  # =====================================================
  
  output$total_revenue <- renderValueBox({
    valueBox(
      paste0("£", comma(sum(filtered_data()$Revenue, na.rm = TRUE))),
      "Revenue",
      icon = icon("pound-sign"),
      color = "aqua"
    )
  })
  
  output$total_bookings <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$Bookings, na.rm = TRUE)),
      "Bookings",
      icon = icon("calendar-check"),
      color = "green"
    )
  })

  output$new_customers_kpi <- renderValueBox({
    
    valueBox(
      comma(sum(filtered_data()$New_Customers, na.rm = TRUE)),
      "New Customers",
      icon = icon("user-plus"),
      color = "yellow"
    )
    
  })
  
  output$best_revenue_franchise_kpi <- renderValueBox({
    
    top_franchise <- franchise_summary() %>%
      arrange(desc(Revenue)) %>%
      slice(1)
    
    valueBox(
      top_franchise$Franchise,
      "Top Revenue Franchise",
      icon = icon("trophy"),
      color = "green"
    )
    
  })
  
  output$revenue_growth_kpi <- renderValueBox({
    
    valueBox(
      paste0("£", round(mean(franchise_summary()$Revenue_Per_Booking),1)),
      "Revenue per Booking",
      icon = icon("pound-sign"),
      color = "aqua"
    )
    
  })
  
  output$new_customers <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$New_Customers, na.rm = TRUE)),
      "New Customers",
      icon = icon("user-plus"),
      color = "yellow"
    )
  })
  
  output$complaint_rate <- renderValueBox({
    total_bookings <- sum(filtered_data()$Bookings, na.rm = TRUE)
    total_complaints <- sum(filtered_data()$Complaints, na.rm = TRUE)
    
    rate <- round(safe_divide(total_complaints, total_bookings) * 100, 2)
    
    valueBox(
      paste0(rate, "%"),
      "Complaint Rate",
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  # =====================================================
  # EXECUTIVE INSIGHTS
  # =====================================================
  
  output$executive_insights <- renderUI({
    data_now <- filtered_data()
    summary_df <- franchise_summary() %>% arrange(desc(Revenue))
    
    if (nrow(summary_df) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    top_revenue <- summary_df %>% slice(1)
    highest_complaints <- summary_df %>% arrange(desc(Complaint_Rate)) %>%
      slice_head(n = 5) %>% slice(1)
    best_satisfaction <- summary_df %>% arrange(desc(Avg_Satisfaction)) %>% slice(1)
    
    HTML(
      paste0(
        "<ul>",
        "<li><b>Selected Franchises:</b> ", selected_or_network(), "</li>",
        "<li><b>Period:</b> ", period_text(), "</li>",
        "<li><b>Total Revenue:</b> £", comma(sum(data_now$Revenue, na.rm = TRUE)), "</li>",
        "<li><b>Total Bookings:</b> ", comma(sum(data_now$Bookings, na.rm = TRUE)), "</li>",
        "<li><b>New Customers:</b> ", comma(sum(data_now$New_Customers, na.rm = TRUE)), "</li>",
        "<li><b>Average Satisfaction:</b> ", round(mean(data_now$Customer_Satisfaction, na.rm = TRUE), 2), "/5</li>",
        "<li><b>Highest Revenue:</b> ", top_revenue$Franchise, " (£", comma(top_revenue$Revenue), ")</li>",
        "<li><b>Highest Complaint Rate:</b> ", highest_complaints$Franchise, " (", round(highest_complaints$Complaint_Rate, 2), "%)</li>",
        "<li><b>Best Customer Satisfaction:</b> ", best_satisfaction$Franchise, " (", round(best_satisfaction$Avg_Satisfaction, 2), "/5)</li>",
        "<li><b>Recommendation 1:</b> Focus support and coaching efforts on ", highest_complaints$Franchise,
        " as it has the highest complaint rate (", round(highest_complaints$Complaint_Rate,2), "%).</li>",
        
        "<li><b>Recommendation 2:</b> Review operational practices in ", top_revenue$Franchise,
        " to identify strategix1wses that can be shared across the franchise network.</li>",
        
        "<li><b>Recommendation 3:</b> Investigate opportunities to improve capacity utilisation in lower-performing franchises to maximise revenue potential.</li>",
        "</ul>"
      )
    )
  })
  
  # =====================================================
  # OVERVIEW CHARTS
  # =====================================================
  
  output$revenue_bar <- renderPlotly({
    plot_df <- franchise_summary() %>% arrange(Revenue)
    
    p <- ggplot(plot_df, aes(x = reorder(Franchise, Revenue), y = Revenue)) +
      geom_col(fill = "#1E88E5") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = "", y = "Revenue (£)") +
      theme_minimal(base_size = 11)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 90, r = 20, t = 20, b = 50))
  })
  
  output$revenue_trend <- renderPlotly({
    plot_df <- filtered_data()
    
    if (is.null(input$franchise) || length(input$franchise) == 0) {
      plot_df <- plot_df %>%
        group_by(Date) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(plot_df, aes(Date, Revenue)) +
        geom_line(colour = "#00A6A6", linewidth = 1.3) +
        geom_point(colour = "#00A6A6", size = 2) +
        labs(
          title = paste("Network Revenue Trend |", period_text()),
          x = "Date",
          y = "Revenue (£)"
        ) +
        theme_minimal(base_size = 11)
    } else {
      plot_df <- plot_df %>%
        group_by(Date, Franchise) %>%
        summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")
      
      p <- ggplot(plot_df, aes(Date, Revenue, colour = Franchise)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(
          title = paste(paste(input$franchise, collapse = " vs "), "|", period_text()),
          x = "Date",
          y = "Revenue (£)"
        ) +
        theme_minimal(base_size = 11)
    }
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 60, r = 20, t = 45, b = 50))
  })
  
  # =====================================================
  # REVENUE & GROWTH CHARTS
  # =====================================================
  
  output$revenue_vs_complaints <- renderPlotly({
    plot_df <- franchise_summary()
    
    plot_ly(
      data = plot_df,
      x = ~Revenue,
      y = ~Complaint_Rate,
      type = "scatter",
      mode = "markers",
      color = ~Franchise,
      colors = "Set3",
      marker = list(size = 13, opacity = 0.8),
      text = ~paste0(
        "<b>", Franchise, "</b><br>",
        "Revenue: £", comma(Revenue), "<br>",
        "Complaint Rate: ", round(Complaint_Rate, 2), "%<br>",
        "Bookings: ", comma(Bookings), "<br>",
        "Satisfaction: ", round(Avg_Satisfaction, 2), "/5"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Revenue vs Complaint Rate",
        xaxis = list(title = "Revenue (£)", tickformat = ","),
        yaxis = list(title = "Complaint Rate (%)"),
        margin = list(l = 70, r = 20, t = 45, b = 55)
      )
  })
  
  output$revenue_contribution <- renderPlotly({
    plot_df <- franchise_summary() %>%
      arrange(Revenue) %>%
      mutate(Label = paste0("£", comma(round(Revenue, 0))))
    
    p <- ggplot(plot_df, aes(x = reorder(Franchise, Revenue), y = Revenue)) +
      geom_col(fill = "#1E88E5") +
      
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.20))) +
      labs(x = "", y = "Revenue (£)") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 90, r = 25, t = 15, b = 45))
  })
  
  output$revenue_per_booking <- renderPlotly({
    plot_df <- franchise_summary() %>%
      arrange(Revenue_Per_Booking) %>%
      mutate(Label = paste0("£", round(Revenue_Per_Booking, 1)))
    
    p <- ggplot(plot_df, aes(x = reorder(Franchise, Revenue_Per_Booking), y = Revenue_Per_Booking)) +
      geom_col(fill = "#27AE60") +

      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
      labs(x = "", y = "Revenue per Booking (£)") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 90, r = 25, t = 15, b = 45))
  })
  
  output$new_customer_trend <- renderPlotly({
    plot_df <- filtered_data() %>%
      group_by(Date) %>%
      summarise(New_Customers = sum(New_Customers, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(plot_df, aes(Date, New_Customers)) +
      geom_line(colour = "#00A6A6", linewidth = 1.2) +
      geom_point(colour = "#00A6A6", size = 2) +
      scale_y_continuous(labels = comma) +
      labs(x = "Date", y = "New Customers") +
      theme_minimal(base_size = 10)
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 55, r = 20, t = 15, b = 45))
  })
  
  # =====================================================
  # CUSTOMER EXPERIENCE KPI BOXES
  # =====================================================
  
  output$avg_satisfaction <- renderValueBox({
    
    valueBox(
      round(mean(filtered_data()$Customer_Satisfaction, na.rm = TRUE), 2),
      "Average Satisfaction",
      icon = icon("smile"),
      color = "green"
    )
    
  })
  
  output$total_complaints_customer <- renderValueBox({
    
    valueBox(
      comma(sum(filtered_data()$Complaints, na.rm = TRUE)),
      "Total Complaints",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
    
  })
  
  output$best_franchise_customer <- renderValueBox({
    
    best <- franchise_summary() %>%
      arrange(desc(Avg_Satisfaction)) %>%
      slice(1)
    
    valueBox(
      best$Franchise,
      "Highest Satisfaction",
      icon = icon("trophy"),
      color = "aqua"
    )
    
  })
  
  output$worst_franchise_customer <- renderValueBox({
    
    worst <- franchise_summary() %>%
      arrange(desc(Complaint_Rate)) %>%
      slice(1)
    
    valueBox(
      paste0(round(worst$Complaint_Rate,2), "%"),
      paste("Highest Complaints:", worst$Franchise),
      icon = icon("triangle-exclamation"),
      color = "red"
    )
    
  })
  
  # =====================================================
  # CUSTOMER EXPERIENCE CHARTS
  # =====================================================
  
  output$satisfaction_vs_complaints <- renderPlotly({
    plot_df <- franchise_summary()
    
    plot_ly(
      data = plot_df,
      x = ~Avg_Satisfaction,
      y = ~Complaint_Rate,
      type = "scatter",
      mode = "markers",
      color = ~Franchise,
      colors = "Set3",
      marker = list(size = 13, opacity = 0.8),
      text = ~paste0(
        "<b>", Franchise, "</b><br>",
        "Satisfaction: ", round(Avg_Satisfaction, 2), "/5<br>",
        "Complaint Rate: ", round(Complaint_Rate, 2), "%<br>",
        "Revenue: £", comma(Revenue), "<br>",
        "Bookings: ", comma(Bookings)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Customer Satisfaction vs Complaint Rate",
        xaxis = list(title = "Customer Satisfaction (1-5)"),
        yaxis = list(title = "Complaint Rate (%)"),
        margin = list(l = 70, r = 20, t = 45, b = 55)
      )
  })
  
  output$satisfaction_trend <- renderPlotly({
    plot_df <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        Satisfaction = mean(Customer_Satisfaction, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(plot_df, aes(Date, Satisfaction)) +
      geom_line(colour = "#2ECC71", linewidth = 1.2) +
      geom_point(colour = "#2ECC71", size = 2) +
      scale_y_continuous(limits = c(3.8, 5)) +
      labs(x = "Date", y = "Satisfaction") +
      theme_minimal(base_size = 10)
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE) %>%
      layout(margin = list(l = 55, r = 20, t = 15, b = 45))
  })
  
  output$complaint_table <- renderDT({
    
    franchise_summary() %>%
      arrange(desc(Complaint_Rate)) %>%
      select(
        Franchise,
        `Complaint Rate (%)` = Complaint_Rate,
        Complaints,
        `Satisfaction` = Avg_Satisfaction
      )%>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          searching = FALSE,
          lengthChange = FALSE,
          paging = FALSE,
          info = FALSE,
          scrollY = "250px",
          dom = "t"
        )
      ) %>%
      formatRound(
        c("Complaint Rate (%)", "Satisfaction"),
        digits = 2
      )
    
  })
  # =====================================================
  # OPERATIONS CHARTS
  # =====================================================
  output$attendance_kpi <- renderValueBox({
    
    valueBox(
      paste0(round(mean(filtered_data()$Attendance_Rate, na.rm = TRUE),1), "%"),
      "Average Attendance",
      icon = icon("users"),
      color = "green"
    )
    
  })
  
  output$capacity_kpi <- renderValueBox({
    
    valueBox(
      paste0(round(mean(franchise_summary()$Capacity_Utilisation),1), "%"),
      "Capacity Utilisation",
      icon = icon("chart-bar"),
      color = "blue"
    )
    
  })
  
  output$revenue_per_booking_ops <- renderValueBox({
    
    valueBox(
      paste0("£", round(mean(franchise_summary()$Revenue_Per_Booking),1)),
      "Revenue per Booking (£)",
      icon = icon("pound-sign"),
      color = "aqua"
    )
    
  })
  
  output$bookings_kpi <- renderValueBox({
    
    valueBox(
      comma(sum(filtered_data()$Bookings)),
      "Bookings",
      icon = icon("calendar-check"),
      color = "yellow"
    )
    
  })

  output$capacity_bar <- renderPlotly({
    plot_df <- franchise_summary() %>%
      arrange(Capacity_Utilisation) %>%
      mutate(Label = round(Capacity_Utilisation, 1))
    
    p <- ggplot(plot_df, aes(x = reorder(Franchise, Capacity_Utilisation), y = Capacity_Utilisation)) +
      geom_col(fill = "#42A5F5") +
      
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "", y = "Capacity Utilisation (%)") +
      theme_minimal(base_size = 9)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 85, r = 20, t = 15, b = 45))
  })
  output$operations_table <- renderDT({
    
    franchise_summary() %>%
      arrange(desc(Capacity_Utilisation)) %>%
      select(
        Franchise,
        `Attendance (%)` = Avg_Attendance,
        `Capacity (%)` = Capacity_Utilisation,
        `Revenue/Booking (£)` = Revenue_Per_Booking
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = "t",
          scrollY = "250px"
        )
      ) %>%
      formatRound(
        c("Attendance (%)", "Capacity (%)"),
        digits = 1
      ) %>%
      formatCurrency(
        "Revenue/Booking (£)",
        currency = "£",
        digits = 1
      )
    
  })
  
  
  output$total_revenue_growth_kpi <- renderValueBox({
    
    valueBox(
      paste0("£", comma(sum(filtered_data()$Revenue, na.rm = TRUE))),
      "Total Revenue",
      icon = icon("chart-line"),
      color = "green"
    )
    
  })
  output$attendance_vs_capacity <- renderPlotly({
    
    plot_ly(
      data = franchise_summary(),
      x = ~Avg_Attendance,
      y = ~Capacity_Utilisation,
      type = "scatter",
      mode = "markers+text",
      text = ~Franchise,
      textposition = "top center",
      marker = list(size = 12)
    ) %>%
      layout(
        title = "Attendance vs Capacity Utilisation",
        xaxis = list(title = "Attendance Rate (%)"),
        yaxis = list(title = "Capacity Utilisation (%)")
      )
    
  })
  
  # =====================================================
  # RANKING TABLE
  # =====================================================
  
  output$ranking_table <- renderDT({
    ranking_df <- franchise_summary() %>%
      arrange(desc(Revenue)) %>%
      mutate(Rank = row_number()) %>%
      select(
        Rank,
        Franchise,
        Revenue,
        Bookings,
        New_Customers,
        Complaints,
        Complaint_Rate,
        Avg_Attendance,
        Avg_Satisfaction,
        Revenue_Per_Booking
      )
    
    datatable(
      ranking_df,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Revenue", currency = "£", digits = 0) %>%
      formatCurrency("Revenue_Per_Booking", currency = "£", digits = 1) %>%
      formatRound("Complaint_Rate", 2) %>%
      formatRound("Avg_Attendance", 1) %>%
      formatRound("Avg_Satisfaction", 2)
  })
}

shinyApp(ui, server)

