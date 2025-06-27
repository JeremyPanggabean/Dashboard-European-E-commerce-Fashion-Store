# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(DBI)
library(RPostgres)
library(lubridate)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(viridis)

# Database connection function
get_db_connection <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = "yamabiko.proxy.rlwy.net",
    port = 51854,
    dbname = "railway",
    user = "postgres",
    password = "rgetBCThHJPNJtXIACPKNvMmohtdaEAI"
  )
  return(con)
}

# Function to fetch data
fetch_data <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  query <- "SELECT * FROM gabungan_data_customers_sales_items"
  data <- dbGetQuery(con, query)
  
  # Convert date columns
  data$tanggal_penjualan <- as.Date(data$tanggal_penjualan)
  data$tanggal_mendaftar <- as.Date(data$tanggal_mendaftar)
  
  return(data)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard E-commerce European Fashion Store"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales Analysis", tabName = "sales", icon = icon("chart-line")),
      menuItem("Customer Analysis", tabName = "customer", icon = icon("users")),
      menuItem("Product Analysis", tabName = "product", icon = icon("shopping-bag")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_sales"),
          valueBoxOutput("total_transactions"),
          valueBoxOutput("total_customers"),
          valueBoxOutput("avg_order_value")
        ),
        fluidRow(
          box(
            title = "Penjualan Berdasarkan Saluran", status = "primary", solidHeader = TRUE,
            width = 6, height = "400px",
            plotlyOutput("sales_by_channel")
          ),
          box(
            title = "Top 10 Negara Berdasarkan Penjualan", status = "success", solidHeader = TRUE,
            width = 6, height = "400px",
            plotlyOutput("sales_by_country")
          )
        ),
        fluidRow(
          box(
            title = "Tren Penjualan Harian", status = "info", solidHeader = TRUE,
            width = 12, height = "400px",
            plotlyOutput("daily_sales_trend")
          )
        )
      ),
      
      # Sales Analysis Tab
      tabItem(
        tabName = "sales",
        fluidRow(
          box(
            title = "Penjualan Berdasarkan Kategori Usia", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("sales_by_age")
          ),
          box(
            title = "Penjualan Berdasarkan Hari dalam Minggu", status = "success", solidHeader = TRUE,
            width = 6,
            plotlyOutput("sales_by_weekday")
          )
        ),
        fluidRow(
          box(
            title = "Tren Penjualan Bulanan", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("monthly_sales_trend")
          ),
          box(
            title = "Analisis Diskon", status = "warning", solidHeader = TRUE,
            width = 6,
            plotlyOutput("discount_analysis")
          )
        )
      ),
      
      # Customer Analysis Tab
      tabItem(
        tabName = "customer",
        fluidRow(
          box(
            title = "Distribusi Profil Pelanggan", status = "success", solidHeader = TRUE,
            width = 6,
            plotlyOutput("customer_profile")
          ),
          box(
            title = "Rata-rata Nilai Pesanan per Usia", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("aov_by_age")
          )
        ),
        fluidRow(
          box(
            title = "Tren Registrasi Pelanggan", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("customer_registration")
          ),
          box(
            title = "Analisis Loyalitas Pelanggan", status = "warning", solidHeader = TRUE,
            width = 6,
            plotlyOutput("customer_loyalty")
          )
        )
      ),
      
      # Product Analysis Tab
      tabItem(
        tabName = "product",
        fluidRow(
          box(
            title = "Top 10 Produk Terlaris", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("top_products")
          )
        ),
        fluidRow(
          box(
            title = "Distribusi Harga Produk", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_distribution")
          ),
          box(
            title = "Korelasi Harga vs Kuantitas", status = "success", solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_quantity_correlation")
          )
        )
      ),
      
      # Raw Data Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Tabel Data Mentah", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("raw_data_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data
  data <- reactive({
    fetch_data()
  })
  
  # Value boxes
  output$total_sales <- renderValueBox({
    total <- sum(data()$jumlah_total, na.rm = TRUE)
    valueBox(
      value = paste0("€", format(round(total, 2), big.mark = ".")),
      subtitle = "Total Penjualan",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  output$total_transactions <- renderValueBox({
    total <- length(unique(data()$id_penjualan))
    valueBox(
      value = format(total, big.mark = "."),
      subtitle = "Total Transaksi",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$total_customers <- renderValueBox({
    total <- length(unique(data()$id_pelanggan))
    valueBox(
      value = format(total, big.mark = "."),
      subtitle = "Total Pelanggan",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$avg_order_value <- renderValueBox({
    aov <- data() %>%
      group_by(id_penjualan) %>%
      summarise(order_value = first(jumlah_total)) %>%
      summarise(avg = mean(order_value, na.rm = TRUE)) %>%
      pull(avg)
    
    valueBox(
      value = paste0("€", format(round(aov, 2), big.mark = ".")),
      subtitle = "Rata-rata Nilai Pesanan",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # Overview Charts
  output$sales_by_channel <- renderPlotly({
    channel_sales <- data() %>%
      group_by(saluran) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(channel_sales, aes(x = reorder(saluran, total_sales), y = total_sales, fill = saluran)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Saluran", y = "Total Penjualan (€)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$sales_by_country <- renderPlotly({
    country_sales <- data() %>%
      group_by(negara) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales)) %>%
      head(10)
    
    p <- ggplot(country_sales, aes(x = reorder(negara, total_sales), y = total_sales, fill = negara)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Negara", y = "Total Penjualan (€)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$daily_sales_trend <- renderPlotly({
    daily_sales <- data() %>%
      group_by(tanggal_penjualan) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(tanggal_penjualan)
    
    p <- ggplot(daily_sales, aes(x = tanggal_penjualan, y = total_sales)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "darkred", size = 1, alpha = 0.7) +
      labs(x = "Tanggal", y = "Total Penjualan (€)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Sales Analysis Charts
  output$sales_by_age <- renderPlotly({
    age_sales <- data() %>%
      group_by(kategori_usia) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(age_sales, aes(x = kategori_usia, y = total_sales, fill = kategori_usia)) +
      geom_bar(stat = "identity") +
      labs(x = "Kategori Usia", y = "Total Penjualan (€)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$sales_by_weekday <- renderPlotly({
    weekday_sales <- data() %>%
      mutate(weekday = weekdays(tanggal_penjualan)) %>%
      group_by(weekday) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE))
    
    p <- ggplot(weekday_sales, aes(x = weekday, y = total_sales, fill = weekday)) +
      geom_bar(stat = "identity") +
      labs(x = "Hari", y = "Total Penjualan (€)") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Spectral")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$monthly_sales_trend <- renderPlotly({
    monthly_sales <- data() %>%
      mutate(month_year = floor_date(tanggal_penjualan, "month")) %>%
      group_by(month_year) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(month_year)
    
    p <- ggplot(monthly_sales, aes(x = month_year, y = total_sales)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(x = "Bulan", y = "Total Penjualan (€)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$discount_analysis <- renderPlotly({
    discount_data <- data() %>%
      group_by(status_diskon) %>%
      summarise(
        count = n(),
        total_sales = sum(jumlah_total, na.rm = TRUE)
      )
    
    p <- ggplot(discount_data, aes(x = status_diskon, y = count, fill = status_diskon)) +
      geom_bar(stat = "identity") +
      labs(x = "Status Diskon", y = "Jumlah Transaksi") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("Ya" = "lightcoral", "Tidak" = "lightblue"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Customer Analysis Charts
  output$customer_profile <- renderPlotly({
    profile_dist <- data() %>%
      group_by(profil_pelanggan) %>%
      summarise(count = n_distinct(id_pelanggan)) %>%
      arrange(desc(count))
    
    plot_ly(profile_dist, labels = ~profil_pelanggan, values = ~count, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = c('#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FECA57'))) %>%
      layout(showlegend = TRUE)
  })
  
  output$aov_by_age <- renderPlotly({
    aov_age <- data() %>%
      group_by(kategori_usia, id_penjualan) %>%
      summarise(order_value = first(jumlah_total), .groups = "drop") %>%
      group_by(kategori_usia) %>%
      summarise(avg_order_value = mean(order_value, na.rm = TRUE))
    
    p <- ggplot(aov_age, aes(x = kategori_usia, y = avg_order_value, fill = kategori_usia)) +
      geom_bar(stat = "identity") +
      labs(x = "Kategori Usia", y = "AOV (€)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$customer_registration <- renderPlotly({
    reg_trend <- data() %>%
      filter(!is.na(tanggal_mendaftar)) %>%
      mutate(month_year = floor_date(tanggal_mendaftar, "month")) %>%
      group_by(month_year) %>%
      summarise(new_customers = n_distinct(id_pelanggan)) %>%
      arrange(month_year)
    
    p <- ggplot(reg_trend, aes(x = month_year, y = new_customers)) +
      geom_line(color = "purple", size = 1) +
      geom_point(color = "orange", size = 2) +
      labs(x = "Bulan", y = "Pelanggan Baru") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$customer_loyalty <- renderPlotly({
    loyalty <- data() %>%
      group_by(id_pelanggan) %>%
      summarise(total_orders = n_distinct(id_penjualan)) %>%
      mutate(
        loyalty_segment = case_when(
          total_orders == 1 ~ "One-time",
          total_orders <= 3 ~ "Low Loyalty",
          total_orders <= 6 ~ "Medium Loyalty",
          TRUE ~ "High Loyalty"
        )
      ) %>%
      group_by(loyalty_segment) %>%
      summarise(customer_count = n())
    
    p <- ggplot(loyalty, aes(x = loyalty_segment, y = customer_count, fill = loyalty_segment)) +
      geom_bar(stat = "identity") +
      labs(x = "Segmen Loyalitas", y = "Jumlah Pelanggan") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "RdYlGn")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Product Analysis Charts
  output$top_products <- renderPlotly({
    top_products <- data() %>%
      group_by(nama_produk) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales)) %>%
      head(10)
    
    p <- ggplot(top_products, aes(x = reorder(nama_produk, total_sales), y = total_sales, fill = nama_produk)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Produk", y = "Total Penjualan (€)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$price_distribution <- renderPlotly({
    price_dist <- data() %>%
      mutate(price_range = cut(harga_per_unit, 
                             breaks = c(0, 50, 100, 200, 500, Inf),
                             labels = c("€0-50", "€50-100", "€100-200", "€200-500", "€500+"))) %>%
      group_by(price_range) %>%
      summarise(count = n()) %>%
      filter(!is.na(price_range))
    
    p <- ggplot(price_dist, aes(x = price_range, y = count, fill = price_range)) +
      geom_bar(stat = "identity") +
      labs(x = "Rentang Harga", y = "Jumlah Produk") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Blues")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$price_quantity_correlation <- renderPlotly({
    price_qty <- data() %>%
      group_by(nama_produk) %>%
      summarise(
        avg_price = mean(harga_per_unit, na.rm = TRUE),
        total_quantity = sum(kuantitas, na.rm = TRUE)
      ) %>%
      filter(total_quantity > 0 & avg_price > 0)
    
    p <- ggplot(price_qty, aes(x = avg_price, y = total_quantity)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(x = "Rata-rata Harga (€)", y = "Total Kuantitas Terjual") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(
      data(),
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        autoWidth = TRUE
      ),
      filter = "top",
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(columns = c("harga_per_unit", "jumlah_total"), currency = "€", digits = 2) %>%
      formatDate(columns = c("tanggal_penjualan", "tanggal_mendaftar"), method = "toLocaleDateString")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
