# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(RPostgreSQL)
library(lubridate)
library(scales)

# Database connection function
get_db_connection <- function() {
  # Railway PostgreSQL connection
  con <- dbConnect(
    PostgreSQL(),
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
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Ringkasan Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_sales"),
          valueBoxOutput("total_transactions"),
          valueBoxOutput("total_customers")
        ),
        fluidRow(
          box(
            title = "Penjualan Berdasarkan Saluran", status = "primary", solidHeader = TRUE,
            width = 6, height = "400px",
            plotlyOutput("sales_by_channel")
          ),
          box(
            title = "Penjualan Berdasarkan Negara", status = "success", solidHeader = TRUE,
            width = 6, height = "400px",
            plotlyOutput("sales_by_country")
          )
        )
      ),
      
      # Analisis Penjualan Tab
      tabItem(
        tabName = "sales",
        fluidRow(
          box(
            title = "Tren Penjualan Harian", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("daily_sales_trend")
          )
        ),
        fluidRow(
          box(
            title = "Penjualan Berdasarkan Kategori Usia", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("sales_by_age")
          ),
          box(
            title = "Analisis Diskon", status = "warning", solidHeader = TRUE,
            width = 6,
            plotlyOutput("discount_analysis")
          )
        )
      ),
      
      # Analisis Pelanggan Tab
      tabItem(
        tabName = "customer",
        fluidRow(
          box(
            title = "Tren Registrasi Pelanggan", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("customer_registration")
          )
        ),
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
        )
      ),
      
      # Data Mentah Tab
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
  
  # Charts
  output$sales_by_channel <- renderPlotly({
    channel_sales <- data() %>%
      group_by(saluran) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(channel_sales, aes(x = reorder(saluran, total_sales), y = total_sales)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Penjualan Berdasarkan Saluran", x = "Saluran", y = "Total Penjualan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$sales_by_country <- renderPlotly({
    country_sales <- data() %>%
      group_by(negara) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(country_sales, aes(x = reorder(negara, total_sales), y = total_sales)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Penjualan Berdasarkan Negara", x = "Negara", y = "Total Penjualan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$daily_sales_trend <- renderPlotly({
    daily_sales <- data() %>%
      group_by(tanggal_penjualan) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(tanggal_penjualan)
    
    p <- ggplot(daily_sales, aes(x = tanggal_penjualan, y = total_sales)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Tren Penjualan Harian", x = "Tanggal", y = "Total Penjualan") +
      theme_minimal() +
      scale_x_date(date_labels = "%d-%m-%Y")
    
    ggplotly(p)
  })
  
  output$sales_by_age <- renderPlotly({
    age_sales <- data() %>%
      group_by(kategori_usia) %>%
      summarise(total_sales = sum(jumlah_total, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(age_sales, aes(x = kategori_usia, y = total_sales, fill = kategori_usia)) +
      geom_bar(stat = "identity") +
      labs(title = "Penjualan Berdasarkan Kategori Usia", x = "Kategori Usia", y = "Total Penjualan") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$discount_analysis <- renderPlotly({
    discount_data <- data() %>%
      group_by(status_diskon) %>%
      summarise(
        count = n(),
        avg_discount = mean(persentase_diskon, na.rm = TRUE)
      )
    
    p <- ggplot(discount_data, aes(x = status_diskon, y = count, fill = status_diskon)) +
      geom_bar(stat = "identity") +
      labs(title = "Penggunaan Diskon", x = "Status Diskon", y = "Jumlah") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
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
      labs(title = "Tren Registrasi Pelanggan", x = "Bulan", y = "Pelanggan Baru") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$customer_profile <- renderPlotly({
    profile_dist <- data() %>%
      group_by(profil_pelanggan) %>%
      summarise(count = n_distinct(id_pelanggan)) %>%
      arrange(desc(count))
    
    p <- ggplot(profile_dist, aes(x = "", y = count, fill = profil_pelanggan)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Distribusi Profil Pelanggan") +
      theme_void()
    
    ggplotly(p)
  })
  
  output$aov_by_age <- renderPlotly({
    aov_age <- data() %>%
      group_by(kategori_usia, id_penjualan) %>%
      summarise(order_value = first(jumlah_total), .groups = "drop") %>%
      group_by(kategori_usia) %>%
      summarise(avg_order_value = mean(order_value, na.rm = TRUE))
    
    p <- ggplot(aov_age, aes(x = kategori_usia, y = avg_order_value, fill = kategori_usia)) +
      geom_bar(stat = "identity") +
      labs(title = "Rata-rata Nilai Pesanan per Kategori Usia", 
           x = "Kategori Usia", y = "Rata-rata Nilai Pesanan") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(
      data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)