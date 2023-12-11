# Load necessary libraries
#install.packages('shiny')
#install.packages('ggplot2')
#install.packages('readr')
#install.packages('dplyr')
#install.packages('RColorBrewer')
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)

# Set working directory to where CSV file is stored
setwd('[Insert Your Local Working Directory Here]')

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Recession Analysis"),
  sidebarLayout(
    sidebarPanel(
      # Slider inputs for start dates of each recession
      sliderInput("startDotcom", "Start Date for Dot-com bubble (2000-2002)",
                  min = as.Date("2000-12-10"), max = as.Date("2002-10-01"), value = as.Date("2001-01-01")),
      sliderInput("startGFC", "Start Date for Global Financial Crisis (2007-2009)",
                  min = as.Date("2007-12-03"), max = as.Date("2008-12-30"), value = as.Date("2008-08-01")),
      sliderInput("startCOVID", "Start Date for COVID-19 pandemic (2020)",
                  min = as.Date("2020-02-03"), max = as.Date("2020-04-01"), value = as.Date("2020-02-01")),
      sliderInput("dateRange", "Select Date Range:",
                  min = as.Date("2000-12-08"), 
                  max = as.Date("2022-12-30"),
                  value = c(as.Date("2000-01-01"), as.Date("2022-12-31")),
                  timeFormat = "%Y-%m-%d")
    ),
    mainPanel(
      plotOutput("recoveryPlot"),
      plotOutput("sp500Plot"),
      plotOutput("iooPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read and preprocess data
  sp500_data <- read_csv("sp500_data.csv", show_col_types = FALSE)
  ioo_data <- read_csv("ioo_data.csv", show_col_types = FALSE)
  sp500_data$Date <- as.Date(sp500_data$Date)
  ioo_data$Date <- as.Date(ioo_data$Date)
  sp500_data <- sp500_data %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
  ioo_data <- ioo_data %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
  
  # Function to calculate recovery time
  calculate_recovery_time <- function(data, recession_periods, indexName) {
    recovery_times <- data.frame(
      Recession = character(),
      PeakDate = as.Date(character()),
      TroughDate = as.Date(character()),
      RecoveryDate = as.Date(character()),
      DaysToRecover = numeric(),
      Index = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(recession_periods)) {
      peak_data <- data %>% filter(Date < recession_periods$Start[i])
      trough_data <- data %>% filter(Date >= recession_periods$Start[i], Date <= recession_periods$End[i])
      
      if (nrow(peak_data) > 0 && nrow(trough_data) > 0) {
        peak_date <- peak_data$Date[which.max(peak_data$Close)]
        peak <- max(peak_data$Close, na.rm = TRUE)
        trough_date <- trough_data$Date[which.min(trough_data$Close)]
        trough <- min(trough_data$Close, na.rm = TRUE)
        
        recovery_data <- data %>% filter(Date > trough_date, Close >= peak)
        if (nrow(recovery_data) > 0) {
          recovery_date <- min(recovery_data$Date)
          days_to_recover <- as.numeric(recovery_date - trough_date)
        } else {
          recovery_date <- NA
          days_to_recover <- NA
        }
      } else {
        peak_date <- NA
        trough_date <- NA
        recovery_date <- NA
        days_to_recover <- NA
      }
      
      recovery_times <- rbind(recovery_times, data.frame(
        Recession = recession_periods$Recession[i],
        PeakDate = peak_date,
        TroughDate = trough_date,
        RecoveryDate = recovery_date,
        DaysToRecover = days_to_recover,
        Index = indexName
      ))
    }
    return(recovery_times)
  }
  
  
  # Reactive expression to recalculate recovery times
  recoveryData <- reactive({
    recession_periods <- data.frame(
      Start = as.Date(c(input$startDotcom, input$startGFC, input$startCOVID)),
      End = as.Date(c("2002-10-01", "2009-01-01", "2020-04-01")),
      Recession = c("Dot-com bubble burst", "Global Financial Crisis", "COVID-19 pandemic")
    )
    
    sp500_recovery <- calculate_recovery_time(sp500_data, recession_periods, "S&P 500")
    sp100_recovery <- calculate_recovery_time(ioo_data, recession_periods, "S&P 100")
    combined_recovery <- rbind(sp500_recovery, sp100_recovery)
    
    return(combined_recovery)
  })
  
  
  # Plot output
  output$recoveryPlot <- renderPlot({
    ggplot(recoveryData(), aes(x = factor(Recession), y = DaysToRecover, fill = Index)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Days to Recover Post-Recession", x = "Recession", y = "Days to Recover") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5))
  })

# Render the updated plots with filtered data
  output$sp500Plot <- renderPlot({
    filtered_sp500_data <- sp500_data %>% 
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    ggplot(filtered_sp500_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(title = "S&P 500 Over Time", x = "Date", y = "Closing Price")
  })

  output$iooPlot <- renderPlot({
    filtered_ioo_data <- ioo_data %>% 
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    ggplot(filtered_ioo_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(title = "S&P 100 Over Time", x = "Date", y = "Closing Price")
  })
}
# Run the application
shinyApp(ui, server)
