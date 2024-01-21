library(shiny)
library(plotly)
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Stock Price Visualization"),
  sidebarLayout(
    sidebarPanel(
      # Initialize slider with placeholder values
      sliderInput("dateRange", "Select Date Range:",
                  min = as.Date("2000-01-01"), 
                  max = as.Date("2000-01-02"), 
                  value = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
                  timeFormat = "%Y-%m-%d")
    ),
    mainPanel(
      plotlyOutput("sp500Plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read and preprocess data
  sp500_data <- read_csv("sp500_data.csv", show_col_types = FALSE)
  sp500_data$Date <- as.Date(sp500_data$Date)
  
  # Update sliderInput with actual data range
  observe({
    min_date <- min(sp500_data$Date)
    max_date <- Sys.Date()  # Today's date
    updateSliderInput(session, "dateRange", 
                      min = min_date, 
                      max = max_date, 
                      value = c(min_date, max_date))
  })
  
  # S&P 500 line graph
  output$sp500Plot <- renderPlotly({
    filtered_sp500_data <- sp500_data %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    
    plot_ly(filtered_sp500_data, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',
            line = list(width = 2, shape = "spline"),
            hoverinfo = 'text', 
            text = ~paste('<b>Date:</b>', format(Date, "%Y-%m-%d"), '<br><b>Close:</b>', round(Close, 4)),
            hoverlabel = list(bgcolor = 'white', font = list(size = 12))) %>%
      layout(title = 'S&P 500 Performance',
             xaxis = list(title = 'Date', showline = TRUE, showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = 'Close', showline = TRUE, showgrid = FALSE, zeroline = FALSE),
             plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(b=60)) # adds margin to separate plots
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
