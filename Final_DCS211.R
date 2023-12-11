# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(stringr)

# Set working directory to where CSV file is stored
setwd('[Insert your working directory here]')

# Read the data
sp500_data <- read_csv("sp500_data.csv")
ioo_data <- read_csv("ioo_data.csv")

# Exploratory Data Analysis
# EDA
summary(sp500_data)
summary(ioo_data)

# Convert Date column to Date type
sp500_data$Date <- as.Date(sp500_data$Date)
ioo_data$Date <- as.Date(ioo_data$Date)

# Indexing by Date and filtering for data from 2000 onwards
sp500_data <- sp500_data %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))
ioo_data <- ioo_data %>% arrange(Date) %>% filter(Date >= as.Date("2000-01-01"))

# Basic Line Graph for S&P 500 from 2000 onwards
ggplot(sp500_data, aes(x = Date, y = Close)) +
  geom_line() +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs(title = "S&P 500 Over Time (From 2000)", x = "Date", y = "Closing Price")

# Basic Line Graph for IOO World Index from 2000 onwards
ggplot(ioo_data, aes(x = Date, y = Close)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "IOO World Index Over Time (From 2000)", x = "Date", y = "Closing Price")

# Create a data frame with recession periods
recession_periods <- data.frame(
  Start = as.Date(c("2001-01-01", "2007-08-01", "2020-02-01")),
  End = as.Date(c("2002-10-01", "2009-01-01", "2020-04-01")),
  Recession = c("Dot-com bubble burst", "Global Financial Crisis", "COVID-19 pandemic")
)

# Plot with recessions highlighted (S&P 500)
ggplot(sp500_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_rect(data = recession_periods, inherit.aes = FALSE,
            aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Recession),
            alpha = 0.2) +
  scale_fill_manual(values = c("Dot-com bubble burst" = "orange",
                               "Global Financial Crisis" = "red",
                               "COVID-19 pandemic" = "purple")) +
  scale_x_date(limits = c(as.Date("2000-01-01"), max(sp500_data$Date)), date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "S&P 500 with Recessions Highlighted (From 2000)", x = "Date", y = "Closing Price") +
  theme_minimal()  # Optional: Set a minimal theme for the plot


#Now, I will do the same thing for (S&P100)

ggplot(ioo_data, aes(x = Date, y = Close)) +
  geom_line() +
  geom_rect(data = recession_periods, inherit.aes = FALSE,
            aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = Recession),
            alpha = 0.2) +
  scale_fill_manual(values = c("Dot-com bubble burst" = "orange",
                               "Global Financial Crisis" = "red",
                               "COVID-19 pandemic" = "purple")) +
  scale_x_date(limits = c(as.Date("2000-01-01"), max(ioo_data$Date)), date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "S&P 100 with Recessions Highlighted (From 2000)", x = "Date", y = "Closing Price") +
  theme_minimal()  # Optional: Set a minimal theme for the plot

# Assuming that the recession_periods dataframe is already created as before

# Function to calculate the percentage drop from peak to trough within the recession period
calculate_percentage_drop <- function(data, recession_periods) {
  drops <- vector("numeric", length(recession_periods$Recession))
  names(drops) <- recession_periods$Recession
  
  for (i in seq_along(drops)) {
    pre_recession_peak <- max(data$Close[data$Date < recession_periods$Start[i]], na.rm = TRUE)
    recession_trough <- min(data$Close[data$Date >= recession_periods$Start[i] & data$Date <= recession_periods$End[i]], na.rm = TRUE)
    drops[i] <- (pre_recession_peak - recession_trough) / pre_recession_peak * 100
  }
  
  return(drops)
}

# Calculate percentage drops for S&P 500 and S&P Global 100
sp500_drops <- calculate_percentage_drop(sp500_data, recession_periods)
ioo_drops <- calculate_percentage_drop(ioo_data, recession_periods)

print(sp500_drops)
print(ioo_drops)

# Create a combined data frame for plotting
drop_data <- data.frame(
  Index = rep(c("S&P 500", "S&P Global 100"), each = length(sp500_drops)),
  Recession = rep(names(sp500_drops), 2),
  Percent_Drop = c(sp500_drops, ioo_drops)
)



# Make sure the drop_data is ordered chronologically for the recessions
drop_data$Recession <- factor(drop_data$Recession, levels = c("Dot-com Bubble", "Global Financial Crisis", "COVID-19 Pandemic"))

# Plot the percentage drops using a more professional color palette
ggplot(drop_data, aes(x = Recession, y = Percent_Drop, fill = Index)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +  # Using a ColorBrewer palette for a professional look
  labs(title = "Percentage Drops During Recessions", x = "Recession", y = "Percentage Drop (%)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())  # Remove legend title and place at bottom



# Function to calculate recovery time for each recession
calculate_recovery_time <- function(data, recession_periods) {
  recovery_times <- data.frame(
    Recession = character(),
    PeakDate = as.Date(character()),
    TroughDate = as.Date(character()),
    RecoveryDate = as.Date(character()),
    DaysToRecover = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(recession_periods)) {
    # Find peak before the recession
    peak_data <- data %>% filter(Date < recession_periods$Start[i])
    peak_date <- peak_data$Date[which.max(peak_data$Close)]
    peak <- max(peak_data$Close, na.rm = TRUE)
    
    # Find trough during the recession
    trough_data <- data %>% filter(Date >= recession_periods$Start[i], Date <= recession_periods$End[i])
    trough_date <- trough_data$Date[which.min(trough_data$Close)]
    trough <- min(trough_data$Close, na.rm = TRUE)
    
    # Find the recovery point
    recovery_data <- data %>% filter(Date > trough_date, Close >= peak)
    if (nrow(recovery_data) > 0) {
      recovery_date <- min(recovery_data$Date)
      days_to_recover <- as.numeric(recovery_date - trough_date)
    } else {
      # If there is no recovery within the data, set to NA
      recovery_date <- NA
      days_to_recover <- NA
    }
    
    # Append to the recovery times dataframe
    recovery_times <- rbind(recovery_times, data.frame(
      Recession = recession_periods$Recession[i],
      PeakDate = peak_date,
      TroughDate = trough_date,
      RecoveryDate = recovery_date,
      DaysToRecover = days_to_recover
    ))
  }
  
  return(recovery_times)
}

# Calculate recovery times for S&P 500
sp500_recovery <- calculate_recovery_time(sp500_data, recession_periods)

# Calculate recovery times for S&P 100 (represented by ioo_data)
sp100_recovery <- calculate_recovery_time(ioo_data, recession_periods)

print(sp500_recovery)
print(sp100_recovery)

# Add an 'Index' column to each dataframe
sp500_recovery$Index <- 'S&P 500'
sp100_recovery$Index <- 'S&P 100'

# Combine the dataframes
combined_recovery <- rbind(sp500_recovery, sp100_recovery)

print(head(combined_recovery))


# Plot the recovery times with professional colors and adjusted x-axis labels
# Corrected Plot for Recovery Times
ggplot(combined_recovery, aes(x = factor(Recession, levels = c("Dot-com bubble burst", "Global Financial Crisis", "COVID-19 pandemic")), y = DaysToRecover, fill = Index)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +  # Using a professional color palette from RColorBrewer
  labs(title = "Days to Recover Post-Recession", x = "Recession", y = "Days to Recover") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal x-axis text for readability
  )
