import yfinance as yf

# Define the ticker symbol for the S&P 500
sp500_symbol = "^GSPC"

# Fetch the latest available data
sp500_data = yf.Ticker(sp500_symbol)

# Get historical market data (daily closing prices)
hist_data = sp500_data.history(period="max", interval="1d")['Close']

# Saving the closing price data to CSV
hist_data.to_csv("sp500_data.csv")
