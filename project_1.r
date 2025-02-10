install.packages("quantmod")

library("quantmod")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

symbols <- c("AAPL", "ADBE", "AMD", "EBAY", "HPQ", "IBM", "JNPR", "MSFT", "ORCL", "QCOM")
getSymbols(symbols)

# Convert AAPL to a regular time series and plot
aapl_prices <- as.numeric(AAPL[, "AAPL.Close"])
aapl_dates <- index(AAPL)
plot(aapl_dates, aapl_prices, 
     type = "l",
     main = "AAPL Stock Price",
     xlab = "Date",
     ylab = "Price",
     col = "blue")
grid()

# Convert AMD to a regular time series and plot
amd_prices <- as.numeric(AMD[, "AMD.Close"])
amd_dates <- index(AMD)
plot(amd_dates, amd_prices, 
     type = "l",
     main = "AMD Stock Price",
     xlab = "Date",
     ylab = "Price",
     col = "red")
grid()


# Convert EBAY to a regular time series and plot
ebay_prices <- as.numeric(EBAY[, "EBAY.Close"])
ebay_dates <- index(EBAY)
plot(ebay_dates, ebay_prices, 
     type = "l",
     main = "EBAY Stock Price",
     xlab = "Date",
     ylab = "Price",
     col = "green")
grid()

dev.off()

par(mfrow = c(2, 3))

# Define a function to plot histograms
plot_histogram <- function(data, symbol, color) {
  hist(as.numeric(data[, paste0(symbol, ".Close")]),
       main = paste(symbol, "Histogram"),
       xlab = "Price",
       col = color,
       border = "black")
}

par(mfrow = c(2, 3))
# Plot histograms for each symbol
plot_histogram(AAPL, "AAPL", "blue")
plot_histogram(ADBE, "ADBE", "purple")
plot_histogram(AMD, "AMD", "red")
plot_histogram(EBAY, "EBAY", "green")
plot_histogram(HPQ, "HPQ", "orange")
plot_histogram(IBM, "IBM", "brown")


