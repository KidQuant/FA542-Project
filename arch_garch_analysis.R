# Install and load packages
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)

# Define analysis function
analyze_volatility <- function(ticker, arch_order = 1, garch_order = c(1,1)) {
  # Get real price data
  getSymbols(ticker, src = "yahoo", auto.assign = TRUE)
  returns <- na.omit(ROC(Ad(get(ticker)), type = "discrete"))
  
  # Create model specifications
  models <- list(
    ARCH = ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(arch_order, 0)),
      mean.model = list(armaOrder = c(0,0)),
      distribution.model = "std"
    ),
    GARCH = ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = garch_order),
      mean.model = list(armaOrder = c(0,0)),
      distribution.model = "std"
    )
  )
  
  # Fit models and run diagnostics
  results <- lapply(names(models), function(model_type) {
    cat("\n\n====", ticker, model_type, "Model ====\n")
    
    # Fit model
    fit <- ugarchfit(models[[model_type]], data = returns)
    show(fit)
    
    # Get standardized residuals
    std_res <- residuals(fit, standardize = TRUE)
    
    # Ljung-Box tests
    cat("\nLjung-Box Tests:\n")
    print(Box.test(std_res, lag = 10, type = "Ljung-Box"))
    print(Box.test(std_res^2, lag = 10, type = "Ljung-Box"))
    
    # ACF/PACF plots
    par(mfrow = c(2,2))
    acf(std_res, main = paste(ticker, model_type, "Residuals ACF"))
    pacf(std_res, main = paste(ticker, model_type, "Residuals PACF"))
    acf(std_res^2, main = paste(ticker, model_type, "Squared Residuals ACF"))
    pacf(std_res^2, main = paste(ticker, model_type, "Squared Residuals PACF"))
    
    return(fit)
  })
  
  names(results) <- names(models)
  return(results)
}

# Run analysis for both stocks
nvda_results <- analyze_volatility("NVDA")
qcom_results <- analyze_volatility("QCOM")
