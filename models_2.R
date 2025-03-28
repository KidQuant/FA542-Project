# Load required libraries
library(quantmod)
library(forecast)
library(tidyverse)
install.packages("TSA")
library(TSA)
library(quantmod)
library(tseries)
library(ggplot2)


# Get NVDA stock data
getSymbols("NVDA", src = "yahoo", to = "2025-02-10")
nvda_close <- na.omit(Cl(NVDA))  # Use closing prices

# Log-transform and difference to make stationary
log_returns <- diff(log(nvda_close))[-1]

# Define range for AR and MA terms
p_max <- 10
q_max <- 10

# Initialize dataframe to store AIC values
aic_table <- expand.grid(p = 0:p_max, q = 0:q_max) %>%
  mutate(AIC = NA)

# Fit ARIMA models and record AIC
for (i in 1:nrow(aic_table)) {
  p <- aic_table$p[i]
  q <- aic_table$q[i]
  
  model <- tryCatch({
    Arima(log_returns, order = c(p, 0, q), include.mean = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    aic_table$AIC[i] <- AIC(model)
  }
}

# Arrange and print table
aic_table <- aic_table %>% arrange(AIC)
print(aic_table)

par(mfrow=c(1,2))

acf(log_returns, main = "ACF of NVDA Log Returns", lag.max = 50)
pacf(log_returns, main = "PACF of NVDA Log Returns", lag.max = 50)

eacf_result <- eacf(log_returns)

##############################
# QCOM
#############################

getSymbols("QCOM", src = "yahoo", to = "2025-02-10")
qcom_close <- na.omit(Cl(QCOM))  # Use closing prices

p_max <- 10
q_max <- 10

log_returns <- diff(log(qcom_close))[-1]

eacf_result <- eacf(log_returns)

acf(log_returns, main = "ACF of QCOM Log Returns", lag.max = 50)
pacf(log_returns, main = "PACF of QCOM Log Returns", lag.max = 50)

# Initialize dataframe to store AIC values
aic_table <- expand.grid(p = 0:p_max, q = 0:q_max) %>%
  mutate(AIC = NA)

# Fit ARIMA models and record AIC
for (i in 1:nrow(aic_table)) {
  p <- aic_table$p[i]
  q <- aic_table$q[i]
  
  model <- tryCatch({
    Arima(log_returns, order = c(p, 0, q), include.mean = FALSE)
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    aic_table$AIC[i] <- AIC(model)
  }
}

# Arrange and print table
aic_table <- aic_table %>% arrange(AIC)
print(aic_table)


####################################
#
###################################

symbols <- c("QCOM", "NVDA")

getSymbols(symbols, src = "yahoo", to = "2025-02-10")
nvda_close <- na.omit(Cl(NVDA))  # Use closing prices
qcom_close <- na.omit(Cl(QCOM))
log_returns_nvda <- diff(log(nvda_close))[-1]
log_returns_qcom <- diff(log(qcom_close))[-1]

par(mfrow=c(1,2))
plot(log_returns_nvda, main="NVDA")
plot(log_returns_qcom, main="QCOM")

adf.test(log_returns_nvda)

adf.test(log_returns_qcom)

####################################
# NVDA Models
#####################################

arimaModel1_0 <- arima(log_returns_nvda, order=c(1,0,0))
arimaModel2_0 <- arima(log_returns_nvda, order=c(2,0,0))
arimaModel0_1 <- arima(log_returns_nvda, order=c(0,0,1))
arimaModel0_2 <- arima(log_returns_nvda, order=c(0,0,2))
arimaModel1_1 <- arima(log_returns_nvda, order=c(1,0,1))
arimaModel1_2 <- arima(log_returns_nvda, order=c(1,0,2))
print(arimaModel1_0);print(arimaModel2_0);print(arimaModel0_1);print(arimaModel0_2);
print(arimaModel1_1);print(arimaModel1_2);


AutoArimaModel=auto.arima(log_nvda_returns)
AutoArimaModel

forecast1=predict(arimaModel0_2, h=10)
forecast2=predict(arimaModel1_1, 10)
forecast3=predict(arimaModel1_2, 10)

checkresiduals(arimaModel1_2)

autoplot(forecast(AutoArimaModel,h=10), ylab="Log Returns")



####################################
# QCOM MOdels
#####################################

arimaModel1_0 <- arima(log_returns_qcom, order=c(1,0,0))
arimaModel2_0 <- arima(log_returns_qcom, order=c(2,0,0))
arimaModel0_1 <- arima(log_returns_qcom, order=c(0,0,1))
arimaModel0_2 <- arima(log_returns_qcom, order=c(0,0,2))
arimaModel1_1 <- arima(log_returns_qcom, order=c(1,0,1))
print(arimaModel1_0);print(arimaModel2_0);print(arimaModel0_1);print(arimaModel0_2);
print(arimaModel1_1);


AutoArimaModel=auto.arima(log_returns_qcom)
AutoArimaModel

autoplot(forecast(AutoArimaModel,h=10), ylab="Log Returns")
