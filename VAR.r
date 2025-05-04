library(quantmod)
library(forecast)
library(tseries)
library(rugarch)
library(TSA)
library(ggplot2)
library(MTS)

symbols <- c("AMD", "QCOM")
getSymbols(symbols, src = "yahoo", from = "2007-01-01", to = "2025-02-10")


amd_returns <- monthlyReturn(AMD$AMD.Close) 
log_amd_returns <- log(amd_returns + 1) * 252

qcom_returns <- monthlyReturn(QCOM$QCOM.Close) 
log_qcom_returns <- log(qcom_returns + 1) * 252

combined_log_returns <- cbind(log_amd_returns, log_qcom_returns)
MTSplot(combined_log_returns)
ccm(combined_log_returns)
VARorder(log_qcom_returns)
VARorder(log_amd_returns)

VARselect(combined_log_returns, lag.max = 12, type = "const")
var_model <- VAR(combined_log_returns, p = 1, type = "const")
var_model
# Perform the multivariate Ljung-Box test
mq(combined_log_returns, lag = 12)
