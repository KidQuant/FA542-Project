library(quantmod)
library(forecast)
library(tseries)
library(rugarch)
library(TSA)
library(ggplot2)
library(MTS)

symbols <- c("NVDA", "QCOM")
getSymbols(symbols, src = "yahoo", from = "2007-01-01", to = "2025-02-10")

nvda_close <- na.omit(Cl(NVDA))
qcom_close <- na.omit(Cl(QCOM))

stocks <- cbind(nvda_close, qcom_close)

log_returns_nvda <- diff(log(nvda_close))[-1] * 252
log_returns_qcom <- diff(log(qcom_close))[-1] * 252

combined_log_returns <- cbind(log_returns_nvda, log_returns_qcom)
MTSplot(combined_log_returns)
ccm(combined_log_returns)



ccf_result <- ccf(as.numeric(combined_log_returns[,1]), as.numeric(combined_log_returns[,2]), lag.max = 12, plot=TRUE)
