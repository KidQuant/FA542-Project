# -------------------------------------------------------------
# 0. Load Required Libraries
# -------------------------------------------------------------
library(quantmod)
library(forecast)
library(tseries)
library(rugarch)
library(TSA)
library(ggplot2)

# -------------------------------------------------------------
# 1. Get Data
# -------------------------------------------------------------
symbols <- c("NVDA", "QCOM")
getSymbols(symbols, src = "yahoo", from = "2007-01-01", to = "2025-02-10")

nvda_close <- na.omit(Cl(NVDA))
qcom_close <- na.omit(Cl(QCOM))

log_returns_nvda <- diff(log(nvda_close))[-1] * 252
log_returns_qcom <- diff(log(qcom_close))[-1] * 252


par(mfrow=c(1,2))
plot(log_returns_nvda, main="NVDA")
plot(log_returns_qcom, main="QCOM")

# -------------------------------------------------------------
# 2. Stationarity Tests
# -------------------------------------------------------------
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


AutoArimaModel=auto.arima(log_returns_nvda)
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



# -------------------------------------------------------------
# 6. GARCH FAMILY MODELS — NVDA
# -------------------------------------------------------------
returns_nvda <- na.omit(log_returns_nvda)

spec_garch_nvda <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
fit_garch_nvda <- ugarchfit(spec = spec_garch_nvda, data = returns_nvda)

spec_tgarch_nvda <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
fit_tgarch_nvda <- ugarchfit(spec = spec_tgarch_nvda, data = returns_nvda)

spec_igarch_nvda <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars     = list(omega = 0)
)
fit_igarch_nvda <- ugarchfit(spec = spec_igarch_nvda, data = returns_nvda)

# Compare
nvda_models <- list(fit_garch_nvda, fit_tgarch_nvda, fit_igarch_nvda)
nvda_model_names <- c("GARCH", "TGARCH", "IGARCH")
aic_nvda <- sapply(nvda_models, infocriteria)
colnames(aic_nvda) <- nvda_model_names
cat("\nNVDA - AIC/BIC Comparison:\n")
print(round(aic_nvda, 4))

plot(fit_garch_nvda, which = 3)

# -------------------------------------------------------------
# 7. GARCH FAMILY MODELS — QCOM
# -------------------------------------------------------------
returns_qcom <- na.omit(log_returns_qcom)

spec_garch_qcom <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
fit_garch_qcom <- ugarchfit(spec = spec_garch_qcom, data = returns_qcom)

spec_tgarch_qcom <- ugarchspec(
  variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
fit_tgarch_qcom <- ugarchfit(spec = spec_tgarch_qcom, data = returns_qcom)

spec_igarch_qcom <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std",
  fixed.pars     = list(omega = 0)
)
fit_igarch_qcom <- ugarchfit(spec = spec_igarch_qcom, data = returns_qcom)

# Compare
qcom_models <- list(fit_garch_qcom, fit_tgarch_qcom, fit_igarch_qcom)
qcom_model_names <- c("GARCH", "TGARCH", "IGARCH")
aic_qcom <- sapply(qcom_models, infocriteria)
colnames(aic_qcom) <- qcom_model_names
cat("\nQCOM - AIC/BIC Comparison:\n")
print(round(aic_qcom, 4))

plot(fit_tgarch_qcom, which = 3) 


# -------------------------------------------------------------
# 8. VAR MODEL — Multi-Asset Analysis (NVDA & QCOM)
# -------------------------------------------------------------
library(vars)

combined_returns <- na.omit(cbind(log_returns_nvda, log_returns_qcom))
colnames(combined_returns) <- c("NVDA", "QCOM")

# Lag selection
VARselect(combined_returns, lag.max = 10, type = "const")

# Estimate VAR with optimal lag (e.g., p=1 from output)
var_model <- VAR(combined_returns, p = 1, type = "const")
summary(var_model)

# Granger causality tests
causality(var_model, cause = "NVDA")
causality(var_model, cause = "QCOM")

# -------------------------------------------------------------
# 9. Value-at-Risk (VaR) Analysis
# -------------------------------------------------------------
alpha <- 0.05  # 95% confidence

# ----- 9.1 Parametric VaR (Normal & Student-t) for NVDA -----
mu_nvda <- mean(returns_nvda)
sd_nvda <- sd(returns_nvda)
df_t_nvda <- fit_garch_nvda@fit$coef["shape"]

# Normal
var_nvda_normal <- -(mu_nvda + qnorm(alpha) * sd_nvda)
# t-distribution
scale_t_nvda <- sd_nvda * sqrt((df_t_nvda - 2)/df_t_nvda)
var_nvda_t <- -(mu_nvda + qt(alpha, df = df_t_nvda) * scale_t_nvda)

# ----- 9.2 Historical Simulation VaR -----
var_nvda_hist <- -quantile(returns_nvda, probs = alpha)

# ----- 9.3 GARCH-based VaR -----
garch_fc_nvda <- ugarchforecast(fit_garch_nvda, n.ahead = 1)
sigma_garch_nvda <- sigma(garch_fc_nvda)
var_nvda_garch <- - (qnorm(alpha) * sigma_garch_nvda)

# ----- Print Results -----
cat("\n--- NVDA VaR Estimates (95%) ---\n")
cat("Parametric Normal VaR:", round(var_nvda_normal * 100, 3), "%\n")
cat("Parametric t VaR:", round(var_nvda_t * 100, 3), "%\n")
cat("Historical Simulation VaR:", round(var_nvda_hist * 100, 3), "%\n")
cat("GARCH-based VaR (1-day):", round(var_nvda_garch * 100, 3), "%\n")


# ----- Repeat for QCOM -----
mu_qcom <- mean(returns_qcom)
sd_qcom <- sd(returns_qcom)
df_t_qcom <- fit_garch_qcom@fit$coef["shape"]

var_qcom_normal <- -(mu_qcom + qnorm(alpha) * sd_qcom)
scale_t_qcom <- sd_qcom * sqrt((df_t_qcom - 2)/df_t_qcom)
var_qcom_t <- -(mu_qcom + qt(alpha, df = df_t_qcom) * scale_t_qcom)
var_qcom_hist <- -quantile(returns_qcom, probs = alpha)
garch_fc_qcom <- ugarchforecast(fit_garch_qcom, n.ahead = 1)
sigma_garch_qcom <- sigma(garch_fc_qcom)
var_qcom_garch <- - (qnorm(alpha) * sigma_garch_qcom)

cat("\n--- QCOM VaR Estimates (95%) ---\n")
cat("Parametric Normal VaR:", round(var_qcom_normal * 100, 3), "%\n")
cat("Parametric t VaR:", round(var_qcom_t * 100, 3), "%\n")
cat("Historical Simulation VaR:", round(var_qcom_hist * 100, 3), "%\n")
cat("GARCH-based VaR (1-day):", round(var_qcom_garch * 100, 3), "%\n")
