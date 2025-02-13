library("quantmod")
library("fBasics")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

symbols <- c("ADBE", "AMD", "MSFT", "QCOM", "CSCO", "INTU")
getSymbols(symbols)
#########################
# MSFT
##########################


msft_returns <- dailyReturn(MSFT$MSFT.Close) * 100
basicStats(msft_returns)

log_msft_returns <- log(msft_returns + 1)

basicStats(log_msft_returns)
normalTest(log_msft_returns,method="jb")

#########################
# AMD
##########################

amd_returns <- dailyReturn(AMD$AMD.Close) *100
basicStats(amd_returns)
log_amd_returns <- log(amd_returns + 1)

basicStats(log_amd_returns)

normalTest(log_amd_returns,method="jb")

#########################
# ADBE
##########################

adbe_returns <- dailyReturn(ADBE$ADBE.Close) *100
basicStats(adbe_returns)
log_adbe_returns <- log(adbe_returns + 1)

basicStats(log_adbe_returns)

normalTest(log_adbe_returns,method="jb")

#########################
# QCOM
##########################

qcom_returns <- dailyReturn(QCOM$QCOM.Close) *100
basicStats(qcom_returns)
log_qcom_returns <- log(qcom_returns + 1)

basicStats(log_qcom_returns)
normalTest(log_qcom_returns,method="jb")

#########################
# CSCO
##########################

csco_returns <- dailyReturn(CSCO$CSCO.Close) * 100
basicStats(csco_returns)

log_csco_returns <- log(csco_returns + 1)

basicStats(log_csco_returns)

normalTest(log_csco_returns,method="jb")

#########################
# INTU
##########################

intu_returns <- dailyReturn(INTU$INTU.Close) *100
basicStats(intu_returns)
log_intu_returns <- log(intu_returns + 1)

basicStats(log_intu_returns)









par(mfrow=c(2,3))  # Set up the plotting area to have 2 rows and 2 columns

acf(log_msft_returns, main="MSFT", lag.max=20)
acf(log_amd_returns, main="AMD ", lag.max=20)
acf(log_adbe_returns, main="ADBE", lag.max=20)
acf(log_qcom_returns, main="QCOM", lag.max=20)
acf(log_csco_returns, main="CSCO", lag.max=20)
acf(log_intu_returns, main="INTU", lag.max=20)

par(mfrow=c(1,1))  # Reset the plotting area to default


# Save the plot to the specified directory
plot_path <- "C:/Users/drebi/dev/FA542Project/Project Report #1/plots/acf_plots.png"
png(filename=plot_path)

par(mfrow=c(1,4))  # Set up the plotting area to have 1 row and 4 columns

acf(log_msft_returns, main="ACF of MSFT Log Returns", lag.max=20)
acf(log_amd_returns, main="ACF of AMD Log Returns", lag.max=20)
acf(log_adbe_returns, main="ACF of ADBE Log Returns", lag.max=20)
acf(log_qcom_returns, main="ACF of QCOM Log Returns", lag.max=20)

par(mfrow=c(1,1))  # Reset the plotting area to default

Box.test(log_amd_returns,lag=17,type='Ljung')
Box.test(log_adbe_returns,lag=6,type='Ljung')
Box.test(log_adbe_returns,lag=12,type='Ljung')
Box.test(log_qcom_returns,lag=19,type='Ljung')
Box.test(log_qcom_returns,lag=20,type='Ljung')
Box.test(log_csco_returns,lag=14,type='Ljung')
Box.test(log_csco_returns,lag=9,type='Ljung')
Box.test(log_intu_returns,lag=18,type='Ljung')


par(mfrow=c(2,3))  # Set up the plotting area to have 2 rows and 2 columns

pacf(log_msft_returns, main="MSFT", lag.max=20)
pacf(log_amd_returns, main="AMD ", lag.max=20)
pacf(log_adbe_returns, main="ADBE", lag.max=20)
pacf(log_qcom_returns, main="QCOM", lag.max=20)
pacf(log_csco_returns, main="CSCO", lag.max=20)
pacf(log_intu_returns, main="INTU", lag.max=20)

par(mfrow=c(1,1))
