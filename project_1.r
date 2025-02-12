library("quantmod")
library("fBasics")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

symbols <- c("ADBE", "AMD", "MSFT", "QCOM", "EBAY", "ORCL")
getSymbols(symbols)

msft_returns <- dailyReturn(MSFT$MSFT.Close)
msft_returns <- msft_returns * 100
basicStats(msft_returns)
log_msft_returns <- log(msft_returns + 1)
log_msft_returns <- log_msft_returns * 100
basicStats(log_msft_returns)

amd_returns <- dailyReturn(AMD$AMD.Close)
amd_returns <- amd_returns * 100
basicStats(amd_returns)
log_amd_returns <- log(amd_returns + 1)
log_amd_returns <- log_amd_returns * 100
basicStats(log_amd_returns)

adbe_returns <- dailyReturn(ADBE$ADBE.Close)
adbe_returns <- adbe_returns * 100
basicStats(adbe_returns)
log_adbe_returns <- log(adbe_returns + 1)
log_adbe_returns <- log_adbe_returns * 100
basicStats(log_adbe_returns)


qcom_returns <- dailyReturn(QCOM$QCOM.Close)
qcom_returns <- qcom_returns * 100
basicStats(qcom_returns)
log_qcom_returns <- log(qcom_returns + 1)
log_qcom_returns <- log_qcom_returns * 100
basicStats(log_qcom_returns)

orcl_returns <- dailyReturn(ORCL$ORCL.Close)
log_orcl_returns <- log(orcl_returns + 1)
basicStats(log_orcl_returns)


typeof(symbol)
head(MSFT)

normalTest(log_msft_returns,method="jb")
normalTest(log_amd_returns,method="jb")
normalTest(log_adbe_returns,method="jb")
normalTest(log_qcom_returns,method="jb")




