library("quantmod")
library("fBasics")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

symbols <- c("ADBE", "AMD", "MSFT", "QCOM", "EBAY", "ORCL", "CSCO", "INTU")
getSymbols(symbols)

# Microsoft (MSFT)
msft_returns <- dailyReturn(MSFT$MSFT.Close) * 100
basicStats(msft_returns)
log_msft_returns <- log(msft_returns + 1) * 100
basicStats(log_msft_returns)

# AMD
amd_returns <- dailyReturn(AMD$AMD.Close) * 100
basicStats(amd_returns)
log_amd_returns <- log(amd_returns + 1) * 100
basicStats(log_amd_returns)

# Adobe (ADBE)
adbe_returns <- dailyReturn(ADBE$ADBE.Close) * 100
basicStats(adbe_returns)
log_adbe_returns <- log(adbe_returns + 1) * 100
basicStats(log_adbe_returns)

# Qualcomm (QCOM)
qcom_returns <- dailyReturn(QCOM$QCOM.Close) * 100
basicStats(qcom_returns)
log_qcom_returns <- log(qcom_returns + 1) * 100
basicStats(log_qcom_returns)

# Oracle (ORCL)
orcl_returns <- dailyReturn(ORCL$ORCL.Close) * 100
basicStats(orcl_returns)
log_orcl_returns <- log(orcl_returns + 1) * 100
basicStats(log_orcl_returns)

# Cisco (CSCO)
csco_returns <- dailyReturn(CSCO$CSCO.Close) * 100
basicStats(csco_returns)
log_csco_returns <- log(csco_returns + 1) * 100
basicStats(log_csco_returns)

# Intuit (INTU)
intu_returns <- dailyReturn(INTU$INTU.Close) * 100
basicStats(intu_returns)
log_intu_returns <- log(intu_returns + 1) * 100
basicStats(log_intu_returns)

# Normality Tests (Jarque-Bera Test)
normalTest(log_msft_returns, method = "jb")
normalTest(log_amd_returns, method = "jb")
normalTest(log_adbe_returns, method = "jb")
normalTest(log_qcom_returns, method = "jb")
normalTest(log_csco_returns, method = "jb")
normalTest(log_intu_returns, method = "jb")

typeof(symbols)
head(MSFT)
