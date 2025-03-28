library(timeSeries)
library(timeDate)
library(quantmod)
library(fUnitRoots)
library(forecast)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

symbols <- c("NVDA")
data_w<-getSymbols("NVDA",auto.assign = FALSE,from="2015-01-01",to=Sys.Date())
data1<-data.frame(data_w,date=as.Date(rownames(data.frame(data_w))))
head(data1)
NVDA = timeSeries(data1$NVDA.Close,data1$date)
View(NVDA)
NVDA_D <- diff(NVDA)
plot(NVDA_D)

adfTest(NVDA_D,lags = 0,type = "c")
par(mfrow=c(1,1))

Acf(NVDA_D,main="ACF PLOT")
Pacf(NVDA_D,main="PACF PLOT")

#####################################
# Seasonality
######################################
par(mfrow=c(2,2))
ts.plot(NVDA$NVDA.Close)
ts.plot(diff(NVDA$NVDA.Close))
ts.plot(diff(NVDA$NVDA.Close,lag = 4))
ts.plot(diff(diff(NVDA$NVDA.Close,lag = 4)))


par(mfrow=c(2,2))
acf(NVDA$NVDA.Close)
acf(diff(NVDA$NVDA.Close))
acf(diff(NVDA$NVDA.Close,lag = 4))
acf(diff(diff(NVDA$NVDA.Close,lag = 4)))


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
Box.test(log_nvda_returns,lag=4,type='Ljung')


par(mfrow=c(2,3))  # Set up the plotting area to have 2 rows and 2 columns

pacf(log_msft_returns, main="MSFT", lag.max=20)
pacf(log_amd_returns, main="AMD ", lag.max=20)
pacf(log_adbe_returns, main="ADBE", lag.max=20)
pacf(log_qcom_returns, main="QCOM", lag.max=20)
pacf(log_csco_returns, main="CSCO", lag.max=20)
pacf(log_intu_returns, main="INTU", lag.max=20)

par(mfrow=c(2,2))  # Set up the plotting area to have 2 rows and 2 columns

acf(na.omit(NVDA$NVDA.Close), main="ACF of NVDA Close")
acf(na.omit(diff(NVDA$NVDA.Close)), main="ACF of Differenced NVDA Close")
acf(na.omit(diff(NVDA$NVDA.Close, lag = 4)), main="ACF of Differenced NVDA Close with Lag 4")
acf(na.omit(diff(diff(NVDA$NVDA.Close, lag = 4))), main="ACF of Twice Differenced NVDA Close with Lag 4")

par(mfrow=c(1,1))  # Reset the plotting area to default
# Test the once differenced NVDA series for stationarity using the Augmented Dickey-Fuller test

adf_test_nvda <- adf.test(na.omit(diff(log_nvda_returns)), alternative = "stationary")

# The function ggtsdisplay is not recognized. It seems like a typo or a non-existent function.
# Assuming the intention was to use a generic plot function for time series display.
NVDA %>% diff() %>% diff(lag = 4) %>% plot(main="")