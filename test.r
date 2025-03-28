library(tidyquant)
library(timetk)
library(ggplot2)
library(dplyr)

tickers <- c("AAPL","ADBE","NVDA", "AMD", "MSFT", "QCOM", "CSCO", "INTU")
#tickers <- c("AAPL", "NVDA")

# Dowload the stock price data

multpl_stocks <- tq_get(tickers,
                        from = "2007-01-01",
                        get = "stock.prices")








multpl_stock_daily_returns <- multpl_stocks %>%
  group_by(symbol) %>%                            # We are grouping the stocks by the stock symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')

multpl_stock_daily_returns %>%
  ggplot(aes(x = date, y = returns)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol, scales = "free_y", nrow = 2, ncol = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Returns") +
  scale_color_brewer(palette = "Set2",
                     name = "",
                     guide = FALSE) +
  theme_classic()

multpl_stock_daily_returns %>%
  ggplot(aes(x = returns)) +
  geom_histogram(bins = 100, fill = "black", color = "black", alpha = 0.7) +
  facet_wrap(~symbol, scales = "free_y", nrow = 2, ncol = 4) +
  labs(x = "Returns", y = "Frequency") +
  theme_classic()
