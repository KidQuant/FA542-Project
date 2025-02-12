
library(tidyquant)
library(timetk)
library(ggplot2)
library(dplyr)
library("fBasics")


tickers <- c("ADBE", "AMD", "MSFT", "QCOM", "IBM", "EBAY", "ORCL", "GOOG") 

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
  facet_wrap(~symbol, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Daily returns for FAANG stock") +
  labs(x = "Date", y = "Returns") +
  scale_color_brewer(palette = "Set2",
                     name = "",
                     guide = FALSE) +
  theme_classic()

multpl_stock_daily_returns %>%
  mutate(returns = if_else(date == "2007-01-31", 0, returns)) %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns, color = symbol)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Cumulative returns for all since 2007") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set1",
                     name = "") +
  theme_bw()


