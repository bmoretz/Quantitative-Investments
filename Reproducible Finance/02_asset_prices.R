library(quantmod)
library(dplyr)
library(xts)
library(tidyverse)
library(tidyquant)
library(readr)
library(timetk)
library(readxl)
library(lubridate)
library(tibbletime)
library(highcharter)

path.data <- "D:/Projects/MSDS-RiskAnalytics/datasets"
setwd(path.data)

symbols <- c("SPY", "EFA", "IJS", "EEM", "AGG")

# Yahoo! Finance
prices <- getSymbols(symbols,
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = T,
             warnings = F) %>%
          map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

# CSV
prices <- read_csv("Reproducible Finance.csv",
                   col_types =
                     cols(date =
                            col_date(format = "%Y-%m-%d"))) %>%
  tk_xts(date_var = date)


# Excel

prices <-
  read_excel("Reproducible Finance.xlsx",
             col_types = c("text", "numeric",
                           "numeric", "numeric",
                           "numeric", "numeric")) %>%
  mutate(date = ymd(date)) %>%
  tk_xts(date_var = date)

# Verfiy Import
head(prices, 3)

# convert to monthly prices.

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = F)

head(prices_monthly)

# Convert to monthly returns, xts.

asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()

head(asset_returns_xts, 3)

# Convert to monthly returns, dplyr.

asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = F) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>%
  gather(asset, prices, - date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)

head(asset_returns_dplyr_byhand)

asset_returns_dplyr_byhand <- asset_returns_dplyr_byhand %>%
  na.omit()

head(asset_returns_dplyr_byhand)

# convert to monthly returns, tidyquant.

asset_returns_tq_builtin <-
  prices %>%
  tk_tbl(preserve_index = T,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)

head(asset_returns_tq_builtin)

# convert to monthly returns, tibbletime.

asset_returns_tbltime <-
  prices %>%
  tk_tbl(preserve_index = T,
         rename_index = "date") %>%
  # this is the tibbletime function
  as_tbl_time(index = date) %>%
  as_period(period = "monthly",
            side = "end") %>%
  gather(asset, returns, - date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)

head(asset_returns_tbltime)


asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, - date) %>%
  group_by(asset)

head(asset_returns_long)

# visualize return data


highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[, symbols[3]],
                name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[, symbols[5]],
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = F) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled = T) %>%
  hc_legend(enabled = T)


hc_hist_fun <- function(n = 1, object, color ) {
  hc_hist <- hist(object[, symbols[n]],
                  breaks = 50,
                  plot = F)
  
  hchart(hc_hist, color = color) %>%
    hc_title(text =
               paste(symbols[n],
                     "Log Returns Distribution",
                     sep = " ")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = F) %>%
    hc_legend(enabled = F)
}

hc_hist_fun(1, asset_returns_xts, "cornflowerblue")
hc_hist_fun(2, asset_returns_xts, "green")
hc_hist_fun(3, asset_returns_xts, "pink")
hc_hist_fun(4, asset_returns_xts, "purple")
hc_hist_fun(5, asset_returns_xts, "yellow")


map(1:5, hc_hist_fun, asset_returns_xts, "blue")


asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = 0.005) +
  ggtitle("Monthly Returns Since 2013") +
  theme_update(plot.title = element_text(hjust = 0.5))


asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  theme_update(plot.title = element_text(hjust = 0.5))


asset_returns_long %>%
  ggplot(aes(x = returns, colour = asset)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = 0.01) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))


asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))







# convert from wide to long format.