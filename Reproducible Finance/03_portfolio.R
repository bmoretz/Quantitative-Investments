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
library(scales)

path.data <- "D:/Projects/MSDS-RiskAnalytics/datasets"
setwd(path.data)

symbols <- c("SPY", "EFA", "IJS", "EEM", "AGG")

# Load price data
prices <- read_csv("Reproducible Finance.csv",
                   col_types =
                      cols(date =
                            col_date(format = "%Y-%m-%d"))) %>%
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

# convert to monthly returns, tidyquant.

asset_returns_tq <-
  prices %>%
  tk_tbl(preserve_index = T,
         rename_index = "date") %>%
         gather(asset, prices, - date) %>%
         group_by(asset) %>%
         tq_transmute(mutate_fun = periodReturn,
              period = "monthly",
              type = "log") %>%
              spread(asset, monthly.returns) %>%
              select(date, symbols) %>%
              slice(-1)

head(asset_returns_tq)

# convert from wide to tidy/long format.

asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, - date) %>%
  group_by(asset)

head(asset_returns_long)

# Asset Weights

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

stopifnot(sum(w) == 1)

tibble(w, symbols) %>%
  summarise(total_weights = sum(w))

# Portfolio returns, long-hand.

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]

asset1 <- asset_returns_xts[, 1]
asset2 <- asset_returns_xts[, 2]
asset3 <- asset_returns_xts[, 3]
asset4 <- asset_returns_xts[, 4]
asset5 <- asset_returns_xts[, 5]

portfolio_returns_byhand <-
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5)

names(portfolio_returns_byhand) <- "returns"

# Portfolio returns, xts.

portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly, 3)

asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                            asset == symbols[2] ~ w[2],
                            asset == symbols[3] ~ w[3],
                            asset == symbols[4] ~ w[4],
                            asset == symbols[5] ~ w[5])) %>%
  head(3)

# Portfolio returns, dplyr.

portfolio_returns_dplyr_byhand <-
  asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                            asset == symbols[2] ~ w[2],
                            asset == symbols[3] ~ w[3],
                            asset == symbols[4] ~ w[4],
                            asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarize(returns = sum(weighted_returns))

head(portfolio_returns_dplyr_byhand)

# Portfolio returns, tidyquant.

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

head(portfolio_returns_tq_rebalanced_monthly, 3)

portfolio_returns_dplyr_byhand %>%
  rename(tidyverse = returns) %>%
  mutate(equation = coredata(portfolio_returns_byhand),
         tq = portfolio_returns_tq_rebalanced_monthly$returns,
         xts = coredata(portfolio_returns_xts_rebalanced_monthly)) %>%
         mutate_if(is.numeric, funs(round(., 3))) %>%
         head()

# Visualize portfolio returns

# line chart

highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = F) %>%
  hc_scrollbar(enabled = F) %>%
  hc_legend(enabled = T) %>%
  hc_exporting(enabled = T)

# histogram

hc_portfolio <-
  hist(portfolio_returns_xts_rebalanced_monthly$returns,
       breaks = 50,
       plot = F)

hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
       hc_title(text = "Portfolio Returns Distribution") %>%
       hc_add_theme(hc_theme_flat()) %>%
       hc_exporting(enabled = T)

# ggplot

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue") +
  xlab("date") +
  ylab("monthly return") +
  ggtitle("Portfolio Returns Scatter") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(breaks = pretty_breaks(n = 6))

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = 0.005,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))


asset_returns_long %>%
  ggplot(aes(x = returns,
            fill = asset)) +
  geom_histogram(alpha = 0.15,
                  binwidth = 0.01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,
                  fill = "cornflowerblue",
                  binwidth = 0.01) +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = 0.01,
                 color = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("date") +
  ylab("monthly return") +
  ggtitle("Portfolio Histogram and Density") +
  theme_update(plot.title = element_text(hjust = 0.5))