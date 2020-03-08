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

# load prices
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

# convert from wide to long format.

asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, - date) %>%
  group_by(asset)

head(asset_returns_long)

# Portfolio returns, xts.

portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly, 3)

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

# Asset Weights

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

stopifnot(sum(w) == 1)

tibble(w, symbols) %>%
  summarise(total_weights = sum(w))

# Calculate Covarariance, by hand

covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix, 5)

# Standard Deviation, by hand
sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)

sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>%
  `colnames<-`("standard deviation")

sd_matrix_algebra_percent[1,]

# SD, xts

portfolio_sd_xts_builtin <-
  StdDev(asset_returns_xts, weights = w)

portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100, 2)

portfolio_sd_xts_builtin_percent[1,]

# SD, tidyverse

portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>%
  summarise(
    sd = sd(returns),
    sd_byhand = 
      sqrt(sum((returns - mean(returns))^2) / (nrow(.)-1))) %>%
  mutate(dplyr = round(sd, 4) * 100,
         dplyr_byhand = round(sd_byhand, 4) * 100)

portfolio_sd_tidy_builtin_percent %>%
  select(dplyr, dplyr_byhand)

# SD, tidyquant

portfolio_sd_tidyquant_builtin_percent <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_performance(Ra = returns,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev) %>%
  mutate(tq_sd = round(Stdev, 4) * 100)

head(portfolio_sd_tidyquant_builtin_percent)

# SD, PerformanceAnalytics

portfolio_sd_tidy_builtin_percent %>%
  select(dplyr, dplyr_byhand) %>%
  mutate(xts_builtin = portfolio_sd_xts_builtin_percent,
         matrix = sd_matrix_algebra_percent,
         tq = portfolio_sd_tidyquant_builtin_percent$tq_sd)

### Visualization

# Portfolio returns

portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))

sd_plot <-
  sd(portfolio_returns_tq_rebalanced_monthly$returns)

mean_plot <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red = 
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue = 
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = date)) +
  
  geom_point(aes(y = hist_col_red),
             color = "red") +
  
  geom_point(aes(y = hist_col_green),
             color = "green") +
  
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  
  geom_hline(yintercept = (mean_plot - sd_plot),
             color = "purple",
             linetype = "dotted") +
  
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

# By asset

asset_returns_long %>%
  group_by(asset) %>%
  summarize(sd = 100 * sd(returns)) %>%
  add_row(asset = "Portfolio",
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>%
  ggplot(aes(x = asset,
             y = sd,
             colour = asset)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
    geom_text(
      aes(x = "Portfolio",
          y = 
            portfolio_sd_tidy_builtin_percent$dplyr + .2),
            label = "Portfolio",
          color = "cornflowerblue") +
  labs(y = "standard deviation")


asset_returns_long %>%
  group_by(asset) %>%
  summarize(expected_return = mean(returns),
            stand_dev = sd(returns)) %>%
  add_row(asset = "Portfolio",
          stand_dev = 
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expected_return = 
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>%
  
  ggplot(aes(x = stand_dev,
             y = expected_return,
             colour = asset)) +
  geom_point(size = 2) +
  geom_text(
    aes(x = 
          sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.11,
        y = 
          mean(portfolio_returns_tq_rebalanced_monthly$returns),
        label = "Portfolio")
  ) +
  xlab("expected return") +
  ylab("standard deviation") +
  ggtitle("Expected Monthly Returns versus Risk") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_update(plot.title = element_text(hjust = 0.5))

# Rolling StdDev

window <- 24

# rolling sd, xts
port_rolling_sd_xts <- 
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>%
  # omit the 23 months for which there is no rolling 24 month sd
  na.omit() %>%
  `colnames<-`("rolling_sd")

tail(port_rolling_sd_xts, 3)

# rolling sd, tidyverse

port_rolling_sd_tidy_does_not_work <-
  portfolio_returns_dplyr_byhand %>%
  mutate(rolling_sd = rollapply(returns,
                                FUN = sd,
                                width = window,
                                fill = NA)) %>%
  select(date, rolling_sd) %>%
  na.omit()

tail(port_rolling_sd_tidy_does_not_work, 3)


# rolling sd, tibbletime

sd_roll_24 <- rollify(sd, window = window)

port_rolling_sd_tidy_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = date) %>%
  mutate(sd = sd_roll_24(returns)) %>%
  select(-returns) %>%
  na.omit()

tail(port_rolling_sd_tidy_tibbletime, 3)

# rolling sd, tidyquant

port_rolling_sd_tq <-
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(date, rolling_sd) %>%
  na.omit()

port_rolling_sd_tidy_tibbletime %>%
  mutate(sd_tq = port_rolling_sd_tq$rolling_sd,
         sd_xts = round(port_rolling_sd_xts$rolling_sd, 4)) %>%
  tail(3)

port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xts, 4) * 100

highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
             opposite = F) %>%
  hc_navigator(enabled = F) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled = T) %>%
  hc_legend(enabled = T)


# rolling sd vis, ggplot

port_rolling_sd_tq %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
    theme(plot.title = element_text(hjust = 0.5))

