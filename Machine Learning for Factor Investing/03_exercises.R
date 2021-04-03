library(data.table)
library(quantmod)
library(tidyquant)
library(PerformanceAnalytics)
library(cowplot)
library(forecast)
library(tidyverse)
library(lubridate)
library(here)

load(here("Machine Learning for Factor Investing", "data_ml.RData"))

head(data_ml)

data_ml %>%
  group_by(date) %>%
  mutate(category = ifelse(Pb > median(Pb), "growth", "value")) %>%
  ungroup() %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year, category) %>%
  summarise(return = mean(R1M_Usd)) %>%
  ggplot(aes(year, return, fill = category)) +
    geom_col(position = "dodge")


monthly_return <- data_ml %>%
  group_by(date) %>%
  mutate(growth = Pb > median(Pb)) %>%
  ungroup() %>%
  group_by(date, growth) %>%
  summarise(return = mean(R1M_Usd)) %>%
  spread(key = growth, value = return) %>%
  ungroup()

colnames(monthly_return)[2:3] <- c("value", "growth")

monthly_return %>%
  mutate(growth = cumprod(1 + growth), value = cumprod(1 + value)) %>%
  gather(key = portfolio, value = value, -date) %>%
  ggplot(aes(x = date, y = value, color = portfolio)) +
    geom_line()

data_ml %>%
  group_by(date) %>%
  mutate(capitalization = ntile(Mkt_Cap_3M_Usd, 4)) %>%
  ungroup() %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year, capitalization) %>%
  summarise(return = mean(R1M_Usd)) %>%
  mutate(capitalization = factor(capitalization, levels = 1:4, labels = c("small", "medium", "large", "x-large"))) %>%
  ggplot(aes(year, return, fill = capitalization)) +
    geom_col(position = "dodge")