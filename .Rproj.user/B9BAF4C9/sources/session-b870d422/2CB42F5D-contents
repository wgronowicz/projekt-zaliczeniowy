# Load libraries

library(dplyr)
library(tidyverse)

# Load data

prices <- read.csv("data-sets/s-and-p-500/prices.csv")
tickerlist <- read.csv("data-sets/s-and-p-500/tickerlist.csv")
returns <- read.csv("data-sets/s-and-p-500/returns.csv")

# Annual rate of return Buy&Hold

index_monthly_returns <- returns %>%
  mutate(Index_Return = rowMeans(select(., -Date), na.rm = TRUE)) %>%
  select(1, Index_Return)

print(index_monthly_returns)

index_annual_returns <- index_monthly_returns %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_return = prod(1 + Index_Return) - 1
  )

print(index_annual_returns)

#fdsfdsfsf

ann_vol <- index_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  sd(sp500_monthly$index_monthly_returns) * sqrt(12)