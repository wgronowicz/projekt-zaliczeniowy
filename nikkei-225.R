##############################
# Title: Nikkei 225
# @: Szymon Zagroba, Ireneusz Sołtykiewicz, Wojciech Gronowicz
# Date: 05.01.2026
##############################

# Load  libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Load data

nikkei_raw <- read.csv("data-sets/NIKKEI225.csv")

# Data processing: date conversion, gap cleaning and aggregation to monthly returns

index_monthly_returns <- nikkei_raw %>%
  mutate(
    Date = as.Date(observation_date),
    Price = as.numeric(NIKKEI225)
  ) %>%
  na.omit() %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Price = last(Price)) %>%
  mutate(index_return = (Price / lag(Price)) - 1) %>%
  na.omit() %>%
  mutate(Date = as.Date(paste0(Month, "-01"))) %>%
  select(Date, index_return)

# Annual rate of return - Buy&Hold

index_annual_returns <- index_monthly_returns %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_return = prod(1 + index_return) - 1
  )

# Annual volatility - Buy&Hold

index_annual_volatility <- index_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_volatility = sd(index_return) * sqrt(12)
  )

# Sharpe ratio - Buy&Hold

us_treasury_bonds_rates <- 0.0347

index_annual_sharpe_ratio <- index_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>%
  summarise(
    sharpe_ratio = ((mean(index_return) * 12) - us_treasury_bonds_rates) / (sd(index_return) * sqrt(12))
  )

# Max drawdown - Buy&Hold

index_monthly_returns <- index_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + index_return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

max_drawdown <- min(index_monthly_returns$Drawdown)

# Annual rate of return - Momentum

momentum_monthly_returns <- index_monthly_returns %>%
  mutate(
    Momentum_Signal = (lag(index_return, 1) / lag(index_return, 13)) - 1,
    Weight_Momentum = ifelse(Momentum_Signal > 0, 1, 0),
    Weight_Momentum = ifelse(is.na(Weight_Momentum), 0, Weight_Momentum),
    Strategy_Return = Weight_Momentum * index_return
  )

momentum_annual_returns <- momentum_monthly_returns %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_return = prod(1 + Strategy_Return) - 1
  )

# Annual volatility - Momentum

momentum_annual_volatility <- momentum_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    momentum_volatility = sd(Strategy_Return) * sqrt(12)
  )

# Sharpe ratio - Momentum

momentum_annual_sharpe_ratio <- momentum_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>%
  summarise(
    sharpe_ratio = ((mean(Strategy_Return) * 12) - us_treasury_bonds_rates) / (sd(Strategy_Return) * sqrt(12))
  )

# Max drawdown - Momentum

momentum_monthly_returns <- momentum_monthly_returns %>%
  mutate(
    Cumulative_Return_Strategy = cumprod(1 + Strategy_Return),
    Running_Max_Strategy = cummax(Cumulative_Return_Strategy),
    Drawdown_Strategy = (Cumulative_Return_Strategy - Running_Max_Strategy) / Running_Max_Strategy
  )

momentum_max_drawdown <- min(momentum_monthly_returns$Drawdown_Strategy, na.rm = TRUE)

# T-test returns difference

reduced_index_monthly_returns <- index_monthly_returns[-(1:13),]
reduced_momentum_monthly_returns <- momentum_monthly_returns[-(1:13),]

t_test_result <- t.test(
  reduced_index_monthly_returns$index_return,
  reduced_momentum_monthly_returns$Strategy_Return, 
  paired = TRUE
)

print(t_test_result)

# Capital curves chart

comparison_data <- data.frame(
  Date = as.Date(reduced_index_monthly_returns$Date),
  Return_BH = reduced_index_monthly_returns$index_return,
  Return_Mom = reduced_momentum_monthly_returns$Strategy_Return
) %>%
  mutate(
=    Equity_BH = cumprod(1 + Return_BH),
    Equity_Mom = cumprod(1 + Return_Mom),
    DD_BH = (Equity_BH / cummax(Equity_BH)) - 1,
    DD_Mom = (Equity_Mom / cummax(Equity_Mom)) - 1
  )

equity_long <- comparison_data %>%
  select(Date, Equity_BH, Equity_Mom) %>%
  pivot_longer(-Date, names_to = "Strategy", values_to = "Value")

ggplot(equity_long, aes(x = Date, y = Value, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Equity_BH" = "#3498db", "Equity_Mom" = "#e74c3c"),
    labels = c("Buy & Hold (Nikkei 225)", "Momentum 12-1")
  ) +
  labs(
    title = "Skumulowany wzrost kapitału - Nikkei 225",
    subtitle = "Porównanie strategii na danych oczyszczonych (po 13 mies.)",
    y = "Wartość portfela", x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Summary

bh_sharpe_total <- (mean(reduced_index_monthly_returns$index_return) * 12 - 0.0347) / (sd(reduced_index_monthly_returns$index_return) * sqrt(12))
mom_sharpe_total <- (mean(reduced_momentum_monthly_returns$Strategy_Return) * 12 - 0.0347) / (sd(reduced_momentum_monthly_returns$Strategy_Return) * sqrt(12))

cat("--- WYNIKI NIKKEI ---\n",
    "B&H Zysk śr. roczny:", mean(index_annual_returns$annual_return), "\n",
    "B&H Zmienność roczna:", sd(reduced_index_monthly_returns$index_return) * sqrt(12), "\n",
    "B&H Sharpe Ratio (całość):", bh_sharpe_total, "\n",
    "B&H Max Drawdown:", max_drawdown, "\n",
    "------------------------\n",
    "Mom. Zysk śr. roczny:", mean(momentum_annual_returns$annual_return), "\n",
    "Mom. Zmienność roczna:", sd(reduced_momentum_monthly_returns$Strategy_Return) * sqrt(12), "\n",
    "Mom. Sharpe Ratio (całość):", mom_sharpe_total, "\n",
    "Mom. Max Drawdown:", momentum_max_drawdown, "\n",
    "P-value (czy różnica istotna?):", t_test_result$p.value, "\n")
