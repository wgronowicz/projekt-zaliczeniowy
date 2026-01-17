##############################
# Title: Gold
# @: Szymon Zagroba, Ireneusz Sołtykiewicz, Wojciech Gronowicz
# Date: 05.01.2026
##############################

# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)

# Load data

annual <- read.csv("data-sets/gold/annual_csv.csv")
monthly <- read.csv("data-sets/gold/monthly_csv.csv")

# Annual rate of return - Buy&Hold

gold_monthly_returns <- monthly %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),
    Monthly_Return = (Price / lag(Price)) - 1,
    Year = year(Date),
    Month = month(Date)
  ) %>%
  select(Date, Year, Month, Price, Monthly_Return) %>%
  filter(!is.na(Monthly_Return))

gold_annual_returns <- annual %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),
    Annual_Return = (Price / lag(Price)) - 1,     
    Year = year(Date)                              
  ) %>% 
  select(Year, Annual_Return) %>%                  
  filter(!is.na(Annual_Return))


# Annual volatility - Buy&Hold

gold_annual_volatility <- gold_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_volatility = sd(Monthly_Return) * sqrt(12)
  )

# Sharpe ratio - Buy&Hold

us_treasury_bonds_rates <- 0.0347

gold_annual_sharpe_ratio <- gold_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>%
  summarise(
    Sharpe_Ratio = ((mean(Monthly_Return) * 12) - us_treasury_bonds_rates) / (sd(Monthly_Return) * sqrt(12))
  )

# Max drawdown - Buy&Hold

gold_monthly_returns <- gold_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + Monthly_Return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

max_drawdown <- min(gold_monthly_returns$Drawdown)

# Annual rate of return - Momentum

momentum_monthly_returns <- gold_monthly_returns %>%
  mutate(
    Momentum_Signal = (lag(Price, 1) / lag(Price, 13)) - 1,
    Weight_Momentum = ifelse(Momentum_Signal > 0, 1, 0),
    Weight_Momentum = ifelse(is.na(Weight_Momentum), 0, Weight_Momentum),
    Strategy_Return = Weight_Momentum * Monthly_Return
  )

momentum_annual_returns <- momentum_monthly_returns %>%
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
    Sharpe_Ratio = ((mean(Strategy_Return) * 12) - us_treasury_bonds_rates) / (sd(Strategy_Return) * sqrt(12))
  )

# Max drawdown - Momentum

momentum_monthly_returns <- momentum_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + Strategy_Return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

momentum_max_drawdown <- min(momentum_monthly_returns$Drawdown)

# T-test returns difference

reduced_gold_monthly_returns <- gold_monthly_returns[-(1:13),]
reduced_momentum_monthly_returns <- momentum_monthly_returns[-(1:13),]

t_test_return_difference <- t.test(
  reduced_gold_monthly_returns$Monthly_Return,
  reduced_momentum_monthly_returns$Strategy_Return,
  paired = TRUE
  )

# Capital curves chart

comparison_data <- data.frame(
  Date = as.Date(reduced_gold_monthly_returns$Date),
  Return_BH = reduced_gold_monthly_returns$Monthly_Return,
  Return_Mom = reduced_momentum_monthly_returns$Strategy_Return
) %>%
  mutate(
    Equity_BH = cumprod(1 + Return_BH),
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
    labels = c("Buy & Hold (Gold)", "Momentum 12-1")
  ) +
  labs(
    title = "Skumulowany wzrost kapitału",
    subtitle = "Porównanie strategii na danych oczyszczonych (po 13 mies.)",
    y = "Wartość portfela (USD)", x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Summmary

bh_sharpe_total <- (mean(reduced_gold_monthly_returns$Monthly_Return) * 12 - 0.0347) / (sd(reduced_gold_monthly_returns$Monthly_Return) * sqrt(12))
mom_sharpe_total <- (mean(reduced_momentum_monthly_returns$Strategy_Return) * 12 - 0.0347) / (sd(reduced_momentum_monthly_returns$Strategy_Return) * sqrt(12))

cat("--- WYNIKI GOLD ---\n",
    "B&H Zysk śr. roczny:", mean(gold_annual_returns$Annual_Return), "\n",
    "B&H Zmienność roczna:", sd(reduced_gold_monthly_returns$Monthly_Return) * sqrt(12), "\n",
    "B&H Sharpe Ratio (całość):", bh_sharpe_total, "\n",
    "B&H Max Drawdown:", max_drawdown, "\n",
    "------------------------\n",
    "Mom. Zysk śr. roczny:", mean(momentum_annual_returns$annual_return), "\n",
    "Mom. Zmienność roczna:", sd(reduced_momentum_monthly_returns$Strategy_Return) * sqrt(12), "\n",
    "Mom. Sharpe Ratio (całość):", mom_sharpe_total, "\n",
    "Mom. Max Drawdown:", momentum_max_drawdown, "\n",
    "P-value (czy różnica istotna?):", t_test_return_difference$p.value, "\n")
