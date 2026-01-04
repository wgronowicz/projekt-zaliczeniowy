# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Load data

prices <- read.csv("data-sets/s-and-p-500/prices.csv")
tickerlist <- read.csv("data-sets/s-and-p-500/tickerlist.csv")
returns <- read.csv("data-sets/s-and-p-500/returns.csv")

# Annual rate of return Buy&Hold

index_monthly_returns <- returns %>%
  mutate(index_return = rowMeans(select(., -Date), na.rm = TRUE)) %>%
  select(1, index_return)

print(index_monthly_returns)

index_annual_returns <- index_monthly_returns %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_return = prod(1 + index_return) - 1
  )

print(index_annual_returns)
 
#annual volatility - Buy&Hold

index_annual_volatility <- index_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_volatility = sd(index_return) * sqrt(12)
  )


#sharpe ratio - Buy&Hold

us_treasury_bonds_rates <- 0.0347

index_annual_sharpe_ratio <- index_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>%
  summarise(
    ((mean(index_return) * 12) - us_treasury_bonds_rates) / (sd(index_return) * sqrt(12))
  )
  


#max drawdown - Buy&Hold

index_monthly_returns <- index_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + index_return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

max_drawdown <- min(index_monthly_returns$Drawdown)

#Momentum

#

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

#annual volatility - Momentum

momentum_annual_volatility <- momentum_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    momentum_volatility = sd(Strategy_Return) * sqrt(12)
  )


#sharpe ratio - Momentum

momentum_annual_sharpe_ratio <- momentum_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>%
  summarise(
    ((mean(Strategy_Return) * 12) - us_treasury_bonds_rates) / (sd(Strategy_Return) * sqrt(12))
  )

#max drawdown - Momentum

momentum_monthly_returns <- momentum_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + Strategy_Return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

momentum_max_drawdown <- min(momentum_monthly_returns$Drawdown)

# Test t różnicy średnich stóp zwrotu
## z miesięcznych zwrotów usuwam pierwsze 13 miesięcy, gdy momentum nie miało wyników

reduced_index_monthly_returns <- index_monthly_returns[-(1:13),]
reduced_momentum_monthly_returns <- momentum_monthly_returns[-(1:13),]

t_test_result <- t.test(
  reduced_index_monthly_returns$index_return,
  reduced_momentum_monthly_returns$Strategy_Return, 
  paired = TRUE
)

print(t_test_result)

# wykres krzywych kapitału

# Łączymy dane w jedną strukturę do wykresu
comparison_data <- data.frame(
  Date = as.Date(reduced_index_monthly_returns$Date),
  Return_BH = reduced_index_monthly_returns$index_return,
  Return_Mom = reduced_momentum_monthly_returns$Strategy_Return
) %>%
  mutate(
    # Obliczamy kapitał (Equity) startując od 1.0
    Equity_BH = cumprod(1 + Return_BH),
    Equity_Mom = cumprod(1 + Return_Mom),
    # Obliczamy bieżące obsunięcia (Drawdowns) dla obu strategii
    DD_BH = (Equity_BH / cummax(Equity_BH)) - 1,
    DD_Mom = (Equity_Mom / cummax(Equity_Mom)) - 1
  )


# Transformacja do formatu long dla ggplot
equity_long <- comparison_data %>%
  select(Date, Equity_BH, Equity_Mom) %>%
  pivot_longer(-Date, names_to = "Strategy", values_to = "Value")

ggplot(equity_long, aes(x = Date, y = Value, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Equity_BH" = "#3498db", "Equity_Mom" = "#e74c3c"),
    labels = c("Buy & Hold (S&P 500)", "Momentum 12-1")
  ) +
  labs(
    title = "Skumulowany wzrost kapitału",
    subtitle = "Porównanie strategii na danych oczyszczonych (po 13 mies.)",
    y = "Wartość portfela (USD)", x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
