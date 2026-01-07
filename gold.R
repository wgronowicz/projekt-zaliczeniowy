# Load libraries

library(dplyr)
library(tidyverse)
library(ggplot2)

# Load data

annual <- read.csv("data-sets/gold/annual_csv.csv")
monthly <- read.csv("data-sets/gold/monthly_csv.csv")

gold_monthly_returns <- monthly %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")), #zmiana formatu daty
    Monthly_Return = (Price / lag(Price)) - 1,
    Year = year(Date),
    Month = month(Date)
  ) %>%
  select(Date, Year, Month, Price, Monthly_Return) %>%
  filter(!is.na(Monthly_Return))

gold_annual_returns <- annual %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),           #zmiana formatu daty
    Annual_Return = (Price / lag(Price)) - 1,     
    Year = year(Date)                              
  ) %>% 
  select(Year, Annual_Return) %>%                  
  filter(!is.na(Annual_Return))


#annual volatility - Buy&Hold

gold_annual_volatility <- gold_monthly_returns  %>%
  mutate(
    Date = as.Date(Date),
    Year = format(Date, "%Y")
  ) %>%
  group_by(Year) %>% 
  summarise(
    annual_volatility = sd(Monthly_Return) * sqrt(12)
  )

#sharpe ratio - Buy&Hold

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


#max drawdown - Buy&Hold

gold_monthly_returns <- gold_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + Monthly_Return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

max_drawdown <- min(gold_monthly_returns$Drawdown)


#Momentum

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
    Sharpe_Ratio = ((mean(Strategy_Return) * 12) - us_treasury_bonds_rates) / (sd(Strategy_Return) * sqrt(12))
  )

#max drawdown - Momentum

momentum_monthly_returns <- momentum_monthly_returns %>%
  mutate(
    Cumulative_Return = cumprod(1 + Strategy_Return),
    Running_Max = cummax(Cumulative_Return),
    Drawdown = (Cumulative_Return - Running_Max) / Running_Max
  )

momentum_max_drawdown <- min(momentum_monthly_returns$Drawdown)

#t-test returns difference

reduced_gold_monthly_returns <- gold_monthly_returns[-(1:13),]
reduced_momentum_monthly_returns <- momentum_monthly_returns[-(1:13),]

t_test_return_difference <- t.test(
  reduced_gold_monthly_returns$Monthly_Return,
  reduced_momentum_monthly_returns$Strategy_Return,
  paired = TRUE
  )

#plot

# wykres krzywych kapitału

# Łączymy dane w jedną strukturę do wykresu
comparison_data <- data.frame(
  Date = as.Date(reduced_gold_monthly_returns$Date),
  Return_BH = reduced_gold_monthly_returns$Monthly_Return,
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


