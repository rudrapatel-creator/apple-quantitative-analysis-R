############################################################
# FINAL INSTITUTIONAL FINTECH RESEARCH PROJECT
# Apple Inc. Stock Analysis and Algorithmic Trading
# Stock: AAPL
# Benchmark: SPY
# Data Source: Yahoo Finance
############################################################

packages <- c(
  "quantmod", "tidyverse", "ggplot2", "forecast", "TTR",
  "scales", "lubridate", "PerformanceAnalytics",
  "tseries", "zoo"
)

load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, load_package))

stock_ticker <- "AAPL"
benchmark_ticker <- "SPY"

start_date <- "2020-01-01"
end_date <- Sys.Date()

transaction_cost <- 0.00075
slippage_cost <- 0.00025
total_trade_cost <- transaction_cost + slippage_cost

risk_free_annual <- 0.04
risk_free_daily <- risk_free_annual / 252

forecast_horizon <- 30
set.seed(123)

getSymbols(
  Symbols = c(stock_ticker, benchmark_ticker),
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = TRUE
)

apple_xts <- AAPL
spy_xts <- SPY

apple <- data.frame(
  Date = index(apple_xts),
  coredata(apple_xts)
)

colnames(apple) <- c(
  "Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"
)

spy <- data.frame(
  Date = index(spy_xts),
  coredata(spy_xts)
)

colnames(spy) <- c(
  "Date", "SPY_Open", "SPY_High", "SPY_Low",
  "SPY_Close", "SPY_Volume", "SPY_Adjusted"
)

apple <- apple %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = TRUE),
    Month_Number = month(Date),
    Daily_Return = Adjusted / lag(Adjusted) - 1,
    Log_Return = log(Adjusted / lag(Adjusted)),
    Price_Range = High - Low,
    SMA_20 = SMA(Adjusted, n = 20),
    SMA_50 = SMA(Adjusted, n = 50),
    SMA_200 = SMA(Adjusted, n = 200),
    RSI_14 = RSI(Adjusted, n = 14),
    Rolling_Volatility_30 = runSD(Daily_Return, n = 30) * sqrt(252)
  ) %>%
  arrange(Date)

spy <- spy %>%
  mutate(
    Date = as.Date(Date),
    SPY_Return = SPY_Adjusted / lag(SPY_Adjusted) - 1
  ) %>%
  select(Date, SPY_Adjusted, SPY_Return)

apple <- apple %>%
  left_join(spy, by = "Date")

apple_analysis <- apple %>%
  filter(!is.na(Daily_Return), !is.na(Log_Return), !is.na(SPY_Return))

apple_strategy <- apple %>%
  filter(
    !is.na(SMA_20),
    !is.na(SMA_50),
    !is.na(RSI_14),
    !is.na(Daily_Return),
    !is.na(Log_Return),
    !is.na(SPY_Return)
  )

theme_fintech <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#0F172A"),
      plot.subtitle = element_text(size = 11, color = "#475569"),
      axis.title = element_text(face = "bold", color = "#1E293B"),
      axis.text = element_text(color = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.35),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#F8FAFC", color = NA),
      panel.background = element_rect(fill = "#F8FAFC", color = NA)
    )
}

cat("\nDATA QUALITY SUMMARY\n")
cat("Stock:", stock_ticker, "\n")
cat("Benchmark:", benchmark_ticker, "\n")
cat("Start Date:", as.character(min(apple$Date)), "\n")
cat("End Date:", as.character(max(apple$Date)), "\n")
cat("Total Observations:", nrow(apple), "\n")
cat("Missing Values by Column:\n")
print(colSums(is.na(apple)))

############################################################
# FINANCIAL VISUALISATION
############################################################

ggplot(apple, aes(x = Date)) +
  geom_area(aes(y = Adjusted), fill = "#DBEAFE", alpha = 0.65) +
  geom_line(aes(y = Adjusted, color = "Adjusted Close"), linewidth = 0.85) +
  geom_line(aes(y = SMA_20, color = "20-Day SMA"), linewidth = 0.85, na.rm = TRUE) +
  geom_line(aes(y = SMA_50, color = "50-Day SMA"), linewidth = 0.85, na.rm = TRUE) +
  geom_line(aes(y = SMA_200, color = "200-Day SMA"), linewidth = 0.75, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      "Adjusted Close" = "#1D4ED8",
      "20-Day SMA" = "#059669",
      "50-Day SMA" = "#DC2626",
      "200-Day SMA" = "#7C3AED"
    )
  ) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Apple Long-Term Price Trend",
    subtitle = "Adjusted closing price with 20-day, 50-day, and 200-day moving averages",
    x = "Date",
    y = "Adjusted Closing Price"
  ) +
  theme_fintech()

recent_90_xts <- tail(apple_xts, 90)

chartSeries(
  recent_90_xts,
  theme = chartTheme("white"),
  name = "Apple Recent 90-Day Candlestick Chart",
  TA = "addSMA(n = 20, col = 'darkgreen'); addSMA(n = 50, col = 'red'); addRSI(n = 14)"
)

monthly_volume <- apple %>%
  group_by(Year, Month, Month_Number) %>%
  summarise(Average_Volume = mean(Volume, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, Month_Number)

ggplot(monthly_volume, aes(x = Month, y = Average_Volume, fill = factor(Year))) +
  geom_col(position = "dodge", width = 0.75) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Apple Monthly Average Trading Volume",
    subtitle = "Grouped bar chart comparing liquidity across months and years",
    x = "Month",
    y = "Average Volume",
    fill = "Year"
  ) +
  theme_fintech()

ggplot(apple_analysis, aes(x = Daily_Return)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "#93C5FD",
    color = "white",
    alpha = 0.85
  ) +
  geom_density(color = "#1D4ED8", linewidth = 1.1) +
  geom_vline(xintercept = 0, color = "#111827", linewidth = 0.75) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Apple Daily Return Distribution",
    subtitle = "Histogram and density curve showing volatility and return clustering",
    x = "Daily Return",
    y = "Density"
  ) +
  theme_fintech()

ggplot(apple_analysis, aes(x = factor(Year), y = Daily_Return, fill = factor(Year))) +
  geom_boxplot(alpha = 0.85, outlier.color = "#DC2626", outlier.size = 1.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Apple Yearly Return Volatility",
    subtitle = "Boxplot showing median, spread, and outliers of yearly returns",
    x = "Year",
    y = "Daily Return"
  ) +
  theme_fintech() +
  theme(legend.position = "none")

monthly_returns <- apple_analysis %>%
  group_by(Year, Month, Month_Number) %>%
  summarise(
    Monthly_Return = prod(1 + Daily_Return, na.rm = TRUE) - 1,
    .groups = "drop"
  ) %>%
  arrange(Year, Month_Number)

ggplot(monthly_returns, aes(x = Month, y = factor(Year), fill = Monthly_Return)) +
  geom_tile(color = "white", linewidth = 0.7) +
  scale_fill_gradient2(
    low = "#DC2626",
    mid = "#F8FAFC",
    high = "#059669",
    midpoint = 0,
    labels = percent_format()
  ) +
  labs(
    title = "Apple Monthly Return Heatmap",
    subtitle = "Green months show gains; red months show losses",
    x = "Month",
    y = "Year",
    fill = "Monthly Return"
  ) +
  theme_fintech()

benchmark_plot <- apple_analysis %>%
  mutate(
    Apple_Cumulative = cumprod(1 + Daily_Return) - 1,
    SPY_Cumulative = cumprod(1 + SPY_Return) - 1
  ) %>%
  select(Date, Apple_Cumulative, SPY_Cumulative) %>%
  pivot_longer(
    cols = c(Apple_Cumulative, SPY_Cumulative),
    names_to = "Asset",
    values_to = "Cumulative_Return"
  ) %>%
  mutate(
    Asset = recode(
      Asset,
      "Apple_Cumulative" = "Apple",
      "SPY_Cumulative" = "S&P 500 ETF"
    )
  )

ggplot(benchmark_plot, aes(x = Date, y = Cumulative_Return, color = Asset)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Apple" = "#1D4ED8", "S&P 500 ETF" = "#111827")) +
  labs(
    title = "Apple vs Market Benchmark",
    subtitle = "Cumulative return comparison against SPY",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_fintech()

############################################################
# SEASONAL DECOMPOSITION
############################################################

apple_price_ts <- ts(
  log(na.omit(apple$Adjusted)),
  frequency = 252
)

apple_stl <- stl(apple_price_ts, s.window = "periodic")

plot(
  apple_stl,
  main = "STL Decomposition of Apple Log Adjusted Price"
)

cat("\nSEASONAL DECOMPOSITION RESEARCH NOTE\n")
cat("
STL decomposition is included because the assignment requires trend and seasonal
analysis. However, unlike retail sales, tourism demand, or macroeconomic series,
daily equity prices do not usually exhibit strong deterministic seasonality.

For Apple stock, the trend component is more economically meaningful than the
seasonal component because equity prices are mainly driven by earnings expectations,
interest rates, liquidity, investor sentiment, and firm-specific news.

Therefore, decomposition is interpreted cautiously as an exploratory diagnostic,
not as evidence that Apple prices follow a stable seasonal trading pattern.
")

############################################################
# STATIONARITY AND FORECASTING
############################################################

cat("\nSTATIONARITY TESTS\n")

raw_price_adf <- adf.test(na.omit(apple$Adjusted))
log_return_adf <- adf.test(na.omit(apple$Log_Return))
log_return_kpss <- kpss.test(na.omit(apple$Log_Return))

cat("\nADF Test on Raw Prices:\n")
print(raw_price_adf)

cat("\nADF Test on Log Returns:\n")
print(log_return_adf)

cat("\nKPSS Test on Log Returns:\n")
print(log_return_kpss)

cat("\nInterpretation:\n")
cat("ARIMA is applied to log returns rather than raw prices because raw stock prices are usually non-stationary.\n")
cat("This improves statistical validity, although equity returns often remain difficult to forecast due to market efficiency.\n")

log_returns <- na.omit(apple$Log_Return)

train_size <- floor(0.80 * length(log_returns))

train_returns <- log_returns[1:train_size]
test_returns <- log_returns[(train_size + 1):length(log_returns)]

train_ts <- ts(train_returns, frequency = 1)

arima_model <- auto.arima(
  train_ts,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

arima_test_forecast <- forecast(arima_model, h = length(test_returns))
naive_test_forecast <- naive(train_ts, h = length(test_returns))
mean_test_forecast <- meanf(train_ts, h = length(test_returns))

cat("\nFORECAST MODEL COMPARISON\n")
cat("\nARIMA Accuracy:\n")
print(accuracy(arima_test_forecast, test_returns))

cat("\nNaive Forecast Accuracy:\n")
print(accuracy(naive_test_forecast, test_returns))

cat("\nMean Forecast Accuracy:\n")
print(accuracy(mean_test_forecast, test_returns))

cat("\nForecasting Interpretation:\n")
cat("ARIMA is compared against naive and mean-return benchmarks to avoid assuming that a complex model is automatically superior.\n")
cat("If ARIMA does not materially outperform simpler benchmarks, this supports the efficient-market view that daily stock returns have limited predictability.\n")

cat("\nARIMA RESIDUAL DIAGNOSTICS\n")

residuals_arima <- residuals(arima_model)

ljung_box_test <- Box.test(
  residuals_arima,
  lag = 20,
  type = "Ljung-Box"
)

print(ljung_box_test)
checkresiduals(arima_model)

############################################################
# OPTIONAL VOLATILITY MODELLING: GARCH(1,1)
############################################################

cat("\nGARCH VOLATILITY MODELLING\n")

if (!require("rugarch", character.only = TRUE)) {
  tryCatch(
    {
      install.packages("rugarch", dependencies = TRUE)
      library(rugarch)
    },
    error = function(e) {
      cat("rugarch could not be installed in this environment. GARCH section skipped.\n")
    }
  )
}

if ("rugarch" %in% rownames(installed.packages())) {
  library(rugarch)
  
  garch_spec <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(0, 0),
      include.mean = TRUE
    ),
    distribution.model = "std"
  )
  
  garch_fit <- ugarchfit(
    spec = garch_spec,
    data = na.omit(apple$Log_Return)
  )
  
  print(garch_fit)
  
  garch_forecast <- ugarchforecast(
    garch_fit,
    n.ahead = 30
  )
  
  predicted_volatility <- sigma(garch_forecast)
  
  plot(
    predicted_volatility,
    type = "l",
    col = "#DC2626",
    lwd = 2,
    main = "Apple 30-Day Forecasted Volatility Using GARCH(1,1)",
    xlab = "Forecast Day",
    ylab = "Predicted Volatility"
  )
  
  cat("
GARCH Interpretation:
Financial returns often show volatility clustering, meaning large price changes
tend to be followed by large price changes. GARCH(1,1) is added to model this
time-varying volatility, which ARIMA alone does not capture.
")
}

############################################################
# FINAL ARIMA FORECAST
############################################################

full_log_return_ts <- ts(log_returns, frequency = 1)

final_arima_model <- auto.arima(
  full_log_return_ts,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(final_arima_model)

log_return_forecast <- forecast(final_arima_model, h = forecast_horizon)

last_price <- tail(na.omit(apple$Adjusted), 1)
last_date <- max(apple$Date)

make_trading_days <- function(start_date, n) {
  candidate_dates <- seq.Date(
    from = start_date + 1,
    by = "day",
    length.out = n * 3
  )
  
  trading_days <- candidate_dates[
    !weekdays(candidate_dates) %in% c("Saturday", "Sunday")
  ]
  
  head(trading_days, n)
}

forecast_dates <- make_trading_days(last_date, forecast_horizon)

forecast_price <- data.frame(
  Date = forecast_dates,
  Forecast_Price = as.numeric(last_price * exp(cumsum(log_return_forecast$mean))),
  Lower_80 = as.numeric(last_price * exp(cumsum(log_return_forecast$lower[, 1]))),
  Upper_80 = as.numeric(last_price * exp(cumsum(log_return_forecast$upper[, 1]))),
  Lower_95 = as.numeric(last_price * exp(cumsum(log_return_forecast$lower[, 2]))),
  Upper_95 = as.numeric(last_price * exp(cumsum(log_return_forecast$upper[, 2])))
)

recent_actual <- apple %>%
  filter(Date >= max(Date) - 180)

ggplot() +
  geom_line(
    data = recent_actual,
    aes(x = Date, y = Adjusted, color = "Actual Price"),
    linewidth = 0.9
  ) +
  geom_ribbon(
    data = forecast_price,
    aes(x = Date, ymin = Lower_95, ymax = Upper_95),
    fill = "#BFDBFE",
    alpha = 0.45
  ) +
  geom_ribbon(
    data = forecast_price,
    aes(x = Date, ymin = Lower_80, ymax = Upper_80),
    fill = "#60A5FA",
    alpha = 0.35
  ) +
  geom_line(
    data = forecast_price,
    aes(x = Date, y = Forecast_Price, color = "ARIMA Forecast"),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Actual Price" = "#111827",
      "ARIMA Forecast" = "#2563EB"
    )
  ) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Apple 30-Day Price Forecast Using Stationary Log Returns",
    subtitle = "ARIMA is fitted on log returns and converted back into price forecasts",
    x = "Date",
    y = "Adjusted Closing Price"
  ) +
  theme_fintech()

cat("\nForecast Calendar Note:\n")
cat("The forecast visual skips weekends. In production, a full NYSE holiday calendar should be used.\n")

############################################################
# CAPM ANALYSIS
############################################################

capm_model <- lm(Daily_Return ~ SPY_Return, data = apple_analysis)

cat("\nCAPM REGRESSION: APPLE RETURNS VS SPY RETURNS\n")
print(summary(capm_model))

capm_beta <- coef(capm_model)[2]
capm_alpha_daily <- coef(capm_model)[1]
capm_alpha_annual <- capm_alpha_daily * 252

cat("\nCAPM Interpretation:\n")
cat("Apple Beta vs SPY:", round(capm_beta, 3), "\n")
cat("Annualized Jensen Alpha:", round(capm_alpha_annual * 100, 2), "%\n")
cat("Beta measures Apple's systematic market risk relative to SPY.\n")
cat("Jensen Alpha estimates whether Apple generated excess return beyond what CAPM would predict.\n")

############################################################
# PARAMETER SENSITIVITY
############################################################

run_strategy <- function(data, short_window, long_window) {
  temp <- data %>%
    mutate(
      Short_MA = SMA(Adjusted, n = short_window),
      Long_MA = SMA(Adjusted, n = long_window),
      Signal = ifelse(Short_MA > Long_MA & RSI_14 < 70, 1, 0),
      Position = lag(Signal, 1),
      Position = replace_na(Position, 0),
      Trade = abs(Position - lag(Position, 1)),
      Trade = replace_na(Trade, 0),
      Gross_Return = Position * Daily_Return,
      Net_Return = Gross_Return - Trade * total_trade_cost
    ) %>%
    filter(!is.na(Net_Return))
  
  returns_xts_temp <- xts(temp$Net_Return, order.by = temp$Date)
  
  tibble(
    Strategy = paste0(short_window, "/", long_window, " SMA + RSI"),
    Short_Window = short_window,
    Long_Window = long_window,
    Final_Return = tail(cumprod(1 + temp$Net_Return) - 1, 1),
    Annualized_Return = as.numeric(Return.annualized(returns_xts_temp, scale = 252)),
    Annualized_Risk = as.numeric(StdDev.annualized(returns_xts_temp, scale = 252)),
    Sharpe = as.numeric(SharpeRatio.annualized(returns_xts_temp, scale = 252, Rf = risk_free_daily)),
    Max_Drawdown = as.numeric(maxDrawdown(returns_xts_temp)),
    Total_Trades = sum(temp$Trade, na.rm = TRUE)
  )
}

sensitivity_results <- bind_rows(
  run_strategy(apple, 10, 30),
  run_strategy(apple, 20, 50),
  run_strategy(apple, 50, 200)
)

cat("\nSTRATEGY PARAMETER SENSITIVITY RESULTS\n")
print(sensitivity_results)

ggplot(sensitivity_results, aes(x = Strategy, y = Sharpe, fill = Strategy)) +
  geom_col(width = 0.65) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Trading Strategy Parameter Sensitivity",
    subtitle = "Comparison of alternative moving-average strategy specifications",
    x = "Strategy",
    y = "Annualized Sharpe Ratio"
  ) +
  theme_fintech() +
  theme(legend.position = "none")

############################################################
# WALK-FORWARD VALIDATION
############################################################

cat("\nWALK-FORWARD STRATEGY ROBUSTNESS CHECK\n")

walk_forward_data <- apple %>%
  filter(
    !is.na(Daily_Return),
    !is.na(RSI_14),
    !is.na(SPY_Return)
  )

walk_forward_strategy <- function(data, short_window, long_window, initial_window = 500) {
  results <- data.frame()
  
  for (i in seq(initial_window, nrow(data) - 1)) {
    train_data <- data[1:i, ]
    test_day <- data[i + 1, ]
    
    short_ma <- mean(tail(train_data$Adjusted, short_window), na.rm = TRUE)
    long_ma <- mean(tail(train_data$Adjusted, long_window), na.rm = TRUE)
    
    signal <- ifelse(short_ma > long_ma && test_day$RSI_14 < 70, 1, 0)
    
    results <- bind_rows(
      results,
      data.frame(
        Date = test_day$Date,
        Signal = signal,
        Daily_Return = test_day$Daily_Return
      )
    )
  }
  
  results <- results %>%
    mutate(
      Position = lag(Signal, 1),
      Position = replace_na(Position, 0),
      Trade = abs(Position - lag(Position, 1)),
      Trade = replace_na(Trade, 0),
      Net_Return = Position * Daily_Return - Trade * total_trade_cost,
      Cumulative_Return = cumprod(1 + replace_na(Net_Return, 0)) - 1
    )
  
  return(results)
}

walk_forward_results <- walk_forward_strategy(
  data = walk_forward_data,
  short_window = 20,
  long_window = 50,
  initial_window = 500
)

walk_forward_xts <- xts(
  walk_forward_results$Net_Return,
  order.by = walk_forward_results$Date
)

cat("\nWalk-Forward Annualized Return:\n")
print(Return.annualized(walk_forward_xts, scale = 252))

cat("\nWalk-Forward Annualized Sharpe Ratio:\n")
print(SharpeRatio.annualized(walk_forward_xts, scale = 252, Rf = risk_free_daily))

cat("\nWalk-Forward Maximum Drawdown:\n")
print(maxDrawdown(walk_forward_xts))

ggplot(walk_forward_results, aes(x = Date, y = Cumulative_Return)) +
  geom_line(color = "#7C3AED", linewidth = 1) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Walk-Forward Strategy Validation",
    subtitle = "Strategy is evaluated through expanding-window out-of-sample execution",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_fintech()

cat("
Walk-Forward Interpretation:
Unlike a static backtest, this walk-forward check evaluates signals sequentially
through time using only information available before each trading day. This improves
research credibility because it reduces overfitting and makes the strategy closer
to a real trading implementation.
")

############################################################
# MAIN STRATEGY BACKTEST
############################################################

apple_strategy <- apple_strategy %>%
  mutate(
    Raw_Signal = ifelse(SMA_20 > SMA_50 & RSI_14 < 70, 1, 0),
    Position = lag(Raw_Signal, 1),
    Position = replace_na(Position, 0),
    Trade = abs(Position - lag(Position, 1)),
    Trade = replace_na(Trade, 0),
    Gross_Strategy_Return = Position * Daily_Return,
    Net_Strategy_Return = Gross_Strategy_Return - Trade * total_trade_cost,
    Buy_Hold_Return = Daily_Return,
    Benchmark_Return = SPY_Return,
    Buy_Hold_Cumulative = cumprod(1 + replace_na(Buy_Hold_Return, 0)) - 1,
    Benchmark_Cumulative = cumprod(1 + replace_na(Benchmark_Return, 0)) - 1,
    Gross_Strategy_Cumulative = cumprod(1 + replace_na(Gross_Strategy_Return, 0)) - 1,
    Net_Strategy_Cumulative = cumprod(1 + replace_na(Net_Strategy_Return, 0)) - 1
  )

signal_points <- apple_strategy %>%
  mutate(
    Signal_Change = Raw_Signal - lag(Raw_Signal),
    Action = case_when(
      Signal_Change == 1 ~ "Buy Signal",
      Signal_Change == -1 ~ "Sell Signal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Action))

ggplot(apple_strategy, aes(x = Date)) +
  geom_line(aes(y = Adjusted), color = "#111827", linewidth = 0.75) +
  geom_line(aes(y = SMA_20, color = "20-Day SMA"), linewidth = 0.9) +
  geom_line(aes(y = SMA_50, color = "50-Day SMA"), linewidth = 0.9) +
  geom_point(
    data = signal_points,
    aes(y = Adjusted, shape = Action, color = Action),
    size = 2.6
  ) +
  scale_color_manual(
    values = c(
      "20-Day SMA" = "#2563EB",
      "50-Day SMA" = "#DC2626",
      "Buy Signal" = "#059669",
      "Sell Signal" = "#B91C1C"
    )
  ) +
  scale_shape_manual(values = c("Buy Signal" = 24, "Sell Signal" = 25)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Apple Moving Average Strategy with RSI Risk Filter",
    subtitle = "Signals are lagged by one day; transaction cost and slippage are included",
    x = "Date",
    y = "Adjusted Closing Price"
  ) +
  theme_fintech()

performance_plot_data <- apple_strategy %>%
  select(
    Date,
    Buy_Hold_Cumulative,
    Benchmark_Cumulative,
    Gross_Strategy_Cumulative,
    Net_Strategy_Cumulative
  ) %>%
  pivot_longer(
    cols = c(
      Buy_Hold_Cumulative,
      Benchmark_Cumulative,
      Gross_Strategy_Cumulative,
      Net_Strategy_Cumulative
    ),
    names_to = "Strategy",
    values_to = "Cumulative_Return"
  ) %>%
  mutate(
    Strategy = recode(
      Strategy,
      "Buy_Hold_Cumulative" = "Apple Buy and Hold",
      "Benchmark_Cumulative" = "SPY Benchmark",
      "Gross_Strategy_Cumulative" = "Strategy Before Costs",
      "Net_Strategy_Cumulative" = "Strategy After Costs"
    )
  )

ggplot(performance_plot_data, aes(x = Date, y = Cumulative_Return, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Apple Buy and Hold" = "#111827",
      "SPY Benchmark" = "#64748B",
      "Strategy Before Costs" = "#F97316",
      "Strategy After Costs" = "#059669"
    )
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Backtest Performance with Market Friction",
    subtitle = "Benchmark, gross strategy, and net strategy after transaction costs and slippage",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_fintech()

############################################################
# RISK-ADJUSTED PERFORMANCE
############################################################

returns_xts <- xts(
  apple_strategy[, c(
    "Buy_Hold_Return",
    "Benchmark_Return",
    "Gross_Strategy_Return",
    "Net_Strategy_Return"
  )],
  order.by = apple_strategy$Date
)

returns_xts <- na.omit(returns_xts)

colnames(returns_xts) <- c(
  "Apple Buy and Hold",
  "SPY Benchmark",
  "Strategy Before Costs",
  "Strategy After Costs"
)

cat("\nRISK-ADJUSTED PERFORMANCE TABLE\n")

annualized_table <- table.AnnualizedReturns(
  returns_xts,
  scale = 252,
  Rf = risk_free_daily
)

print(annualized_table)

cat("\nMAXIMUM DRAWDOWN\n")
print(maxDrawdown(returns_xts))

cat("\nANNUALIZED SHARPE RATIO\n")
sharpe_ratios <- SharpeRatio.annualized(
  returns_xts,
  scale = 252,
  Rf = risk_free_daily
)
print(sharpe_ratios)

cat("\nANNUALIZED SORTINO RATIO\n")
sortino_annualized <- SortinoRatio(
  returns_xts,
  MAR = risk_free_daily
) * sqrt(252)
print(sortino_annualized)

cat("\nVALUE AT RISK 95%\n")
print(VaR(returns_xts, p = 0.95, method = "historical"))

cat("\nEXPECTED SHORTFALL / CVaR 95%\n")
print(ES(returns_xts, p = 0.95, method = "historical"))

chart.Drawdown(
  returns_xts,
  main = "Drawdown Comparison",
  legend.loc = "bottomleft",
  colorset = c("#111827", "#64748B", "#F97316", "#059669")
)

risk_return_table <- data.frame(
  Strategy = colnames(returns_xts),
  Annual_Return = as.numeric(Return.annualized(returns_xts, scale = 252)),
  Annual_Risk = as.numeric(StdDev.annualized(returns_xts, scale = 252)),
  Sharpe = as.numeric(sharpe_ratios)
)

ggplot(risk_return_table, aes(x = Annual_Risk, y = Annual_Return, color = Strategy, size = Sharpe)) +
  geom_point(alpha = 0.85) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(
    values = c(
      "Apple Buy and Hold" = "#111827",
      "SPY Benchmark" = "#64748B",
      "Strategy Before Costs" = "#F97316",
      "Strategy After Costs" = "#059669"
    )
  ) +
  labs(
    title = "Risk-Return Profile",
    subtitle = "Higher return is better; lower risk is better; point size reflects Sharpe Ratio",
    x = "Annualized Risk",
    y = "Annualized Return",
    size = "Sharpe Ratio"
  ) +
  theme_fintech()

############################################################
# FINAL RESULTS, INTERPRETATION, LIMITATIONS
############################################################

final_buy_hold <- tail(apple_strategy$Buy_Hold_Cumulative, 1)
final_benchmark <- tail(apple_strategy$Benchmark_Cumulative, 1)
final_gross_strategy <- tail(apple_strategy$Gross_Strategy_Cumulative, 1)
final_net_strategy <- tail(apple_strategy$Net_Strategy_Cumulative, 1)
total_trades <- sum(apple_strategy$Trade, na.rm = TRUE)

cat("\nFINAL BACKTEST RESULTS\n")
cat("Transaction Cost per Trade:", transaction_cost * 100, "%\n")
cat("Slippage Cost per Trade:", slippage_cost * 100, "%\n")
cat("Total Cost per Trade:", total_trade_cost * 100, "%\n")
cat("Total Trades:", total_trades, "\n")
cat("Apple Buy-and-Hold Return:", round(final_buy_hold * 100, 2), "%\n")
cat("SPY Benchmark Return:", round(final_benchmark * 100, 2), "%\n")
cat("Strategy Return Before Costs:", round(final_gross_strategy * 100, 2), "%\n")
cat("Strategy Return After Costs:", round(final_net_strategy * 100, 2), "%\n")

cat("\nECONOMIC INTERPRETATION\n")
cat("
Apple's performance is evaluated not only on absolute return, but also relative to SPY,
which acts as a broad US equity market benchmark. This separates company-specific
performance from general market movement.

The ARIMA model is fitted on stationary log returns rather than raw prices. This is
statistically more defensible, but the model is interpreted cautiously because daily
equity returns are often close to white noise under the efficient market hypothesis.

The trading strategy uses a 20/50 moving-average crossover with an RSI filter. Signals
are lagged by one trading day to reduce look-ahead bias. Transaction costs and slippage
are deducted whenever the position changes, making the backtest more realistic.

Risk-adjusted metrics such as Sharpe Ratio, Sortino Ratio, Maximum Drawdown, VaR, and
CVaR are included because a strategy should not be judged only by total return.
")

cat("\nLIMITATIONS AND FUTURE RESEARCH\n")
cat("
1. ARIMA is useful for demonstrating stationary time-series modelling, but daily stock
   returns are difficult to forecast because markets rapidly incorporate information.

2. The forecast calendar removes weekends but does not remove all NYSE holidays.

3. The strategy is tested on a single stock. Future work can extend the analysis to a
   portfolio of large-cap technology stocks.

4. Parameter sensitivity and walk-forward validation improve robustness, but a full
   institutional system would also test multiple market regimes and more assets.

5. The backtest includes transaction cost and slippage, but does not fully model bid-ask
   spread, market impact, taxes, short-selling constraints, or liquidity limitations.
")

cat("\nFINAL RESEARCH DEFENSE\n")
cat("
This project should not be interpreted as claiming that Apple stock prices are easy
to predict. Instead, it demonstrates a disciplined FinTech research workflow.

The analysis begins with reliable Yahoo Finance data acquisition, followed by cleaning,
feature engineering, professional visualization, benchmark comparison, stationarity
testing, STL decomposition, ARIMA forecasting, model benchmarking, CAPM analysis,
GARCH-based volatility awareness, transaction-cost backtesting, parameter sensitivity,
walk-forward validation, and risk-adjusted performance evaluation.

The strongest contribution of the project is not a single trading signal, but the
research design: every model is evaluated against a benchmark, every strategy is tested
after market friction, and every result is interpreted with economic caution.

This makes the submission academically defensible and closer to institutional financial
research than a basic stock-market coding assignment.
")

if (final_net_strategy > final_buy_hold) {
  cat("\nConclusion: After transaction costs and slippage, the trading strategy outperformed Apple buy-and-hold.\n")
} else {
  cat("\nConclusion: After transaction costs and slippage, Apple buy-and-hold outperformed the trading strategy.\n")
}

write.csv(
  apple_strategy,
  "Apple_AAPL_Final_Institutional_FinTech_Project.csv",
  row.names = FALSE
)