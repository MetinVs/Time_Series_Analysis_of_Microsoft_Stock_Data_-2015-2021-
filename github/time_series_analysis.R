# ==============================================================================
# Financial Time Series Forecasting — Microsoft Stock (2015-2020)
# ==============================================================================
# Data:  Kaggle — Microsoft stock daily OHLCV, Nov 2015 - Nov 2020
#        https://www.kaggle.com/datasets/vijayvvenkitesh/microsoft-stock-time-series-analysis
# Goal:  decompose the series, test for stationarity, compare benchmark
#        forecasting methods, and fit SARIMA models at daily and monthly
#        granularity.
# ==============================================================================

# ---- 1. Setup ----------------------------------------------------------------
required_pkgs <- c("ggplot2", "zoo", "TTR", "xts", "fpp2", "tseries",
                    "imputeTS", "dplyr", "lubridate", "ggfortify", "forecast")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- 2. Load & clean data ------------------------------------------------------
file_path <- "data/yahoo_stock.csv"
dataset <- read.csv(file_path)
colnames(dataset) <- c("Date", "High", "Low", "Open", "Close", "Volume", "Adj_Close")
dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")
stopifnot(sum(is.na(dataset$Date)) == 0)

yahoo_data <- ts(dataset$Close, frequency = 365, start = c(2015, 327)) # day 327 = Nov 23

# ---- 3. Decomposition ----------------------------------------------------------
decomp_classical <- decompose(yahoo_data)
plot(decomp_classical)

decomp_stl <- stl(yahoo_data, s.window = "periodic")
plot(decomp_stl)
deseasonalized <- seasadj(decomp_stl)
plot(deseasonalized)

# Smoothing at two window lengths for comparison
sma3 <- SMA(deseasonalized, n = 3)
sma10 <- SMA(deseasonalized, n = 10)
plot.ts(sma3)
plot.ts(sma10)

# ---- 4. Structural break: COVID-19 (2020) --------------------------------------
# The series shows a clear structural break at the March 2020 crash; the
# pre-pandemic window is analysed separately since the shock destabilises the
# seasonal/trend decomposition.
dataset_xts_full <- xts(dataset$Close, order.by = dataset$Date)
pre_pandemic <- dataset_xts_full["2015-11-23/2019-12-28"]
pre_pandemic_ts <- ts(coredata(pre_pandemic), frequency = 260, start = c(2015, 1))
decomp_pre_pandemic <- decompose(pre_pandemic_ts)
plot(decomp_pre_pandemic)

# ---- 5. Exploratory visualisation ----------------------------------------------
ggplot(data = dataset, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), size = 1) +
  geom_line(aes(y = Open,  color = "Open"),  size = 1) +
  labs(title = "Microsoft Stock Price (Open vs Close)", x = "Date", y = "Price",
       color = "Series") +
  scale_color_manual(values = c("Close" = "blue", "Open" = "red"))

ggseasonplot(yahoo_data, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Seasonal Plot: MSFT Close Price")
ggseasonplot(yahoo_data, polar = TRUE) + ylab("$") +
  ggtitle("Polar Seasonal Plot: MSFT Close Price")
ggsubseriesplot(deseasonalized) + ylab("$") +
  ggtitle("Seasonal Subseries Plot: MSFT Close Price (deseasonalised)")
gglagplot(window(yahoo_data, start = 2019))

# ---- 6. Stationarity: ADF test + differencing -----------------------------------
adf.test(yahoo_data)                        # p > 0.05 -> series is non-stationary
ggAcf(yahoo_data)                           # slow decay confirms non-stationarity

yahoo_diff1        <- diff(yahoo_data)                 # first difference
yahoo_diff2        <- diff(yahoo_data, differences = 2)
yahoo_log_diff      <- diff(log(yahoo_data))            # log-return
yahoo_scaled_diff   <- diff(scale(yahoo_data))
yahoo_seasonal_diff <- diff(yahoo_data, lag = 7)         # weekly seasonal difference

for (series in list(yahoo_diff1, yahoo_diff2, yahoo_log_diff,
                     yahoo_scaled_diff, yahoo_seasonal_diff)) {
  print(ggAcf(series))
}

Box.test(yahoo_diff1, lag = 30, type = "Ljung-Box")
# Residual autocorrelation persists across every transformation tried
# (differencing, log-return, scaling, seasonal differencing) -- a common
# feature of daily financial data rather than a data-prep error.

# ---- 7. Benchmark forecasts: Mean / Naive / Seasonal Naive -----------------------
h <- 30
train_window <- window(log(yahoo_data), start = 2017, end = c(2020))
autoplot(train_window) +
  autolayer(meanf(train_window, h = h),   series = "Mean",           PI = FALSE) +
  autolayer(naive(train_window, h = h),   series = "Naive",          PI = FALSE) +
  autolayer(snaive(train_window, h = h),  series = "Seasonal Naive", PI = FALSE) +
  ggtitle("Benchmark Forecasts (log Close price)") + xlab("Year") + ylab("log(Price)") +
  guides(colour = guide_legend(title = "Forecast"))
# Naive consistently outperforms Mean and Seasonal Naive on this series --
# expected for a near-random-walk price series with no strong seasonal signal.

# ---- 8. Price indices -----------------------------------------------------------
base_price <- dataset$Close[1]
dataset$Fixed_Base_Index <- (dataset$Close / base_price) * 100

dataset$Price_Change_Rate <- c(NA, diff(log(dataset$Close)))
dataset$Chained_Index <- cumprod(1 + ifelse(is.na(dataset$Price_Change_Rate), 0,
                                             dataset$Price_Change_Rate)) * 100

# Laspeyres / Paasche / Fisher indices need a quantity series; none exists for
# this dataset, so quantities are simulated for illustration only.
set.seed(1)
quantities   <- ts(runif(length(yahoo_data), min = 50, max = 150),
                    start = start(yahoo_data), frequency = frequency(yahoo_data))
base_year    <- start(yahoo_data)[1]
base_price_w <- window(yahoo_data, start = base_year, end = base_year)
base_qty_w   <- window(quantities, start = base_year, end = base_year)

laspeyres <- sum(base_price_w * quantities) / sum(base_price_w * base_qty_w)
paasche   <- sum(yahoo_data   * quantities) / sum(base_price_w * quantities)
fisher    <- sqrt(laspeyres * paasche)
cat("Laspeyres:", laspeyres, "| Paasche:", paasche, "| Fisher:", fisher, "\n")

# ---- 9. SARIMA modelling ---------------------------------------------------------
dataset_xts <- xts(dataset$Close, order.by = dataset$Date)

## Model 1: daily
sarima_daily <- auto.arima(dataset_xts, seasonal = TRUE, stepwise = TRUE,
                            approximation = FALSE)
summary(sarima_daily)
forecast_daily <- forecast(sarima_daily, h = 30)
plot(forecast_daily, main = "SARIMA — Daily Forecast (Model 1)")

## Model 2: monthly average (seasonal period = 12)
monthly_avg <- apply.monthly(dataset_xts, function(x) mean(x, na.rm = TRUE))
monthly_ts <- ts(as.numeric(monthly_avg),
                  start = c(year(index(monthly_avg)[1]), month(index(monthly_avg)[1])),
                  frequency = 12)
sarima_monthly <- auto.arima(monthly_ts, seasonal = TRUE)
summary(sarima_monthly)
forecast_monthly <- forecast(sarima_monthly, h = 12)
plot(forecast_monthly, main = "SARIMA — Monthly Forecast (Model 2)")

# Model comparison (see README for the full write-up):
#   Model 1 (daily)   -- residual autocorrelation 0.0003, residual variance 5.14
#   Model 2 (monthly) -- residual autocorrelation -0.1291, residual variance 34.04
# Model 1 fits better on both metrics; Model 2's 12-period seasonality can
# still be useful when a monthly-resolution forecast is the actual need.
