**Time Series Analysis of Microsoft Stock Data (2015–2021)**

This project involves a comprehensive time series analysis of Microsoft stock data using R. The dataset spans from April 2015 to April 2021 and includes daily stock prices such as Open, Close, High, Low, and Volume.

**Objective**
- To apply time series decomposition, forecasting, and indexing techniques on real-world financial data.
- To compare and evaluate different modeling and forecasting methods (Mean, Naïve, Seasonal Naïve, SARIMA).
- To examine seasonality, stationarity, autocorrelation, and structural breaks within the time series.

**Dataset**
- Source: [Kaggle – Microsoft Stock Data](https://www.kaggle.com/datasets/vijayvvenkitesh/microsoft-stock-time-series-analysis)
- Variables: `Date`, `Open`, `High`, `Low`, `Close`, `Volume`
- Frequency: Daily (converted to weekly and monthly in parts)

**Key Steps**
- **Data preprocessing**: Missing value checks, data formatting, filtering pre-COVID data (removal of structural breaks).
- **Decomposition**: Classical and STL to extract trend & seasonality.
- **Stationarity tests**: ADF Test, differencing, log transform, scaling.
- **Forecasting models**:
  - **SARIMA** (daily & monthly)
  - Mean / Naïve / Seasonal Naïve forecasts
- **Index Construction**:
  - Fixed-base and chained index
  - Laspeyres, Paasche, and Fisher price indices
- **Visualization**:
  - Time series plots, seasonal subseries, lag plots, polar seasonal plots
  - ACF & PACF diagnostics

**Tools & Libraries**
- `R`, `fpp2`, `forecast`, `TTR`, `xts`, `zoo`, `ggplot2`, `imputeTS`, `lubridate`, `tseries`

**Key Insights**
- The time series contains clear seasonality and an increasing trend.
- Structural breaks due to COVID-19 were handled by trimming data.
- ADF tests showed non-stationarity; differencing was applied.
- SARIMA models outperformed simple forecasts (lower RMSE, AIC).
- Data visualization was key in understanding autocorrelation and seasonality patterns.

**Conclusion**
This project helped translate theoretical knowledge into practical forecasting workflows. Challenges such as removing autocorrelation and achieving full stationarity were met with various transformations, and visual tools played a critical role in iterative model development.

