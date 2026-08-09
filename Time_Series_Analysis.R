install.packages("ggplot2")
library('ggplot2')
install.packages("zoo")
library("zoo")
install.packages("TTR")
library("TTR")
install.packages("xts")
library(xts)
install.packages('fpp2', dependencies = TRUE)
library("fpp2")
library(xts)
library(tseries)
library(zoo)
library(stats)
install.packages("imputeTS")
library(imputeTS)
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("ggfortify")
install.packages("forecast")
library("ggfortify")
library(forecast)
# ts() func 2020-present random veri
sales <- c(0.982716, 1.697239, 0.323146, 1.606725, 0.758019, 1.225429, 1.705847, 
           0.679039, 0.849232, 1.149105, 0.926340, 1.392482, 1.416663, 1.684588, 
           1.091457, 0.895126, 1.385147, 1.216367, 1.197553, 0.601648, 0.846092, 
           1.493991, 0.884929, 1.357743, 0.675743, 1.004754, 1.115058, 1.396485, 
           0.704323, 1.447149, 1.703308, 1.536081, 0.944853, 1.198320, 1.053133, 
           1.542146, 1.095249, 0.520796, 1.370186, 0.504466, 1.144440, 1.270736, 
           1.384385, 0.670116, 1.157045, 1.314007, 0.777445, 0.493667, 1.523826, 
           0.492337, 1.394343, 1.362239, 1.357225, 0.348570, 0.441050, 1.219929,
           0.798493,1.294245,0.926323)
sales_ts <- ts(sales, start = c(2020, 1), frequency = 12)
print(sales_ts)

plot.ts(sales_ts, main = "Monthly Time Serie", ylab = "Sales", xlab = "Year")
logsales_ts <- log(sales_ts)
plot.ts(logsales_ts)
dytime <- ts(sales_ts, frequency=4, start=c(2010,1))
dycomponents <- decompose(dytime)
print(dycomponents)
dycomponents$seasonal
dycomponents$trend

----------------------------------------------------------------------------------

#lecturedata pdf
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted) 
plot(birthstimeseriescomponents) #lecturedata

#--------------------------------------------------------------------------------
#My Data  
file_path <- "/Users/metinvs/Desktop/Akademi/Statistics/Present/Time Series/Microsoft_Stock.csv"
dataset <- read.csv(file_path)
colnames(dataset) <- c("Date", "Open", "High", "Low", "Close", "Volume")
dataset$Date <- as.Date(dataset$Date, format="%m/%d/%Y")
sum(is.na(dataset$Date))
mcstimeseries <- ts(dataset, frequency=260, start=c(2016))
head(dataset)
summary(dataset)

mcstimeseries2 <- ts(dataset$Close, frequency=260, start=c(2015, 1))
mcstimeseriescomponents <- decompose(mcstimeseries2)
mcstimeseriescomponents$seasonal
mcstimeseriescomponents <- decompose(mcstimeseries2)
mcstimeseriesseasonallyadjusted <- mcstimeseries2 - mcstimeseriescomponents$seasonal
plot(mcstimeseriescomponents) 

seassontestdata <- ts(dataset$Close, start = c(2015, 1), frequency = 260)
seasonal_decomposition <- stl(seassontestdata, s.window = "periodic")
plot(seasonal_decomposition)

# As we see in the plots there was a seassonal effect in our data. Also we see increasing trend. 
#And our data has structural break at 2020 global pandemic. after the pandemic our data looks more unstable
#So we delete after pandemic data
index(mcstimeseries) <- as.Date(index(mcstimeseries))
dataset_filtered <- mcstimeseries["2015-01-01/2019-12-28"]
dataset_filtered2 <- ts(dataset_filtered$Close, frequency=260, start=c(2015, 1))
filtreddatacomponents <- decompose(dataset_filtered2)
filtreddatacomponents$seasonal
filtreddatacomponents <- decompose(dataset_filtered2)
dataset_filteredseasonallyadjusted <- dataset_filtered2 - filtreddatacomponents$seasonal
plot(filtreddatacomponents) 
plot(dataset_filtered)

plot(dataset) 
plot(dataset$Volume) #mydata
plot(dataset$High) #mydata
plot(dataset$Low) #mydata
plot(dataset$Close) #mydata
plot(dataset$Open) #mydata
ggplot(data = dataset, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), size = 1) +  # Close çizgisi
  geom_line(aes(y = Open, color = "Open"), size = 1) +    # Open çizgisi
  labs(title = "Microsoft Stock Prices (Open ve Close)",
       x = "Date",
       y = "Price",
       color = "Price Type") +
  scale_color_manual(values = c("Close" = "blue", "Open" = "red")) 

#Mevsimsellik var dersek seriyi arındırma
# Veriyi yalnızca 'Close' sütunu ile ts formatına dönüştürme
ts_data <- ts(coredata(mcstimeseries[,"Close"]), start = c(2015, 1), frequency = 260)
seasonal_decomposition <- stl(ts_data, s.window = "periodic")
detrended_data <- seasadj(seasonal_decomposition)
sales_tsseasonaladjusted <- dytime - dycomponents$seasonal
plot(sales_tsseasonaladjusted) 

# Mevsimsellik yok dersek seriyi düzleştirme.. Bu HO'da QTR'ler üstündeyse çok mantıklı değil gibi
sales_tsseasonaladjustedSMA3 <- SMA(sales_tsseasonaladjusted,n=3) 
plot.ts(sales_tsseasonaladjustedSMA3) 
sales_tsseasonaladjustedSMA10 <- SMA(sales_tsseasonaladjusted,n=10) 
plot.ts(sales_tsseasonaladjustedSMA10) 

#lecturedata
#---------------------------------------------------------------------------------------------------------
install.packages('fpp2', dependencies = TRUE)
library("fpp2")
data(a10)
autoplot(a10)
library(forecast)
ggseasonplot(ts_data, year.labels = TRUE, year.labels.left = TRUE) + ggtitle("Seasonal Plot: Microsoft Stock Data")
autoplot(mcstimeseries[,"Close"])
#ggseasonplot(ts_data, polar = TRUE) + 
 # ylab("$") + 
 #ggtitle("Polar Seasonal Plot: Microsoft Stock Price Data") + 
 #scale_x_continuous(breaks = seq(1, length(ts_data), by = 65), 
 #                  labels = c("Q1", "Q2", "Q3", "Q4"))
#veri 1511 elemanlı asal sayı olduğu için düzgün biçimde çeyreklere bölemedik. o yüzden quarter grafik yapamadım.



# Zaman serisini kontrol etme
head(ts_data)

# ggseasonplot ile mevsimsel grafik oluşturma
ggseasonplot(ts_data, polar = TRUE) + 
  ylab("$") + 
  ggtitle("Polar Seasonal Plot: Microsoft Stock Data") + 
  scale_x_continuous(
    breaks = seq(1, length(ts_data), by = 65),  # 65 günlük aralıklarla
  )

#Similar Data (auscafe)
autoplot(ts_data)
ggseasonplot(sales_tsseasonaladjusted, year.labels=TRUE, year.labels.left=TRUE) + ylab("$") + ggtitle("Seasonal plot: Microsoft Stock Data") 
ggseasonplot(sales_tsseasonaladjusted, polar=TRUE) + ylab("$") + ggtitle("Polar seasonal plot: Microsoft Stock Data") 
ggsubseriesplot(sales_tsseasonaladjusted) + ylab("$") + ggtitle("Seasonal subseries plot: Microsoft Stock Data")

#lecturedata
data(elecdemand) > autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) + xlab("Year: 2014") + 
  ylab("") + ggtitle("Half-hourly electricity demand: Victoria, Australia") 
#lecturedata (visnights)
autoplot(visnights[,1:5], facets=TRUE) + ylab("Number of visitor nights each quarter (millions)") 
autoplot(uschange[,1:4], facets=TRUE) + ylab("US Personal Consumption and Income Growth Rates") 

#Another comparison
install.packages('GGally', dependencies = TRUE)
GGally::ggpairs(as.data.frame(ts_data))
GGally::ggpairs(as.data.frame(visnights[,1:5]))

#delay diagrams comparisons
data(ausbeer)
beer2 <- window(ausbeer, start=1992) 
gglagplot(beer2) 

data(qcement)
qcement2 <- window(qcement, start=1992)
gglagplot(qcement2)

data(qauselec)
qauselec2 <- window(qauselec, start=1992)
gglagplot(qauselec2)

data(euretail)
euretail2 <- window(euretail, start=1992)
gglagplot(euretail2)

#My delayed data
ts_datadelay <- window(ts_data, start=2016)
gglagplot(ts_datadelay)
ts_datadelay <- window(ts_data, start=2017)
gglagplot(ts_datadelay)
ts_datadelay <- window(ts_data, start=2019)
gglagplot(ts_datadelay)
ts_datadelay <- window(ts_data, start=2020)
gglagplot(ts_datadelay)


#Autocorrelationtest
recent_production <- acf(ts_data, lag_max = 9) 
recent_production <- acf(euretail2, lag_max = 9) 
recent_production <- acf(qcement2, lag_max = 9) 
recent_production <- acf(qgas2, lag_max = 9) 
recent_production <- acf(beer2, lag_max = 9) 
recent_production <- acf(qauselec2, lag_max = 9) 

#veri durağan değil
#Dickley-Fuller Testi
adf.test(ts_data)
#test sonucu p-value > 0.05 olduğu için serimiz durapan değildir. şimdi fark alma işlemi yapıcam
ts_data_diff <- diff(ts_data)
acf(ts_data_diff, lag.max = 30)  # İlk 30 günlük gecikmeleri analiz ettik ve güçlü bir otokorelasyon olduğunu gördük
ts_data_diff2 <- diff(ts_data, differences = 2)
acf(ts_data_diff2, lag.max = 30)

ts_data_stl <- stl(ts_data, s.window = "periodic")  # # STL ile ayrıştırma
seasonal_component <- ts_data_stl$time.series[, "seasonal"]
ts_data_adjusted <- ts_data - seasonal_component
acf(ts_data_adjusted, lag.max = 30)

ts_data_log <- log(ts_data)
ts_data_diff <- diff(ts_data_log)
acf(ts_data_diff, lag.max = 30)

ts_data_scaled <- scale(ts_data)
ts_data_diff <- diff(ts_data_scaled)
acf(ts_data_diff, lag.max = 30)

# p-value = 0.01 çıktı.
recent_production <- acf(ts_data, lag_max = 9) 
recent_production <- acf(msctimeseries, lag_max = 9) 


Box.test(ts_data_diff, lag = 30, type = "Ljung-Box")
#seride hala otokorelasyon var. pes edicem

ts_data_seasonal_diff <- diff(ts_data, lag = 7)  # Haftalık mevsimsellik farklaması
acf(ts_data_seasonal_diff, lag.max = 30)

##Sonuç olarak Mevsimsellik ve otokorelasyondan arındırmadım....
#same outout different code
ggAcf(euretail2) 
ggAcf(qcement2) 
ggAcf(qgas2) 
ggAcf(beer2) 
ggAcf(qauselec2) 
ggAcf(ts_data_diff2)
#There was no seasonalling for our auscafe data because graph not like a tomb
auscafeds <- window(auscafe, start=1990)
autoplot(auscafeds) + xlab("Year") + ylab("SPP") 
ggAcf(auscafeds, lag=48) 

#Comparising lecture and random data// Both series has white noise because there was no outlier value 
set.seed(30) 
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise") 
set.seed(50) 
x <- ts(rnorm(80))
autoplot(y) + ggtitle("White noise")
ggAcf(y) 
ggAcf(x) 


beer2 <- window(ausbeer,start=1992,end=c(2007,4)) 
autoplot(beer2) +
  autolayer(meanf(beer2, h=11), series = "Mean", PI = FALSE) +
  autolayer(naive(beer2, h=11), series = "Naïve", PI = FALSE) +
  autolayer(snaive(beer2, h=11), series = "Seasonal naïve", PI = FALSE) +
  xlab("Year") +
  ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))

#predict with log data
ts_dataclose <- window(ts_data_log,start=2015,end=c(2020,4)) 
autoplot(ts_dataclose) +autolayer(meanf(ts_dataclose, h=11),series="Mean", PI=FALSE) +
  autolayer(naive(ts_dataclose, h=11),series="Naïve", PI=FALSE) +
  autolayer(snaive(ts_dataclose, h=11),series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Stock Price") +xlab("Year") + ylab("Stock Price") +
  guides(colour=guide_legend(title="Forecast")) 
#predict with normal data
ts_dataclose <- window(ts_data,start=2015,end=c(2019,4)) 
autoplot(ts_dataclose) +
  autolayer(meanf(ts_dataclose, h=11), series = "Mean", PI = FALSE) +
  autolayer(naive(ts_dataclose, h=11), series = "Naïve", PI = FALSE) +
  autolayer(snaive(ts_dataclose, h=11), series = "Seasonal naïve", PI = FALSE) +
  xlab("Year") +
  ylab("Close Price") +
  guides(colour = guide_legend(title = "Forecast"))
###naive method more usefull than others
#------------------------------------------------------------
# Part 1-2
# Sabit Esaslı İndeks
base_price <- dataset$Close[1]  # İlk gözlem
dataset$Fixed_Base_Index <- (dataset$Close / base_price) * 100

# Bileşik Esaslı İndeks
dataset$Price_Change_Rate <- c(NA, diff(log(dataset$Close)))  # Günlük değişim oranı
dataset$Chained_Index <- cumprod(1 + ifelse(is.na(dataset$Price_Change_Rate), 0, dataset$Price_Change_Rate)) * 100

# Laspeyres, Paasche ve Fischer Endeksleri Hesaplama

# Örnek veri seti
# ts_data serisinin fiyatlar olduğunu varsayıyoruz
# Miktar verisini de tanımlamanız gerekiyor
fiyatlar <- ts_data
miktarlar <- ts(runif(length(ts_data), min = 50, max = 150), start = start(ts_data), frequency = frequency(ts_data))

baz_yil <- start(ts_data)[1]
baz_fiyat <- window(close, start = baz_yil, end = baz_yil)
baz_miktar <- window(miktarlar, start = baz_yil, end = baz_yil)

laspeyres <- sum(baz_fiyat * miktarlar) / sum(baz_fiyat * baz_miktar)
paasche <- sum(fiyatlar * miktarlar) / sum(baz_fiyat * miktarlar)
fischer <- sqrt(laspeyres * paasche)
cat("Laspeyres Endeksi:", laspeyres, "\n")
cat("Paasche Endeksi:", paasche, "\n")
cat("Fischer Endeksi:", fischer, "\n")


# Date değişkenini zaman serisi indeksi olarak ayarla
dataset$Date <- as.Date(dataset$Date)
dataset_xts <- xts(dataset$Close, order.by = dataset$Date)
# SARIMA 
sarima_model <- auto.arima(dataset_xts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
# Model summary
summary(sarima_model)

# 30 day predict
forecast_result <- forecast(sarima_model, h = 30)
plot(forecast_result, main = "SARIMA Modeli ile Günlük Tahmin")
lines(daily_data, col = "blue", lwd = 2, type = "l")

##Seriyi aylık veriye ddönüştürüp çözdüğüm kısım aşağıda. Ben hem günlük hem aylık tahminleme denedim.

dataset_xts <- xts(dataset$Close, order.by = dataset$Date)
monthly_avg <- apply.monthly(dataset_xts, function(x) mean(x, na.rm = TRUE))

# Aylık ortalama zaman serisini bir R ts objesine dönüştür
monthly_ts <- ts(
  as.numeric(monthly_avg),
  start = c(year(index(monthly_avg)[1]), month(index(monthly_avg)[1])),
  frequency = 12
)
# Modelleme
sarima_model2 <- auto.arima(monthly_ts, seasonal = TRUE)
forecast_result <- forecast(sarima_model2, h = 12)  # 12 aylık tahmin
plot(forecast_result, main = "SARIMA Modeli ile Aylık Tahmin")
lines(monthly_ts, col = "blue", lwd = 2, type = "l")
summary(sarima_model2)

