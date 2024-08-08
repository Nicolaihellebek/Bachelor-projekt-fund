# Install and load necessary packages
install.packages(c("forecast", "tseries", "lmtest", "ggplot2"))
install.packages("quantmod")
install.packages("readxl")
install.packages("xts")
install.packages("ggplot2")
install.packages("zoo")
library(readxl)
library(forecast)
library(tseries)
library(lmtest)
library(ggplot2)
library(quantmod)
library(pwt8)
library(tstools)
library(xts)
library(ggplot2)
library(zoo)
library(dynlm)

#data anvendt
data_SPY <- read_excel("C:/NH_computer/data_SPY_ETF_daily.xlsx")

data_SPY_W <- read_excel("C:/Users/nicol/OneDrive/Skrivebord/data_SPY_ETF_weekly.xlsx")




ETF_daily_train <- ts(data_SPY$`Adj Close`, frequency=365, start = c(2016,1))
plot(ETF_daily_train)

ETF_weekly_train <- ts(data_SPY_W$`Adj Close`, frequency = 52)
plot(ETF_weekly_train)

###############################################################################
#Daily ETF

#Integrations orden - integreret i orden 0 I(0)
summary(dfuller.reg <- dynlm(diff(ETF_daily_train) ~0 + L(ETF_daily_train, 1)))
#kan afvise H0
diff_ETF_daily_train <- diff(ETF_daily_train)

#uden konstant og trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_daily_train) ~0 + L(diff_ETF_daily_train, 1)))
#Med konstant og uden trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_daily_train) ~1 + L(diff_ETF_daily_train, 1)))
#med konstant og trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_daily_train) ~1 + L(diff_ETF_daily_train, 1) + trend(diff_ETF_daily_train)))
#alle 3 afviser at der er en unit root tilstede


acf(diff_ETF_daily_train) #4 signifikante lags (1, 8, 9, 15)
pacf(diff_ETF_daily_train) #5 signifikante lags (7, 8, 13, 16, 23)

#Find den bedste model
ARIMA_daily = auto.arima(ETF_daily_train)
ARIMA_daily #Arima (3,1,0)   

autoplot(ARIMA_daily)

res_daily = residuals(ARIMA_daily)
Box.test(res_daily, lag=10, type = "Ljung-Box")

shapiro.test(res_daily)

AIC(ARIMA_daily)
BIC(ARIMA_daily)

# Assuming ARIMA_daily is already fitted
ahead <- 10  # Future values for forecasting
forecast_model_daily <- forecast(ARIMA_daily, h = ahead)
plot(forecast_model_daily)

# Convert forecast to a data frame
forecast_dates <- seq(as.Date("2019-12-16"), by = "day", length.out = ahead)
forecast_df <- data.frame(Date = forecast_dates, Forecast = as.numeric(forecast_model_daily$mean))


# Load test data and plot it
test_daily_SPY <- read_excel("test_daily_SPY.xlsx")

# Extracting dates and actual values from test data
test_df <- data.frame(
  Date = forecast_dates,
  Actual = test_daily_SPY$`Adj Close`
)

# Plotting forecast vs actual data
ggplot() +
  geom_line(data = test_df, aes(x = Date, y = Actual), color = "blue", size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "red", size = 1) +
  labs(title = "Forecast vs Actual Data", x = "Date", y = "Values") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate RMSE
rmse <- sqrt(mean((forecast_df$Forecast - test_df$Actual)^2))

# Calculate MAPE
mape <- mean(abs((test_df$Actual - forecast_df$Forecast) / test_df$Actual)) * 100


# Print the evaluation metrics
cat("RMSE daily: ", rmse, "\n")
cat("MAPE daily: ", mape, "%\n")

##############################################################################
#Weekly ETF

#Integrations orden - integreret i orden 0 I(0)
summary(dfuller.reg <- dynlm(diff(ETF_weekly_train) ~0 + L(ETF_weekly_train, 1)))
#kan afvise H0
diff_ETF_weekly_train <- diff(ETF_weekly_train)

#uden konstant og trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_weekly_train) ~0 + L(diff_ETF_weekly_train, 1)))
#Med konstant og uden trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_weekly_train) ~1 + L(diff_ETF_weekly_train, 1)))
#med konstant og trend
summary(dfuller.reg <- dynlm(diff(diff_ETF_weekly_train) ~1 + L(diff_ETF_weekly_train, 1) + trend(diff_ETF_weekly_train)))
#alle 3 afviser at der er en unit root tilstede


acf(diff_ETF_weekly_train) #3 signifikante lags (1, 2, 9, 14)
pacf(diff_ETF_weekly_train) #1 signifikante lags (1)

#Find den bedste model
ARIMA_weekly = auto.arima(ETF_weekly_train)
ARIMA_weekly #Arima (0,1,1) drift   

autoplot(ARIMA_weekly)

res_weekly = residuals(ARIMA_weekly)
Box.test(res_weekly, lag=10, type = "Ljung-Box")

shapiro.test(res_weekly)

AIC(ARIMA_weekly)
BIC(ARIMA_weekly)

# Assuming ARIMA_daily is already fitted
ahead_w <- 5  # Future values for forecasting
forecast_model_weekly <- forecast(ARIMA_weekly, h = ahead_w)
plot(forecast_model_weekly)

# Convert forecast to a data frame
forecast_dates_w <- seq(as.Date("2019-11-29"), by = "week", length.out = ahead_w)
forecast_df_w <- data.frame(Date = forecast_dates_w, Forecast = as.numeric(forecast_model_weekly$mean))


# Load test data and plot it
test_weekly_SPY <- read_excel("test_weekly_SPY.xlsx")

# Extracting dates and actual values from test data
test_df_w <- data.frame(
  Date = forecast_dates_w,
  Actual = test_weekly_SPY$`Adj Close`
)

# Plotting forecast vs actual data
ggplot() +
  geom_line(data = test_df_w, aes(x = Date, y = Actual), color = "blue", size = 1) +
  geom_line(data = forecast_df_w, aes(x = Date, y = Forecast), color = "red", size = 1) +
  labs(title = "Forecast vs Actual Data", x = "Date", y = "Values") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate RMSE
rmse_w <- sqrt(mean((forecast_df_w$Forecast - test_df_w$Actual)^2))

# Calculate MAPE
mape_w <- mean(abs((test_df_w$Actual - forecast_df_w$Forecast) / test_df_w$Actual)) * 100



# Print the evaluation metrics
cat("RMSE weekly: ", rmse_w, "\n")
cat("MAPE weekly: ", mape_w, "%\n")
