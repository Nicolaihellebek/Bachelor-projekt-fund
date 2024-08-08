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
data_SWPPX <- read_excel("data_SWPPX_fond_daily.xlsx")

data_SWPPX_W <- read_excel("data_SWPPX_fond_weekly.xlsx")




Fund_daily_train <- ts(data_SWPPX$`Adj close`, frequency=365, start = c(2016,1))
plot(Fund_daily_train)

Fund_weekly_train <- ts(data_SWPPX_W$`Adj close`, frequency = 52)
plot(Fund_weekly_train)

###############################################################################
#Daily Fund

#Integrations orden - integreret i orden 0 I(0)
summary(dfuller.reg <- dynlm(diff(Fund_daily_train) ~0 + L(Fund_daily_train, 1)))

acf(diff_Fund_daily_train) #4 signifikante lags (1, 8, 9, 14)
pacf(diff_Fund_daily_train) #5 signifikante lags (7, 8, 14, 17, 24)

#Find den bedste model
ARIMA_daily_fund <- auto.arima(Fund_daily_train)
ARIMA_daily_fund #Arima (3,1,0) with non-zero mean



autoplot(ARIMA_daily_fund)

res_daily_fund = residuals(ARIMA_daily_fund)
Box.test(res_daily_fund, lag=10, type = "Ljung-Box")

shapiro.test(res_daily_fund)

AIC(ARIMA_daily_fund)
BIC(ARIMA_daily_fund)


# Assuming ARIMA_daily is already fitted
ahead <- 10  # Future values for forecasting
forecast_model_daily_fund <- forecast(ARIMA_daily_fund, h = ahead)
plot(forecast_model_daily)


# Convert forecast to a data frame
forecast_dates_fund <- seq(as.Date("2019-12-16"), by = "day", length.out = ahead)
forecast_df_fund <- data.frame(Date = forecast_dates, Forecast = as.numeric(forecast_model_daily_fund$mean))


# Load test data and plot it
test_daily_SWPPX <- read_excel("test_daily_SWPPX.xlsx")

# Extracting dates and actual values from test data
test_df_fund <- data.frame(
  Date = forecast_dates_fund,
  Actual = test_daily_SWPPX$`Adj Close`
)

# Plotting forecast vs actual data
ggplot() +
  geom_line(data = test_df_fund, aes(x = Date, y = Actual), color = "blue", size = 1) +
  geom_line(data = forecast_df_fund, aes(x = Date, y = Forecast), color = "red", size = 1) +
  labs(title = "Forecast vs Actual Data", x = "Date", y = "Values") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate RMSE
rmse_fund <- sqrt(mean((forecast_df_fund$Forecast - test_df_fund$Actual)^2))

# Calculate MAPE
mape_fund <- mean(abs((test_df_fund$Actual - forecast_df_fund$Forecast) / test_df_fund$Actual)) * 100



# Print the evaluation metrics
cat("RMSE daily: ", rmse_fund, "\n")
cat("MAPE daily: ", mape_fund, "%\n")

##############################################################################
#Weekly fund

#Integrations orden - integreret i orden 0 I(0)
summary(dfuller.reg <- dynlm(diff(Fund_weekly_train) ~0 + L(Fund_weekly_train, 1)))


acf(diff_Fund_weekly_train) #4 signifikante lags (1, 2, 10, 14)
pacf(diff_Fund_weekly_train) #1 signifikante lags (1)

#Find den bedste model
ARIMA_weekly = auto.arima(Fund_weekly_train)
ARIMA_weekly #Arima (0,1,1) with non- zero mean   

autoplot(ARIMA_weekly)

res_weekly = residuals(ARIMA_weekly)
Box.test(res_weekly, lag=10, type = "Ljung-Box")

shapiro.test(res_weekly)


AIC(ARIMA_weekly)
BIC(ARIMA_weekly)

#Forecasting
ahead_w <- 5  
forecast_model_weekly <- forecast(ARIMA_weekly, h = ahead_w)
plot(forecast_model_weekly)



#forecast to a data frame
forecast_dates_w <- seq(as.Date("2019-11-29"), by = "week", length.out = ahead_w)
forecast_df_w <- data.frame(Date = forecast_dates_w, Forecast = as.numeric(forecast_model_weekly$mean))


# Load test data and plot it
test_weekly_SWPPX <- read_excel("test_weekly_SWPPX.xlsx")

# Extracting dates and actual values from test data
test_df_w <- data.frame(
  Date = forecast_dates_w,
  Actual = test_weekly_SWPPX$`Adj Close`
)

#forecast vs actual data
ggplot() +
  geom_line(data = test_df_w, aes(x = Date, y = Actual), color = "blue", size = 1) +
  geom_line(data = forecast_df_w, aes(x = Date, y = Forecast), color = "red", size = 1) +
  labs(title = "Forecast vs Actual Data", x = "Date", y = "Values") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#RMSE
rmse_w <- sqrt(mean((forecast_df_w$Forecast - test_df_w$Actual)^2))

#MAPE
mape_w <- mean(abs((test_df_w$Actual - forecast_df_w$Forecast) / test_df_w$Actual)) * 100



#evaluation 
cat("RMSE weekly: ", rmse_w, "\n")
cat("MAPE weekly: ", mape_w, "%\n")






