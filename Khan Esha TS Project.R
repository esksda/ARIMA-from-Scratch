# Checking the working directory
getwd()
# Loading the necessary libraries to conduct time series analysis
library(tseries)
library(urca)
library(forecast)
library(TSA)
library(readr)
# Importing the dataset 
Case_study = read_csv("Case_study.csv")

# Specifying my assigned column V48 as time series data
data = ts(Case_study$V48)

#Checking whether the dats is time series data ot not
class(data)
time(data)
season(data) # there is no seasonality

# PLotting the time series data
plot(data, main = "Time Series Plot of V48", xlab = "Time", ylab = "V48")
## This time series is not stationary by visual representation
## It is showing an increasing trend and an increasing Varience

# Augmented Dickey–Fuller test
adf.test(data, alternative = "stationary")
# H0 : Data is not stationary
# High p value, H0 is not rejected
# Need to differencing for order untill H0 gets rejected

# Autocorrelation and partial aurocorrelation at different lags
par(mfrow = c(1,2))
acf(data) # acf is dampening as the lags increases
pacf(data) # pacf is zero after lag 1
par(mfrow = c(1,1))

# the recommended model is AR(1) model
# blue is the significant zero line
# Significant zeroes are after lag 1
# So the approach will be AR(1)

## To make the data stationary, I am going to difference the ts data
diff1 = diff(data, differences = 1) ; diff1
class(diff1)
plot(diff1, main = "When d = 1")
# Showing non constant varience and also formed a curvier shape
par(mfrow = c(1,2))
acf(diff1)
pacf(diff1)
par(mfrow = c(1,1))
adf.test(diff1)$p.value
# not stationary at k = 1

diff2 = diff(data, differences = 2) ; diff2
plot(diff2)
par(mfrow = c(1,2))
acf(diff2)
pacf(diff2)
par(mfrow = c(1,1))
adf.test(diff2) # p = 0.01 Stationary at k = 2

## Based on the plotting main data, diff1, diff2,
## maximum we can suggest is lag 4 for as p AR(4) and MA(4)

# Define the Upper bound of p and q
pmax = 4
qmax = 4

# Create an empty dataframe to store AIC and BIC values
AIC_BIC = data.frame(p = integer(0), q = integer(0), AIC = numeric(0), BIC = numeric(0))

# Loop through all combinations of p and q
for (p in 1:pmax) {
  for (q in 1:qmax) {
    # Skip combinations (0, 0), (0, q), and (p, 0)
    if (p == 0 && q == 0) next
    if (p == 0 || q == 0) next
    
    # Fit ARIMA model
    model = arima(data, order = c(p, 2, q))
    
    # Compute AIC, BIC
    aic = AIC(model)
    bic = BIC(model)
    
    # Store results in the dataframe
    AIC_BIC = rbind(AIC_BIC, data.frame(p = p, q = q, AIC = aic, BIC = bic))
  }
}

# Printing the dataframe with AIC and BIC values
print(AIC_BIC)
which.min(AIC_BIC[,"AIC"])
which.min(AIC_BIC[,"BIC"])
# Both showing index 4
AIC_BIC[4,]
model_1 = arima(data, order = c(1,2,4))
summary(model_1)
res = resid(model_1)

# H0: Data is not stationary
adf.test(res)
# since p = 0.01, res is stationary
par(mfrow = c(1,2))
acf(res)
pacf(res)
par(mfrow = c(1,1))
# H0 : There is no autocorrelation in the residuals
Box.test(res, lag = 10, type = "Ljung-Box")
# We can not reject null hypothesis
# There is no correlation in the residuals in the model

### auto arima and arima(2,2,1) is same thing
### Second lowest BIC
my_arima = auto.arima(data) ; my_arima
my_res = resid(my_arima) ; my_res
# H0: Data is not stationary
adf.test(my_res)
# since p = 0.01, my_res is stationary
par(mfrow = c(1,2))
acf(my_res)
pacf(my_res)
par(mfrow = c(1,1))
# H0 : There is no autocorrelation in the residuals
Box.test(my_res, lag = 10, type = "Ljung-Box")
# p value is 0.18
# There is no autocorrelation
###
# Normality test
hist(res)
qqnorm(res)
qqline(res)


# H0: Observations are normally distributed
shapiro.test(res)

# Skewness and Kurtosis observation
library(moments)
skew = skewness(res) ; skew
kurt = kurtosis(res) ; kurt


## testing for arima(2,2,1)
hist(my_res)
qqnorm(my_res)
qqline(my_res)
shapiro.test(my_res)
skewness(my_res)
kurtosis(my_res)

#	Plotting original time series and best fitted model (2,2,1)
plot(arima(data, order = c(2,2,1)))

# forecasting 
my_arima = auto.arima(data)
my_arima
# When h=10 and level=95
forecast_10 = forecast(my_arima, h = 10, level = 95) ; forecast_10
# When h=25 and level=95
forecast_25 = forecast(my_arima, h = 25, level = 95) ; forecast_25

# Plotting the original time series with forecasts and confidence intervals (10,95)
plot(data, main = "Forecast with 95% Confidence Interval (h = 10)",
     xlab = "Time", ylab = "Value", xlim = c(0,120))
lines(forecast_10$mean, col = "blue")
lines(forecast_10$lower, col = "red", lty = 2)
lines(forecast_10$upper, col = "red", lty = 2)
legend("topleft", legend = c("Original", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2))

# Plot the original time series with forecasts and confidence intervals for h = 25
plot(data, main = "Forecast with 95% Confidence Interval (h = 25)",
     xlab = "Time", ylab = "Value", xlim = c(0,140))
lines(forecast_25$mean, col = "blue")
lines(forecast_25$lower, col = "red", lty = 2)
lines(forecast_25$upper, col = "red", lty = 2)

# Add legend
legend("topleft", legend = c("Original", "Forecast", "95% CI"), col = c("black", "blue", "red"), lty = c(1, 1, 2))

