# Load necessary libraries
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(tsibble)
library(feasts)
library(forecast)
library(fpp2)
library(tseries)
library(urca)
library(ggplot2)
library(fabletools)
library(AICcmodavg)
library(fpp3)
library(MuMIn)

# Set working directory (adjust this to your path)
knitr::opts_knit$set(root.dir = "C:/Users/Aspire 5/Desktop/Gratia Files/UP BS AMAT/2ND YEAR 2ND SEM/AMAT 132/Group Project")

# Clear the environment
rm(list = ls())

# Set working directory (adjust this to your path)
setwd("/Users/Aspire 5/Desktop/Gratia Files/UP BS AMAT/2ND YEAR 2ND SEM/AMAT 132/Group Project")

# Load the data
mydata <- read.csv("PhilHealth_admissions_diarrhoea_individual_2006-2018.csv")

# Filter data based on desired condition
filtered_data <- mydata %>%
  filter(Illness1Code %in% c("A06.0", "A06.1", "A06.2", "A06.9",
                             "A09.0", "A09.1", "A03.9", "A04.9", "A05.0",
                             "A06.4", "A06.8", "A02.9", "A05.9", "A02.0",
                             "A02.1", "A04.1", "A04.8", "A06.3", "A05.2", "A04.2"))

# Convert AdmitDate to a date format
filtered_data <- filtered_data %>%
  mutate(AdmitDate = dmy(AdmitDate))

# Generate Year and Month columns from AdmitDate
filtered_data <- filtered_data %>%
  mutate(
    Year = year(AdmitDate),
    Month = month(AdmitDate)
  )

# Create a Month column and count admissions per month
filtered_data_ts <- filtered_data %>%
  mutate(Month = yearmonth(AdmitDate)) %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  arrange(Month) %>%
  as_tsibble(index = Month)

# Fill gaps in the time series
filtered_data_ts <- filtered_data_ts %>%
  fill_gaps(Count = 0)

# Filter the data to include only the years from 2010 to 2018
filtered_data_ts <- filtered_data_ts %>%
  filter(year(Month) >= 2010 & year(Month) <= 2018)

# View the filtered data
filtered_data_ts


# Set training data from Jan 2010 to Dec 2013
train <- filtered_data_ts |>
  filter_index("2010-01" ~ "2014-12") |>
  select(Count)

test <- filtered_data_ts |>
  filter_index("2015-01" ~ "2018-12") |>
  select(Count)

#View the filtered datasets
train
test
train_set <- as.data.frame(train)
test_set <- as.data.frame(test)


# Create the time series plot using autoplot from feasts
autoplot(train, Count) +
  labs(title = "Admissions by Month",
       x = "Month",
       y = "Number of Admissions") +
  theme_minimal() +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")

#STL Composition of the training set
train |>
  model(
    STL(Count ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

#Seasonal Plot of the Training Set
train |>
  gg_season(Count, labels = "both") +
  labs(y = "Admissions",
       title = "Seasonal plot: Number of Admissions")

#Subseries ploy of the Training Set
train |>
  gg_subseries(Count) +
  labs(
    y = "Admissions",
    title = "Number of Admissions"
  )


# Assuming 'train' and 'test' are your tsibbles

# Fit models
train_fit <- train |>
  model(
    Mean = MEAN(Count),
    `Naïve` = NAIVE(Count),
    `Seasonal naïve` = SNAIVE(Count ~ lag("year"))
  )

# Generate forecasts for 48 periods (e.g., months)
train_fc <- train_fit |> forecast(h = 48)

# Combine the training set, test set, and forecasted values in a single plot
autoplot(train_fc, train, level = NULL) +
  autolayer(test, Count, series = "Test Set", colour = "red") +
  autolayer(
    filter_index(filtered_data_ts, "2010-01-01" ~ .),  # Adjust the date range as needed
    colour = "black"
  ) +
  labs(y = "Cases", title = "Admissions per Month") +
  guides(colour = guide_legend(title = "Forecast"))

# Augment the training fit
augmented_data <- augment(train_fit)



# Fit the Mean model
mean_fit <- train |>
  model(Mean = MEAN(Count))

# Generate forecasts for the test period
mean_fc <- mean_fit |> forecast(new_data = test)

# Plot forecasts against actual values including the test set
autoplot(mean_fc, level = c(80, 95)) +
  autolayer(train, Count, series = "Training Data") +
  autolayer(test, Count, series = "Test Data", colour = "red") +
  labs(y = "Cases", title = "Admissions per Month") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal()
# Calculate accuracy metrics using the actual test data and the forecasts
accuracy_metrics <- accuracy(mean_fc, test)
print(accuracy_metrics)
# Augment the training fit
augmented_data <- augment(mean_fit)

# Plot residuals for the 'Mean' model
mean_fit |>
  gg_tsresiduals()



#NAIVE MODEL
naive_fit <- train |>
  model(
    `Naïve` = NAIVE(Count)
  )

# Generate forecasts for the test period
naive_fc <- naive_fit |> forecast(new_data = test)

# Plot forecasts against actual values including the test set
autoplot(naive_fc, level = c(80, 95)) +
  autolayer(train, Count, series = "Training Data") +
  autolayer(test, Count, series = "Test Data", colour = "red") +
  labs(y = "Cases", title = "Admissions per Month") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal()
# Calculate accuracy metrics using the actual test data and the forecasts
accuracy_metrics <- accuracy(naive_fc, test)

# View the accuracy metrics
print(accuracy_metrics)
# Plot residuals for the 'Naive' model
naive_fit |>
  gg_tsresiduals()



# Fit the Seasonal Naïve model to the training data
snaive_fit <- train |>
  model(
    `Seasonal naïve` = SNAIVE(Count)
  )

# Generate forecasts for the test period
snaive_fc <- snaive_fit |> forecast(new_data = test)

# Plot forecasts against actual values including the test set
autoplot(snaive_fc, level = c(80, 95)) +
  autolayer(train, Count, series = "Training Data") +
  autolayer(test, Count, series = "Test Data", colour = "red") +
  labs(y = "Cases", title = "Admissions per Month") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal()
# Calculate accuracy metrics using the actual test data and the forecasts
accuracy_metrics <- accuracy(snaive_fc, test)

# View the accuracy metrics
print(accuracy_metrics)

# Plot residuals for the 'Mean' model
snaive_fit |>
  gg_tsresiduals()

# Calculate accuracy metrics for each model
accuracy_metrics_mean <- accuracy(mean_fc, test)
accuracy_metrics_naive <- accuracy(naive_fc, test)
accuracy_metrics_snaive <- accuracy(snaive_fc, test)

# Create a data frame to store accuracy metrics
accuracy_stats <- data.frame(
  Model = c("Mean Method", "Naive Method", "Seasonal Naive Method"),
  MAE = c(accuracy_metrics_mean$MAE, accuracy_metrics_naive$MAE, accuracy_metrics_snaive$MAE),
  RMSE = c(accuracy_metrics_mean$RMSE, accuracy_metrics_naive$RMSE, accuracy_metrics_snaive$RMSE)
)

print(accuracy_stats)


# Extract the Count column as a numeric vector
count_vector <- train %>% pull(Count)

# Perform the Augmented Dickey-Fuller test
adf_result <- adf.test(count_vector, k = 1)
print(adf_result)

# Perform the KPSS test on the original time series
kpss_result <- kpss.test

acf((train),main = "ACF of Training Set")
pacf((train),main = "PACF of Training Set")


# Perform differencing on the Count variable
diff_train <- train |>
  mutate(diff_Count = difference(Count))

# Plot the differenced time series
diff_train |>
  ggplot(aes(x = Month, y = diff_Count)) +
  geom_line() +
  labs(title = "First-Order Differenced Monthly Admissions", x = "Month", y = "Differenced Count") +
  theme_minimal()

# Plot ACF of the differenced time series
diff_train |>
  ACF(diff_Count) |>
  autoplot() +
  labs(title = "Autocorrelation Function (ACF) of Differenced Monthly Admissions", 
       x = "Lag", 
       y = "Autocorrelation",
       caption = "Using default method") +  # Adding a caption
  theme_minimal()

diff_train |>
  PACF(diff_Count) |>
  autoplot() +
  labs(title = "Partial Autocorrelation Function (PACF) of Differenced Monthly Admissions", 
       x = "Lag", 
       y = "Autocorrelation",
       caption = "Using default method") +  # Adding a caption
  theme_minimal()



# Perform second-order differencing on the diff_Count variable
secdiff_train <- train |>
  mutate(second_order_diff_Count = difference(difference(Count)))

# Plot the differenced time series
secdiff_train |>
  ggplot(aes(x = Month, y = second_order_diff_Count)) +
  geom_line() +
  labs(title = "Second-Order Differenced Monthly Admissions", x = "Month", y = "Second-Order Differenced Count") +
  theme_minimal()

# Plot the ACF of the second-order differenced time series
secdiff_train |>
  ACF(second_order_diff_Count) %>%
  autoplot() +
  labs(title = "ACF of Second-Order Differenced Monthly Admissions", 
       x = "Lag", 
       y = "Autocorrelation",
       caption = "Using default method") +  # Adding a caption
  theme_minimal()

# Plot the ACF of the second-order differenced time series
secdiff_train |>
  PACF(second_order_diff_Count) %>%
  autoplot() +
  labs(title = "PACF of Second-Order Differenced Monthly Admissions", 
       x = "Lag", 
       y = "Autocorrelation",
       caption = "Using default method") +  # Adding a caption
  theme_minimal()

# Convert to ts object
training_data_ts <- ts(train$Count, start = c(year(train$Month[1]), month(train$Month[1])), frequency = 12)

# Apply seasonal differencing (assuming yearly seasonality with lag = 12)
seasonal_diff <- diff(training_data_ts, lag = 12)

# Apply first-order differencing to the seasonal difference
seasonal_first_order_diff <- diff(seasonal_diff)

# Display the time series with ACF and PACF for first-order differencing of the seasonal difference
ggtsdisplay(seasonal_first_order_diff, main = "First-Order Differenced Seasonal Time Series with ACF and PACF")


##Exponential Smoothing
# Fit an Exponential Smoothing model
hw_es_fit <- train %>%
  model(es = fable::ETS(Count ~ error("A") + trend("A") + season("A")))

# Forecast future values
hw_forecast1 <- hw_es_fit %>%
  forecast(test)  # Forecasting for the next 48 months

# Plot forecasts against actual values including the test set
autoplot(hw_forecast1, level = c(80, 95)) +
  autolayer(train, Count, series = "Training Data") +
  autolayer(test, Count, series = "Test Data", colour = "red") +
  labs(y = "Cases", title = "Holt-Winters Forecast") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal()
#Print the AIC, AICc, and BIC
report(hw_es_fit)



# Fit an Exponential Smoothing model
dhw_es_fit <- train %>%
  model(es = fable::ETS(Count ~ error("A") + trend("Ad") + season("A")))

# Forecast future values
dhw_forecast1 <- dhw_es_fit %>%
  forecast(test)  # Forecasting for the next 48 months

# Plot forecasts against actual values including the test set
autoplot(dhw_forecast1, level = c(80, 95)) +
  autolayer(train, Count, series = "Training Data") +
  autolayer(test, Count, series = "Test Data", colour = "red") +
  labs(y = "Cases", title = "Damped Holt-Winters Forecast") +
  guides(colour = guide_legend(title = "Series")) +
  theme_minimal()
#Print the AIC, AICc, and BIC
report(dhw_es_fit)



# Convert train data to a tsibble object
train_tsibble <- as_tsibble(train)
test_tsibble <- as_tsibble(test)

# Fit an Exponential Smoothing model
hw_es_fit <- train_tsibble %>%
  model(es = fable::ETS(Count ~ error("A") + trend("A") + season("A")))

# Forecast future values
hw_forecast1 <- hw_es_fit %>%
  forecast(new_data = test_tsibble)  # Forecasting for the next 48 periods

# Calculate accuracy metrics for the Holt-Winters Exponential Smoothing model
accuracy_hw_es <- accuracy(hw_forecast1, data = test_tsibble)
print(accuracy_hw_es)

# Fit an Exponential Smoothing model
dhw_es_fit <- train_tsibble %>%
  model(es = fable::ETS(Count ~ error("A") + trend("Ad") + season("A")))

# Forecast future values
dhw_forecast1 <- dhw_es_fit %>%
  forecast(new_data = test_tsibble)  # Forecasting for the next 48 months


# Calculate accuracy metrics for the Holt-Winters Exponential Smoothing model
accuracy_dhw_es <- accuracy(dhw_forecast1, data = test_tsibble)
print(accuracy_dhw_es)



# Ensure `train` is a univariate time series
# Assuming `train` is a tsibble with a column named 'Count'
train_ts <- train %>% pull(Count)

# Check if `train_ts` is a time series object; if not, convert it
if (!is.ts(train_ts)) {
  train_ts <- ts(train_ts, frequency = 12, start = c(2010, 01)) # Specify the start year and month
}
# Assuming `train` is a tsibble with a column named 'Count'
test_data_ts <- test %>% pull(Count)

# Check if `train_ts` is a time series object; if not, convert it
if (!is.ts(test_data_ts)) {
  ttest_data_ts <- ts(test_data_ts, frequency = 12, start = c(2015, 01)) # Specify the start year and month
}

# Convert filtered_data_ts to a ts object
filtered_data_ts <- as.ts(filtered_data_ts)



##ARIMA MODELS
# Fit ARIMA models
arima_model010 <- Arima(train_ts, order = c(0, 1, 0))
arima_model111 <- Arima(train_ts, order = c(1, 1, 1))
sarima_model101 <- Arima(train_ts, order = c(1, 0, 1), seasonal = c(1, 1, 1))
sarima_model010 <- Arima(train_ts, order = c(1, 1, 1), seasonal = c(1, 1, 0))
sarima_model111 <- Arima(train_ts, order = c(1, 2, 1), seasonal = c(0, 1, 1))


# Create forecasts
fc_arima010 <- forecast(arima_model010, h = 48) # Forecasting 5 years ahead
fc_arima111 <- forecast(arima_model111, h = 48)
fc_sarima122 <- forecast(sarima_model101, h = 48)
fc_sarima010 <- forecast(sarima_model010, h = 48)
fc_sarima111 <- forecast(sarima_model111, h = 48)

#Plot the forecasts for 4 years
autoplot(fc_arima010, h=48)
autoplot(fc_arima111, h=48)
autoplot(fc_sarima122, h=48)
autoplot(fc_sarima010, h=48)
autoplot(fc_sarima111, h=48)

# Plot the forecasts
autoplot(train_ts) +
  autolayer(fc_arima010, series = "ARIMA(0,1,0)", PI = FALSE) +
  autolayer(fc_arima111, series = "ARIMA(1,1,1)", PI = FALSE) +
  autolayer(fc_sarima122, series = "SARIMA(1,0,1)(1,1,1)[12]", PI = FALSE) +
  autolayer(fc_sarima010, series = "SARIMA(1,1,1)(1,1,0)[12]", PI = FALSE) +
  autolayer(fc_sarima111, series = "SARIMA(1,2,1)(0,1,1)[12]", PI = FALSE) +
  autolayer(filtered_data_ts, series = "Observed", PI = FALSE) +  # Add observed data
  ggtitle("Forecasts from ARIMA Models") +
  xlab("Year") + ylab("Count") +
  guides(colour = guide_legend(title = "Models"))


# Extract AIC, AICc, and BIC values
aic_values <- c(AIC(arima_model010), AIC(arima_model111), AIC(sarima_model101), AIC(sarima_model010), AIC(sarima_model111))
aicc_values <- c(AICc(arima_model010), AICc(arima_model111), AICc(sarima_model101), AICc(sarima_model010), AICc(sarima_model111))
bic_values <- c(BIC(arima_model010), BIC(arima_model111), BIC(sarima_model101), BIC(sarima_model010), BIC(sarima_model111))

# Create a data frame to display the results
model_names <- c("ARIMA(0,1,0)", "ARIMA(1,1,1)", "SARIMA(1,0,1)(1,1,1)[12]", "SARIMA(1,1,1)(1,1,0)[12]", "SARIMA(1,2,1)(0,1,1)[12]")
model_stats <- data.frame(
  Model = model_names,
  AIC = aic_values,
  AICc = aicc_values,
  BIC = bic_values
)

print(model_stats)


# Calculate accuracy metrics for each model
accuracy_arima010 <- accuracy(fc_arima010, test_data_ts)
accuracy_arima111 <- accuracy(fc_arima111, test_data_ts)
accuracy_sarima122 <- accuracy(fc_sarima122, test_data_ts)
accuracy_sarima010 <- accuracy(fc_sarima010, test_data_ts)
accuracy_sarima111 <- accuracy(fc_sarima111, test_data_ts)

#Print the accuracy tests
print(accuracy_arima010)
print(accuracy_arima111)
print(accuracy_sarima122)
print(accuracy_sarima010)
print(accuracy_sarima111)


# Convert filtered_data_ts to a ts object
filtered_data_ts <- as.ts(filtered_data_ts)

sarima_model122.raw <- Arima(filtered_data_ts, order = c(1, 0, 1), seasonal = c(1, 1, 1))


# Create forecasts
fc_sarima122.raw <- forecast(sarima_model122.raw, h = 120)

# Plot the forecasts
autoplot(filtered_data_ts) +
  autolayer(fc_sarima122.raw, series = "SARIMA(0,1,0)(1,1,1)[12]", PI = TRUE) +
  ggtitle("Forecasts from SARIMA(0,1,0)(1,1,1) for 10 years '18-'28") +
  xlab("Year") + ylab("Count") +
  guides(colour = guide_legend(title = "Models"))


