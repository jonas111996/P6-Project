library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
library(moments)
library(forecast)
library(vars)
library(strucchange)
library(dLagM)

##################### IMPORTERE ELPRISER SAMT SÆSONKORRIGERE DET

# Fetch data (KUN FOR ELPRISER, HUSK AT LAVE FOR ELFORBRUGET OGSÅ)
res <- GET("https://api.energidataservice.dk/dataset/Elspotprices?limit=1000000&start=2016-02-24T00:00&end=2023-08-24T00:00&filter={\"PriceArea\":\"DK1\"}")
rescontent <- content(res, as = "text")

# Parse JSON content
strjson <- fromJSON(rescontent)

# Convert to data frame
Elpriser <- strjson$records

## Jeg vender her data rigtigt:

Elpriser <- Elpriser[order(Elpriser$HourDK),]
rownames(Elpriser) <- NULL  # Fjerner oprindeligt obs. nummer

Elpriser$Week <- strftime(Elpriser$HourDK, format = "%Y-%V")  # Giver År samt ugenumre med ugestart mandag


# Group by week and calculate mean
weekly_means <- Elpriser %>%
  group_by(Week) %>%
  summarise(MeanPrice = mean(SpotPriceDKK, na.rm = TRUE))

weekly_mean <- weekly_means$MeanPrice[1:393]
print(weekly_mean)

weekly_mean <- weekly_means$MeanPrice[1:393]


# Create the time series object
start_week <- which(week(ymd("2016-01-01")) == weekly_means$Week[1])
weekly_mean_ts <- ts(weekly_mean, frequency = 52, start = c(2016, start_week))

# Plot the time series
plot(weekly_mean_ts, xlab = "Time", ylab = "Mean Price (DKK)", main = "Weekly Mean Spot Prices in DK1")

# Highlight the point at observation 297
# Using time index for the x-coordinate
points(time(weekly_mean_ts)[297], weekly_mean_ts[297], col = "red", pch = 19)




### Log af elpris tidsrække
lweekly_mean_ts <- log(weekly_mean_ts)
plot(lweekly_mean_ts, xlab = "Time", ylab = "Mean Price (DKK)", main = "Weekly Mean Spot Prices in DK1")



Acf(lweekly_mean_ts, lag.max = 104)

dlweekly_mean_ts <- diff(lweekly_mean_ts)

ts.plot(dlweekly_mean_ts, ylab = "Log of the Mean Price(DKK)")


Acf(dlweekly_mean_ts, lag.max = 52)



################# IMPORTERE ELFORBRUG SAMT SÆSONKORRIGERE DET
## Importere dataen for
res <- GET("https://api.energidataservice.dk/dataset/productionconsumptionsettlement?limit=1000000&start=2016-02-24T00:00&end=2023-08-24T00:00&filter={\"PriceArea\":\"DK1\"}")
rescontent <- content(res, as = "text")

# Parse JSON content
strjson <- fromJSON(rescontent)

# Convert to data frame
Elforbrug <- strjson$records

Elforbrug <- Elforbrug[order(Elforbrug$HourDK),]
rownames(Elforbrug) <- NULL  # Fjerner oprindeligt obs. nummer


#NB NB NB NB 
Elforbrug$Week <- strftime(Elforbrug$HourDK, format = "%Y-%V") 



# Group by week and calculate mean
weekly_means_forbrug <- Elforbrug %>%
  group_by(Week) %>%
  summarise(MeanConsumptions = mean(GrossConsumptionMWh, na.rm = TRUE))


print(weekly_means_forbrug)
weekly_mean_forbrug <- weekly_means_forbrug$MeanConsumptions[1:393]


print(weekly_mean_forbrug)


plot(weekly_mean_forbrug)

weekly_mean_forbrugts <- ts(weekly_mean_forbrug, frequency = 52, start = c(2016, which(week(ymd("2016-02-24")) == weekly_means$Week[1])))
print(weekly_mean_forbrugts)

#NB NB NB tag logaritmen
lweekly_mean_forbrug <- log(weekly_mean_forbrug)

lweekly_mean_forbrugts <- ts(lweekly_mean_forbrug, frequency = 52, start = c(2016, which(week(ymd("2016-02-24")) == weekly_means$Week[1])))


plot(lweekly_mean_forbrugts)
Acf(lweekly_mean_forbrugts)
Pacf(lweekly_mean_forbrugts)


t <- c(1:length(lweekly_mean_forbrugts)) 

Deterministisk <- lm(lweekly_mean_forbrugts ~ t+sin(t*2*pi/52)+cos(t*2*pi/52)+sin(t*4*pi/52)
                     +cos(t*4*pi/52)+sin(t*2*pi/(52/12))+cos(t*2*pi/(52/12)))
summary(Deterministisk)

fitted_values <- fitted(Deterministisk)

ts.plot(lweekly_mean_forbrugts,fitted_values, xlab = "Time", ylab = "Mean Consumption (Mwh)", main = "Weekly Mean Spot Prices in DK1",gpars = list(col = c("blue", "red")))

### Stokastisk komponent
residuals_ts <- lweekly_mean_forbrugts - fitted_values

plot(residuals_ts)
Acf(residuals_ts, lag.max = 104)
Pacf(residuals_ts, lag.max = 104)
# residual_ts er ikke-stationær så den skal gøres stationær ved at differense og sæsondifferense


### SARIMA Model

sarma_model <- Arima(y = residuals_ts, 
                     order = c(0, 1, 0), #  for the non-seasonal part
                     seasonal = list(order = c(0, 1, 0), period = 52)) # Seasonal settings
## Svarer faktisk bare til at differense 1 gang samt sæsondifferense 1 gang.


summary(sarma_model)


# Extract the fitted seasonal component
fitted_seasonal <- fitted(sarma_model)
# fitted_seasonal er faktisk bare differensen og sæsondifferensen af residual_ts. 

ts.plot(residuals_ts,fitted_seasonal, xlab = "Time", ylab = "Mean Consumption (Mwh)", main = "Weekly Mean Spot Prices in DK1",gpars = list(col = c("blue", "red")))

# Deseasonalize the original time series by subtracting the fitted seasonal component
Stokastisk_forbrug <- residuals_ts - fitted_seasonal



################## KONSTRUKTION AF VAR-MODEL

#Dl_weekly_mean er vores elpriser og Stokastisk forbrug er elforbruget. (Tidsrækker for det)
# Assuming dl_weekly_mean_ts has 393 entries (Since we have differenced it) and Stokastisk_forbrug has 393
dl_weekly_mean_ts <- dlweekly_mean_ts
Stokastisk_forbrug_trimmed <- Stokastisk_forbrug[1:392]

# Now create the dataframe for the VAR-data
var_data <- data.frame(
  Electricity_price = dl_weekly_mean_ts,
  Electricity_consumption = Stokastisk_forbrug_trimmed
)
VARselect(var_data, lag.max = 104)


Første_var <- VAR(var_data,p = 3)
print(Første_var)
summary(Første_var)

roots(Første_var)




# Perform the Chow test
chow_test <- chow.test(Første_var, SB = 297.5)
# View the summary of the test
summary(chow_test)


################### VAR før structucal break konstrueres
var_data_2 <- data.frame(
  Electricity_price = dl_weekly_mean_ts[1:297],
  Electricity_consumption = Stokastisk_forbrug_trimmed[1:297])


VARselect(var_data_2, lag.max = 52)
VAR_2 <- VAR(var_data_2 , p= 3)


summary(VAR_2)
VAR_2_restrict <- restrict(VAR_2)
summary(VAR_2_restrict)


roots(VAR_2_restrict)


# Create a sequence of dates with weekly intervals
start_date <- as.Date("2016-02-24")
residuals <- VAR_2_restrict$varresult$Electricity_price$residuals
dates <- seq.Date(from = start_date, by = "week", length.out = length(residuals))

# Plot the residuals with dates on the x-axis
plot(dates, residuals, type = "l", ylab = "Standardized residuals for electricity price", xlab = "Date")

### ACF for residualerne for elpris
Acf(VAR_2_restrict$varresult$Electricity_price$residuals, lag.max = 52, main = "The ACF of the standardized residuals for electricity price")

### Plot for residualerne for elforbrug
start_date <- as.Date("2016-02-24")
residuals <- VAR_2_restrict$varresult$Electricity_consumption$residuals
dates <- seq.Date(from = start_date, by = "week", length.out = length(residuals))

# Plot the residuals with dates on the x-axis
plot(dates, residuals, type = "l", ylab = "Standardized residuals for electricity consumption", xlab = "Date")


Acf(VAR_2_restrict$varresult$Electricity_consumption$residuals, lag.max = 52, main = "The ACF of the standardized residuals for electricity consumption")


### Chok i Elforbrug påvirkning af Elpris
feir <- irf(VAR_2_restrict, impulse = "Electricity_consumption", response = "Electricity_price",
            n.ahead = 10, ortho = TRUE, runs = 10000)
plot(feir)

feir <- irf(VAR_2_restrict, impulse = "Electricity_price", response = "Electricity_consumption",
            n.ahead = 15, ortho = TRUE, runs = 10000)
plot(feir)





####PREDICTION ###


Første_var_predict <- predict(VAR_2_restrict, n.ahead = 2)


print(Første_var_predict)
######################## Elpris predict

# Assuming Første_var_predict$fcst$Electricity_price fetches the forecast column correctly
Første_var_elprice <- Første_var_predict$fcst$Electricity_price

print(Første_var_elprice)
print(exp(Første_var_elprice[,1]))


# Extract the two values from Første_var_elprice[,1]
values_to_append <- Første_var_elprice[,1]

### Begrænset tidsrække
l_dlweekly_mean_ts <- dlweekly_mean_ts[1:297]

# Create a new time series object with the combined values
new_values <- c(l_dlweekly_mean_ts, values_to_append)
new_values_pi_lower <- c(l_dlweekly_mean_ts, Første_var_elprice[,2])
new_values_pi_upper <- c(l_dlweekly_mean_ts, Første_var_elprice[,3])



# Adjust the start and frequency according to your data and adding the predicted priceses
new_ts <- ts(new_values, start = start(l_dlweekly_mean_ts), frequency = frequency(l_dlweekly_mean_ts))
new_ts_pi_lower <- ts(new_values_pi_lower, start = start(l_dlweekly_mean_ts), frequency = frequency(l_dlweekly_mean_ts))
new_ts_pi_upper <- ts(new_values_pi_upper, start = start(l_dlweekly_mean_ts), frequency = frequency(l_dlweekly_mean_ts))

### Inverse differencer
inv_diff <- cumsum(c(lweekly_mean_ts[1],new_ts))
inv_diff_exp <- exp(inv_diff)
print(inv_diff_exp)


### Inverse differencer prædikationsintervaller
inv_diff_PI_lower <- cumsum(c(lweekly_mean_ts[1],new_ts_pi_lower))
inv_diff_PI_lower_exp <- exp(inv_diff_PI_lower)
inv_diff_PI_upper <- cumsum(c(lweekly_mean_ts[1],new_ts_pi_upper))
inv_diff_PI_upper_exp <- exp(inv_diff_PI_upper)


elprice_with_prediction <- c(weekly_mean_ts[1:298], inv_diff_exp[299:300])
ts_elprice_with_prediction <- ts(elprice_with_prediction, start = start(weekly_mean_ts), frequency = frequency(weekly_mean_ts))
plot(ts_elprice_with_prediction)
print(ts_elprice_with_prediction)


# Assuming ts_elprice_with_prediction and inv_diff_PI_lower_exp are already loaded and are ts objects

# Create a new series that matches the length of ts_elprice_with_prediction but only has data at 394 and 395
full_length_inv_diff_lower = rep(NA, length(ts_elprice_with_prediction))  # Fill with NAs
full_length_inv_diff_lower[299:300] = inv_diff_PI_lower_exp[299:300]  # Assign values at positions 394 and 395


full_length_inv_diff_upper = rep(NA, length(ts_elprice_with_prediction))  # Fill with NAs
full_length_inv_diff_upper[299:300] = inv_diff_PI_upper_exp[299:300]  # Assign values at positions 394 and 395



# Convert it back to a ts object, ensuring it matches the time attributes of ts_elprice_with_prediction
full_length_inv_diff_ts_lower = ts(full_length_inv_diff_lower, start=start(ts_elprice_with_prediction), frequency=frequency(ts_elprice_with_prediction))
full_length_inv_diff_ts_upper = ts(full_length_inv_diff_upper, start=start(ts_elprice_with_prediction), frequency=frequency(ts_elprice_with_prediction))


start_index <- 200
end_index <- 300
ts_elprice_segment <- window(ts_elprice_with_prediction, start = start(ts_elprice_with_prediction) + c(0, start_index - 1), end = start(ts_elprice_with_prediction) + c(0, end_index - 1))
full_length_inv_diff_ts_lower_segment <- window(full_length_inv_diff_ts_lower, start = start(full_length_inv_diff_ts_lower) + c(0, start_index - 1), end = start(full_length_inv_diff_ts_lower) + c(0, end_index - 1))
full_length_inv_diff_ts_upper_segment <- window(full_length_inv_diff_ts_upper, start = start(full_length_inv_diff_ts_upper) + c(0, start_index - 1), end = start(full_length_inv_diff_ts_upper) + c(0, end_index - 1))


# Combine the data using cbind
combined_ts <- cbind(ts_elprice_segment, full_length_inv_diff_ts_lower_segment, full_length_inv_diff_ts_upper_segment)

# Plot the combined time series
ts.plot(combined_ts, col = c("blue", "red", "red"), ylab = "Weekly Mean Spot price")


# Rigtige værdier
full_length_reel = rep(NA, length(ts_elprice_with_prediction))
full_length_reel[299:300] = weekly_mean_ts[299:300]
full_length_reel_ts = ts(full_length_reel, start=start(ts_elprice_with_prediction), frequency=frequency(ts_elprice_with_prediction))



# Plot the combined time series
ts.plot(combined_ts,full_length_reel_ts[200:300], col = c("blue", "red", "red","black"), ylab = "Weekly Mean Spot price")


##################### Elforbrug predict


Deterministisk_values <- Deterministisk$fitted.values[299:300]



print(Første_var_predict$fcst$Electricity_consumption)
Prediction_tal <- exp(Første_var_predict$fcst$Electricity_consumption[1,] + Deterministisk_values[1])
Prediction_tal2 <- exp(Første_var_predict$fcst$Electricity_consumption[2,] + Deterministisk_values[2])


elconsum_with_prediction <- c(weekly_mean_forbrugts[1:298], Prediction_tal[1],Prediction_tal2[1])
ts_elconsum_with_prediction <- ts(elconsum_with_prediction, start = start(weekly_mean_forbrugts), frequency = frequency(weekly_mean_forbrugts))
ts.plot(ts_elconsum_with_prediction)


Prediction_tal_exp <- c(Prediction_tal[2],Prediction_tal2[2])
full_length_con_lower = rep(NA, length(ts_elconsum_with_prediction))  # Fill with NAs
full_length_con_lower[299:300] = Prediction_tal_exp[1:2]  # Assign values at positions 394 and 395
print(full_length_con_lower)

Prediction_tal_exp2 <- c(Prediction_tal[3],Prediction_tal2[3])
full_length_con_upper = rep(NA, length(ts_elconsum_with_prediction))  # Fill with NAs
full_length_con_upper[299:300] = Prediction_tal_exp2[1:2]  # Assign values at positions 394 and 395


# Convert it back to a ts object, ensuring it matches the time attributes of ts_elprice_with_prediction
full_length_ts_lower = ts(full_length_con_lower, start=start(ts_elconsum_with_prediction), frequency=frequency(ts_elconsum_with_prediction))
full_length_ts_upper = ts(full_length_con_upper, start=start(ts_elconsum_with_prediction), frequency=frequency(ts_elconsum_with_prediction))



start_index <- 200
end_index <- 300
ts_elconsump_segment <- window(ts_elconsum_with_prediction, start = start(ts_elconsum_with_prediction) + c(0, start_index - 1), end = start(ts_elconsum_with_prediction) + c(0, end_index - 1))
full_length_inv_diff_ts_lower_segment_consump <- window(full_length_ts_lower, start = start(full_length_ts_lower) + c(0, start_index - 1), end = start(full_length_ts_lower) + c(0, end_index - 1))
full_length_inv_diff_ts_upper_segment_consump <- window(full_length_ts_upper, start = start(full_length_ts_upper) + c(0, start_index - 1), end = start(full_length_ts_upper) + c(0, end_index - 1))

# Combine the data using cbind
combined_ts_consump <- cbind(ts_elconsump_segment, full_length_inv_diff_ts_lower_segment_consump, full_length_inv_diff_ts_upper_segment_consump)

# Plot the combined time series
ts.plot(combined_ts_consump, col = c("blue", "red", "red"), ylab = "Weekly Mean Consumption")















### Rigtige værdier
full_length_forbrug_reel = rep(NA, length(ts_elconsum_with_prediction))
full_length_forbrug_reel[299:300] = weekly_mean_forbrugts[299:300]
full_length_forbrug__reel_ts = ts(full_length_forbrug_reel, start=start(ts_elconsum_with_prediction), frequency=frequency(ts_elconsum_with_prediction))

#Plot the combined time series with actual consumption
ts.plot(combined_ts_consump,full_length_forbrug__reel_ts[200:300], col = c("blue", "red", "red","black"), ylab = "Weekly Mean Consumption")




#### ANDEN VAR MODEL


# Now create the dataframe
var_data_chow <- data.frame(
  Electricity_price = dl_weekly_mean_ts[298:392],
  Electricity_consumption = Stokastisk_forbrug_trimmed[298:392]
)


VARselect(var_data_chow)

Var_chow <- VAR(var_data_chow, p = 3)
summary(Var_chow)
########## ADL MODEL

# Define your data
y <- var_data_chow$Electricity_consumption
x <- var_data_chow$Electricity_price




# Parameters to remove
rem.p <- 0 # 0 removes the main effect of X.t (the intercept)
rem.q <- NULL # remove lagged terms

# Create the remove list
remove <- list(p = rem.p, q = rem.q)

# Adjusted model with parameters removed
model.ardl <- ardlDlm(x = x, y = y, p = 1, q = 3, remove = remove)




# Display the summary of the adjusted model
summary(model.ardl)


residuals <- residuals(model.ardl)

# Calculate the mean and standard deviation of the residuals
residuals_mean <- mean(residuals)
residuals_sd <- sd(residuals)

# Standardize the residuals
standardized_residuals <- (residuals - residuals_mean) / residuals_sd


standardized <- rstandard(residuals)

# Print standardized residuals
print(standardized_residuals)

ts.plot(standardized_residuals)
Acf(standardized_residuals, lag.max = 52)




# Convert residuals to a numeric vector
numeric_residuals <- as.numeric(standardized_residuals)



#### Forecast af ADL model. 
forecast_test <- forecast(model = model.ardl , x = c(var_data_chow$Electricity_price[95]) , 
         h = 1 , interval = TRUE)
print(forecast_test)
forecast_value <- forecast_test$forecasts$Forecast

last_t <- 393
future_t <- c(last_t + 1, last_t + 2)  # This produces t = 394 and 395

# Create a new data frame for these future time points
future_data <- data.frame(
  t = future_t,
  sin_t_week = sin(future_t * 2 * pi / 52),
  cos_t_week = cos(future_t * 2 * pi / 52),
  sin_t_biweek = sin(future_t * 4 * pi / 52),
  cos_t_biweek = cos(future_t * 4 * pi / 52),
  sin_t_month = sin(future_t * 2 * pi / (52 / 12)),
  cos_t_month = cos(future_t * 2 * pi / (52 / 12))
)


# Assuming Deterministisk is your linear model
Deterministisk_predict <- predict(Deterministisk, newdata = future_data)




Forecast_value <- exp(Deterministisk_predict[1] + forecast_value)
Forecast_value_lower <- exp(forecast_test$forecasts$`95% LB`+ Deterministisk_predict[1])
Forecast_value_upper <- exp(forecast_test$forecasts$`95% UB`+ Deterministisk_predict[1])


# Define the start week and year
start_year <- 2021
start_week <- 44

# Combine the original and forecasted values
elconsum_with_prediction_adl <- c(weekly_mean_forbrugts[298:393], Forecast_value)

# Create the time series object with the specified start and frequency
ts_elconsum_with_prediction_adl <- ts(elconsum_with_prediction_adl, start = c(start_year, start_week), frequency = 52)




full_length_con_lower_adl = rep(NA, length(ts_elconsum_with_prediction_adl))  # Fill with NAs
full_length_con_lower_adl[97] = Forecast_value_lower  # Assign values at positions 394 and 395
print(full_length_con_lower_adl)

full_length_con_upper_adl = rep(NA, length(ts_elconsum_with_prediction_adl))  # Fill with NAs
full_length_con_upper_adl[97] = Forecast_value_upper  # Assign values at positions 394 and 395
print(full_length_con_lower_adl)

# Convert it back to a ts object, ensuring it matches the time attributes of ts_elprice_with_prediction
full_length_ts_lower_adl = ts(full_length_con_lower_adl, start=start(ts_elconsum_with_prediction_adl), frequency=frequency(ts_elconsum_with_prediction_adl))
full_length_ts_upper_adl = ts(full_length_con_upper_adl, start=start(ts_elconsum_with_prediction_adl), frequency=frequency(ts_elconsum_with_prediction_adl))


start_index <- 1
end_index <- 97
ts_elconsump_segment_adl <- window(ts_elconsum_with_prediction_adl, start = start(ts_elconsum_with_prediction_adl) + c(0, start_index - 1), end = start(ts_elconsum_with_prediction_adl) + c(0, end_index - 1))
full_length_inv_diff_ts_lower_segment_consump_adl <- window(full_length_ts_lower_adl, start = start(full_length_ts_lower_adl) + c(0, start_index - 1), end = start(full_length_ts_lower_adl) + c(0, end_index - 1))
full_length_inv_diff_ts_upper_segment_consump_adl <- window(full_length_ts_upper_adl, start = start(full_length_ts_upper_adl) + c(0, start_index - 1), end = start(full_length_ts_upper_adl) + c(0, end_index - 1))

# Combine the data using cbind
combined_ts_consump <- cbind(ts_elconsump_segment_adl, full_length_inv_diff_ts_lower_segment_consump_adl, full_length_inv_diff_ts_upper_segment_consump_adl)



# Plot the combined time series
ts.plot(ts_elconsum_with_prediction_adl, col = c("blue"), ylab = "Weekly Mean Consumption")


# Add the point for full_length_ts_upper_adl at observation 97
points(time(ts_elconsum_with_prediction_adl)[97], full_length_ts_upper_adl[97], col = "red", pch = 19)

# Add the point for full_length_ts_lower_adl at observation 97
points(time(ts_elconsum_with_prediction_adl)[97], full_length_ts_lower_adl[97], col = "red", pch = 19)

#### R-koden for det andet dokument skal køres for at hente denne værdi.
points(time(ts_elconsum_with_prediction_adl)[97], weekly_mean_forbrug_rigtig_ts[1], col = "black", pch = 19)







