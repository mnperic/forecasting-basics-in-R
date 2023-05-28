
#Load the required libraries 
library(readxl)
library(ggplot2)
library(dplyr)
library(forecast)


#load the dataset
daily_sales <- read_excel("Daily Vitamin Sales.xlsx")
daily_sales <- as.data.frame(daily_sales)

head(daily_sales)

#remove redundant columns and rename
daily_sales <- daily_sales[,c(8,4)]
names(daily_sales)[names(daily_sales) == "Units sold - packs"] <- "sales_units"


#sum by month to get monthly data rather than daily
daily_sales <- daily_sales %>% 
  mutate(month = floor_date(daily_sales$Date, "month"))

monthly_sales <- daily_sales %>% 
  group_by(month) %>% 
  summarise(total = sum(sales_units))

head(monthly_sales)

monthly_sales<- monthly_sales[c(1:30),]

monthly_sales$month <- as.Date(monthly_sales$month)

#plot the data to see visually
ggplot(monthly_sales, aes(x = month, y = total)) + 
  geom_line() + scale_x_date(date_breaks = "2 months", date_labels ="%b-%Y")+
  theme_light()

#convert to time series object and check the individual components 
monthly_sales_ts <- ts(monthly_sales$total, frequency=12, start=c(2019,7))
plot(decompose(monthly_sales_ts))


#######################################
#Forecasting Models

#fit a naive model
fit_naive <- naive(monthly_sales_ts, h=12)
print(summary(fit_naive))
plot(fit_naive)
checkresiduals(fit_naive)

#fit a seasonal naive model
fit_snaive <- snaive(monthly_sales_ts, h=12)
print(summary(fit_snaive))
plot(fit_snaive)
checkresiduals(fit_snaive)

#fit ets models and forecast with them AAA, ZZZ, MAA
fit_ets1 <- ets(monthly_sales_ts, model = "AAA")
print(summary(fit_ets1))
checkresiduals(fit_ets1)

fit_ets2 <- ets(monthly_sales_ts, mode = "ZZZ")
print(summary(fit_ets2))
checkresiduals(fit_ets2)

fit_ets3 <- ets(monthly_sales_ts, mode = "MAM")
print(summary(fit_ets3))
checkresiduals(fit_ets3)

#forecast with the ets models 
forecast_ets1<- forecast(fit_ets1, h=12) 
plot(forecast_ets1)

forecast_ets2<- forecast(fit_ets2, h=12) 
plot(forecast_ets2)

forecast_ets3<- forecast(fit_ets3, h=12) 
plot(forecast_ets3)
accuracy(forecast_ets3)

#fit an arima model 
fit_arima <- auto.arima(monthly_sales_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#forecast with the arima models 
forecast_arima <- forecast(fit_arima, h=12)
plot(forecast_arima)


naive_df <- cbind("Month" = rownames(as.data.frame(fit_naive)), as.data.frame(fit_naive))
naive_df$Date <- as.POSIXct(paste("01-", naive_df$Month, sep = ""), format = "%d-%b %Y")
naive_df$Model <- rep("Naive") 

snaive_df <- cbind("Month" = rownames(as.data.frame(fit_snaive)), as.data.frame(fit_snaive))
snaive_df$Date <- as.POSIXct(paste("01-", snaive_df$Month, sep = ""), format = "%d-%b %Y")
snaive_df$Model <- rep("Seasonal Naive") 

ets1_df <- cbind("Month" = rownames(as.data.frame(forecast_ets1)), as.data.frame(forecast_ets1))
ets1_df$Date <- as.POSIXct(paste("01-", ets1_df$Month, sep = ""), format = "%d-%b %Y")
ets1_df$Model <- rep("ETS AAA") 

ets2_df <- cbind("Month" = rownames(as.data.frame(forecast_ets2)), as.data.frame(forecast_ets2))
ets2_df$Date <- as.POSIXct(paste("01-", ets2_df$Month, sep = ""), format = "%d-%b %Y")
ets2_df$Model <- rep("ETS ZZZ") 

ets3_df <- cbind("Month" = rownames(as.data.frame(forecast_ets3)), as.data.frame(forecast_ets3))
ets3_df$Date <- as.POSIXct(paste("01-", ets3_df$Month, sep = ""), format = "%d-%b %Y")
ets3_df$Model <- rep("ETS MAM")

arima_df <- cbind("Month" = rownames(as.data.frame(forecast_arima)), as.data.frame(forecast_arima))
arima_df$Date <- as.POSIXct(paste("01-", arima_df$Month, sep = ""), format = "%d-%b %Y")
arima_df$Model <- rep("ARIMA") 


forecast_all <- rbind(naive_df, snaive_df, ets1_df, ets2_df, ets3_df, arima_df)
forecast_all$Date <- as.Date(forecast_all$Date)

#forecast plots
ggplot() +
    geom_line(data = monthly_sales, aes(x = month, y = total), size=0.75) +  # Plotting original data
    geom_line(data = forecast_all, aes(x = Date, y = `Point Forecast`, colour = Model), size=0.75) +  # Plotting model forecasts
    scale_color_brewer(palette = "Set2")+theme_minimal()


#write the point forecasts into an Excel file
writexl::write_xlsx(forecast_all,"Point Forecasts.xlsx")


