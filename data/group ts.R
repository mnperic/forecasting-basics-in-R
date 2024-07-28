
## Read required libraries
library(readxl)
library(forecast)
library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)


#Read the dataset
sales <- read_excel("gummies_sold.xlsx", sheet = "all_data")

#convert month column to type Date
sales$Month <- as.Date(sales$Month, "%Y-%m-%d")


#visualize
ggplot(sales, aes(x = Month, y = Sales, group = SKU)) +
  geom_line(aes(color = SKU), size = 0.75) +
  geom_point(aes(color = SKU), size = 0.95) + labs(
    title = "Gummies: Units Sold",
    subtitle = "Data from 2021 to 2023")+
  theme_tq()


sales_nest <- sales %>%
  group_by(SKU) %>%
  nest()

#convert to ts
sales_ts <- sales_nest %>%
  mutate(data.ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -Month, 
                       start    = c(2021,4),
                       freq     = 12))

#model the ts
sales_fit <- sales_ts %>%
  mutate(fit.HoltWinters = map(.x = data.ts,
                               .f = HoltWinters))


#tidying
tidydata <- sales_fit %>%
  mutate(tidy = map(fit.HoltWinters,sw_tidy))  %>%
  unnest(tidy) %>%
  spread(key = SKU, value = estimate)


#check model accuracy
glancedata <- sales_fit %>%
  mutate(glance = map(fit.HoltWinters, sw_glance)) %>%
  unnest(glance)

glancedata<- glancedata[,c(1, 5,6,10:16)]

#check fitted and residual values
augmentdata <- sales_fit %>%
  mutate(augment = map(fit.HoltWinters, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

#plot the residuals/actuals
augmentdata %>%
  ggplot(aes(x = date, y = .resid, group = SKU)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Quantity Sold By SKU",
       subtitle = "HoltWinters Model Residuals", x = "") +
  theme_tq() +
  facet_wrap(~ SKU, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

#forecast with the model
sales_forecast <- sales_fit %>%
  mutate(forecast.HoltWinters = map(fit.HoltWinters, forecast, h = 12))
sales_forecast

#tidy the forecast
sales_forecast_tidy <- sales_forecast %>%
  mutate(sweep = map(forecast.HoltWinters, sw_sweep, timetk_idx = TRUE)) %>%
  unnest(sweep)


#visualize
sales_forecast_tidy %>%
  ggplot(aes(x = index, y = Sales, color = key, group = SKU)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Quantity Sold By SKU",
       subtitle = "Holt Winters Model Forecasts",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ SKU, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#write the tidy sales forecast to excel
point_forecasts <- as.data.frame(sales_forecast_tidy[,c(1,6:12)])
writexl::write_xlsx(point_forecasts,"Point Forecasts.xlsx")








