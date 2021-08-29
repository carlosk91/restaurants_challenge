# Answering Challenge 1
# Utility
max_date <- max(restaurant_visitors$visit_date)
max_month <-
  max(floor_date(restaurant_visitors$visit_date, 'month'))

#### Question 1 ####
# The top restaurant has an average of 33 daily customers on holidays
top_five_rest_in_holiday.barplot <-
  ggplotly(top_five_rest_in_holiday %>%
  mutate(position = paste0('Restaurant #', row_number())) %>%
  ggplot(aes(y = avg_daily_visitors_on_holiday, x = position)) +
  geom_bar(stat = "identity") +
  labs(
    title = 'Top 5 restaurants avg daily visitors on holiday',
    x = 'Top 5 restaurants',
    y = 'Avg daily visitors on holiday'
  ))

# But in reality th rest of the top restaurants are no that easy to separate.
# The next top restaurants can be re-arranged using the median, or the total
# distribution.
top_five_rest_in_holiday.boxplot <-
  ggplotly(restaurant_visitors %>%
  inner_join(top_five_rest_in_holiday %>%
               mutate(position = paste0('Restaurant #', row_number())),
             by = c('id' = 'restaurant_id')) %>%
  left_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  filter(holiday_flg == 1) %>%
  group_by(position, visit_date) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(x = position, y = visitors, fill = position)) +
  geom_boxplot() +
  labs(
    title = 'Top 5 restaurants daily visitors on holiday',
    x = 'Top 5 restaurants',
    y = 'Quartiles of daily visitors on holiday', 
    fill = 'Top 5 Restaurants'
  ) + 
  theme(legend.position = "none"))

# Its important to make sure that the distribution is normal when you want to
# compare mean against mean.
# Restaurants from 2 to 5 follow a Beta-like distribution.
top_five_rest_in_holiday.densityplot <-
  ggplotly(restaurant_visitors %>%
  inner_join(top_five_rest_in_holiday %>%
               mutate(position = paste('Restaurant Top ', row_number())),
             by = c('id' = 'restaurant_id'))  %>%
  left_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  filter(holiday_flg == 1) %>%
  group_by(position, visit_date) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(x = visitors, color = position, fill = position)) +
  geom_density(alpha = 0.15) +
  labs(
    title = 'Top 5 restaurants distribution of daily visitors on holiday',
    x = 'Visitors on holiday',
    y = 'Distribution', 
    fill = 'Top 5 Restaurants'
  ) +
  scale_y_continuous(labels = scales::percent) +
  guides(color = "none"))

#### Question 2 ####
median_weekday_visitors <-
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ungroup() %>%
  group_by(day_of_week) %>%
  summarise(median_visitors = median(visitors, na.rm = T),
            avg_visitors = mean(visitors, na.rm = T)) %>%
  ungroup()

# Saturday is the weekday with more avg costumers (bar plot).
# But the median (blue points) is almost the same on Friday and Saturday.
# The distribution is skewed to the right, that's whey there's a big difference
# between median and mean.
weekday_w_more_visitors.barplot <-
  ggplotly(weekday_w_more_visitors %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  inner_join(median_weekday_visitors) %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(stat = 'identity',
           aes(y = avg_visitors_by_weekday),
           alpha = 0.75) +
  geom_point(aes(y = median_visitors), colour = "blue") +
  labs(title = 'Avg and Meadian visitors by weekday',
       x = 'Weekday',
       y = 'Avg (bar plot) and Median (blue dots) visitors'))

# But the distribution of Saturday tends to be higher.
# Just with almost the same median.
weekday_w_more_visitors.boxplot <-
  ggplotly(restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(y = visitors, x = day_of_week, fill = day_of_week)) +
  geom_boxplot() +
  labs(title = 'Distribution of visitors by weekday',
       x = 'Weekday',
       y = 'Visitors') +
  guides(fill = "none"))

# In average, Saturday is the weekday with more avg visitors by store.
# In other words you could say that on Saturday you have an expected value of
# 17 customers, ceteris paribus.
# But again, the distribution of Friday and Saturday is not that different.
# The median number of visitors on each store in Friday and Saturday is 12.
# There are multiple outliers in all weekdays
median_weekday_restaurant_visitors_by_rest <-
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(id, visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ungroup() %>%
  group_by(day_of_week) %>%
  summarise(median_visitors = median(visitors),
            avg_visitors = mean(visitors))

weekday_w_more_visitors_by_rest.barplot <-
  weekday_w_more_visitors_by_rest %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  inner_join(median_weekday_restaurant_visitors_by_rest) %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(stat = 'identity',
           aes(y = avg_visitors_by_store_by_weekday),
           alpha = 0.75) +
  geom_point(aes(y = median_visitors), colour = "blue") +
  labs(title = 'Avg and Meadian visitors by weekday by restaurant',
       x = 'Weekday',
       y = 'Avg (bar plot) and Median (blue dots) visitors by restaurant')

weekday_w_more_visitors_by_rest.boxplot <-
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(id, visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(y = visitors, colour = day_of_week)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = 'Distribution of visitors by weekday by restaurant',
       x = 'Weekday',
       y = 'Visitors')

#### Question 3 ####
# There has been a continuous decrease in visitors the last 4 weeks
visitors_weekly_growth_percentage.lineplot <-
  visitors_weekly_growth_percentage %>%
  ggplot(aes(x = visit_week, y = weekly_visitors_growth)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

# There's a gap in 2016, from the weeks of Jul 25 to Sep 12.
# And it appears that the number of visitors drastically decreased from
# the week of Apr 17, 2017 onward.
weekly_visitors <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  mutate(
    day_of_week = factor(day_of_week, levels = levels_weekday()),
    visit_week = floor_date(calendar_date,
                            unit = 'weeks',
                            week_start = 1)
  ) %>%
  group_by(visit_week) %>%
  summarise(visitors = sum(reserve_visitors, na.rm = T))

weekly_visitors.lineplot <-
  plotly::ggplotly(
    weekly_visitors %>%
      ggplot(aes(x = visit_week, y = visitors)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Weekly visitors',
           x = 'Week',
           y = 'Visitors')
  )

daily_visitors <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  group_by(calendar_date) %>%
  summarise(visitors = sum(reserve_visitors, na.rm = T))

daily_visitors.lineplot <-
  plotly::ggplotly(
    daily_visitors %>%
      ggplot(aes(x = calendar_date, y = visitors)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Daily visitors',
           x = 'Date',
           y = 'Visitors')
  )


# The number of visitors last month drastically decreased. It appears that it
# went to the same average of 2016.
monthly_visitors <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  mutate(visit_month = floor_date(calendar_date,
                                  unit = 'month')) %>%
  group_by(visit_month) %>%
  summarise(visitors = sum(reserve_visitors, na.rm = T))

monthly_visitors.lineplot <-
  plotly::ggplotly(
    monthly_visitors %>%
      ggplot(aes(x = visit_month, y = visitors)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Monthly visitors',
           x = 'Month',
           y = 'Visitors')
  )

# But the number of unique restaurants didn't decreased as much
monthly_restaurants <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  mutate(visit_month = floor_date(calendar_date,
                                  unit = 'month')) %>%
  group_by(visit_month) %>%
  summarise(restaurants = n_distinct(id, na.rm = T)) %>%
  ungroup()

monthly_restaurants.lineplot <-
  plotly::ggplotly(
    monthly_restaurants %>%
      ggplot(aes(x = visit_month, y = restaurants)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Monthly restaurants',
           x = 'Month',
           y = 'Restaurants')
  )

# The average visitors by restaurant decreased in May 2017. But in general is
# around 161 visitors
monthly_avg_visitors_by_rest <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  mutate(visit_month = floor_date(calendar_date,
                                  unit = 'month')) %>%
  group_by(visit_month, id) %>%
  summarise(visitors = sum(reserve_visitors, na.rm = T)) %>%
  ungroup() %>%
  group_by(visit_month) %>%
  summarise(avg_visitors = mean(visitors, na.rm = T)) %>%
  ungroup()

monthly_avg_visitors_by_rest.lineplot <-
  plotly::ggplotly(
    monthly_avg_visitors_by_rest %>%
      ggplot(aes(x = visit_month, y = avg_visitors)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Monthly avg visitors by restaurant',
           x = 'Month',
           y = 'Avg visitors by restaurant')
  )

#### Question 4 ####
# For the forecast I will create two forecasts and challenge against each other:
# a) An ensemble model, the product of two forecasts. The first will be the
#   number of restaurants and the second the average visitors by restaurant.
#   This so the model can be more accurate and to have more control on the results
#   if an actionable is known, i.e. a strategy to acquire more restaurants.
# b) I'll create a forecast directly for the total number of visitors.
# The model with the least MAE will be used to forecast the next 6 months.

# I'll split the forecast into three time series:
# a) Number of average visitors by restaurant
# b) Number of restaurants
# c) Number of visitors

# Given the change of distribution in the behavior of the time series,
# and the shortage of data, I will create the forecast using an ARIMA,
# averages, Naive, Seasonal Naive, and TBATS. I will select the best forecast
# comparing the MAE of the model results using a training and test set.

# The last 4 dates will be used to back test, so those will be holdout-
holdout <- 4
series <- list()
series[[1]] <-
  ts(monthly_avg_visitors_by_rest$avg_visitors, frequency = 12)
series[[2]] <- ts(monthly_restaurants$restaurants, frequency = 12)
series[[3]] <- ts(monthly_visitors$visitors, frequency = 12)

weekly_visitors_ts <- ts(
  weekly_visitors$visitors,
  frequency = 365.25 / 7,
  start = min(weekly_visitors$visit_week)
)

daily_visitors_ts <- ts(
  daily_visitors$visitors,
  frequency = 365.25 / 30,
  start = min(daily_visitors$calendar_date)
)

##### ARIMA test #####
# Applying Ljung-box test to visitors it shows that it is not white noise.
Box.test(series[[3]],
         lag = 15,
         fitdf = 0,
         type = 'Lj')
# The first lag shows significant autocorrelation
ggAcf(series[[3]])

# Applying and auto.arima, p and q are 0.
# For average visitors by restaurant is purely white noise (d is 0).
# For restaurants is a random walk (d is 1).
# For total visitors is, again, a random walk (d is 1).
# Given average visitors is white noise the point forecast will be the same as
# a mean method.
# And because restaurants and total visitors are a random walk, the point forecast
# be the same as a naive model.
# Reference:  https://people.duke.edu/~rnau/411arim.htm
arima_models <- lapply(series, function(foo) {
  subseries <- ts_wo_holdout(timeserie = foo, h = holdout)
  auto.arima(subseries,
             max.d = 12,
             max.p = 12,
             max.q = 12)
})

arima_forecasts <-
  lapply(arima_models, function(foo) {
    forecast(foo, h = holdout)
  })

arima_model_result <- mapply(
  FUN = accuracy,
  object = arima_forecasts,
  x = series,
  SIMPLIFY = FALSE
)

arima_model_residuals <- mapply(
  FUN = checkresiduals,
  object = arima_forecasts,
  MoreArgs = list(plot = F),
  SIMPLIFY = FALSE
)

# ARIMA forecast mean absolute errors on test set are:
# * Avg visitors is 78, ~48% of the ts mean. Residuals are white noise.
# * Restaurants is 3, ~19% of the ts mean. Residuals are not white noise.
# * Total visitors is 2137, ~74% of the ts mean. Residuals are not white noise.
# The gap between train and test set MAE is really high for Restaurants and
# total visitors.

##### Simple average test #####
avg_models <- lapply(series, function(foo) {
  subseries <- ts_wo_holdout(timeserie = foo, h = holdout)
  meanf(subseries)
})

avg_forecasts <-
  lapply(avg_models, function(foo) {
    forecast(foo, h = holdout)
  })

avg_model_result <- mapply(
  FUN = accuracy,
  object = avg_forecasts,
  x = series,
  SIMPLIFY = FALSE
)

avg_model_residuals <- mapply(
  FUN = checkresiduals,
  object = avg_forecasts,
  MoreArgs = list(plot = F),
  SIMPLIFY = FALSE
)

# Average forecast mean absolute errors on test set are:
# * Avg visitors is 78, ~48% of the ts mean. Residuals are white noise.
# * Restaurants is 14, ~89% of the ts mean. Residuals are white noise.
# * Avg visitors is 3626, ~125% of the ts mean. Residuals are not white noise.


##### Naive test #####
naive_models <- lapply(series, function(foo) {
  subseries <- ts_wo_holdout(timeserie = foo, h = holdout)
  naive(subseries)
})

naive_forecasts <-
  lapply(naive_models, function(foo) {
    forecast(foo, h = holdout)
  })

naive_model_result <- mapply(
  FUN = accuracy,
  object = naive_forecasts,
  x = series,
  SIMPLIFY = FALSE
)

naive_model_residuals <- mapply(
  FUN = checkresiduals,
  object = naive_forecasts,
  MoreArgs = list(plot = F),
  SIMPLIFY = FALSE
)

# Naive forecast mean absolute errors on test set are:
# * Avg visitors is 70, ~43% of the ts mean. Residuals are not white noise.
# * Restaurants is 3, ~19% of the ts mean. Residuals are not white noise.
# * Avg visitors is 2137, ~74% of the ts mean. Residuals are not white noise.

##### Seasonal Naive test #####
snaive_models <- lapply(series, function(foo) {
  subseries <- ts_wo_holdout(timeserie = foo, h = holdout)
  snaive(subseries)
})

snaive_forecasts <-
  lapply(snaive_models, function(foo) {
    forecast(foo, h = holdout)
  })

snaive_model_result <- mapply(
  FUN = accuracy,
  object = snaive_forecasts,
  x = series,
  SIMPLIFY = FALSE
)

snaive_model_residuals <- mapply(
  FUN = checkresiduals,
  object = snaive_forecasts,
  MoreArgs = list(plot = F),
  SIMPLIFY = FALSE
)

# Seasonal Naive forecast mean absolute errors are:
# * Avg visitors is 62.6 (test set), ~38.7% of the ts mean.
# * Restaurants is 21 (test set), ~134%% of the ts mean.
# * Avg visitors is 4080.75 (test set), ~141% of the ts mean.

##### TBATS test #####

daily_tbats_model <- tbats(daily_visitors_ts)
daily_tbats_forecast <- forecast(daily_tbats_model, h = 180)
plot(daily_tbats_forecast, ylab = "Daily visitors")

weekly_tbats_model <- tbats(weekly_visitors_ts)
weekly_tbats_forecast <- forecast(weekly_tbats_model, h = 28)
plot(weekly_tbats_forecast, ylab = "Weekly visitors")

tbats_models <- lapply(series, function(foo) {
  subseries <- ts_wo_holdout(timeserie = foo, h = holdout)
  tbats(subseries)
})

tbats_forecasts <-
  lapply(tbats_models, function(foo) {
    forecast(foo, h = holdout)
  })

tbats_model_result <- mapply(
  FUN = accuracy,
  object = tbats_forecasts,
  x = series,
  SIMPLIFY = FALSE
)

tbats_model_residuals <- mapply(
  FUN = checkresiduals,
  object = tbats_forecasts,
  MoreArgs = list(plot = F),
  SIMPLIFY = FALSE
)

# TBATS mean absolute errors are:
# * Avg visitors is 62.65 (test set), ~38.7% of the ts mean.
# * Restaurants is 3.6 (test set), ~23% of the ts mean.
# * Avg visitors is 2137.1 (test set), ~74% of the ts mean.
# The issue with TBATS is that it over fits Avg visitors data.

##### Ensemble model vs Total visitor model test #####
# Now I'll evaluate which model is more accurate. I'll do an ensemble model
# multiplying avg visitors by restaurants forecast and compare it against
# total visitors forecast. I'll choose whichever method has the least MAE
# against test set.
ensemble_fcst <-
  tbats_forecasts[[1]]$mean * naive_forecasts[[2]]$mean

mae_ttl_visitors <-
  c(
    total_visitors_ensemble_mae = mean(abs(ensemble_fcst - series[[3]])),
    total_visitors_tbats_mae = tbats_model_result[[3]]['Test set', 'MAE']
  )

# Ensemble model was more accurate. It has an average absolute error of 1986.
# That means that the result in average each month will have a forecast ± 1986
# total visitors.

##### Forecast next 6 months #####
# I'll update naive for the number of restaurants and
# forecast with TBATs from Jun to Nov
average_visitors_model <- tbats(series[[1]])
restaurant_model <- naive(series[[2]])

average_visitors_forecast <-
  pmax(forecast(average_visitors_model, h = 6)$mean, 0)
restaurant_forecast <- forecast(restaurant_model, h = 6)$mean

total_visitors_forecast <-
  average_visitors_forecast * restaurant_forecast

monthly_visitors_fcst.lineplot <-
  plotly::ggplotly(
    monthly_visitors %>%
      bind_rows(
        tibble(visit_month =
                 as_date(seq(
                   as_date('2017-06-01'),
                   as_date('2017-11-01'),
                   by = "month"
                 )),
               visitors = total_visitors_forecast)
      ) %>%
      mutate(type = case_when(
        visit_month > max_month ~ 'Forecast',
        T ~ 'Actual'
      )) %>%
      ggplot(aes(
        x = visit_month, y = visitors, color = type
      )) +
      geom_line() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Monthly visitors',
           x = 'Month',
           y = 'Visitors')
  )

#### Question 5 ####
full_dataset <- 
  restaurant_visitors %>%
  inner_join(store_info, by = c('id' = 'store_id')) %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(
    day_of_week = factor(day_of_week, levels = levels_weekday()),
    visit_month = floor_date(visit_date, 'month'),
    days_between_reserve_and_visit = as.numeric(visit_date - 
                                                  as_date(reserve_datetime))
  )

# Strategies to could double the amount of visitors
# a) Add more restaurants in cities with high population density.
# There are multiple cities where there's high population density but with lower
# number of restaurants.

jp_map <-  map('world2', 'japan', plot = FALSE, fill = TRUE)

restaurants_position <-
  autoplot(jp_map, geom = 'polygon', fill = 'subregion') +
  geom_point(data  = store_info, aes(x = longitude, y = latitude),
             color = 'blue', size = 0.2) +
  theme(legend.position = "none")

qmplot(x=longitude, y=latitude, 
       data = store_info, 
       geom = "blank",
       maptype = "toner-background", 
       darken = .5, 
       legend = "topright") + 
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", 
                  alpha = .2,
                  color = NA) + 
  scale_fill_gradient2(low = "blue", 
                       mid = "green", 
                       high = "red") +
  geom_point(color = 'blue', size = 0.2)

# b) Add more genres of food
# There was a hughe increase after more genres were implemented. 
# Izakaya had a great impact on visitors. I would suggest to explore a healthy 
# or international food segment.

visitors_by_genre.barplot <-
  plotly::ggplotly(
    full_dataset %>%
      group_by(visit_month, genre_name) %>%
      summarise(visitors = sum(reserve_visitors)) %>%
      ungroup() %>%
      ggplot(aes(
        x = visit_month, y = visitors, fill = genre_name
      )) +
      geom_bar(position = "stack", stat = "identity") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = 'Monthly visitors',
        x = 'Month',
        y = 'Visitors',
        fill = 'Genre'
      )
  )

full_dataset %>%
  mutate()

# c) Set reminder to reserve a visit to favorite place or suggest other option.
# Fridays and Saturdays have a higher number of days between reservation and
# visit. It could help send reminders to reserve a restaurant. And if the
# the restaurant is already fully booked we can suggest a similar place with 
# available seats.

days_outlier <- 
  boxplot.stats(full_dataset$days_between_reserve_and_visit)$stats[c(1, 5)]

days_from_reserve_to_visit.boxplot <- 
  full_dataset %>%
  ggplot(aes(y = days_between_reserve_and_visit, 
             fill = day_of_week)) +
  geom_boxplot() + 
  coord_cartesian(ylim = days_outlier*1.05) +
  labs(
    title = 'Days from reservation to visit',
    x = 'Weekday of the visit',
    y = 'Days',
    fill = 'Day of week'
  )

#### Question 6 ####
# a) User information. This is crucial to understand what drives the user to 
# reserve or purchase anything. Age, consumption habits, etc.
# b) App usage information is extremely important. 
# Maybe there's a high traffic of requests to visit a place but something in the
# funnel makes the user drop the reservation.
# c) Prices of the places and more information of the restaurant is important. 
# There could be the opportunity to understand demand elasticity given different
# prices. I've stopped placing orders in some restaurants because the price 
# increased a lot.
