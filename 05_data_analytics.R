# Answering The challenge with an analytical approach

#### Question 1 ####
# The top restaurant has an average of 33 daily customers on holidays
top_five_rest_in_holiday.barplot <- 
  top_five_rest_in_holiday %>%
  mutate(position = paste('Restaurant Top ', row_number())) %>%
  ggplot(aes(y = avg_daily_visitors_on_holiday, x = position)) +
  geom_bar(stat="identity")

# But in reality th rest of the top restaurants are no that easy to separate.
# The next top restaurants can be re-arranged using the median, or the total
# distribution.
top_five_rest_in_holiday.boxplot <-
  restaurant_visitors %>%
  inner_join(top_five_rest_in_holiday %>%
               mutate(position = paste('Restaurant Top ', row_number())),
             by = c('id' = 'restaurant_id')) %>%
  group_by(position, visit_date) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(y = visitors, colour = position)) +
  geom_boxplot()

# Its important to make sure that the distribution is normal when you want to 
# compare mean against mean. 
# Restaurants from 2 to 5 follow a Beta-like distribution. 
top_five_rest_in_holiday.densityplot <- 
  restaurant_visitors %>%
  inner_join(top_five_rest_in_holiday %>%
               mutate(position = paste('Restaurant Top ', row_number())),
             by = c('id' = 'restaurant_id')) %>%
  group_by(position, visit_date) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(x = visitors, colour = position)) +
  geom_density()

#### Question 2 ####
median_weekday_restaurant_visitors <- 
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ungroup() %>%
  group_by(day_of_week) %>%
  summarise(median_visitors = median(visitors))

# Saturday is the weekday with more avg costumers (bar plot).
# But the median (blue points) is almost the same on Friday and Saturday.
# The distribution is skewed to the right, that's whey there's a big difference
# between median and mean.
weekday_w_more_visitors.barplot <-
  weekday_w_more_visitors %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  inner_join(median_weekday_restaurant_visitors) %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(stat='identity', aes(y = avg_visitors_by_weekday), alpha = 0.75) +
  geom_point(aes(y=median_visitors), colour="blue") + 
  labs(title = 'Avg and Meadian visitors by weekday', 
       x = 'Weekday',
       y = 'Avg (bar plot) and Median (blue dots) visitors')

# But the distribution of Saturday tends to be higher. 
# Just with almost the same median.
weekday_w_more_visitors.boxplot <-
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  group_by(visit_date, day_of_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(y = visitors, colour = day_of_week)) +
  geom_boxplot() +  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = 'Distribution of visitors by weekday', 
       x = 'Weekday',
       y = 'Visitors')

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
  summarise(median_visitors = median(visitors))

weekday_w_more_visitors_by_rest.barplot <- 
  weekday_w_more_visitors_by_rest %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday())) %>%
  inner_join(median_weekday_restaurant_visitors_by_rest) %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(stat='identity', aes(y = avg_visitors_by_store_by_weekday), 
           alpha = 0.75) +
  geom_point(aes(y=median_visitors), colour="blue") + 
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
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
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
weekly_visitors.lineplot<-
  plotly::ggplotly(restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(day_of_week = factor(day_of_week, levels = levels_weekday()),
         visit_week = floor_date(visit_date, 
                                 unit = 'weeks',
                                 week_start = 1)) %>%
  group_by(visit_week) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ggplot(aes(x = visit_week, y = visitors)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Weekly visitors', 
       x = 'Week',
       y = 'Visitors'))

# The number of visitors last month drastically decreased. It appears that it
# went to the same average of 2016.
monthly_visitors.lineplot<-
  plotly::ggplotly(restaurant_visitors %>%
                     inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
                     mutate(visit_month = floor_date(visit_date, 
                                                    unit = 'month')) %>%
                     group_by(visit_month) %>%
                     summarise(visitors = sum(reserve_visitors)) %>%
                     ggplot(aes(x = visit_month, y = visitors)) +
                     geom_line() + 
                     scale_y_continuous(labels = scales::comma) +
                     labs(title = 'Monthly visitors', 
                          x = 'Month',
                          y = 'Visitors'))

# But the number of unique restaurants didn't decreased as much
monthly_restaurants <-
  date_info %>%
  left_join(restaurant_visitors, by = c('calendar_date' = 'visit_date')) %>%
  mutate(visit_month = floor_date(calendar_date, 
                                  unit = 'month')) %>%
  group_by(visit_month) %>%
  summarise(restaurants = n_distinct(id, na.rm = T)) %>%
  ungroup()

monthly_restaurants.lineplot<-
  plotly::ggplotly(monthly_restaurants %>%
                     ggplot(aes(x = visit_month, y = restaurants)) +
                     geom_line() + 
                     scale_y_continuous(labels = scales::comma) +
                     labs(title = 'Monthly restaurants', 
                          x = 'Month',
                          y = 'Restaurants'))

# The average visitors by restaurant decreased in May 2017. But in general is
# around 180 visitors
monthly_avg_visitors_by_rest <-
  restaurant_visitors %>%
  inner_join(date_info, by = c('visit_date' = 'calendar_date')) %>%
  mutate(visit_month = floor_date(visit_date, 
                                  unit = 'month')) %>%
  group_by(visit_month, id) %>%
  summarise(visitors = sum(reserve_visitors)) %>%
  ungroup() %>%
  group_by(visit_month) %>%
  summarise(avg_visitors = mean(visitors)) %>%
  ungroup()

monthly_avg_visitors_by_rest.lineplot<-
  plotly::ggplotly(monthly_avg_visitors_by_rest %>%
                     ggplot(aes(x = visit_month, y = avg_visitors)) +
                     geom_line() + 
                     scale_y_continuous(labels = scales::comma) +
                     labs(title = 'Monthly avg visitors by restaurant', 
                          x = 'Month',
                          y = 'Avg visitors by restaurant'))

#### Question 4 ####
# For the forecast I will do the product of two forecasts. The first will be the
# number of restaurants and the second the average visitors by restaurant.
# This so the model can be more accurate and to have more control on the results
# if an actionable is known, i.e. a strategy to acquire more restaurants.

# First lets split the forecast into two time series: 
# a) Number of average visitors by restaurant
# b) Number of restaurants
series <- list()
series[[1]] <- ts(monthly_avg_visitors_by_rest$avg_visitors,frequency=12)
series[[2]] <- ts(monthly_restaurants$restaurants,frequency=12)
# The last 4 known data points will be used to back test the forecast
holdout <- 4

forecasts <- lapply(series,function(foo) {
  subseries <- ts(head(foo,length(foo)-holdout),
                  start=start(foo),
                  frequency=frequency(foo))
  forecast(auto.arima(subseries),h=holdout)
})


#### Question 5 ####

jp <-  map('world2', 'japan', plot = FALSE, fill = TRUE)

jap_plot <- 
  autoplot(jp, geom = 'polygon', fill = 'subregion') + 
  theme(legend.position="none") +
  geom_point(data = store_info, aes(x = longitude, y = latitude),
             colour = 'blue', size = 0.15)


