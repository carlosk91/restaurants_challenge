# Answering Challenge 2

#### Question 1 ####
# New user definition:
# Count of unique user ids on each user min order_info.order_date
# Active user definition:
# Unique user with order grouped by order_info.order_date.
new_and_active_weekly_users.lineplot <-
  plotly::ggplotly(
    new_and_active_weekly_users %>%
      pivot_longer(!order_week, names_to = 'user_type', values_to = 'count') %>%
      mutate(count = as.integer(count)) %>%
      ggplot(aes(
        x = order_week,
        y = count,
        color = user_type
      )) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = 'Weekly users',
        x = 'Order week',
        y = 'Users',
        color = 'User type'
      )
  )

#### Question 2 ####
# Reengaged Users metric definition:
# Count of unique user ids where the number of weeks between current order date
# and previous one is greater than 1.
weekly_reengaged_users.lineplot <-
  plotly::ggplotly(
    weekly_reengaged_users %>%
      mutate(reengaged_users = as.integer(reengaged_users)) %>%
      ggplot(aes(x = order_week,
                 y = reengaged_users)) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = 'Weekly reengaged users',
           x = 'Order week',
           y = 'Reengaged users')
  )

#### Question 3 ####
# IMPORTANT: The definitions of each of the three user types are not mutually
# exclusive, new users and reengaged users are in fact active users.
avg_gmv_per_user_type.lineplot <-
  plotly::ggplotly(
    avg_gmv_per_user_type %>%
      ggplot(aes(
        x = order_week,
        y = avg_gmv,
        color = user_type
      )) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::dollar) +
      labs(
        title = 'Weekly average GMV',
        x = 'Order week',
        y = 'Avg GMV',
        color = 'User type'
      )
  )


#### Question 4 ####
# Question 4 was to create graphs for each metric. Those are above

#### Question 5 ####
# Recommendations:
# a) Create segments for recurring users based on the number of orders they've
# made. It's important to have order_date as time stamp.
# It allows you to understand whats the number of orders before churning and
# the level of engagement the users have with your application.
full_order_dataset <-
  order_info %>%
  mutate(order_week = floor_date(order_date, 'week', week_start = 1)) %>%
  arrange(user_id, order_date) %>%
  group_by(user_id) %>%
  mutate(
    order_n = row_number(),
    days_to_recur = as.integer(order_date - lag(order_date)),
    next_payment_type = lead(payment_type),
    order_n_bracket =
      case_when(
        order_n == 1 ~ '1st order',
        order_n < 6 ~ '2nd to 5th order',
        order_n >= 6 ~ '6th+ order'
      )
  ) %>%
  ungroup()

full_order_dataset %$%
  quantile(order_n, c(0, .25, .50, .75, 1))


avg_gmv_by_order_n <-
  full_order_dataset %>%
  group_by(
    order_week,
    order_n_bracket
  ) %>%
  summarise(avg_gmv = mean(gmv))

avg_gmv_by_order_n <-
  avg_gmv_by_order_n %>%
  ggplot(aes(x = order_week, y = avg_gmv, color = order_n_bracket)) +
  geom_line()

# In general as the number of orders increases the ticket size decreases.


# b) A high percentage of users only has 1 single order. If there's a high 
# churn rate then DiDi won't be able to have a sustainable growth.
links <-
  full_order_dataset %>%
  filter(order_n < 4) %>%
  mutate(
    source = paste('Order', order_n),
    target = case_when(
      is.na(next_payment_type) ~ 'Became inactive',
      T ~ paste('Order', order_n + 1)
    )
  ) %>%
  group_by(source, target) %>%
  summarise(value = n()) %>%
  ungroup()

nodes <- data.frame(name = c(as.character(links$source),
                             as.character(links$target)) %>% unique())

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = 'Orders',
  sinksRight = FALSE
)
p


# c) There should be experiments with behavioral economics to accelerate adoption.
# As the number of order increases, the number of days to recur decreases.
ylim1 <- boxplot.stats(full_order_dataset$days_to_recur)$stats[c(1, 5)]

full_order_dataset %>%
  filter(order_n < 5 & order_n > 1 & !is.na(days_to_recur)) %>%
  mutate(order_number = as.factor(order_n)) %>%
  ggplot(aes(y = days_to_recur, fill = order_number)) +
  geom_boxplot() +
  coord_cartesian(ylim = ylim1*1.05)



