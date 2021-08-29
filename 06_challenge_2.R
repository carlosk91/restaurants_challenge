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
      ggplot(aes(
        x = order_week,
        y = reengaged_users
      )) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = 'Weekly reengaged users',
        x = 'Order week',
        y = 'Reengaged users'
      )
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

