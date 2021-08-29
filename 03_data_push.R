# Script to push the csv into a local DB

#### Read CSVs and fixing the data ####
# For restaurant visitors there're 3,886 #VALUE! points, those will be replaced
# by date of visit_datetime.
# Based on some examples, a format of '%d/%m/%Y %H:%M' was used for the
# time stamps.
restaurant_visitors  <-
  read_csv('DataSet/restaurants_visitors.csv') %>%
  mutate(
    visit_date = as_date(visit_date),
    visit_datetime = as_datetime(visit_datetime,
                                 format = '%d/%m/%Y %H:%M'),
    reserve_datetime = as_datetime(reserve_datetime,
                                   format = '%d/%m/%Y %H:%M'),
    visit_date = case_when(is.na(visit_date) ~ as_date(visit_datetime),
                           T ~ visit_date)
  )

# No issues found so far. Weekdays are correct.
date_info  <- read_csv('DataSet/date_info.csv')

# No issues found so far.
store_info  <- read_csv('DataSet/store_info.csv')

# No issues found so far
user_info <- read_delim(
  'DataSet/File_user_info_Challenge2.txt',
  delim = '\t',
  col_types = c(
    user_id = 'c',
    first_complete_time = 'T',
    first_order = 'c',
    last_complete_time = 'T',
    last_order = 'c'
  )
)

# Changed gmv to currency amount. 
# There are 570 orders with different dates compared to user_info table.
# 22 of those are more than 69 days apart. All the first orders of user info
# are available in order info.
order_info <-
  read_delim(
    'DataSet/File_order_info_Challenge2.txt',
    delim = '\t',
    col_types = c(
      order_date = 'D',
      user_id = 'c',
      order_id = 'c',
      gmv = 'd',
      payment_type = 'c'
    )
  ) %>%
  mutate(gmv = gmv / 100)

#### Push tables to DB ####
# First lets connect to the DB
con <- connect_to_db()

push_table(
  con = con,
  table_to_push = restaurant_visitors,
  table_name = 'restaurant_visitors',
  overwrite_or_append = 'overwrite',
  exit_after_fetch = F
)

push_table(
  con = con,
  table_to_push = date_info,
  table_name = 'date_info',
  overwrite_or_append = 'overwrite',
  exit_after_fetch = F
)

push_table(
  con = con,
  table_to_push = store_info,
  table_name = 'store_info',
  overwrite_or_append = 'overwrite',
  exit_after_fetch = F
)

push_table(
  con = con,
  table_to_push = user_info,
  table_name = 'user_info',
  overwrite_or_append = 'overwrite',
  exit_after_fetch = F
)

push_table(
  con = con,
  table_to_push = order_info,
  table_name = 'order_info',
  overwrite_or_append = 'overwrite',
  exit_after_fetch = T
)
