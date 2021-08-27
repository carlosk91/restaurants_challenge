# Script to push the csv into a local DB

#### Read CSVs and fixing the data ####
# For restaurant visitors there're 3,886 #VALUE! points, those will be replaced
# by date of visit_datetime. 
# Based on some examples, a format of '%d/%m/%Y %H:%M' was used for the
# time stamps.
restaurant_visitors  <- 
  read_csv('DataSet/restaurants_visitors.csv') %>%
  mutate(visit_date = as_date(visit_date),
         visit_datetime = as_datetime(visit_datetime, 
                                      format = '%d/%m/%Y %H:%M'),
         reserve_datetime = as_datetime(reserve_datetime, 
                                            format = '%d/%m/%Y %H:%M'),
         visit_date = case_when(is.na(visit_date) ~ as_date(visit_datetime),
                                T ~ visit_date))

# No issues found with data so far. I've checked weekdays are correct.
date_info  <- read_csv('DataSet/date_info.csv')

# No issues found with data so far.
store_info  <- read_csv('DataSet/store_info.csv')

#### Push tables to DB ####
# First lets connect to the DB
con <- connect_to_db()

push_table(con = con,
           table_to_push = restaurant_visitors, 
           table_name = 'restaurant_visitors', 
           overwrite_or_append = 'overwrite', 
           exit_after_fetch = F)

push_table(con = con,
           table_to_push = date_info, 
           table_name = 'date_info', 
           overwrite_or_append = 'overwrite', 
           exit_after_fetch = F)

push_table(con = con,
           table_to_push = store_info, 
           table_name = 'store_info', 
           overwrite_or_append = 'overwrite', 
           exit_after_fetch = T)


