# First lets connect to the DB
con <- connect_to_db()

##### Challenge 1 ####

#### Question 1 ####
top_five_rest_in_holiday <-
  get_query(con = con,
            query = 'Q1_top_five_rest_in_holiday.sql',
            exit_after_fetch = F)

#### Question 2 ####
# Answer 1, avg customers total.
weekday_w_more_visitors <-
  get_query(con = con,
            query = 'Q2_weekday_w_more_visitors.sql',
            exit_after_fetch = F)

# Answer 2, avg customers by store.
weekday_w_more_visitors_by_rest <-
  get_query(con = con,
            query = 'Q2_weekday_w_more_visitors_by_rest.sql',
            exit_after_fetch = F)

#### Question 3 ####
visitors_weekly_growth_percentage <-
  get_query(con = con,
            query = 'Q3_visitors_weekly_growth_percentage.sql',
            exit_after_fetch = T)


##### Challenge 2 ####

