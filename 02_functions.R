#### Utility functions ####

#' Month Difference
#'
#' Function to get the number of months between two dates
#' @param start_date First date of the interval
#' @param end_date Second date of the interval
#' @returns Integer with the number of months
month_dif <- function(start_date, end_date) {
  x <- interval(start_date,
                end_date) %/% months(1)
  return(x)
}

#' Levels of weekday
#'
#' Function to get a list of weekdays arranged. Set as function to not set
#' variable in environment.
#' 
#' @returns Arranged list of weekdays
levels_weekday <- function(){ 
  levels <- c(
    'Monday',
    'Tuesday',
    'Wednesday',
    'Thursday',
    'Friday',
    'Saturday',
    'Sunday'
  )
  
  return(levels)
}

#' Week Difference
#'
#' Function to get the number of weeks between two dates.
#' i.e. Aug 29, 2021 - Aug 28, 2021 would be 0, but 
#' Aug 30, 2021 - Aug 29, 2021 would be 1
#' 
#' @param start_date First date of the interval
#' @param end_date Second date of the interval
#' @returns Integer with the number of months
week_dif <- function(start_date, end_date) {
  x <- interval(floor_date(start_date,
                           unit = 'weeks',
                           week_start = 1),
                floor_date(end_date,
                           unit = 'weeks',
                           week_start = 1)) %/% weeks(1)
  return(x)
}

#' Timeserie without holdout
#' 
#' Function to get a train set for a time serie given h holdout.
#' 
#' @param timeserie Timeserie.
#' @param h Holdout. Number of data points to leave out.
#' @returns ts without the latest h data points.
ts_wo_holdout <- function(timeserie, h){
  ts(head(timeserie, length(timeserie) - holdout),
     start = start(timeserie),
     frequency = frequency(timeserie))
}


#### DB functions ####

#' Get SQL
#'
#' Function to translate a .sql file to string
#' It helps other read/write functions.
#'
#' @param filepath this is the file path for the sql file
#' @returns string of SQL
get_sql <- function(filepath) {
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE) {
    line <- readLines(con, n = 1)
    
    if (length(line) == 0) {
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if (grepl("--", line) == TRUE) {
      line <- paste(sub("--", "/*", line), "*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}

#' Deparse name
#'
#' Function to get name of variable
#' @param x name of variable
#' @returns name of variable deparsed
deparse_name <- function(x) {
  deparsed_name <- deparse(substitute(x))
  return(deparsed_name)
}

#' Connect to database
#'
#' Function to connect database. Created as function so the keys are never saved
#' in the environment.
#'
#' @param dms In this case MySQL will be used
#' @param db DB to connect
#' @param connection_schema Schema to connect
#' @returns name of variable deparsed
connect_to_db <-
  function() {
    keys <-
      read_yaml(file = 'keys.yaml')
    
    con <- dbConnect(
      drv = MariaDB() ,
      user = keys$local_instance$uid,
      password = keys$local_instance$pwd,
      host = keys$local_instance$host,
      port = keys$local_instance$port,
      keys$local_instance$schema
    )
    
    return(con)
  }

#' Get Query
#'
#' Function to read a .sql file and fetch the results,
#'
#' @param con Connection to db. Get from connect_to_db function
#' @param query This is the file path for the sql file
#' @param exit_after_fetch Boolean, if true then exits after fetching
#' @returns Data frame fetched form the query
get_query <- function(con,
                      query,
                      exit_after_fetch = T,
                      ...) {
  if (exit_after_fetch) {
    on.exit(dbUnloadDriver(drv), add = TRUE)
    on.exit(dbDisconnect(con))
  }
  
  query_fetch <-
    dbGetQuery(con,
               glue(get_sql(query)))
  
  return(query_fetch)
}

#' Push Table
#'
#' Function to push data to snowflake, recommended to push table that
#' was processed in R/Excel/Python etc...
#' @inheritParams get_query
#'
#' @param query File path of the sql file
#' @param dms DMS that will be used to push the data
#' @param table_to_push Data frame or tibble object to be uploaded
#' @param db Data base name to upload the data
#' @param write_to_schema Schema to upload the data
#' @param overwrite_or_append 'overwrite' | 'append' are the available options
push_table <- function(table_to_push,
                       table_name,
                       con,
                       overwrite_or_append = 'overwrite',
                       field_types = NULL,
                       exit_after_fetch = T) {
  if (exit_after_fetch) {
    on.exit(dbUnloadDriver(drv), add = TRUE)
    on.exit(dbDisconnect(con))
  }
  
  dbWriteTable(
    conn = con,
    name = table_name,
    if (overwrite_or_append == 'overwrite') {
      overwrie = T
    } else if (overwrite_or_append == 'append') {
      append = T
    },
    row.names = F,
    value = table_to_push,
    batch_rows = 300000,
    field.types = field_types
  )
  
}


