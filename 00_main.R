#### DiDi Challenge script ####
### version: 0.0.1
### developer: carlosk91
###
### Objective: Solve the challenge in an organized fashion

#### Scripts to answer challenge ####
source('01_libraries.R') # Required libraries
source('02_functions.R') # Required functions
source('03_data_push.R') # Pushing csvs to MySQL local DB
source('04_data_read.R') # Creating queries to answer questions 1, 2, and 5
source('05_data_analytics.R') # Script to answer the challenge
render('06_mdr_creation.Rmd')

h2o.init()
# h2o.clusterInfo()
# h2o.shutdown(prompt = F)
