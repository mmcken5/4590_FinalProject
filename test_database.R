# Set your working directory
#getwd()
setwd("C:/Users/mikem/Projects/4590/4590_FinalProject/")

# Clear environment
rm(list = ls())

source("database.R")

# Create db (if does not already exist)
# Connect to db
my_db <- connectToDB("stock_db.sqlite3")

# Create sample data
my_list <- createSampleTestData()

# Create data frame from sample data list
my_df <- createDF(my_list)

# Check to see if the db already contains the stock table
if (!doesDBContainTable(my_db, "stocksData")){
  # Load the data frame into the db as a table
  loadDFIntoDB(my_db, my_df)
}

# Get the table
my_tbl <- getTable(my_db, "stocksData")

# Get the tickers (which is the first column)
tickers <- getFirstColumnFromTable(my_tbl)

my_tbl
tickers