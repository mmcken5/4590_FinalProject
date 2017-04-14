# IF THERE IS AN ERROR, PLEASE RUN R AS AN ADMINISTRATOR, THANK YOU :)
# Create a database using dplyr with the inputs of your dcf for all the stocks in the DOW

# Install the appropraite packages, if you have not already done so (only need to do once, not each time running program)
# Uncomment the packages you need to install
# If you get an error, run RStudio in Admin Mode (right click on the icon and run as administrator)
# install.packages("dplyr")
# install.packages("RMySQL")
# install.packages("data.table")
# devtools::install_github("rstudio/pool")
# devtools::install_github("rstudio/shiny")
# install.packages("tidyquant")
# install.packages("RSQLite")
# install.packages("shiny")
#install.packages("DBI")

# Load packages
library(dplyr)
library(RMySQL)
library(tidyquant)
library(RSQLite)
library(shiny)
library(DBI)


# Create a list of sample test data
createSampleTestData <- function(){
  my_list <- list(
  ticker = "TEST1",
  stock_price = 400,
  current_revenue = 108249,
  annual_revenue_growth_growth = (1+0.06),
  growth_period = 7,
  annual_revenue_growth_stable = (1+0.03),
  stable_period = 13,
  annual_cogs_rate = 0.8165,
  excess_periods = 20,
  current_depreciation = 1814,
  annual_depreciation_growth_now = (1-0.0683317211644177),
  adg_now_period = 1,
  annual_depreciation_growth_growth = 1.06,
  adg_growth_period = 5,
  annual_depreciation_growth_stable = 1.03,
  adg_stable_period = 14,
  tax_rate = 0.2422,
  current_nol = 200000,
  current_capex = 7696,
  annual_capex_growth_now = (1+0.023939668853227),
  acg_now_period = 1,
  annual_capex_growth_growth = 1.06,
  acg_growth_period = 4,
  annual_capex_growth_stable = 1.03,
  acg_stable_period = 15,
  working_cap_rate = 0.05,
  wcr_period = 20,
  cap_periods = 5,
  beta_stock = 1,
  cost_of_equity = 0.07,
  cost_of_debt = 0.025,
  risk_free_rate = 0.02,
  risk_premium = 0.05,
  publicly_traded_flag = TRUE,
  last_traded_price = 500,
  shares_outstanding = 929.3,
  market_val_debt = 0.1,
  book_val_debt = 0.1,
  book_val_equity = 76615,
  debt_capital_ratio = 0,
  beta_stable_period = 0,
  debt_capital_ratio_stable_period = 0.15,
  cost_of_debt_stable_period = 0.08,
  growth_rate_stable_period = 0.03,
  operating_expense_stable_period = 0.8165,
  capex_stable_period = 1.1,
  working_cap_rate_stable_period = 0.01,
  cost_of_debt_flag = TRUE
  )
  
  return(my_list)
}


# Clear environment
rm(list = ls())


# Set your working directory
#getwd()
#setwd("C:/Users/mikem/Projects/4590/4590_FinalProject/")
#dir()


# Create a SQL Lite DB in the current working directory (if does not already exist) and connect to it
# name: "stock_db.sqlite3"
connectToDB <- function(dbName){
  my_db <- src_sqlite(dbName, create = T)
  return(my_db)
}


# Create a data frame from a list of input data
createDF <-function(input_data){
  
  # Put values into DB (each variable is a column)
  ticker <- input_data$ticker
  stock_price <- input_data$stock_price
  current_revenue = input_data$current_revenue
    
  annual_revenue_growth_growth <- input_data$annual_revenue_growth_growth
  growth_period <- input_data$growth_period
  annual_revenue_growth_stable <- input_data$annual_revenue_growth_stable
  stable_period <- input_data$stable_period
    
  annual_cogs_rate <- input_data$annual_cogs_rate
  excess_periods <- input_data$excess_periods
  current_depreciation <- input_data$current_depreciation
    
  annual_depreciation_growth_now <- input_data$annual_depreciation_growth_now
  adg_now_period <- input_data$adg_now_period
  annual_depreciation_growth_growth <- input_data$annual_depreciation_growth_growth
  adg_growth_period <- input_data$adg_growth_period
  annual_depreciation_growth_stable <- input_data$annual_depreciation_growth_stable
  adg_stable_period <- input_data$adg_stable_period
  
  tax_rate <- input_data$tax_rate
  
  current_nol <- input_data$current_nol
  
  current_capex <- input_data$current_capex
  
  annual_capex_growth_now <- input_data$annual_capex_growth_now
  acg_now_period <- input_data$acg_now_period
  annual_capex_growth_growth <- input_data$annual_capex_growth_growth
  acg_growth_period <- input_data$acg_growth_period
  annual_capex_growth_stable <- input_data$annual_capex_growth_stable
  acg_stable_period <- input_data$acg_stable_period
  
  working_cap_rate <- input_data$working_cap_rate
  wcr_period <- input_data$wcr_period
  cap_periods <- input_data$cap_periods
  beta_stock <- input_data$beta_stock
  cost_of_equity <- input_data$cost_of_equity
  cost_of_debt <- input_data$cost_of_debt
  risk_free_rate <- input_data$risk_free_rate
  risk_premium <- input_data$risk_premium
  publicly_traded_flag <- input_data$publicly_traded_flag
  last_traded_price <- input_data$last_traded_price
  shares_outstanding <- input_data$shares_outstanding
  market_val_debt <- input_data$market_val_debt
  book_val_debt <- input_data$book_val_debt
  book_val_equity <- input_data$book_val_equity
  debt_capital_ratio <- input_data$debt_capital_ratio
  beta_stable_period <- input_data$beta_stable_period
  debt_capital_ratio_stable_period <- input_data$debt_capital_ratio_stable_period
  cost_of_debt_stable_period <- input_data$cost_of_debt_stable_period
  growth_rate_stable_period <- input_data$growth_rate_stable_period
  operating_expense_stable_period <- input_data$operating_expense_stable_period
  capex_stable_period <- input_data$capex_stable_period
  working_cap_rate_stable_period <- input_data$working_cap_rate_stable_period
  cost_of_debt_flag <- input_data$cost_of_debt_flag
    
  # Create a data frame using the variables as columns
  stocks <- data.frame(ticker, stock_price, current_revenue, annual_revenue_growth_growth, growth_period,
                      annual_revenue_growth_stable,stable_period,annual_cogs_rate,excess_periods,
                      current_depreciation,annual_depreciation_growth_now,adg_now_period,
                      annual_depreciation_growth_growth, adg_growth_period,annual_depreciation_growth_stable,
                      adg_stable_period, tax_rate, current_nol, current_capex, annual_capex_growth_now,
                      acg_now_period, annual_capex_growth_growth, acg_growth_period, annual_capex_growth_stable,
                      acg_stable_period, working_cap_rate, wcr_period, cap_periods, beta_stock, cost_of_equity, 
                      cost_of_debt, risk_free_rate, risk_premium, publicly_traded_flag, last_traded_price,
                      shares_outstanding, market_val_debt, book_val_debt, book_val_equity, debt_capital_ratio, 
                      beta_stable_period, debt_capital_ratio_stable_period, cost_of_debt_stable_period,
                      growth_rate_stable_period, operating_expense_stable_period, capex_stable_period, 
                      working_cap_rate_stable_period, cost_of_debt_flag)  
  
  names(stocks) <- c("ticker", "stock_price", "current_revenue", "annual_revenue_growth_growth", "growth_period",
                     "annual_revenue_growth_stable","stable_period","annual_cogs_rate","excess_periods",
                     "current_depreciation","annual_depreciation_growth_now","adg_now_period",
                     "annual_depreciation_growth_growth", "adg_growth_period","annual_depreciation_growth_stable",
                     "adg_stable_period", "tax_rate", "current_nol", "current_capex", "annual_capex_growth_now",
                     "acg_now_period", "annual_capex_growth_growth", "acg_growth_period", "annual_capex_growth_stable",
                     "acg_stable_period", "working_cap_rate", "wcr_period", "cap_periods", "beta_stock", "cost_of_equity",
                     "cost_of_debt", "risk_free_rate", "risk_premium", "publicly_traded_flag", "last_traded_price",
                     "shares_outstanding", "market_val_debt", "book_val_debt", "book_val_equity", "debt_capital_ratio",
                     "beta_stable_period", "debt_capital_ratio_stable_period", "cost_of_debt_stable_period",
                     "growth_rate_stable_period", "operating_expense_stable_period", "capex_stable_period",
                     "working_cap_rate_stable_period", "cost_of_debt_flag")
  
  return(stocks)
}


# Load a data frame into a db as a table
loadDFIntoDB <- function(db, df){
  
  # Load the data frame into the database 
  data <- copy_to(db, df, temporary = FALSE)
}


# Insert a row into the specified table of the specified db
insertRowIntoDB <- function(db, tableName, row_list){
  
  sql_string <- paste("INSERT INTO ", tableName,
                      " VALUES (", row_list, sep = "")
  
  # Send the query to the db
  res <- dbSendQuery(db, sql_string)
}


# Update a specified row in a specified table that belongs to a specified db
updateRowInDB <- function(db, tableName, colId, input_list){
  
  sql_string <- paste("UPDATE ", tableName,
                      " SET tax_rate = ", input_list$tax_rate,
                      ", cost_of_equity = ", input_list$cost_of_equity,
                      ", cost_of_debt = ", input_list$cost_of_debt,
                      ", risk_free_rate = ", input_list$risk_free_rate,
                      ", risk_premium = ", input_list$risk_premium,
                      ", publicly_traded_flag = ", input_list$publicly_traded_flag,
                      ", beta_stable_period = ", input_list$beta_stable_period,
                      ", debt_capital_ratio_stable_period = ", input_list$debt_capital_ratio_stable_period,
                      ", cost_of_debt_stable_period = ", input_list$cost_of_debt_stable_period,
                      ", growth_rate_stable_period = ", input_list$growth_rate_stable_period,
                      ", operating_expense_stable_period = ", input_list$operating_expense_stable_period,
                      ", capex_stable_period = ", input_list$capex_stable_period,
                      ", working_cap_rate_stable_period = ", input_list$working_cap_rate_stable_period,
                      ", cost_of_debt_flag = ", input_list$cost_of_debt_flag,
                      " WHERE ticker = '", colId, "'", sep = "")
  
  # Send the query to the db
  res <- dbSendQuery(db, sql_string)
}


# Check if the db already contains the 'stocks' table
doesDBContainTable <- function(my_db, tableName){
  # set return value to false
  retVal <- FALSE
  
  # Get a list of the table names in the db
  tbl_names <- src_tbls(my_db)
  
  # Check tha that there are tables in the db
  if (length(tbl_names) == 0){
    return(FALSE)
  }
  
  # Search for the input table name
  for(i in 1:length(tbl_names)){
    if(tbl_names[i] == tableName){
      retVal <- TRUE
    }
  }
  
  return(retVal)
}


# Query DB for table
getTable <- function(db, tableName){
  # Specify the table and the db to get it from
  my_tbl <- tbl(db, tableName)
  
  # The collect function actually pulls in the data to R into a data frame
  my_tbl <- collect(my_tbl)
  
  # Return the table
  return(my_tbl)
}


# Get the first column of a table (used to get stock tickers)
getFirstColumnFromTable <- function(table_df){
  return(table_df[,1])
}