# This R script will download all 30 dow stock data and transform the data to the format of 
# Input_List from our long assignment

#Some packages required to run this script
library(tidyquant)
library(dplyr)
library(rlist)
#install.packages("rlist")
#install.packages("tidyquant")
library(tidyverse)

# get_dcf_data will take the stock ticker as an argument and automatically download the financial statments
#   and take the information needed to fill the output_list
# data is downloaded from yahoo finance via tidyquant package
get_dcf_data <- function(ticker){
  
  # Download financial statements using company ticker
  fin_statements <- tq_get(ticker,get="financials")
  annual_fin_stmt<-fin_statements$annual
  
  # Extract balance sheet, cashflow statement and income statement
  annual_bs<-annual_fin_stmt[1]
  annual_cfs<-annual_fin_stmt[2]
  annual_is<-annual_fin_stmt[3]
  
  # transform them from tibble to data frame for manipulation
  df_annual_bs <- as.data.frame(annual_bs)
  df_annual_cfs <- as.data.frame(annual_cfs)
  df_annual_is<- as.data.frame(annual_is)

  # This is the date to be used to filter the statements
  # because our dcf only need data from most recent year statements
  filter_date <- max(df_annual_bs$date)

  # filters out all data points that are not from the most recent year
  latest_bs<-dplyr::filter(df_annual_bs,date==filter_date)
  latest_cfs<-dplyr::filter(df_annual_cfs,date==filter_date)
  latest_is<-dplyr::filter(df_annual_is,date==filter_date)

  # dropping column 'group' and column 'date'
  trimmed_bs <- latest_bs[-c(1,3)]
  trimmed_cfs <- latest_cfs[-c(1,3)]
  trimmed_is <- latest_is[-c(1,3)]
  
  # spreading the table to wide form and replace all NAs with 0
  # final outputs are final_bs, final_cfs and final_is
  final_bs <- spread(trimmed_bs,category,value)
  final_bs[is.na(final_bs)] <- 0
  
  final_cfs <- spread(trimmed_cfs,category,value)
  final_cfs[is.na(final_cfs)] <- 0
  
  final_is <- spread(trimmed_is,category,value)
  final_is[is.na(final_is)] <- 0
  
  
  # get most recent stock price
  stock_price <- tq_get(ticker)
  
  # tranform from tibble to data frame for manipulation
  df_sp <- as.data.frame(stock_price)
  
  # extract the stock price date we want
  sp_date <- max(df_sp$date)
  
  # filter out all other stock prices other than today's
  sp_today <- filter(df_sp,date==sp_date)
  
  # final stock price sp_close
  sp_close <- sp_today[5]


  output_list <- list(
    ticker = ticker,
    stock_price = sp_close$close,
    current_revenue = final_is$`Total Revenue`,
    
    ### Assumption taken from long assignment
    annual_revenue_growth_growth = (1+0.06),
    growth_period = 7,
    annual_revenue_growth_stable = (1+0.03),
    stable_period = 13,
    ###
    
    annual_cogs_rate = 10,#(final_is$`Cost of Revenue, Total`/final_is$`Total Revenue`),
    excess_periods = 20,
    current_depreciation = (final_cfs$`Depreciation/Depletion` + final_cfs$Amortization),
    
    
    ### Assumption taken from long assignment
    annual_depreciation_growth_now = (1-0.0683317211644177),
    adg_now_period = 1,
    annual_depreciation_growth_growth = 1.06,
    adg_growth_period = 5,
    annual_depreciation_growth_stable = 1.03,
    adg_stable_period = 14,
    ###
    
    
    tax_rate = (1-final_is$`Income After Tax`/final_is$`Income Before Tax`),
    
    # No information, assume 0
    current_nol = 0,
    
    
    current_capex = -final_cfs$`Capital Expenditures`,
    
    ### Assumption taken from long assignment
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
      # cost_of_equity allows user to manually set their desired cost of equity
      # To make the function calculate cost of equity set this variable to 0
    
    risk_free_rate = 0.02,
    risk_premium = 0.05,
    
    publicly_traded_flag = TRUE,
    ###
    
    
    last_traded_price = sp_close$close,
    shares_outstanding = final_bs$`Total Common Shares Outstanding`,
    
    #### Assumption taken from long assignment
    market_val_debt = 0.1,
    ####
    
    book_val_debt = final_bs$`Total Debt`,
    book_val_equity = final_bs$`Total Equity`,
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
  return(output_list)
}

# # Get a list of dow jones 30 tickers and transform them from factor to character
# dowtickers <- as.character(unlist(read.csv(file = "dow30tickers.csv",header = FALSE,sep = ",")))
# 
# # dcf_data_list will have the same length of 'dowtickers'
# # each element in dcf_data_list is essentially a 'input_list' from the long assignment
# # therefore, each of these lists is a list of 32 elements...
# dcf_data_list <- list(get_dcf_data(dowtickers[[1]]))
# for (i in 2:length(dowtickers)){
#   dcf_data_list[i] <- list(get_dcf_data(dowtickers[[i]]))
# }
# dcf_data_list
# # the name of each of the 30 lists will be their corresponding stock ticker
# names(dcf_data_list) <- dowtickers
# str(dcf_data_list)

# #"dow30tickers.csv"
# getDowData <- function(csvFile){
#   # Get a list of dow jones 30 tickers and transform them from factor to character
#   dowtickers <- as.character(unlist(read.csv(file = csvFile, header = FALSE,sep = ",")))
#   
#   # dcf_data_list will have the same length of 'dowtickers'
#   # each element in dcf_data_list is essentially a 'input_list' from the long assignment
#   # therefore, each of these lists is a list of 32 elements...
#   dcf_data_list <- list(get_dcf_data(dowtickers[[1]]))
#   for (i in 2:5){#length(dowtickers)){
#     dcf_data_list[i] <- list(get_dcf_data(dowtickers[[i]]))
#   }
#   
#   # the name of each of the 30 lists will be their corresponding stock ticker
#   #names(dcf_data_list) <- dowtickers
#   #str(dcf_data_list)
#   
#   return(dcf_data_list)
# }
# 
# 
# #setwd("C:/Users/mikem/Projects/4590/4590_FinalProject/")
# dowData <- getDowData("dow30tickers.csv")
# #df_dow_Data <- as.data.frame(dowData)
# 
# setwd("C:/Users/mikem/Projects/4590/4590_FinalProject/")
