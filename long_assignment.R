Input_List<- list(
  current_revenue <- 108249,
  annual_revenue_growth<- c(rep((1+0.06),7),rep((1+0.03),13)),
  annual_cogs_rate <- c(rep(0.8165,20)),
  excess_periods <- 20,
  
  current_depreciation <- 1814,
  annual_depreciation_growth <- c((1-0.0683317211644177),(rep((1+0.06),5)),(rep((1+0.03),14))),
  
  tax_rate <- 0.2422,
  current_nol <- 200000,
  
  current_capex <- 7696,
  annual_capex_growth <- c((1+0.023939668853227),(rep((1+0.06),4)),(rep((1+0.03),15))),
  
  working_cap_rate <- c(rep(0.05,20)),
  
  cap_periods <- 5,
  beta_stock <- 1,
  
  cost_of_equity <- 0.07,
  cost_of_debt <- 0.025,
  # cost_of_equity allows user to manually set their desired cost of equity
  # To make the function calculate cost of equity, set this variable to 0
  
  risk_free_rate <- 0.02,
  risk_premium <- 0.05,
  
  publicly_traded_flag <- TRUE,
  last_traded_price <- 500,
  shares_outstanding <- 929.3,
  market_val_debt <- 0.1,
  book_val_debt <- 0.1,
  book_val_equity <- 76615,
  debt_capital_ratio <- 0,
  
  beta_stable_period <- 0,
  debt_capital_ratio_stable_period <- 0.15,
  cost_of_debt_stable_period <- 0.08,
  
  growth_rate_stable_period <- 0.03,
  operating_expense_stable_period <- 0.8165,
  capex_stable_period <- 1.1,
  working_cap_rate_stable_period <- 0.01,
  cost_of_debt_flag <- TRUE
)


dcf_function <- function(Input_List){
  # Annual revenue, depreciation, capital expenditure and chang in working capital
  for(i in 1:excess_periods){
    if(i==1){
      annual_revenue_val <- current_revenue*annual_revenue_growth[i]
      annual_depreciation_val <- current_depreciation*annual_depreciation_growth[i]
      annual_capex_val <- current_capex*annual_capex_growth[i]
      annual_working_cap_change <- (annual_revenue_val[i] - current_revenue)*working_cap_rate[i]
    }
    else{
      annual_revenue_val <- c(annual_revenue_val,annual_revenue_val[i-1]*annual_revenue_growth[i])
      annual_depreciation_val <- c(annual_depreciation_val,annual_depreciation_val[i-1]*annual_depreciation_growth[i])
      annual_capex_val <- c(annual_capex_val,annual_capex_val[i-1]*annual_capex_growth[i])
      annual_working_cap_change <- c(annual_working_cap_change,(annual_revenue_val[i]-annual_revenue_val[i-1])*working_cap_rate[i])
    }
  }
  
  # Annual COGS and EBIT
  annual_cogs_val <- annual_revenue_val*annual_cogs_rate
  annual_ebit_val <- annual_revenue_val-annual_cogs_val
  
  
  # Annual tax and annual net operating loss balance
  for (i in 1:excess_periods){
    # First year tax and nol balance calculation
    if(i==1){
      # Tax calculation
      if(annual_ebit_val[i]>0){
        if(current_nol>annual_ebit_val[i]){
          annual_tax_val <- 0}
        else{
          annual_tax_val <- ((annual_ebit_val[i]-current_nol)*tax_rate)
        }
      }
      
      # nol balance calculation
      if(current_nol>0){
        if(current_nol<annual_revenue_val[i]){
          annual_nol_balance <- 0
        }
        else{
          annual_nol_balance <- current_nol-annual_revenue_val[i]
        }
      }
      else{
        annual_nol_balance <- 0
      }
    }
    
    # Beyond first year annual tax and nol balance calculation
    else{
      # tax calculation beyond year 1
      if(annual_ebit_val[i]>0){
        if(annual_nol_balance[i-1]>annual_ebit_val[i]){
          annual_tax_val <- c(annual_tax_val,0)
        }
        else{
          annual_tax_val <- c(annual_tax_val,((annual_ebit_val[i]-annual_nol_balance[i-1])*tax_rate))
        }
      }
      else{
        annual_tax_val <- c(annual_tax_val,0)
      }
      
      # nol balance calculation beyond year 1
      if(annual_nol_balance[i-1]>annual_ebit_val[i]){
        annual_nol_balance <- c(annual_nol_balance,(annual_nol_balance[i-1]-annual_ebit_val[i]))
      }
      else{
        annual_nol_balance <- c(annual_nol_balance,0)
      }
    }
  }
  # The For loop above produces two vectors
  # 1. annual_tax_val
  # 2. annual_nol_balance
  
  # EBIT-tax
  annual_after_tax_ebit_val <- annual_ebit_val-annual_tax_val
  
  annual_fcff_val <- annual_after_tax_ebit_val+annual_depreciation_val-annual_capex_val-annual_working_cap_change
  
  # Cost of Equity Calculation
  if(cost_of_equity ==!0){
    cost_of_equity_val <- cost_of_equity
  }
  else{
    cost_of_equity_val <- risk_free_rate+beta_stock*risk_premium
  }
  
  # Equity to Capital Ratio Calculation
  if(publicly_traded_flag == TRUE){
    equity_capital_ratio_val <- last_traded_price*shares_outstanding/(market_val_debt+last_traded_price*shares_outstanding)
  }
  else if (debt_capital_ratio ==!0){
    equity_capital_ratio_val <- (1-book_val_debt/(book_val_debt+book_val_equity))
  }
  else{
    equity_capital_ratio_val <- 1-debt_capital_raio
  }
  
  after_tax_cost_of_debt <- cost_of_debt * (1-tax_rate)
  debt_capital_ratio_val <- 1- equity_capital_ratio_val
  cost_of_capital_val <- cost_of_equity_val * equity_capital_ratio_val + after_tax_cost_of_debt*debt_capital_ratio_val
  
  # Annual beta
  for (i in 1:excess_periods){
    if(i==1){
      annual_beta_stock <- beta_stock
      annual_debt_capital_ratio_val <- debt_capital_ratio_val
    }
    else if(i<= cap_periods){
      annual_beta_stock <- c(annual_beta_stock,beta_stock)
      annual_debt_capital_ratio_val <- c(annual_debt_capital_ratio_val,debt_capital_ratio_val)
    }
    else{ 
      if(beta_stable_period==!0){
      annual_beta_stock<-c(annual_beta_stock,annual_beta_stock[cap_periods]-((annual_beta_stock[cap_periods]-beta_stable_period)/cap_periods))
      }
      else{
        annual_beta_stock <- c(annual_beta_stock,beta_stock)
      }
      if(debt_capital_ratio_stable_period ==!0){
        annual_debt_capital_ratio_val <- c(annual_debt_capital_ratio_val,annual_debt_capital_ratio_val[cap_periods]-((annual_debt_capital_ratio_val[cap_periods]-debt_capital_ratio_val)/cap_periods)*(i-cap_periods))
      }
      else{
        annual_debt_capital_ratio_val <- c(annual_debt_capital_ratio_val,debt_capital_ratio_val)
      }
    }  
  }
  
  # Annual cost of equity, cost of debt and cost of capital
  for(i in 1:excess_periods){
    if (i==1){
      if(cost_of_equity==0){
        annual_cost_of_equity <- risk_free_rate+annual_beta_stock[i]*risk_premium
      }
      else{
        annual_cost_of_equity <- cost_of_equity
      }
        annual_cost_of_debt <- cost_of_debt_stable_period*(1-tax_rate)
        annual_cost_of_capital <- annual_cost_of_equity[i]*(1-annual_debt_capital_ratio_val[i])+annual_cost_of_debt[i]*annual_debt_capital_ratio_val
    }
    else{
      if(cost_of_equity ==0){
      annual_cost_of_equity <- c(annual_cost_of_equity,risk_free_rate+annual_beta_stock[i]*risk_premium)
      }
      else{
        annual_cost_of_equity <- c(annual_cost_of_equity, cost_of_equity)
      }
      annual_cost_of_debt <- c(annual_cost_of_debt,cost_of_debt_stable_period*(1-tax_rate))
      annual_cost_of_capital <-c(annual_cost_of_capital, annual_cost_of_equity[i]*(1-annual_debt_capital_ratio_val[i])+annual_cost_of_debt[i]*annual_debt_capital_ratio_val)
    }
  }
  
  # cumulative WACC
  for(i in 1:excess_periods){
    if(i==1){
      cum_WACC <- 1+annual_cost_of_capital[i]
    }
    else{
      cum_WACC <- c(cum_WACC,cum_WACC[i-1]*(1+annual_cost_of_capital[i]))
    }
  }
  
  present_values <- annual_fcff_val/cum_WACC
  
  # 21st element in every annual vector
  annual_revenue_val[excess_periods+1] <- annual_revenue_val[excess_periods]*(1+growth_rate_stable_period)
  annual_cogs_val[excess_periods+1] <- annual_revenue_val[excess_periods+1]*operating_expense_stable_period
  annual_ebit_val[excess_periods+1] <- annual_revenue_val[excess_periods+1]-annual_cogs_val[excess_periods+1]
  
  # 21st element in annual_tax_val
  if(annual_ebit_val[excess_periods+1]>0){
    if(annual_nol_balance[excess_periods]>annual_ebit_val[excess_periods+1]){
      annual_tax_val[excess_periods+1] <- 0
    }
    else{
      annual_tax_val[excess_periods+1] <- (annual_ebit_val[excess_periods+1]-annual_nol_balance[excess_periods])*tax_rate
    }
  }
  else{
    annual_tax_val[excess_periods+1] <- 0
  }
  
  annual_after_tax_ebit_val[excess_periods+1] <- annual_ebit_val[excess_periods+1]-annual_tax_val[excess_periods+1]
  annual_depreciation_val[excess_periods+1] <- annual_depreciation_val[excess_periods]
  
  # 21st element in annual_capex_val
  if(capex_stable_period==0){
    annual_capex_val[excess_periods+1] <- annual_depreciation_val[excess_periods+1]
  }
  else{
    annual_capex_val[excess_periods+1] <- annual_depreciation_val[excess_periods+1]*capex_stable_period
  }
  
  annual_working_cap_change[excess_periods+1] <- (annual_revenue_val[excess_periods+1]-annual_revenue_val[excess_periods])*working_cap_rate_stable_period
  annual_fcff_val[excess_periods+1] <- annual_after_tax_ebit_val[excess_periods+1]+annual_depreciation_val[excess_periods+1]-annual_capex_val[excess_periods+1]-annual_working_cap_change[excess_periods+1]
  annual_beta_stock[excess_periods+1] <- annual_beta_stock[excess_periods]
  
  # 21st element in annual_cost_of_equity
  if(cost_of_equity ==0){
    annual_cost_of_equity[excess_periods+1] <- risk_free_rate+annual_beta_stock[excess_periods+1]*risk_premium
  }
  else{
    annual_cost_of_equity[excess_periods+1] <- cost_of_equity
  }
  
  annual_cost_of_debt[excess_periods+1] <- annual_cost_of_debt[excess_periods]
  annual_debt_capital_ratio_val[excess_periods+1] <- annual_debt_capital_ratio_val[excess_periods]
  annual_cost_of_capital[excess_periods+1] <- annual_cost_of_equity[excess_periods+1]*(1-annual_debt_capital_ratio_val[excess_periods+1])+annual_cost_of_debt[excess_periods+1]*annual_debt_capital_ratio_val[excess_periods+1]
  
  if(beta_stable_period ==0){
    cost_of_equity_stable_period <- risk_free_rate+beta_stock*risk_premium
  }
  else{
    cost_of_debt_stable_period <- risk_free_rate+beta_stable_period*risk_premium
  }
  
  if(debt_capital_ratio_stable_period ==0){
    equity_capital_ratio_stable_period <- equity_capital_ratio_val
  }
  else{
    equity_capital_ratio_stable_period <- (1-debt_capital_ratio_stable_period)
  }
  
  if(cost_of_debt_flag == TRUE){
    AT_cost_of_debt_stable_period <- cost_of_debt_stable_period*(1-tax_rate)
  }
  else{
    AT_cost_of_debt_stable_period <- after_tax_cost_of_debt
  }
  
  equity_capital_ratio_stable_period <- 1 - debt_capital_ratio_stable_period
  cost_of_capital_stable_period <- cost_of_equity_stable_period*equity_capital_ratio_stable_period+AT_cost_of_debt_stable_period*debt_capital_ratio_stable_period
  
  value_end_of_growth_period <- annual_fcff_val[excess_periods+1]/(cost_of_capital_stable_period-growth_rate_stable_period)
  present_value_growth_period <- sum(present_values)
  pv_terminal_value <- value_end_of_growth_period/cum_WACC[excess_periods]
  value_of_firm <- present_value_growth_period+pv_terminal_value
  if(publicly_traded_flag == TRUE){
    output_value_debt <- market_val_debt
  }
  else{
    output_value_debt <- book_val_debt
  }
  output_value_equity <- value_of_firm-output_value_debt
  value_of_equity_per_share <- output_value_equity/shares_outstanding
  
  return(value_of_equity_per_share)
}

dcf_function(Input_List)
