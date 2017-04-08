-- Create a new DB schema
CREATE SCHEMA `dowstockinfo`;
-- Select the newly created schema to use
USE dowstockinfo;

-- Create a table to store the stocks
CREATE TABLE `dowstockinfo`.`stocks` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `current_revenue` DOUBLE NULL,
  `annual_revenue_growth` VARCHAR(200) NULL,
  `annual_cogs_rate` VARCHAR(200) NULL,
  `excess_periods` DOUBLE NULL,
  `current_depreciation` DOUBLE NULL,
  `annual_depreciation_growth` VARCHAR(200) NULL,
  `tax_rate` DOUBLE NULL,
  `current_nol` DOUBLE NULL,
  `current_capex` DOUBLE NULL,
  `annual_capex_growth` VARCHAR(200) NULL,
  `working_cap_rate` VARCHAR(200) NULL,
  `cap_periods` DOUBLE NULL,
  `beta_stock` DOUBLE NULL,
  `cost_of_equity` DOUBLE NULL,
  `cost_of_debt` DOUBLE NULL,
  `risk_free_rate` DOUBLE NULL,
  `risk_premium` DOUBLE NULL,
  `publicly_traded_flag` TINYINT(1) NULL,
  `last_traded_price` DOUBLE NULL,
  `shares_outstanding` DOUBLE NULL,
  `market_val_debt` DOUBLE NULL,
  `book_val_debt` DOUBLE NULL,
  `book_val_equity` DOUBLE NULL,
  `debt_capital_ratio` DOUBLE NULL,
  `beta_stable_period` DOUBLE NULL,
  `debt_capital_ratio_stable_period` DOUBLE NULL,
  `cost_of_debt_stable_period` DOUBLE NULL,
  `growth_rate_stable_period` DOUBLE NULL,
  `operating_expense_stable_period` DOUBLE NULL,
  `capex_stable_period` DOUBLE NULL,
  `working_cap_rate_stable_period` DOUBLE NULL,
  `cost_of_debt_flag` TINYINT(1) NULL,
  PRIMARY KEY (`id`));


-- Select all of the data from the stock table
SELECT * FROM dowstockinfo.stocks;

-- Manually insert values into the stock table for testing
INSERT INTO stocks VALUES (1,
108249,
'1.06 1.06 1.06 1.06 1.06 1.06 1.06 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03',
'0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165 0.8165',
20,
1814,
'0.9316683 1.0600000 1.0600000 1.0600000 1.0600000 1.0600000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000 1.0300000',
0.2422,
200000,
7696,
'1.02394 1.06000 1.06000 1.06000 1.06000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000 1.03000',
'0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05',
5,
1,
0.07,
0.025,
0.02,
0.05,
1,
500,
929.3,
0.1,
0.1,
76615,
0,
0,
0.15,
0.08,
0.03,
0.8165,
1.1,
0.01,
1);