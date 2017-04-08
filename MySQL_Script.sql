-- Create a new DB schema
CREATE SCHEMA `dowstockinfo`;
-- Select the newly created schema to use
USE dowstockinfo;

-- Create a table to store the stocks


-- Select all of the data from the stock table
SELECT * FROM dowstockinfo.stocks;

-- Manually insert values into the stock table for testing
INSERT INTO stocks (current_revenue, annual_rev_growth, annual_cogs_rate, excess_periods) 
	VALUES (108249, 1.03, 0.8165, 20);
