# Create a database using dplyr with the inputs of your dcf for all the stocks in the DOW

# Install the appropraite packages, if you have not already done so (only need to do once, not each time running program)
# Uncomment the packages you need to install
# If you get an error, run RStudio in Admin Mode (right click on the icon and run as administrator)
install.packages("dplyr")
install.packages("RMySQL")
install.packages("data.table")
devtools::install_github("rstudio/pool")
devtools::install_github("rstudio/shiny")

# Load packages
library(dplyr)
library(RMySQL)


# Sample operations on the data 
# See first rows of data
head(airquality)
# Filter (sub-set) the data
filter(airquality, Temp > 70)
# Mutate adds new variables to the data
mutate(airquality, TempInC = (Temp - 32) * 5 / 9)
# Summarize aggregates multiple values into a single value
summarise(airquality, mean(Temp, na.rm = TRUE))
# The group_by function is used to group data by one or more variables.
summarise(group_by(airquality, Month), mean(Temp, na.rm = TRUE))
count(airquality, Month)


# Clear environment
rm(list = ls())

# Establish DB connection
my_db <- src_mysql(
  dbname = "dowStockInfo",
  host = "localhost",
  user = "root",
  password = "bMsMmM18"
)

# Query DB
my_tbl <- tbl(my_db, "stocks")
my_tbl <- select(my_tbl, id, current_revenue)
# The collect function actually pulls in the data to R into a data frame
my_tbl <- collect(my_tbl)
my_tbl[,2]
