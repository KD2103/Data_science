if(!require(Quandl)) install.packages("Quandl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(Quandl)
library(dplyr)
Quandl.api_key("<Your API KEY>")

# Set start date
start_date <- "2018-01-31"
# We use this end date because we don't have data for real estate after it
end_date <- "2020-01-31"

# Download real estate data from Quandl
real_estate_data <- Quandl("ZILLOW/C20210_ZHVIAH", start_date = start_date)

# Remove any rows with missing data
real_estate_data <- na.omit(real_estate_data)

# Rename columns in real estate data
real_estate_data <- real_estate_data %>% rename(re_value = Value)

# Save real estate data to a CSV file
write.csv(real_estate_data, file = "real_estate_data.csv")

# Download S&P 500 data from Quandl
sp500_data <- Quandl("MULTPL/SP500_REAL_PRICE_MONTH", start_date = start_date, end_date = end_date, collapse = "monthly")

# Remove any rows with missing data
sp500_data <- na.omit(sp500_data)

# Rename columns in S&P 500 data
sp500_data <- sp500_data %>% rename(sp500_value = Value)

# Save S&P 500 data to a CSV file
write.csv(sp500_data, file = "sp500_data.csv")

# Download Bitcoin data from Quandl
bitcoin_data <- Quandl("BCHAIN/MKPRU", start_date = start_date, end_date = end_date, collapse = "monthly")

# Rename columns in Bitcoin data
bitcoin_data <- bitcoin_data %>% rename(btc_value = Value)

# Save Bitcoin data to a CSV file
write.csv(bitcoin_data, file = "bitcoin_data.csv")

# Download Gold data
gold_data <- Quandl("LBMA/GOLD", start_date = start_date, end_date = end_date, collapse = "monthly")
gold_data <- gold_data[, c(1, 2)]
colnames(gold_data) <- c("Date", "gold_value")

# Save Gold data to a CSV file
write.csv(gold_data, file = "gold_data.csv")
