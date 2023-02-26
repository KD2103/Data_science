if(!require(Quandl)) install.packages("Quandl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")


library(PerformanceAnalytics)
library(Quandl)
library(caret)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(rpart)

# Set start date
start_date <- "2018-01-31"
# We use this end date because we don't have data for real estate after it
end_date <- "2020-01-31"

# Download real estate data from Quandl
Quandl.api_key("<Your API>")
real_estate_data <- Quandl("ZILLOW/C20210_ZHVIAH", start_date = start_date)

# Set the Region column as the row names
row.names(real_estate_data) <- real_estate_data$Region
real_estate_data$Region <- NULL

# Remove any rows with missing data
real_estate_data <- na.omit(real_estate_data)

# Rename columns in real estate data
real_estate_data <- real_estate_data %>% rename(re_value = Value)

# View the first few rows of the data
real_estate_data

# download S&P 500 data from 2015-01-01
sp500_data <- Quandl("MULTPL/SP500_REAL_PRICE_MONTH", start_date = start_date,end_date = end_date, collapse="monthly")

# Remove any rows with missing data
sp500_data <- na.omit(sp500_data)

# Rename columns in S&P 500 data
sp500_data <- sp500_data %>% rename(sp500_value = Value)

# View the first few rows of the data
sp500_data

# Download Bitcoin data from Quandl
bitcoin_data <- Quandl("BCHAIN/MKPRU", start_date = start_date, end_date = end_date, collapse = "monthly")

# Rename columns in Bitcoin data
bitcoin_data <- bitcoin_data %>% rename(btc_value = Value)

# View the first few rows of the data
bitcoin_data

# Download Gold data
gold_data <- Quandl("LBMA/GOLD", start_date = start_date, end_date = end_date, collapse = "monthly")
gold_data <- gold_data[, c(1, 2)]
colnames(gold_data) <- c("Date", "gold_value")

# View the first few rows of the data
gold_data

# Merge data frames
merged_data <- merge(sp500_data, real_estate_data, by = "Date", all = TRUE)
merged_data <- merge(merged_data, bitcoin_data, by = "Date", all = TRUE)
merged_data <- merge(merged_data, gold_data, by = "Date", all = TRUE)
merged_data <- na.omit(merged_data)

# calculate percentage change for each column
merged_data <- merged_data %>%
  mutate(sp500_pct_change = (sp500_value - lag(sp500_value)) / lag(sp500_value) * 100,
         re_pct_change = (re_value - lag(re_value)) / lag(re_value) * 100,
         btc_pct_change = (btc_value - lag(btc_value)) / lag(btc_value) * 100,
         gold_pct_change = (gold_value - lag(gold_value)) / lag(gold_value) * 100)
merged_data <- na.omit(merged_data)

# Standard deviation of numerical columns
volatility <- sapply(merged_data[, 6:9], sd)
volatility

# View the merged data frame
head(merged_data)

# Summary statistics
summary(merged_data)

# Standard deviation of numerical columns
sapply(merged_data[, 2:5], sd)

correlation_matrix <- cor(merged_data[, 2:5], use = "complete.obs")
print(round(correlation_matrix, 2))

# Create a line plot of the values over time
ggplot(data = merged_data, aes(x = Date)) +
  geom_line(aes(y = sp500_pct_change, color = "S&P 500")) +
  geom_line(aes(y = btc_pct_change, color = "Bitcoin")) +
  geom_line(aes(y = re_pct_change, color = "Real Estate")) +
  geom_line(aes(y = gold_pct_change, color = "Gold")) +
  labs(x = "Date", y = "Percentage Change", title = "Percentage Change Over Time") +
  theme(legend.position = "bottom")

# Calculate monthly returns for each asset
monthly_returns <- data.frame(
  Date = merged_data$Date[-1], 
  sp500_return = diff(log(merged_data$sp500_value)), 
  re_return = diff(log(merged_data$re_value)), 
  btc_return = diff(log(merged_data$btc_value),),
  gold_return = diff(log(merged_data$gold_value))

)

# Plot the histogram of monthly returns for each asset
ggplot(monthly_returns, aes(x = btc_return)) + 
  geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.5) +
  labs(x = "Monthly Returns", y = "Frequency", title = "Histogram of Monthly Returns")

# Create a data frame with only S&P 500 and Real Estate returns
sp500_re_returns <- data.frame(
  Date = monthly_returns$Date,
  sp500_return = monthly_returns$sp500_return,
  re_return = monthly_returns$re_return
)

# Plot the scatter plot of S&P 500 returns vs. Real Estate returns
ggplot(sp500_re_returns, aes(x = sp500_return, y = re_return)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(x = "S&P 500 Monthly Returns", y = "Real Estate Monthly Returns", 
       title = "Scatter Plot of S&P 500 Returns vs. Real Estate Returns")

# Convert the correlation matrix to a data frame
cor_df <- as.data.frame(as.table(correlation_matrix))

# Create a melted version of the data for ggplot
cor_melted <- melt(cor_df, id.vars = c("Var1", "Var2"))

# Create the heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare the data
portfolio_data <- data.frame(
  sp500_pct_change = merged_data$sp500_pct_change,
  re_pct_change = merged_data$re_pct_change,
  btc_pct_change = merged_data$btc_pct_change,
  gold_pct_change = merged_data$gold_pct_change
)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(portfolio_data), 0.8 * nrow(portfolio_data))
train_data <- portfolio_data[train_index, ]
test_data <- portfolio_data[-train_index, ]
# Remove any rows with missing values
train_data <- na.omit(train_data)

###################################
# Train the linear regression model
###################################
lm_model <- lm(btc_pct_change ~ sp500_pct_change + re_pct_change + gold_pct_change, data = train_data)

# Test the linear regression model
predictions <- predict(lm_model, newdata = test_data)
MAE <- mean(abs(predictions - test_data$btc_pct_change))
R_squared <- summary(lm_model)$r.squared
cat("MAE:", MAE, "\n")
cat("R-squared:", R_squared, "\n")

# Use the model to find the optimal portfolio ratio
sp500_weight <- coef(lm_model)[2]
re_weight <- coef(lm_model)[3]
btc_weight <- -1 * coef(lm_model)[1]
gold_weight <- coef(lm_model)[4]
total_weight <- sp500_weight + re_weight + btc_weight + gold_weight
sp500_ratio <- sp500_weight / total_weight
re_ratio <- re_weight / total_weight
btc_ratio <- btc_weight / total_weight
gold_ratio <- gold_weight / total_weight
cat("Optimal portfolio ratio: S&P 500 =", sp500_ratio, ", Real Estate =", re_ratio, ", Bitcoin =", btc_ratio, ", Gold =", gold_ratio, "\n")

# Save results to a data frame
results <- data.frame(
  Model = "Linear Regression",
  MAE = MAE,
  R_squared = R_squared,
  sp500_ratio = sp500_ratio,
  re_ratio = re_ratio,
  btc_ratio = btc_ratio,
  gold_ratio = gold_ratio
)

results
#####################################
# Use Genetic Algorithmt find the optimal portfolio ratio
#####################################
# In this example, we define a fitness function that calculates the mean absolute error (MAE) of the predicted
# Bitcoin returns based on the current set of weights for each asset. We then use the ga function from the GA package
# to find the set of weights that minimizes the MAE. The lower and upper vectors define the minimum and maximum possible
# values for each weight, and popSize and maxiter specify the population size and maximum number of iterations for the
# genetic algorithm. Finally, we extract the optimal weights and calculate the corresponding portfolio ratios.

if(!require(GA)) install.packages("GA", repos = "http://cran.us.r-project.org")
library(GA)

# Define fitness function
fitness <- function(x) {
  sp500_weight <- x[1]
  re_weight <- x[2]
  btc_weight <- x[3]
  gold_weight <- x[4]
  total_weight <- sp500_weight + re_weight + btc_weight + gold_weight
  sp500_ratio <- sp500_weight / total_weight
  re_ratio <- re_weight / total_weight
  btc_ratio <- btc_weight / total_weight
  gold_ratio <- gold_weight / total_weight
  predicted_btc_return <- sp500_ratio * merged_data$sp500_pct_change + 
    re_ratio * merged_data$re_pct_change + 
    btc_ratio * merged_data$btc_pct_change +
    gold_ratio * merged_data$gold_pct_change
  MAE <- mean(abs(predicted_btc_return - merged_data$btc_pct_change))
  return(-MAE)
}

# Define bounds for portfolio weights
lower <- c(0, 0, 0, 0)
upper <- c(1, 1, 1, 1)

# Run the genetic algorithm to find optimal weights
ga_result <- ga(type = "real", fitness = fitness, lower = lower, upper = upper, popSize = 50, maxiter = 100)
ga_result

# Extract the optimal weights
sp500_weight <- ga_result@solution[1]
re_weight <- ga_result@solution[2]
btc_weight <- ga_result@solution[3]
gold_weight <- ga_result@solution[4]
total_weight <- sp500_weight + re_weight + btc_weight + gold_weight
sp500_ratio <- sp500_weight / total_weight
re_ratio <- re_weight / total_weight
btc_ratio <- btc_weight / total_weight
gold_ratio <- gold_weight / total_weight


cat("MAE:", MAE, "\n")
cat("R-squared:", R_squared, "\n")
cat("Optimal portfolio ratio: S&P 500 =", sp500_ratio, ", Real Estate =", re_ratio, ", Bitcoin =", btc_ratio, ", Gold =", gold_ratio, "\n")

# Save results to a data frame
results <- rbind(results, data.frame(
  Model = "Genetic Algorithm",
  MAE = MAE,
  R_squared = R_squared,
  sp500_ratio = sp500_ratio,
  re_ratio = re_ratio,
  btc_ratio = btc_ratio,
  gold_ratio = gold_ratio
))
results

#####################
# Train and test Nelder-Mead algorithm to find the optimal weights
#####################

# Define the regression model
model <- lm(btc_pct_change ~ sp500_pct_change + re_pct_change + gold_pct_change, data = train_data)

# Use the model to predict returns for each asset in the test set
predictions <- predict(model, newdata = test_data)

# Define the expected return and covariance matrices
mu <- colMeans(test_data[, 1:4])
Sigma <- cov(test_data[, 1:4])

# Define the target return (i.e., the Sharpe ratio)
target_return <- max(mu)

# Define the optimization function
obj_func <- function(w) {
  mu_p <- sum(mu * w)
  sigma_p <- sqrt(t(w) %*% Sigma %*% w)
  Sharpe_ratio <- mu_p / sigma_p
  -Sharpe_ratio
}

# Use the Nelder-Mead algorithm to find the optimal weights
opt_weights <- optim(rep(1/4, 4), obj_func, method = "Nelder-Mead")$par

# Calculate the Sharpe ratio of the optimal portfolio
mu_p <- sum(mu * opt_weights)
sigma_p <- sqrt(t(opt_weights) %*% Sigma %*% opt_weights)
Sharpe_ratio <- mu_p / sigma_p

# Print the results
cat("Optimal portfolio ratio: S&P 500 =", opt_weights[1], ", Real Estate =", opt_weights[2], ", Bitcoin =", opt_weights[3], ", Gold =", opt_weights[4], "\n")
cat("Sharpe ratio:", Sharpe_ratio, "\n")

# Calculate the mean absolute error and coefficient of determination
MAE <- mean(abs(predictions - test_data$btc_pct_change))
R_squared <- summary(model)$r.squared
cat("MAE:", MAE, "\n")
cat("R-squared:", R_squared, "\n")

# Save results to a data frame
results <- rbind(results, data.frame(
  Model = "Nelder-Mead",
  MAE = MAE,
  R_squared = R_squared,
  sp500_ratio = opt_weights[1],
  re_ratio = opt_weights[2],
  btc_ratio = opt_weights[3],
  gold_ratio = opt_weights[4]
))

# Print the results
results


