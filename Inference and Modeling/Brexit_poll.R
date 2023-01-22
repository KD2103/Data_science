# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N = 1500

# What is the expected total number of voters in the sample choosing "Remain"?
ev <- p*N
print(ev)

# What is the standard error of the total number of voters in the sample choosing "Remain"?
se <- sqrt(N*p*(1-p))
print(se)

# What is the expected value of X^, the proportion of "Remain" voters?
print(X_hat <- p)

# What is the standard error of X^, the proportion of "Remain" voters?
sqrt(N*p*(1-p))

#What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
print(d)

# What is the standard error of d, the spread between the proportion of "Remain" voters and "Leave" voters?
print(2*sqrt(p*(1-p)/N))

### Question 2: Actual Brexit poll estimates
#Load and inspect the brexit_polls dataset from dslabs, which contains actual polling data for the 6 months before the Brexit vote. Raw proportions of voters preferring "Remain", "Leave", and "Undecided" are available (remain, leave, undecided) The spread is also available (spread), which is the difference in the raw proportion of voters choosing "Remain" and the raw proportion choosing "Leave".

#Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the referendum day (), given the observed spread and the relationship . Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below:
brexit_polls <- brexit_polls %>%
        mutate(x_hat = (spread + 1)/2)

print(head(brexit_polls))

# What is the average of the observed spreads (spread)?
print(mean(brexit_polls$spread))

# What is the standard deviation of the observed spreads?
print(sd(brexit_polls$spread))

# What is the average of x_hat, the estimates of the parameter p?
print(mean(brexit_polls$x_hat))

# What is the standard deviation of x_hat?
print(sd(brexit_polls$x_hat))

# Question 3: Confidence interval of a Brexit poll
# Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
youGov_poll <- brexit_polls[1,]
print(youGov_poll)
p <- youGov_poll$x_hat
N <- youGov_poll$samplesize
se <- sqrt(p*(1-p)/N)

# What is the lower bound of the 95% confidence interval?
lower <- p - qnorm(0.975)*se
print(lower)

# What is the upper bound of the 95% confidence interval?
upper <- p + qnorm(0.975)*se
print(upper)

# Does the 95% confidence interval predict a winner (does not cover )? Does the 95% confidence interval cover the true value of  observed during the referendum?

!between(0.5, p - qnorm(.975)*se, p + qnorm(.975)*se)    # predicts winner
between(0.481, p - qnorm(.975)*se, p + qnorm(.975)*se)    # does not cover p


### Brexit poll analysis - Part 2
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Question 4: Confidence intervals for polls in June
# How many polls are in june_polls?
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
    mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
    estimate_se = 2 * se_x_hat,
    lower = spread - qnorm(0.975) * estimate_se,
    upper = spread + qnorm(0.975) * estimate_se,
    hit = lower < -0.038 & upper > -0.038)
head(june_polls)

# How many polls are in june_polls?
nrow(june_polls)

# What proportion of polls have a confidence interval that covers the value 0?
mean(june_polls$lower < 0 & june_polls$upper > 0)

#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
mean(june_polls$lower > 0)

#What proportion of polls have a confidence interval covering the true value of ?
mean(june_polls$hit)

# Question 5: Hit rate by pollster
#Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
june_polls %>% group_by(pollster) %>%
    summarize(hit_rate = mean(hit), n()) %>%
    arrange(desc(hit_rate))

### Question 6: Boxplot of Brexit polls by poll type
# Make a boxplot of the spread in june_polls by poll type.
june_polls %>% group_by(poll_type) %>%
  ggplot(aes(poll_type,spread)) +
  geom_boxplot()


### Question 7: Combined spread across poll type
combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2)

# What is the lower bound of the 95% confidence interval for online voters?
combined_by_type <- june_polls %>%
    group_by(poll_type) %>%
    summarize(N = sum(samplesize),
              spread = sum(spread*samplesize)/N,
              p_hat = (spread + 1)/2,
              se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
              spread_lower = spread - qnorm(.975)*se_spread,
              spread_upper = spread + qnorm(.975)*se_spread)

combined_by_type %>% filter(poll_type == "Online") %>% pull(spread_lower)

# What is the upper bound of the 95% confidence interval for online voters?
combined_by_type %>% filter(poll_type == "Online") %>% pull(spread_upper)

### Question 8: Interpreting combined spread estimates across poll type
combined_by_type

### Brexit poll analysis - Part 3
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

### Chi-squared p-value
# Define brexit_hit, with the following code, which computes the confidence intervals for all Brexit polls in 2016 and then calculates whether the confidence interval covers the actual value of the spread d=-0.038
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
    se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
    spread_lower = spread - qnorm(.975)*se_spread,
    spread_upper = spread + qnorm(.975)*se_spread,
    hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

# Use brexit_hit to make a two-by-two table of poll type and hit status. Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.
brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)

# Determine which poll type has a higher probability of producing a confidence interval that covers the correct value of the spread. Also determine whether this difference is statistically significant at a p-value cutoff of 0.05. Which of the following is true?
hit_rate <- brexit_hit %>%
    group_by(poll_type) %>%
    summarize(avg = mean(hit))
hit_rate

## Question 10: Odds ratio of online and telephone poll hit rate
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds

# Calculate the odds that a telephone poll generates a confidence interval that covers the actual value of the spread.
phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

# Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.
online_odds/phone_odds

# Question 11: Plotting spread over time
brexit_polls %>%
    ggplot(aes(enddate, spread, color = poll_type)) +
    geom_smooth(method = "loess", span = 0.4) +
    geom_point() +
    geom_hline(aes(yintercept = -.038))

### Question 12: Plotting raw percentages over time
brexit_long <- brexit_polls %>%
    gather(vote, proportion, "remain":"undecided") %>%
    mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, col=vote)) +
  geom_point() +
  geom_smooth(method = "loess", span=0.3)
