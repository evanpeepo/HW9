#ZOO800 HW9 - Evan Peepo

# Load packages -----------------------------------------------------------

library(tidyverse)

# Objective 1 -------------------------------------------------------------

e_1 <- rnorm(n = 100)
e_10 <- rnorm(n = 100, sd = 10)
e_25 <- rnorm(n = 100, sd = 25)

intercept = -10
slope = 2.5

x <- runif(n = 100, min = 0, max = 10)

sigma_1 <- intercept + slope * x + e_1
sigma_10 <- intercept + slope * x + e_10
sigma_25 <- intercept + slope * x + e_25

regression_df <- data.frame(x, sigma_1, sigma_10, sigma_25)
 
regression_df <- regression_df %>%
  pivot_longer(cols = -x, names_to = "sigma", values_to = "y")

ggplot(regression_df, aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ sigma) +
  theme_minimal(base_size = 15)


# Objective 2 -------------------------------------------------------------

#Create function to simulate binomial test 100 times for 1-20 coin flips

sim_func <- function(num_sims, num_flips, prob_head, alpha) { #used ChatGPT to help debug
  prob_reject_null <- length(num_flips) #initialize vector
  for (i in num_flips) {
    reject <- replicate(num_sims, { #repeat 100 times for each i 
      flip <- rbinom(n = i, size = 1, prob = prob_head) #random i coin flips
      sum_flip <- sum(flip) #sum number of heads
      test <- binom.test(x = sum_flip, n = i, p = 0.5, alternative = 'two.sided') #use binom test to test if sum_flip/i is from an unfair coin
      test$p.value < alpha #take p.value from test and compare to alpha, returns TRUE if it is less
      }) 
    prob_reject_null[i] <- mean(reject) * 100 #take mean of rejections of all trials for i flips and store in vec. mean will give percentage of rejections reject = 1, accept = 0. 
  } 
  return(data.frame(prob_reject_null, num_flips, prob_head)) #return dataframe with probability of rejection and associated number of flips and actual prob. of heads
}

# Prob heads = 55-65% ------------------------------------------------------

prob_55 <- sim_func(100, 1:20, 0.55, 0.05)
prob_60 <- sim_func(100, 1:20, 0.6, 0.05)
prob_65 <- sim_func(100, 1:20, 0.65, 0.05)

all_results <- bind_rows(prob_55, prob_60, prob_65)

ggplot(all_results, aes(x = num_flips, y = prob_reject_null, color = as.factor(prob_head))) +
  geom_line() +
  labs(color = "Probability of Heads",
       y = "Prob of determining unfair coin (a = 0.05)",
       x = "# of coin flips") +
  theme_minimal(base_size = 14)


# Same idea using beta distribution ---------------------------------------

sim_func_beta <- function(num_sims, num_flips, prob_head, alpha) { 
  prob_reject_null <- length(num_flips) #initialize vector
  for (i in num_flips) {
    coin_bias <- replicate(num_sims, { #repeat 100 times for each i 
      flip <- rbinom(n = i, size = 1, prob = prob_head) #random i coin flips
      sum_flip <- sum(flip) #sum number of heads
      beta_prob <- 1 - (pbeta(0.5, shape1 = sum_flip + 1, shape2 = (i - sum_flip) + 1)) #use beta distribution to get probability of coin bias
      beta_prob > 1 - alpha #see if probability of coin bias is greater than 1 - alpha. For our example, is it greater than .95?
    }) 
    prob_reject_null[i] <- mean(coin_bias) * 100 #take mean of coin_bias of all trials for i flips and store in vec. mean will give percentage of times coin was determined to be unfair. 
  } 
  return(data.frame(prob_reject_null, num_flips, prob_head)) #return dataframe with probability of determining unfair coin and associated number of flips and actual prob. of heads
}

# Prob heads = 55-65% ------------------------------------------------------

prob_55_beta <- sim_func_beta(100, 1:20, 0.55, 0.05)
prob_60_beta <- sim_func_beta(100, 1:20, 0.6, 0.05)
prob_65_beta <- sim_func_beta(100, 1:20, 0.65, 0.05)

all_results_beta <- bind_rows(prob_55_beta, prob_60_beta, prob_65_beta)

ggplot(all_results_beta, aes(x = num_flips, y = prob_reject_null, color = as.factor(prob_head))) +
  geom_line() +
  labs(color = "Probability of Heads",
       y = "Prob of determining unfair coin (a = 0.05)",
       x = "# of coin flips") +
  theme_minimal(base_size = 14)

