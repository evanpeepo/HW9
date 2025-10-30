
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
  theme_minimal()


# Objective 2 -------------------------------------------------------------

#Create function to simulate binomial test 100 times for 1-20 coin flips

sim_func <- function(num_sims, num_flips, prob_head, alpha) { #used ChatGPT to help debug
  vec <- numeric(length(num_flips))
  for (i in num_flips) {
    reject <- replicate(num_sims, {
      flip <- rbinom(n = i, size = 1, prob = prob_head)
      sum_flip <- sum(flip)
      test <- binom.test(x = sum_flip, n = i, p = 0.5, alternative = 'two.sided')
      test$p.value < alpha }) 
    vec[i] <- mean(reject) * 100
  } 
  return(vec)
}

# Prob heads = 55% --------------------------------------------------------

prob_55_vec <- sim_func(100, 1:20, 0.55, 0.05)

prob_55_df <- data.frame(prob_55_vec)

prob_55_df <- prob_55_df %>% 
  mutate(coin_flips = 1:20, 
         prob_heads = 0.55) %>% 
  rename(prob_rejection = prob_55_vec)

ggplot(prob_55_df, aes(x = coin_flips, y = prob_rejection)) +
  geom_line()

# Prob heads = 60% --------------------------------------------------------

prob_60_vec <- sim_func(100, 1:20, 0.6, 0.05)

prob_60_df <- data.frame(prob_60_vec)

prob_60_df <- prob_60_df %>% 
  mutate(coin_flips = 1:20,
         prob_heads = 0.60) %>% 
  rename(prob_rejection = prob_60_vec)

ggplot(prob_60_df, aes(x = coin_flips, y = prob_rejection)) +
  geom_line()

# Prob heads = 65% --------------------------------------------------------

prob_65_vec <- sim_func(100, 1:20, 0.65, 0.05)

prob_65_df <- data.frame(prob_65_vec)

prob_65_df <- prob_65_df %>% 
  mutate(coin_flips = 1:20,
         prob_heads = 0.65) %>% 
  rename(prob_rejection = prob_65_vec)

ggplot(prob_65_df, aes(x = coin_flips, y = prob_rejection)) +
  geom_line()

all_results <- bind_rows(prob_55_df, prob_60_df, prob_65_df)

ggplot(all_results, aes(x = coin_flips, y = prob_rejection, color = as.factor(prob_heads))) +
  geom_line() +
  labs(color = "Probability of Heads",
       y = "Prob of determining unfair coin (a = 0.05)",
       x = "# of coin flips")
