library(tidyverse)
library(glmnet)
library(rethinking)


# Set baseline log-odds of success
baseline <- -1
# How will the predictors change the log-odds of success?
pred.1.effect <- 1
pred.2.effect <- 2

# Set up trials of different sizes
trials <- seq(from = 10, to = 1000, length.out = 100)

# Generate two Gaussian predictors
pred.1 <- rnorm(n = trials, mean = 0, sd = 1)
pred.2 <- rnorm(n = trials, mean = 0, sd = 1)

# Simulate successes for each trial size
probs <- logistic(baseline + pred.1 * pred.1.effect + pred.2 * pred.2.effect)

sim <- rbinom(
  n = length(trials),
  size = trials,
  prob = probs
)

# Package data together
d <- data.frame(
  first_predictor = pred.1,
  second_predictor = pred.2,
  trials = trials,
  successes = sim
) %>%
  mutate(
    nonsuccesses = trials - sim,
    prop = successes/trials,
    prop_minus = 1 - prop
  )

# Fit model, coding the outcome as non-success and success counts (don't
# need weights this way)
out <- glmnet(
  x = d %>%
    dplyr::select(first_predictor, second_predictor) %>%
    as.matrix(),
  y = d %>%
    dplyr::select(nonsuccesses, successes) %>%
    as.matrix(),
  family = "binomial",
  lambda = 0.000957
)

# Verify that we recover the original parameters
out$a0
out$beta

# Make predictions from the model
predictions <- predict(out, newx = matrix(c(pred.1, pred.2), ncol = 2))

# Plot the original probabilities used for the simulation against the model
# predictions
plot(probs, exp(predictions) / (1 + exp(predictions)))
