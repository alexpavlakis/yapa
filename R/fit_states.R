library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Data --------------------------------------------------------------------

# Results from 2016 (priors) and electoral college votes by state
prior_results <- read_csv("data/state_results_16.csv")

# Read polls data from RCP and process 
source("R/process_polls.R")

# Vector of state names (51 = all states + DC)
state <- prior_results$state

# Return all state polls from 538, add 0s for states with no polls, so that prior dominates
if(!exists("exec_date")) exec_date <- Sys.Date()
polls <- data_frame(state) %>% 
  left_join(
    process_538() %>%
    filter(end_date <= exec_date) 
  ) %>% 
  mutate(Sample = ifelse(is.na(Sample), 0, Sample),
         `Trump (R)` = ifelse(is.na(`Trump (R)`), 0, `Trump (R)`),
         `Biden (D)` = ifelse(is.na(`Biden (D)`), 0, `Biden (D)`),
         Other = ifelse(is.na(Other), 0, Other),
         days_out = ifelse(is.na(days_out), 365, days_out)) 
write_csv(polls, "data/polls.csv")

# Model data --------------------------------------------------------------


# Counts for each option in each state poll
y <- polls %>%
  select(`Trump (R)`, `Biden (D)`, Other) %>%
  as.matrix()

# Total sample in each state poll
n <- polls %>% pull(Sample)

# Number of state polls
N <- nrow(y)

# Prior data
priors <- prior_results %>%
  select(rep, dem, other) %>%
  as.matrix()

# Divide state results in 2016 by national results
## multiplied by 2020 national polling average 
## to create adjusted state priors.

load("results/ge_trend")
latest_polls <- ge_trend %>% filter(day == exec_date) %>% pull(prop)

adj <- latest_polls - c(0.461, 0.482, 0.057)

priors <- t(apply(priors, 1, function(x) x + adj))
priors <- apply(priors, 2, function(x) ifelse(x > 1, 1, x))
priors <- apply(priors, 2, function(x) ifelse(x < 0, 0, x))

# Numeric identifier for each state
state_id <- match(polls$state, unique(polls$state))

# Number of states
n_states <- n_distinct(polls$state)

# Number of candidates
n_options <- ncol(y)

# Days out from election (for weighting)
days_out <- as.numeric(polls$days_out) 

# load historical bias data
## These are calculated in R/fit_bias.R
load("data/swing") 
load("data/swing_sigma")
load("data/state_sigma")

# Combine into list
model_data <- list(n_options = n_options, 
                   n_states = n_states, 
                   N = N,
                   y = y,
                   n = n,
                   state_id = state_id,
                   priors = priors,
                   days_out = days_out,
                   mu_swing = swing,
                   sigma_swing = swing_sigma,
                   sd_swing_state = state_sigma,
                   decay_param = 40)





# Fit model ---------------------------------------------------------------

m <- stan(file = "stan/yapa_states.stan", data = model_data,
          chains = 10, iter = 5000)

em <- rstan::extract(m)


# Results -----------------------------------------------------------------



# Results
means_trump <- apply(em$mu, 2, function(x) mean(x[, 1]))
quantiles_trump <- apply(em$mu, 2, function(x) quantile(x[, 1], c(0.1, 0.9)))

means_biden <- apply(em$mu, 2, function(x) mean(x[, 2]))
quantiles_biden <- apply(em$mu, 2, function(x) quantile(x[, 2], c(0.1, 0.9)))

results_biden <- data_frame(
  state = state,
  lower = quantiles_biden[1, ],
  mean  = means_biden,
  upper = quantiles_biden[2, ],
  cand  = 'biden')

results_trump <- data_frame(
  state = state,
  lower = quantiles_trump[1, ],
  mean  = means_trump,
  upper = quantiles_trump[2, ],
  cand  = 'trump')

# Save
save(results_biden, file = "results/results_biden")
save(results_trump, file = "results/results_trump")

# Formatted Table
state_results <- results_biden %>%
  rename(`Lower Biden` = lower,
         `Upper Biden` = upper,
         `Mean Biden`  = mean) %>%
  select(-cand) %>%
  left_join(results_trump %>%
              rename(`Lower Trump` = lower,
                     `Upper Trump` = upper,
                     `Mean Trump`  = mean) %>%
              select(-cand)) %>%
  rename(State = state) %>%
  mutate_if(is.numeric, function(x) paste0(round(x*100), "%"))

save(state_results, file = "results/state_results")




# P-win --------------------------------------------------------------------

# Probability of winning the state
p_biden <- round(apply(em$mu, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_biden) <- state
p_biden <- data.frame(p_biden) %>%
  tibble::rownames_to_column("state")

save(p_biden, file = "results/p_biden")




# Simulate electoral college ----------------------------------------------

ec_sims <- matrix(0, nrow = dim(em$mu)[1], ncol = dim(em$mu)[3])

for(i in 1:dim(em$mu)[1]) {
  winner <- apply(em$mu[i, , ], 1, function(x) which(x == max(x)))
  for(s in 1:dim(em$mu)[2]) {
    ec_sims[i, winner[s]] <- ec_sims[i, winner[s]] + prior_results$ev[s]
  }
}


save(ec_sims, file = "results/ec_sims")

# Create data frame of results for tracking
ec_ts_today <- data_frame(
  date = exec_date,
  lower_trump = quantile(ec_sims[, 1], 0.05),
  mean_trump = mean(ec_sims[, 1]),
  upper_trump = quantile(ec_sims[, 1], 0.95),
  lower_biden = quantile(ec_sims[, 2], 0.05),
  mean_biden = mean(ec_sims[, 2]),
  upper_biden = quantile(ec_sims[, 2], 0.95)
)

# Append to tracking data
ec_ts <- read_csv("results/ec_ts.csv")

ec_ts <- ec_ts %>%
  filter(date != exec_date) %>%
  rbind(ec_ts_today)

write_csv(ec_ts, "results/ec_ts.csv")




# State simulations -------------------------------------------------------

state_simulations <- data_frame(
  value = round(c(c(em$mu[, , 1]), c(em$mu[, , 2]), c(em$mu[, , 3])), 3),
  state = rep(rep(state,  each = dim(em$mu)[1]), times = 3),
  candidate = rep(c("Trump", "Biden", "Other"), each = dim(em$mu)[1]*51)
) %>%
  group_by(state, candidate) %>%
  mutate(mean = round(mean(value), 3)) %>%
  ungroup() %>%
  arrange(state)

save(state_simulations, file = "results/state_simulations")

tmp_state <- vector("list", 51)
for(s in 1:51) {
  tmp_state[[s]] <- data_frame(
    date = exec_date,
    state = state[s],
    lower_trump = quantile(em$mu[, s, 1], 0.1),
    mean_trump  = quantile(em$mu[, s, 1], 0.5),
    upper_trump = quantile(em$mu[, s, 1], 0.9),
    lower_biden = quantile(em$mu[, s, 2], 0.1),
    mean_biden  = quantile(em$mu[, s, 2], 0.5),
    upper_biden = quantile(em$mu[, s, 2], 0.9)
  )
}

state_ts_today <- do.call(rbind, tmp_state)


# Append to state tracking data
state_ts <- read_csv("results/state_ts.csv")

state_ts <- state_ts %>%
  filter(date != exec_date) %>%
  rbind(state_ts_today)

write_csv(state_ts, "results/state_ts.csv")

