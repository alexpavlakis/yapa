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
polls_state <- data_frame(state) %>% 
  left_join(
    process_538() %>%
      filter(end_date <= exec_date) 
  ) %>%
  mutate(Sample = ifelse(is.na(Sample), 0, Sample),
         `Trump (R)` = ifelse(is.na(`Trump (R)`), 0, `Trump (R)`),
         `Biden (D)` = ifelse(is.na(`Biden (D)`), 0, `Biden (D)`),
         Other = ifelse(is.na(Other), 0, Other),
         days_out = ifelse(is.na(days_out), 365, days_out)) %>%
  arrange(state) 

# GE Model
polls_natl <- process_538_ge() %>%
  filter(end_date <= exec_date) %>%
  arrange(desc(end_date)) 

# Save
polls <- polls_natl %>%
  mutate(state = 'US') %>% 
  rbind(polls_state)

write_csv(polls, "data/polls.csv")

load("data/swing") 
load("data/swing_sigma")
load("data/state_sigma")
load("data/swing_ge")
load("data/swing_sigma_ge")

# Model data --------------------------------------------------------------


# Counts for each option in each state poll
y_r <- polls_state %>%
  select(`Trump (R)`, `Biden (D)`, Other) %>%
  as.matrix()

# Number of state polls
n_polls_r <- nrow(y_r)

# Numeric identifier for each state
state_id <- match(polls_state$state, unique(polls_state$state))

# Number of states
n_states <- n_distinct(polls_state$state)

# Number of candidates
n_options <- ncol(y_r)

# Days out from election (for weighting)
days_out_r <- as.numeric(polls_state$days_out)

# Counts for each GE week
y_g <- polls_natl %>%
  select(`Trump (R)`, `Biden (D)`, Other) %>%
  as.matrix()

n_polls_g <- nrow(y_g)

days_out_g <- as.numeric(polls_natl$days_out)

p_lean <- data_frame(
  rep = prior_results$rep - 0.461,
  dem = prior_results$dem - 0.482,
  other = prior_results$other - 0.057
) %>% as.matrix()


# Combine into list
model_data <- list(n_options = n_options, 
                   n_states = n_states, 
                   N = n_polls_r,
                   N_natl = n_polls_g,
                   y = y_r,
                   state_id = state_id,
                   y_natl = y_g,
                   p_lean = p_lean,
                   days_out = days_out_r,
                   days_out_natl = days_out_g,
                   mu_swing = swing,
                   sigma_swing = swing_sigma,
                   sd_swing_state = state_sigma,
                   mu_swing_ge = swing_ge,
                   sigma_swing_ge = swing_sigma_ge,
                   decay_param = 40)






# Fit model ---------------------------------------------------------------

start <- Sys.time()
m <- stan(file = "stan/yapa_model.stan", data = model_data, verbose = FALSE,
          chains = 10, iter = 5000)
print(Sys.time() - start)

em <- rstan::extract(m)



# Poll averages -----------------------------------------------------------


# National
poll_averages_natl_today <- data_frame(
  date = exec_date,
  lower_trump = quantile(em$theta_natl[, 1], 0.1),
  mean_trump  = quantile(em$theta_natl[, 1], 0.5),
  upper_trump = quantile(em$theta_natl[, 1], 0.9),
  lower_biden = quantile(em$theta_natl[, 2], 0.1),
  mean_biden  = quantile(em$theta_natl[, 2], 0.5),
  upper_biden = quantile(em$theta_natl[, 2], 0.9),
  lower_other = quantile(em$theta_natl[, 3], 0.1),
  mean_other  = quantile(em$theta_natl[, 3], 0.5),
  upper_other = quantile(em$theta_natl[, 3], 0.9)
) %>% 
  gather(metric, value, -date) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)


# Append to tracking data
poll_averages_natl <- read_csv("results/poll_averages_natl.csv")

poll_averages_natl <- poll_averages_natl %>%
  filter(date != exec_date) %>%
  rbind(poll_averages_natl_today)

write_csv(poll_averages_natl, "results/poll_averages_natl.csv")


# State
tmp_state <- vector("list", nrow(colMeans(em$theta)))
for(s in 1:length(tmp_state)) {
  tmp_state[[s]] <- data_frame(
    date = exec_date,
    state = state[s],
    lower_trump = quantile(em$theta[, s, 1], 0.1),
    mean_trump  = quantile(em$theta[, s, 1], 0.5),
    upper_trump = quantile(em$theta[, s, 1], 0.9),
    lower_biden = quantile(em$theta[, s, 2], 0.1),
    mean_biden  = quantile(em$theta[, s, 2], 0.5),
    upper_biden = quantile(em$theta[, s, 2], 0.9),
    lower_other = quantile(em$theta[, s, 3], 0.1),
    mean_other  = quantile(em$theta[, s, 3], 0.5),
    upper_other = quantile(em$theta[, s, 3], 0.9)
  )
}

poll_averages_today <-  do.call(rbind, tmp_state) %>% 
  gather(metric, value, -date, -state) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)


# Append to tracking data
poll_averages <- read_csv("results/poll_averages.csv")

poll_averages <- poll_averages %>%
  filter(date != exec_date) %>%
  rbind(poll_averages_today)

write_csv(poll_averages, "results/poll_averages.csv")





# Simulated results -------------------------------------------------------

# National
# Append results to tracker and save
qs <- apply(em$mu_natl, 2, quantile, c(0.1, 0.5, 0.9))

load("results/ge_trend")

ge_today <- t(qs[2, ]) %>%
  as_data_frame() %>%
  rename(trump = V1, biden = V2, other = V3) %>%
  mutate(day = exec_date) %>%
  gather(candidate, prop, -day) %>%
  left_join(t(qs[1, ]) %>%
              as_data_frame() %>%
              rename(trump = V1, biden = V2, other = V3) %>%
              mutate(day = exec_date) %>%
              gather(candidate, lower, -day)) %>%
  left_join(t(qs[3, ]) %>%
              as_data_frame() %>%
              rename(trump = V1, biden = V2, other = V3) %>%
              mutate(day = exec_date) %>%
              gather(candidate, upper, -day))

ge_trend <- ge_trend %>%
  filter(day != exec_date) %>%
  rbind(ge_today)

save(ge_trend, file = "results/ge_trend")

pv_sims <- em$mu_natl
save(pv_sims, file = "results/pv_sims")



# State

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
