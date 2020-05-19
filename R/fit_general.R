library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/process_polls.R")


# Model data --------------------------------------------------------------

# GE Model
polls_ge <- process_538_ge() %>%
  filter(end_date <= exec_date) 

# Counts for each GE week
y_ge <- polls_ge %>%
  select(`Trump (R)`, `Biden (D)`, Other) %>%
  as.matrix()


# Total weeks
N_ge <- nrow(y_ge)

# Number of candidates
n_options <- ncol(y_ge)

# Days out from election
days_out_ge <- as.numeric(polls_ge$days_out) 

# load historical bias data
load("data/swing_ge")
load("data/swing_sigma_ge")

# Combine into list
model_data_ge <- list(N_ge = N_ge,
                      y_ge = y_ge,
                      n_options = n_options,
                      days_out_ge = days_out_ge,
                      mu_swing = swing_ge,
                      sigma_swing = swing_sigma_ge,
                      decay_param = 40)


# Model -------------------------------------------------------------------

fit_ge <- stan("stan/yapa_general.stan", data = model_data_ge,
               chains = 10, iter = 5000)
efge <- extract(fit_ge)


# Results -----------------------------------------------------------------

# Append results to tracker and save
qs <- apply(efge$mu, 2, quantile, c(0.1, 0.5, 0.9))

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

pv_sims <- efge$mu
save(pv_sims, file = "results/pv_sims")
