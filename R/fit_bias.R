library(tidyverse)
library(rstan)

date_diff <- function(t1, t2) as.numeric(difftime(t1, t2, "day"))

# Data --------------------------------------------------------------------


# Download historical poll averages from 538
pd <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv")

# Process
ge_polls <- pd %>%
  filter(str_detect(type_simple, 'Pres-G')) %>%
  mutate(polldate = as.Date(polldate, '%m/%d/%Y'),
         electiondate = as.Date(electiondate, '%m/%d/%Y'),
         days_out = date_diff(electiondate, polldate)) %>%
  mutate(cand1_actual = cand1_actual/100,
         cand2_actual = cand2_actual/100,
         cand3_actual = 1 - cand1_actual - cand2_actual,
         cand1_pct = cand1_pct/100,
         cand2_pct = cand2_pct/100,
         cand3_pct = 1 - cand1_pct - cand2_pct) %>%
  mutate(cand1_error = cand1_actual - cand1_pct,
         cand2_error = cand2_actual - cand2_pct,
         cand3_error = cand3_actual - cand3_pct) %>%
  select(year, location, samplesize, days_out, contains('cand')) 

# Helper function for mean of binomial
get_est <- function(y, n , wt) {
  (sum(y*wt) + 1)/(sum(y*wt) + 1 + sum(n*wt) - sum(y*wt) + 1)
}

# Calculate historical bias in state poll averages
bias_data <- ge_polls %>%
  mutate(cand1_count = cand1_pct*samplesize,
         cand2_count = cand2_pct*samplesize,
         cand3_count = cand3_pct*samplesize,
         wt = exp(-days_out/40)) %>%
  group_by(year, location) %>%
  summarise(cand1_est = get_est(cand1_count, samplesize, wt),
            cand1_actual = unique(cand1_actual),
            cand2_est = get_est(cand2_count, samplesize, wt),
            cand2_actual = unique(cand2_actual),
            cand3_est = get_est(cand3_count, samplesize, wt),
            cand3_actual = unique(cand3_actual),
            n = n()) %>%
  mutate(cand1_bias = (cand1_actual - cand1_est),
         cand2_bias = (cand2_actual - cand2_est),
         cand3_bias = (cand3_actual - cand3_est)) %>%
  ungroup() %>%
  left_join(
    data_frame(state = state.name, location = state.abb)
  ) %>%
  arrange(state)

# Process into matrices
get_bias_mat <- function(bias_data, yr) {
  df <- bias_data %>%
    filter(year == yr) %>%
    select(state, rep_bias = cand2_bias, dem_bias = cand1_bias, other_bias = cand3_bias) %>%
    right_join(data_frame(state = unique(bias_data$state))) %>%
    select(-state) %>%
    as.matrix()
  df <- apply(df, 2, function(x) ifelse(is.na(x), 0, x))
  names(df) <- NULL
  df
}


# States ------------------------------------------------------------------


d <- lapply(seq(2000, 2016, 4), get_bias_mat, bias_data = bias_data %>% filter(!location %in% c('US', 'M1', 'M2', 'N1', 'N2')))

mdata <- list(
  n_states = nrow(d[[1]]),
  n_options = ncol(d[[1]]),
  d1 = d[[1]],
  d2 = d[[2]],
  d3 = d[[3]],
  d4 = d[[4]],
  d5 = d[[5]]
)

# Model code to estimate distribution of errors in state and ge polls


fb <- stan("stan/bias.stan", data = mdata, chains = 3, iter = 1000)

fb

efb <- extract(fb)

swing <- colMeans(efb$mu)
state_sigma <- round(colMeans(efb$sigma), 3)
swing_sigma <- colMeans(efb$Sigma)

sims <- MASS::mvrnorm(n = 25000, mu = swing, Sigma = swing_sigma)
apply(sims, 2, mean)
apply(sims, 2, sd)

# load historical bias data
save(swing, file = "data/swing")
save(swing_sigma, file = "data/swing_sigma")
save(state_sigma, file = 'data/state_sigma')



# National ----------------------------------------------------------------


d_ge <- lapply(seq(2000, 2016, 4), get_bias_mat, bias_data = bias_data %>% filter(location == 'US'))


mdata <- list(
  n_states = 1,
  n_options = length(d_ge[[1]]),
  d1 = matrix(d_ge[[1]], nrow = 1),
  d2 = matrix(d_ge[[2]], nrow = 1),
  d3 = matrix(d_ge[[3]], nrow = 1),
  d4 = matrix(d_ge[[4]], nrow = 1),
  d5 = matrix(d_ge[[5]], nrow = 1)
)

fb_ge <- stan("stan/bias.stan", data = mdata, chains = 3, iter = 1000)

fb_ge

e_fb_ge <- extract(fb_ge)

swing_ge <- colMeans(e_fb_ge$mu)
swing_sigma_ge <- colMeans(e_fb_ge$Sigma)

sims <- MASS::mvrnorm(n = 25000, mu = swing_ge, Sigma = swing_sigma_ge)
apply(sims, 2, mean)
apply(sims, 2, sd)

# load historical bias data
save(swing_ge, file = "data/swing_ge")
save(swing_sigma_ge, file = "data/swing_sigma_ge")

