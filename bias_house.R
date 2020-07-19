library(tidyverse)
library(rstan)

date_diff <- function(t1, t2) as.numeric(difftime(t1, t2, "day"))

# Data --------------------------------------------------------------------


# Download historical poll averages from 538
pd <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv")

# Process
house_polls <- pd %>%
  filter(str_detect(type_simple, 'House-G')) %>%
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
bias_data <- house_polls %>%
  mutate(cand1_count = cand1_pct*samplesize,
         cand2_count = cand2_pct*samplesize,
         cand3_count = cand3_pct*samplesize,
         wt = exp(-days_out/40)) %>%
  group_by(year, location) %>%
  summarise(cand1_est = get_est(cand1_count, samplesize, wt),
            cand1_actual = unique(cand1_actual)[1],
            cand2_est = get_est(cand2_count, samplesize, wt),
            cand2_actual = unique(cand2_actual)[1],
            cand3_est = get_est(cand3_count, samplesize, wt),
            cand3_actual = unique(cand3_actual)[1],
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


d <- lapply(seq(1998, 2018, 2), get_bias_mat, bias_data = bias_data) %>%
  do.call(rbind, .)


model_data <- list(
  polls = d,
  n_options = ncol(d),
  n_polls = nrow(d)
)

fit_error <- stan("stan/fit_error.stan", data = model_data,
                  chains = 3, iter = 1000)



efe <- extract(fit_error)

house_error <- colMeans(efe$mu)
house_corr <- colMeans(efe$Sigma)

sims <- MASS::mvrnorm(n = 25000, mu = sim_error, Sigma = sim_corr)
colMeans(sims)
plot(sims[, 2], sims[, 3])

save(house_error, file = 'data/house_error')
save(house_corr, file = 'data/house_corr')
