
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/process_polls.R")
prior_data <- read_csv("data/senate_prior_data.csv")
gen_res <- read_csv("data/gen_res.csv")

if(!exists("exec_date")) exec_date <- Sys.Date()

generic_ballot <- process_538_gb() %>%
  filter(end_date <= exec_date, 
         end_date > '2020-01-01') 
senate_polls <- process_538_senate() %>%
  filter(end_date < exec_date, 
         end_date > '2020-01-01') %>%
  distinct()

write_csv(senate_polls, "data/senate_polls.csv")

district_polls <- senate_polls %>%
  right_join(prior_data %>%
               mutate(state = sapply(strsplit(state, '-'), head, 1)) %>%
               select(state)) %>%
  arrange(state) %>%
  mutate(state_id = match(state, unique(state)),
         days_out = ifelse(is.na(days_out), 365, days_out)) %>%
  arrange(state)

state <- unique(prior_data$state)


lean <- prior_data %>%
  select(-inc, -candidate, -last_pct, -gb, -adj) %>%
  spread(party, p_lean1) %>%
  arrange(state) %>%
  mutate(state_id = match(state, unique(state))) %>%
  select(republican, democrat, other) %>%
  mutate(democrat = ifelse(is.na(democrat), -gen_res$gen_t[gen_res$party == 'democrat'], democrat),
         republican = ifelse(is.na(republican), -gen_res$gen_t[gen_res$party == 'republican'], republican),
         other = ifelse(is.na(other), -gen_res$gen_t[gen_res$party == 'other'], other)) %>%
  as.matrix() 

# Adjust lean for absense of candidates
# No Dem challenger in Arkansas
lean[which(state == 'Arkansas'), 3] <- lean[which(state == 'Arkansas'), 3]  - lean[which(state == 'Arkansas'), 2]
lean[which(state == 'Arkansas'), 2] <- -1


# Counts for each option in each district poll
y_r <- district_polls %>%
  select(rep, dem, Other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))

# Counts for each option in each GB poll
y_g <- generic_ballot %>%
  select(rep, dem, Other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))

# Number of district polls
n_polls_r <- nrow(y_r)

# Number of GB polls
n_polls_g <- nrow(y_g)

# Number of candidates
n_options <- ncol(y_g)

# Days out from election (for weighting)
days_out_r <- as.numeric(district_polls$days_out) 
days_out_g <- as.numeric(generic_ballot$days_out) 

region_id <- match(district_polls$state, unique(district_polls$state))
n_regions <- n_distinct(region_id)

load('data/senate_error')
load('data/senate_corr')

region_error <- rep(0.02, n_regions)


# Combine into list
model_data <- list(n_options = n_options, 
                   n_regions = n_regions,
                   n_options = n_options,
                   n_polls_r = n_polls_r,
                   n_polls_g = n_polls_g,
                   y_g = y_g, y_r = y_r,
                   region_id = region_id,
                   lean = lean,
                   days_out_g = days_out_g,
                   days_out_r = days_out_r,
                   non_samp_error = senate_error,
                   non_samp_corr = senate_corr,
                   region_error = region_error,
                   decay_param = 40,
                   prior_g = c(0.45, 0.53, 0.02),
                   prior_sd_g = c(0.01, 0.01, 0.01))

#start <- Sys.time()
fit_senate <- stan("stan/yapa.stan", data = model_data,
                   chains = 10, iter = 5000)
#print(Sys.time() - start)





# results -----------------------------------------------------------------


ef_senate <- extract(fit_senate)


colMeans(ef_senate$res_g)

# Poll averages -----------------------------------------------------------


# National
poll_averages_gb_today <- data_frame(
  date = exec_date,
  lower_rep = quantile(ef_senate$res_g[, 1], 0.1),
  mean_rep  = quantile(ef_senate$res_g[, 1], 0.5),
  upper_rep = quantile(ef_senate$res_g[, 1], 0.9),
  lower_dem = quantile(ef_senate$res_g[, 2], 0.1),
  mean_dem  = quantile(ef_senate$res_g[, 2], 0.5),
  upper_dem = quantile(ef_senate$res_g[, 2], 0.9),
  lower_other = quantile(ef_senate$res_g[, 3], 0.1),
  mean_other  = quantile(ef_senate$res_g[, 3], 0.5),
  upper_other = quantile(ef_senate$res_g[, 3], 0.9)
) %>% 
  gather(metric, value, -date) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)



# District
tmp_district <- vector("list", nrow(colMeans(ef_senate$res_r)))
for(s in 1:length(tmp_district)) {
  tmp_district[[s]] <- data_frame(
    date = exec_date,
    state = unique(prior_data$state)[s],
    lower_rep = quantile(ef_senate$res_r[, s, 1], 0.1),
    mean_rep  = quantile(ef_senate$res_r[, s, 1], 0.5),
    upper_rep = quantile(ef_senate$res_r[, s, 1], 0.9),
    lower_dem = quantile(ef_senate$res_r[, s, 2], 0.1),
    mean_dem  = quantile(ef_senate$res_r[, s, 2], 0.5),
    upper_dem = quantile(ef_senate$res_r[, s, 2], 0.9),
    lower_other = quantile(ef_senate$res_r[, s, 3], 0.1),
    mean_other  = quantile(ef_senate$res_r[, s, 3], 0.5),
    upper_other = quantile(ef_senate$res_r[, s, 3], 0.9)
  )
}

senate_averages_today <-  do.call(rbind, tmp_district) %>% 
  gather(metric, value, -date, -state) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)


# Append to tracking data
senate_averages <- read_csv("results/senate_averages.csv")

senate_averages <- senate_averages %>%
  filter(date != exec_date) %>%
  rbind(senate_averages_today)

write_csv(senate_averages, "results/senate_averages.csv")





# Simulated results -------------------------------------------------------



# State

# Results
means_rep <- apply(ef_senate$res_r, 2, function(x) mean(x[, 1]))
quantiles_rep <- apply(ef_senate$res_r, 2, function(x) quantile(x[, 1], c(0.1, 0.9)))

means_dem <- apply(ef_senate$res_r, 2, function(x) mean(x[, 2]))
quantiles_dem <- apply(ef_senate$res_r, 2, function(x) quantile(x[, 2], c(0.1, 0.9)))

results_dem <- data_frame(
  state = state,
  lower = quantiles_dem[1, ],
  mean  = means_dem,
  upper = quantiles_dem[2, ],
  cand  = 'dem')

results_rep <- data_frame(
  state = state,
  lower = quantiles_rep[1, ],
  mean  = means_rep,
  upper = quantiles_rep[2, ],
  cand  = 'rep')

# Formatted Table
senate_state_results <- results_dem %>%
  rename(`Lower Dem` = lower,
         `Upper Dem` = upper,
         `Mean Dem`  = mean) %>%
  select(-cand) %>%
  left_join(results_rep %>%
              rename(`Lower Rep` = lower,
                     `Upper Rep` = upper,
                     `Mean Rep`  = mean) %>%
              select(-cand)) %>%
  rename(State = state) %>%
  mutate_if(is.numeric, function(x) paste0(round(x*100), "%"))

save(senate_state_results, file = "results/senate_state_results")


# P-win --------------------------------------------------------------------

# Probability of winning the district
p_dem <- round(apply(ef_senate$res_r, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_dem) <- unique(prior_data$state)
p_dem <- data.frame(p_dem) %>%
  tibble::rownames_to_column("state") %>%
  arrange(state)

save(p_dem, file = "results/senate_p_dem")




# Simulate electoral college ----------------------------------------------

res_sims <- matrix(0, nrow = dim(ef_senate$res_r)[1], ncol = dim(ef_senate$res_r)[3])

for(i in 1:dim(ef_senate$res_r)[1]) {
  winner <- apply(ef_senate$res_r[i, , ], 1, function(x) which(x == max(x)))
  for(s in 1:dim(ef_senate$res_r)[2]) {
    res_sims[i, winner[s]] <- res_sims[i, winner[s]] + 1
  }
}


save(res_sims, file = "results/senate_res_sims")

# Create data frame of results for tracking
senate_ts_today <- data_frame(
  date = exec_date,
  lower_rep = quantile(res_sims[, 1], 0.05),
  mean_rep = mean(res_sims[, 1]),
  upper_rep = quantile(res_sims[, 1], 0.95),
  lower_dem = quantile(res_sims[, 2], 0.05),
  mean_dem = mean(res_sims[, 2]),
  upper_dem = quantile(res_sims[, 2], 0.95)
)

# Append to tracking data
senate_ts <- read_csv("results/senate_ts.csv")

senate_ts <- senate_ts %>%
  filter(date != exec_date) %>%
  rbind(senate_ts_today)

write_csv(senate_ts, "results/senate_ts.csv")




# State simulations -------------------------------------------------------

senate_simulations <- data_frame(
  value = round(c(c(ef_senate$res_r[, , 1]), c(ef_senate$res_r[, , 2]), c(ef_senate$res_r[, , 3])), 3),
  state = rep(rep(unique(prior_data$state), each = dim(ef_senate$res_r)[1]), times = 3),
  party = rep(c("rep", "dem", "other"), each = dim(ef_senate$res_r)[1]*n_regions)
) %>%
  group_by(state, party) %>%
  mutate(mean = round(mean(value), 3)) %>%
  ungroup() %>%
  arrange(state)

save(senate_simulations, file = "results/senate_simulations")



tmp_district <- vector("list", n_distinct(unique(prior_data$state)))
for(d in 1:length(tmp_district)) {
  tmp_district[[d]] <- data_frame(
    date = exec_date,
    state = unique(prior_data$state)[d],
    lower_rep = quantile(ef_senate$res_r[, d, 1], 0.1),
    mean_rep  = quantile(ef_senate$res_r[, d, 1], 0.5),
    upper_rep = quantile(ef_senate$res_r[, d, 1], 0.9),
    lower_dem = quantile(ef_senate$res_r[, d, 2], 0.1),
    mean_dem  = quantile(ef_senate$res_r[, d, 2], 0.5),
    upper_dem = quantile(ef_senate$res_r[, d, 2], 0.9)
  )
}

state_ts_today <- do.call(rbind, tmp_district)

# Drop dem estimate for Ark since no dem is running (dem lean was re-assigned to other above)
state_ts_today[state_ts_today$state == 'Arkansas', 6:8] <- -1

# Append to state tracking data
state_ts <- read_csv("results/senate_state_ts.csv")

state_ts <- state_ts %>%
  filter(date != exec_date) %>%
  rbind(state_ts_today)

write_csv(state_ts, "results/senate_state_ts.csv")


