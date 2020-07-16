
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/process_polls.R")
#source("R/house_data.R")
prior_data <- read_csv("data/prior_data.csv")
gen_res <- read_csv("data/gen_res.csv")

if(!exists("exec_date")) exec_date <- Sys.Date()

generic_ballot <- process_538_gb() %>%
  filter(end_date <= exec_date, 
         end_date > '2020-01-01') 
house_races <- process_538_house() %>% 
  filter(end_date <= exec_date, 
         end_date > '2020-01-01') 

write_csv(house_races, "data/house_races.csv")

district_polls <- house_races %>%
  #mutate(dem = round(dem + .4*Other), rep = round(rep + .4*Other), 
  #       Other = round(0.2*Other)) %>%
  right_join(prior_data %>%
               distinct(state, district)) %>%
  mutate(state_district = paste(state, district, sep = "-")) %>%
  arrange(state, district) %>%
  mutate(district_id = match(state_district, unique(state_district)),
         days_out = ifelse(is.na(days_out), 365, days_out)) 

state_district <- unique(district_polls$state_district)


lean <- prior_data %>%
  select(-inc) %>%
  spread(party, p_lean1) %>%
  mutate(state_district = paste(state, district, sep = "-")) %>%
  arrange(state, district) %>%
  mutate(district_id = match(state_district, unique(state_district))) %>%
  select(republican, democrat, other) %>%
  mutate(democrat = ifelse(is.na(democrat), -gen_res$gen_t[gen_res$party == 'democrat'], democrat),
         republican = ifelse(is.na(republican), -gen_res$gen_t[gen_res$party == 'republican'], republican),
         other = ifelse(is.na(other), -gen_res$gen_t[gen_res$party == 'other'], other)) %>%
  as.matrix() 


# Counts for each option in each district poll
y_r <- district_polls %>%
  select(rep, dem, Other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))

# Counts for each option in each GB poll
y_g <- generic_ballot %>%
  #mutate(dem = round(dem + .4*Other), rep = round(rep + .4*Other), 
  #       Other = round(0.2*Other)) %>%
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

region_id <- district_polls$district_id
n_regions <- n_distinct(region_id)

load('data/house_error')
load('data/house_corr')

region_error <- rep(0.01, n_regions)


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
                   non_samp_error = house_error,
                   non_samp_corr = house_corr,
                   region_error = region_error,
                   decay_param = 40,
                   prior_g = c(0.45, 0.53, 0.02),
                   prior_sd_g = c(0.01, 0.01, 0.01))

#start <- Sys.time()
fit_house <- stan("stan/yapa.stan", data = model_data,
                  chains = 10, iter = 2000)
#print(Sys.time() - start)





# results -----------------------------------------------------------------


ef_house <- extract(fit_house)


colMeans(ef_house$res_g)



# Poll averages -----------------------------------------------------------


# National
poll_averages_gb_today <- data_frame(
  date = exec_date,
  lower_rep = quantile(ef_house$res_g[, 1], 0.1),
  mean_rep  = quantile(ef_house$res_g[, 1], 0.5),
  upper_rep = quantile(ef_house$res_g[, 1], 0.9),
  lower_dem = quantile(ef_house$res_g[, 2], 0.1),
  mean_dem  = quantile(ef_house$res_g[, 2], 0.5),
  upper_dem = quantile(ef_house$res_g[, 2], 0.9),
  lower_other = quantile(ef_house$res_g[, 3], 0.1),
  mean_other  = quantile(ef_house$res_g[, 3], 0.5),
  upper_other = quantile(ef_house$res_g[, 3], 0.9)
) %>% 
  gather(metric, value, -date) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)


# Append to tracking data
poll_averages_gb <- read_csv("results/poll_averages_gb.csv")

poll_averages_gb <- poll_averages_gb %>%
  filter(date != exec_date) %>%
  rbind(poll_averages_gb_today)

write_csv(poll_averages_gb, "results/poll_averages_gb.csv")


# District
tmp_district <- vector("list", nrow(colMeans(ef_house$res_r)))
for(s in 1:length(tmp_district)) {
  tmp_district[[s]] <- data_frame(
    date = exec_date,
    district = unique(district_polls$state_district)[s],
    lower_rep = quantile(ef_house$res_r[, s, 1], 0.1),
    mean_rep  = quantile(ef_house$res_r[, s, 1], 0.5),
    upper_rep = quantile(ef_house$res_r[, s, 1], 0.9),
    lower_dem = quantile(ef_house$res_r[, s, 2], 0.1),
    mean_dem  = quantile(ef_house$res_r[, s, 2], 0.5),
    upper_dem = quantile(ef_house$res_r[, s, 2], 0.9),
    lower_other = quantile(ef_house$res_r[, s, 3], 0.1),
    mean_other  = quantile(ef_house$res_r[, s, 3], 0.5),
    upper_other = quantile(ef_house$res_r[, s, 3], 0.9)
  )
}

district_averages_today <-  do.call(rbind, tmp_district) %>% 
  gather(metric, value, -date, -district) %>%
  mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
         metric = sapply(strsplit(metric, "_"), head, 1)) %>%
  spread(metric, value)


# Append to tracking data
district_averages <- read_csv("results/district_averages.csv")

district_averages <- district_averages %>%
  filter(date != exec_date) %>%
  rbind(district_averages_today)

write_csv(district_averages, "results/district_averages.csv")






# Simulated results -------------------------------------------------------



# State

# Results
means_rep <- apply(ef_house$res_r, 2, function(x) mean(x[, 1]))
quantiles_rep <- apply(ef_house$res_r, 2, function(x) quantile(x[, 1], c(0.1, 0.9)))

means_dem <- apply(ef_house$res_r, 2, function(x) mean(x[, 2]))
quantiles_dem <- apply(ef_house$res_r, 2, function(x) quantile(x[, 2], c(0.1, 0.9)))

results_dem <- data_frame(
  district = state_district,
  lower = quantiles_dem[1, ],
  mean  = means_dem,
  upper = quantiles_dem[2, ],
  cand  = 'dem')

results_rep <- data_frame(
  district = state_district,
  lower = quantiles_rep[1, ],
  mean  = means_rep,
  upper = quantiles_rep[2, ],
  cand  = 'rep')

# Save
save(results_dem, file = "results/res_r_dem")
save(results_rep, file = "results/res_r_rep")

# Formatted Table
district_results <- results_dem %>%
  rename(`Lower Dem` = lower,
         `Upper Dem` = upper,
         `Mean Dem`  = mean) %>%
  select(-cand) %>%
  left_join(results_rep %>%
              rename(`Lower Rep` = lower,
                     `Upper Rep` = upper,
                     `Mean Rep`  = mean) %>%
              select(-cand)) %>%
  rename(District = district) %>%
  mutate_if(is.numeric, function(x) paste0(round(x*100), "%"))

save(district_results, file = "results/district_results")


# P-win --------------------------------------------------------------------

# Probability of winning the district
p_dem <- round(apply(ef_house$res_r, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_dem) <- state_district
p_dem <- data.frame(p_dem) %>%
  tibble::rownames_to_column("district") %>%
  mutate(state = str_split(district, '-')[[1]][1],
         no = str_split(district, '-')[[1]][2]) %>%
  arrange(state, no)

save(p_dem, file = "results/district_p_dem")




# Simulate electoral college ----------------------------------------------

res_sims <- matrix(0, nrow = dim(ef_house$res_r)[1], ncol = dim(ef_house$res_r)[3])

for(i in 1:dim(ef_house$res_r)[1]) {
  winner <- apply(ef_house$res_r[i, , ], 1, function(x) which(x == max(x)))
  for(s in 1:dim(ef_house$res_r)[2]) {
    res_sims[i, winner[s]] <- res_sims[i, winner[s]] + 1
  }
}


save(res_sims, file = "results/senate_res_sims")

# Create data frame of results for tracking
house_ts_today <- data_frame(
  date = exec_date,
  lower_rep = quantile(res_sims[, 1], 0.05),
  mean_rep = mean(res_sims[, 1]),
  upper_rep = quantile(res_sims[, 1], 0.95),
  lower_dem = quantile(res_sims[, 2], 0.05),
  mean_dem = mean(res_sims[, 2]),
  upper_dem = quantile(res_sims[, 2], 0.95)
)

# Append to tracking data
house_ts <- read_csv("results/house_ts.csv")

house_ts <- house_ts %>%
  filter(date != exec_date) %>%
  rbind(house_ts_today)

write_csv(house_ts, "results/house_ts.csv")




# State simulations -------------------------------------------------------

house_simulations <- data_frame(
  value = round(c(c(ef_house$res_r[, , 1]), c(ef_house$res_r[, , 2]), c(ef_house$res_r[, , 3])), 3),
  state = rep(rep(state_district, each = dim(ef_house$res_r)[1]), times = 3),
  party = rep(c("rep", "dem", "other"), each = dim(ef_house$res_r)[1]*n_regions)
) %>%
  group_by(state, party) %>%
  mutate(mean = round(mean(value), 3)) %>%
  ungroup() %>%
  arrange(state)

save(house_simulations, file = "results/house_simulations")



tmp_district <- vector("list", n_distinct(state_district))
for(d in 1:length(tmp_district)) {
  tmp_district[[d]] <- data_frame(
    date = exec_date,
    district = state_district[d],
    lower_rep = quantile(ef_house$res_r[, d, 1], 0.1),
    mean_rep  = quantile(ef_house$res_r[, d, 1], 0.5),
    upper_rep = quantile(ef_house$res_r[, d, 1], 0.9),
    lower_dem = quantile(ef_house$res_r[, d, 2], 0.1),
    mean_dem  = quantile(ef_house$res_r[, d, 2], 0.5),
    upper_dem = quantile(ef_house$res_r[, d, 2], 0.9)
  )
}

district_ts_today <- do.call(rbind, tmp_district)


# Append to state tracking data
district_ts <- read_csv("results/district_ts.csv")

district_ts <- district_ts %>%
  filter(date != exec_date) %>%
  rbind(district_ts_today)

write_csv(district_ts, "results/district_ts.csv")

