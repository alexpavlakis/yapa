library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/process_polls.R")
#source("R/house_data.R")
prior_data <- read_csv("data/prior_data.csv")

average_miss <- c(0.0279, 0.176, -0.455)
if(!exists("exec_date")) exec_date <- Sys.Date()

generic_ballot <- process_538_gb() %>%
  filter(end_date <= exec_date, 
         end_date > '2020-01-01') 
house_races <- process_538_house() %>% 
  filter(end_date <= exec_date, 
         end_date > '2020-01-01') 

write_csv(house_races, "data/house_races.csv")

district_polls <- house_races %>%
  mutate(dem = round(dem + .4*Other), rep = round(rep + .4*Other), 
         Other = round(0.2*Other)) %>%
  right_join(prior_data %>%
               distinct(state, district)) %>%
  mutate(state_district = paste(state, district, sep = "-")) %>%
  arrange(state, district) %>%
  mutate(district_id = match(state_district, unique(state_district)),
         days_out = ifelse(is.na(days_out), 365, days_out)) 

state_district <- unique(district_polls$state_district)

gen_res <- house_results %>%
  filter(year == 2018) %>%
  distinct(party, gen_t) 

p_lean <- prior_data %>%
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

inc <- prior_data %>%
  select(-p_lean1) %>%
  spread(party, inc) %>%
  mutate(state_district = paste(state, district, sep = "-")) %>%
  arrange(state, district) %>%
  mutate(district_id = match(state_district, unique(state_district))) %>%
  select(republican, democrat, other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))



# Counts for each option in each district poll
y <- district_polls %>%
  select(rep, dem, Other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))

# Counts for each option in each GB poll
y_gb <- generic_ballot %>%
  mutate(dem = round(dem + .4*Other), rep = round(rep + .4*Other), 
         Other = round(0.2*Other)) %>%
  select(rep, dem, Other) %>%
  as.matrix() %>%
  apply(., 2, function(x) ifelse(is.na(x), 0, x))


any_data <- district_polls %>%
  group_by(state, district) %>%
  summarise(any_data = ifelse(sum(Sample, na.rm = T) > 0, 1, 0)) %>%
  ungroup() %>%
  pull(any_data)

# Number of district polls
N <- nrow(y)

# Number of GB polls
N_gb <- nrow(y_gb)

mean_gb <- colSums(y_gb)/sum(y_gb)

# Number of candidates
n_options <- ncol(y)

# Days out from election (for weighting)
days_out <- as.numeric(district_polls$days_out) 
days_out_gb <- as.numeric(generic_ballot$days_out) 

district_id <- district_polls$district_id
n_districts <- n_distinct(district_id)

gb <- colSums(y_gb*exp(-days_out_gb/40))/sum(rowSums(y_gb)*exp(-days_out_gb/40))




# Combine into list
model_data <- list(n_options = n_options, 
                   n_districts = n_districts,
                   N = N, N_gb = N_gb,
                   y = y, y_gb = y_gb,
                   days_out = days_out,
                   days_out_gb = days_out_gb,
                   p_lean = p_lean,
                   district_id = district_id,
                   tau = c(0.1, 0.1, 0.005),
                   inc = inc, any_data = any_data,
                   decay_param = 40)

start <- Sys.time()
fit_gb <- stan("stan/yapa_house.stan", data = model_data,
               chains = 10, iter = 2000)
print(Sys.time() - start)

efgb <- extract(fit_gb)


colMeans(efgb$theta_gb)

colMeans(efgb$theta)[c(155:157), ]



# Poll averages -----------------------------------------------------------


# National
poll_averages_gb_today <- data_frame(
  date = exec_date,
  lower_rep = quantile(efgb$theta_gb[, 1], 0.1),
  mean_rep  = quantile(efgb$theta_gb[, 1], 0.5),
  upper_rep = quantile(efgb$theta_gb[, 1], 0.9),
  lower_dem = quantile(efgb$theta_gb[, 2], 0.1),
  mean_dem  = quantile(efgb$theta_gb[, 2], 0.5),
  upper_dem = quantile(efgb$theta_gb[, 2], 0.9),
  lower_other = quantile(efgb$theta_gb[, 3], 0.1),
  mean_other  = quantile(efgb$theta_gb[, 3], 0.5),
  upper_other = quantile(efgb$theta_gb[, 3], 0.9)
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
tmp_district <- vector("list", nrow(colMeans(efgb$theta)))
for(s in 1:length(tmp_district)) {
  tmp_district[[s]] <- data_frame(
    date = exec_date,
    district = unique(district_polls$state_district)[s],
    lower_rep = quantile(efgb$theta[, s, 1], 0.1),
    mean_rep  = quantile(efgb$theta[, s, 1], 0.5),
    upper_rep = quantile(efgb$theta[, s, 1], 0.9),
    lower_dem = quantile(efgb$theta[, s, 2], 0.1),
    mean_dem  = quantile(efgb$theta[, s, 2], 0.5),
    upper_dem = quantile(efgb$theta[, s, 2], 0.9),
    lower_other = quantile(efgb$theta[, s, 3], 0.1),
    mean_other  = quantile(efgb$theta[, s, 3], 0.5),
    upper_other = quantile(efgb$theta[, s, 3], 0.9)
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
means_rep <- apply(efgb$theta, 2, function(x) mean(x[, 1]))
quantiles_rep <- apply(efgb$theta, 2, function(x) quantile(x[, 1], c(0.1, 0.9)))

means_dem <- apply(efgb$theta, 2, function(x) mean(x[, 2]))
quantiles_dem <- apply(efgb$theta, 2, function(x) quantile(x[, 2], c(0.1, 0.9)))

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
save(results_dem, file = "results/theta_dem")
save(results_rep, file = "results/theta_rep")

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
p_dem <- round(apply(efgb$theta, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_dem) <- state_district
p_dem <- data.frame(p_dem) %>%
  tibble::rownames_to_column("district") %>%
  mutate(state = str_split(district, '-')[[1]][1],
         no = str_split(district, '-')[[1]][2]) %>%
  arrange(state, no)

save(p_dem, file = "results/district_p_dem")




# Simulate electoral college ----------------------------------------------

res_sims <- matrix(0, nrow = dim(efgb$theta)[1], ncol = dim(efgb$theta)[3])

for(i in 1:dim(efgb$theta)[1]) {
  winner <- apply(efgb$theta[i, , ], 1, function(x) which(x == max(x)))
  for(s in 1:dim(efgb$theta)[2]) {
    res_sims[i, winner[s]] <- res_sims[i, winner[s]] + 1
  }
}


save(res_sims, file = "results/district_res_sims")

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

district_simulations <- data_frame(
  value = round(c(c(efgb$theta[, , 1]), c(efgb$theta[, , 2]), c(efgb$theta[, , 3])), 3),
  district = rep(rep(state_district, each = dim(efgb$theta)[1]), times = 3),
  party = rep(c("rep", "dem", "other"), each = dim(efgb$theta)[1]*n_districts)
) %>%
  group_by(district, party) %>%
  mutate(mean = round(mean(value), 3)) %>%
  ungroup() %>%
  arrange(district)

save(district_simulations, file = "results/district_simulations")



tmp_district <- vector("list", n_distinct(state_district))
for(d in 1:length(tmp_district)) {
  tmp_district[[d]] <- data_frame(
    date = exec_date,
    district = state_district[d],
    lower_rep = quantile(efgb$theta[, d, 1], 0.1),
    mean_rep  = quantile(efgb$theta[, d, 1], 0.5),
    upper_rep = quantile(efgb$theta[, d, 1], 0.9),
    lower_dem = quantile(efgb$theta[, d, 2], 0.1),
    mean_dem  = quantile(efgb$theta[, d, 2], 0.5),
    upper_dem = quantile(efgb$theta[, d, 2], 0.9)
  )
}

district_ts_today <- do.call(rbind, tmp_district)


# Append to state tracking data
district_ts <- read_csv("results/district_ts.csv")

district_ts <- district_ts %>%
  filter(date != exec_date) %>%
  rbind(district_ts_today)

write_csv(district_ts, "results/district_ts.csv")











