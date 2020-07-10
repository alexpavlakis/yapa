library(tidyverse)

# data
house_raw <- read_csv("data/1976-2018-house.csv")
generic_raw <- read_csv("data/generic_polllist.csv")
cand_2020 <- read_csv("data/2020_house_candidates.csv")

house_polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/house_polls.csv")


generic_polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/generic_ballot_polls.csv")


# process data
election_results <- house_raw %>%
  filter(stage == 'gen') %>%
  select(year, state, district, party, candidate,
         candidatevotes, totalvotes) %>%
  group_by(state, year, district, candidate) %>%
  summarise(party = party[candidatevotes == max(candidatevotes)],
            candidatevotes = sum(candidatevotes),
            totalvotes = max(totalvotes)) %>%
  mutate(party = ifelse(str_detect(party, 'democrat'), 'democrat', party),
         party = ifelse(str_detect(party, 'republic'), 'republican', party),
         party = ifelse(!party %in% c('democrat', 'republican'), 'other', party),
         candidate = ifelse(party == 'other', 'other', candidate)) %>%
  group_by(year, state, district, party, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes),
            totalvotes = max(totalvotes)) %>%
  ungroup() %>%
  mutate(pct = round(candidatevotes/totalvotes, 4)) 

gen_results <- election_results %>%
  left_join(
    election_results %>% 
      group_by(year, state, district) %>% 
      summarise(all_votes = max(totalvotes)) %>%
      ungroup() %>% 
      group_by(year) %>% 
      summarise(total_votes_year = sum(all_votes)) %>%
      ungroup()
  ) %>%
  group_by(year, party) %>%
  summarise(gen_t = sum(candidatevotes)/max(total_votes_year)) %>%
  ungroup() %>%
  group_by(party) %>%
  arrange(year) %>%
  mutate(gen_t1 = lag(gen_t)) %>%
  ungroup()

house_results <- election_results %>%
  left_join(gen_results) %>%
  group_by(party, district, state) %>%
  arrange(year) %>%
  mutate(p_lean = (pct) - gen_t,
         p_lean = ifelse(is.na(p_lean), 1 - gen_t, p_lean),
         p_lean1 = lag(p_lean)) %>%
  group_by(year, state, district) %>%
  mutate(win = ifelse(pct == max(pct), 1, 0)) %>%
  ungroup() %>%
  group_by(state, district, party) %>%
  arrange(year) %>%
  mutate(inc = ifelse(candidate == lag(candidate) & lag(win) == 1, 1, 0)) %>%
  ungroup() 




# model data --------------------------------------------------------------
model_data <- house_results %>%
  group_by(year, state, district) %>%
  filter(n_distinct(party) > 1) %>%
  ungroup() %>%
  mutate(adj = gen_t + p_lean1) %>%
  select(year, party, pct, gen_t, p_lean1, adj, inc) 

m <- lm(pct ~ adj + inc + 0, data = model_data)
m
plot(m$model$pct, m$fitted.values, pch = 19, cex = 0.4)

# Pred data
incumbents <- cand_2020 %>%
  filter(incumbent_challenge == 'I') %>%
  mutate(candidate = tolower(name),
         party = case_when(
           party == 'DEM' ~ 'democrat',
           party == 'REP' ~ 'republican',
           TRUE ~ 'other'
         ),
         abb = state,
         district = district_number) %>%
  left_join(
    data_frame(state_name = state.name,
               abb = state.abb)
  ) %>%
  select(state = state_name, district, candidate, party) %>%
  left_join(house_results %>%
              filter(year == 2018, win == 1) %>%
              select(inc_name  = candidate, state, district)
  ) %>%
  select(state, district, party) %>%
  mutate(inc = 1)

prior_data <- house_results %>%
  select(-inc) %>%
  filter(year == 2018) %>%
  left_join(incumbents) %>%
  mutate(inc = ifelse(is.na(inc), 0, inc)) %>%
  distinct(state, district, party, inc, p_lean1 = p_lean) %>%
  group_by(state, district, party) %>%
  arrange(-p_lean1) %>%
  filter(row_number() == 1) %>%
  ungroup()

write_csv(prior_data, "data/prior_data.csv")
