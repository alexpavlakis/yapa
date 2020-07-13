library(tidyverse)

# data
senate_raw <- read_csv("data/1976-2018-senate.csv")

senate_polls <- process_538_senate() 
gen_results <- read_csv("data/house_gen_results.csv")

# process data
election_results <- senate_raw %>%
  filter(stage == 'gen') %>%
  select(year, state, party, candidate,
         candidatevotes, totalvotes) %>%
  group_by(state, year, candidate) %>%
  summarise(party = party[candidatevotes == max(candidatevotes)][1],
            candidatevotes = sum(candidatevotes),
            totalvotes = max(totalvotes)) %>%
  mutate(party = ifelse(str_detect(party, 'democrat'), 'democrat', party),
         party = ifelse(str_detect(party, 'republic'), 'republican', party),
         party = ifelse(!party %in% c('democrat', 'republican'), 'other', party),
         candidate = ifelse(party == 'other', 'other', candidate)) %>%
  mutate(win = ifelse(candidatevotes == max(candidatevotes), 1, 0)) %>%
  group_by(state, party) %>%
  arrange(year) %>%
  mutate(inc = ifelse(
    (candidate == lag(candidate) | candidate == lag(candidate, 2))
    & lag(win) == 1, 1, 0),
    inc = ifelse(is.na(inc), 0, inc)) %>%
  ungroup() %>%
  group_by(year, state, party, win, inc) %>%
  summarise(candidatevotes = sum(candidatevotes),
            totalvotes = max(totalvotes)) %>%
  ungroup() %>%
  mutate(pct = round(candidatevotes/totalvotes, 4)) 


senate_results <- election_results %>%
  group_by(year, state, party) %>%
  summarise(win = max(win)[1], inc = max(inc)[1],
            pct = sum(pct)) %>%
  ungroup() %>%
  left_join(gen_results) %>%
  group_by(party, state) %>%
  arrange(year) %>%
  mutate(p_lean = (pct) - gen_t,
         p_lean = ifelse(is.na(p_lean), 1 - gen_t, p_lean),
         p_lean1 = lag(p_lean)) %>%
  group_by(year, state) %>%
  mutate(win = ifelse(pct == max(pct), 1, 0)) %>%
  ungroup() %>%
  arrange(year, state) 



# model data --------------------------------------------------------------
model_data <- senate_results %>%
  filter(year > min(year)) %>%
  group_by(year, state) %>%
  filter(n_distinct(party) > 1) %>%
  ungroup() %>%
  mutate(adj = gen_t + p_lean1,
         other = ifelse(party == 'other', 1, 0)) %>%
  na.omit() %>%
  select(year, party, pct, gen_t, p_lean1, adj, inc, other) 

m <- lm(pct ~ adj + inc + 0, data = model_data)
m
plot(m$model$pct, m$fitted.values, pch = 19, cex = 0.4)



senate_races <- read_csv("data/2020-senate.csv") %>%
  mutate(party = case_when(party == 'dem' ~ 'democrat',
                           party == 'rep' ~ 'republican', 
                           TRUE ~ 'other')) %>%
  group_by(state) %>%
  filter(row_number() < 4) %>%
  ungroup()


prior_data <- senate_results %>%
  group_by(state, party) %>%
  filter((year == max(year) & !state %in% c('West Virginia', 'Maine'))
         | (state %in% c('West Virginia', 'Maine') & year == 2014)) %>%
  ungroup() %>% 
  select(state, party, p_lean1 = p_lean) %>%
  right_join(
    senate_races
  ) %>%
  mutate(gb = rep(gb[c(2, 1, 3)], 34)) %>%
  mutate(adj = gb + p_lean1)

write_csv(prior_data, "data/senate_prior_data.csv")
