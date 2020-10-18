library(rvest)
library(tidyverse)

# function to calculate numeric difference in two dates in days.
date_diff <- function(t1, t2) {
  round(
    as.numeric(
      difftime(t1, t2, unit = 'days')
    )
  )
}

# Function to process state-level polls from RCP for the 2020 presidential election.
# Parameters:
## site: character string to web address of rcp site;
## election day: character string of election day, used for weighting polls;
## n: how many recent polls should we look at?
process_rcp <- function(site, election_day = "2020-11-03", n = Inf) {
  
  # If site is NULL, assume there is no data
  if(is.null(site)) {
    polls <- data_frame(
      Sample = 0, `Biden (D)` = 0, 
      `Trump (R)` = 0, Other = 0, days_out = 365
    )
  } else {
    
    # Read html from site
    poll_tbl <- site %>%
      read_html() %>%
      html_table() 
    
    # Process data: format sample and date; drop unnecessary rows and columns; weight.
    raw_data <- poll_tbl %>%
      .[[length(poll_tbl)]] %>%
      filter(!Poll %in% c("RCP Average", "Final Results")) %>%
      mutate(Sample = as.numeric(gsub("([0-9]+).*$", "\\1", Sample))) %>%
      mutate(end_date = sapply(strsplit(Date, " - "), tail, 1),
             end_date = as.Date(paste0(end_date, "/2020"), "%m/%d/%Y"),
             end_date = if_else(end_date >= Sys.Date(), as.Date(gsub("2020", "2019", end_date)),
                                end_date)) %>%
      mutate_if(is.character, function(x) gsub("--", "0", x)) %>%
      mutate(days_out = date_diff(election_day, end_date)) %>%
      select(-Poll, -Date, -Spread, -end_date) %>%
      mutate_if(is.character, as.numeric)  %>%
      na.omit() %>%
      filter(Sample < Inf) %>%
      head(n = n)
    
    # Drop MoE column if exists (not in all for some reason)
    if(any(names(raw_data) == "MoE")) raw_data <- select(raw_data, -MoE)
    
    # Process raw data into weighted counts.  Account for "other"
    polls <- raw_data 
    for(n in names(polls)[!names(polls) %in% c("Sample", "days_out")]) {
      polls[, n] <- round(polls[, 1]*polls[, n]/100)
    }
    polls$Other <- polls$Sample - apply(polls[!names(polls) %in% c("Sample", "days_out")], 1, sum)
    polls$Other <- if_else(polls$Other < 0, 0, polls$Other)
  }
  
  polls
  
}

# Return all state presidential election polls from 538 poll database
process_538 <- function() {
  
  fte <- read_csv("https://projects.fivethirtyeight.com/polls-page/president_polls.csv") %>%
    filter(!(fte_grade %in% c('D-', 'C/D')))
  
  pops <- fte %>%
    count(poll_id, population) %>%
    left_join(
      data_frame(population = c("lv", "rv", "v", "a"),
                 rank = c(1, 2, 3, 4))
    ) %>%
    group_by(poll_id) %>%
    arrange(rank) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  polls <- fte %>%
    right_join(pops) %>%
    filter(population != 'a') %>%
    mutate(end_date = as.Date(end_date,  "%m/%d/%y"),
           days_out = as.Date("2020-11-03") - end_date) %>%
    filter(office_type == "U.S. President", !is.na(state)) %>%
    filter(end_date > '2020-03-01') %>%
    group_by(question_id, poll_id) %>%
    filter(all(answer %in% c("Biden", "Trump", "Other", "Jorgensen", "Hawkins"))) %>%
    ungroup() %>% 
    select(poll_id, question_id, answer, state, pct, sample_size, days_out, end_date) %>%   
    group_by(poll_id, end_date) %>%
    arrange(desc(sample_size)) %>%
    filter(row_number() %in% c(1, 2)) %>%
    ungroup() %>%
    spread(answer, pct) %>%
    mutate(`Trump (R)` = round(Trump*sample_size/100),
           `Biden (D)` = round(Biden*sample_size/100),
           Other = round(sample_size - `Trump (R)` - `Biden (D)`)) %>%
    group_by(poll_id) %>%
    arrange(-Other) %>%
    filter(row_number() == 1) %>% # If a pollster asks with and without other option, only keep with
    ungroup() %>%
    select(Sample = sample_size, `Trump (R)`, `Biden (D)`, days_out, Other, 
           state, end_date) %>% 
    na.omit()
  return(polls)
}


# Return all national general election polls for Trump and Biden
process_538_ge <- function() {
  
  fte <- read_csv("https://projects.fivethirtyeight.com/polls-page/president_polls.csv") %>%
    filter(!str_detect(pollster, "Dornsife"), !(fte_grade %in% c('D-', 'C/D', 'C-', 'B/C', 'C')))
  
  # Prefer LV then RV then V then A
  pops <- fte %>%
    count(poll_id, population) %>%
    left_join(
      data_frame(population = c("lv", "rv", "v", "a"),
                 rank = c(1, 2, 3, 4))
    ) %>%
    group_by(poll_id) %>%
    arrange(rank) %>%
    filter(row_number() == 1) %>%
    ungroup()

  general_election <- fte %>%
    right_join(pops) %>%
    filter(population != 'a') %>%
    filter(stage == "general", office_type == "U.S. President", is.na(state)) %>%
    mutate(end_date = as.Date(end_date,  "%m/%d/%y"),
           days_out = as.Date("2020-11-03") - end_date) %>%
    filter(end_date > '2020-03-01') %>%
    group_by(question_id, poll_id) %>%
    filter(all(answer %in% c("Biden", "Trump", "Other"))) %>%
    ungroup() %>% 
    select(poll_id, question_id, answer, pct, sample_size, days_out, end_date) %>% 
    group_by(poll_id, end_date) %>%
    arrange(desc(sample_size)) %>%
    filter(row_number() %in% c(1, 2)) %>%
    ungroup() %>%
    spread(answer, pct) %>%
    mutate(`Trump (R)` = round(Trump*sample_size/100),
           `Biden (D)` = round(Biden*sample_size/100),
           Other = round(sample_size - `Trump (R)` - `Biden (D)`)) %>%
    select(Sample = sample_size, `Trump (R)`, `Biden (D)`, days_out, Other, end_date) %>%
    na.omit() %>%
    arrange(-days_out) 
  
  general_election
}






# Return all house generic ballot polls

process_538_gb <- function() {
  
  gbp <- read_csv("https://projects.fivethirtyeight.com/polls-page/generic_ballot_polls.csv")
  
  # Prefer LV then RV then V then A
  pops <- gbp %>%
    count(poll_id, population) %>%
    left_join(
      data_frame(population = c("lv", "rv", "v", "a"),
                 rank = c(1, 2, 3, 4))
    ) %>%
    group_by(poll_id) %>%
    arrange(rank) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  generic_ballot <- gbp %>%
    right_join(pops) %>%
    mutate(end_date = as.Date(end_date,  "%m/%d/%y"),
           days_out = as.Date("2020-11-03") - end_date) %>%
    filter(cycle == '2020') %>%
    mutate(dem = round(dem*sample_size/100),
           rep = round(rep*sample_size/100),
           Other = round(sample_size - dem - rep)) %>%
    select(Sample = sample_size, dem, rep, days_out, Other, end_date) %>%
    na.omit() %>%
    arrange(-days_out) 
  
  generic_ballot
  
}




process_538_house <- function() {
  
  house_polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/house_polls.csv")
  
  # Prefer LV then RV then V then A
  pops <- house_polls %>%
    count(poll_id, population) %>%
    left_join(
      data_frame(population = c("lv", "rv", "v", "a"),
                 rank = c(1, 2, 3, 4))
    ) %>%
    group_by(poll_id) %>%
    arrange(rank) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  house_data <- house_polls %>%
    right_join(pops) %>%
    filter(population != 'a', cycle == 2020, stage == "general") %>%
    group_by(poll_id, question_id) %>%
    filter(n_distinct(candidate_party) > 1) %>%
    ungroup() %>%
    mutate(end_date = as.Date(end_date,  "%m/%d/%y"),
           days_out = as.Date("2020-11-03") - end_date) %>%
    select(poll_id, question_id, state, district = seat_number,
           pct, candidate_party, sample_size, days_out, end_date) %>% 
    group_by(poll_id, question_id, state, district, 
             candidate_party, sample_size, days_out, end_date) %>%
    summarise(pct = sum(pct)) %>%
    ungroup() %>%
    group_by(poll_id, end_date) %>%
    arrange(desc(sample_size)) %>%
    filter(row_number() %in% c(1, 2)) %>%
    ungroup() %>%
    spread(candidate_party, pct) %>%
    mutate(rep = round(REP*sample_size/100),
           dem = round(DEM*sample_size/100),
           Other = round(sample_size - rep - dem)) %>%
    select(state, district,
           Sample = sample_size, rep, dem, days_out, Other, end_date) %>%
    na.omit() %>%
    arrange(-days_out) 
  
  house_data
  
}





# Function to retrieve and process senate polls

process_538_senate <- function() {
  
  senate_polls <- read_csv("https://projects.fivethirtyeight.com/polls-page/senate_polls.csv")
  
  # Prefer LV then RV then V then A
  pops <- senate_polls %>%
    count(poll_id, population) %>%
    left_join(
      data_frame(population = c("lv", "rv", "v", "a"),
                 rank = c(1, 2, 3, 4))
    ) %>%
    group_by(poll_id) %>%
    arrange(rank) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  senate_data <- senate_polls %>%
    right_join(pops) %>%
    filter(population != 'a', cycle == 2020, stage == "general") %>%
    group_by(poll_id, question_id) %>%
    filter(n_distinct(candidate_party) > 1) %>%
    ungroup() %>%
    mutate(end_date = as.Date(end_date,  "%m/%d/%y"),
           days_out = as.Date("2020-11-03") - end_date) %>%
    group_by(question_id, poll_id) %>%
    filter(!any(answer %in% c("Sessions", "Loeffler", "West"))) %>%
    ungroup() %>% 
    select(poll_id, question_id, state, 
           pct, candidate_party, sample_size, days_out, end_date) %>% 
    filter(candidate_party %in% c("DEM", "REP")) %>%
    spread(candidate_party, pct) %>%
    mutate(rep = round(REP*sample_size/100),
           dem = round(DEM*sample_size/100),
           Other = round(sample_size - rep - dem)) %>%
    select(state, Sample = sample_size, rep, dem, days_out, Other, end_date) %>%
    na.omit() %>%
    arrange(-days_out) 
  
  senate_data
  
}
