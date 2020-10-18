# Fit model and render site
exec_date <- Sys.Date() 

start <- Sys.time()

source("R/fit_yapa.R")
#source("R/fit_house.R")
#source("R/fit_senate.R")

# Render site
setwd("docs")
rmarkdown::render_site()
setwd("..")

print(Sys.time() - start)

system("open docs/index.html")


polls <- polls_natl %>% filter(end_date > '2020-06-01')
dim(polls)
fit <- yapa(y = select(polls, `Biden (D)`, `Trump (R)`), n = polls$Sample, dates = polls$end_date)
plot(fit)

dates <- seq.Date(as.Date("2020-10-16"), Sys.Date(), by = 'day')

for(i in 1:length(dates)) {
  exec_date <- dates[i]
  start <- Sys.time()
  print(exec_date)
  #source("R/fit_house.R")
  #source("R/fit_senate.R")
  source("R/fit_yapa.R")
  print(Sys.time() - start)
}




win_election <- ec_sims[, 2] > ec_sims[, 1]

conditional_win <- vector("numeric", length(state))
for(i in 1:length(state)) {
  win_state <- em$mu[, i, 2] > em$mu[, i, 1]
  conditional_win[i] <- mean(win_election[win_state])
}

win_election_given_win_state <- data_frame(
  state = state,
  conditional_win = conditional_win
)

win_election_given_win_state %>%
  ggplot() +
  aes(x = conditional_win, y = reorder(state, conditional_win)) +
  geom_point() +
  theme_minimal()
