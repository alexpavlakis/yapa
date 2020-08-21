# Fit model and render site
exec_date <- Sys.Date() 

start <- Sys.time()

source("R/fit_yapa.R")
source("R/fit_house.R")
source("R/fit_senate.R")

# Render site
setwd("docs")
rmarkdown::render_site()
#rmarkdown::render("2020-senate.Rmd")
setwd("..")

print(Sys.time() - start)

system("open docs/index.html")



dates <- seq.Date(as.Date("2020-07-16"), Sys.Date(), by = 'day')

for(i in 1:length(dates)) {
  exec_date <- dates[i]
  print(exec_date)
  source("R/fit_yapa.R")
}




dates <- seq.Date(as.Date("2020-08-16"), as.Date("2020-08-17"), by = 'day')

for(i in 1:length(dates)) {
  exec_date <- dates[i]
  start <- Sys.time()
  print(exec_date)
  source("R/fit_yapa.R")
  source("R/fit_house.R")
  source("R/fit_senate.R")
  print(Sys.time() - start)
}

exec_date <- Sys.Date()
source("R/fit_house.R")

colMeans(efgb$theta_gb)


f1 <- data_frame(
  datepartition = c('2020-06-21', '2020-06-21'),
  contract_id = c(12345, 12345),
  member_id = c(6789, 6789),
  funnel_stage = c('aware', 'engaged')
)

f1 %>% knitr::kable()

f1 %>%
  group_by(datepartition, contract_id, member_id) %>%
  summarise(aware_actions = sum(funnel_stage == 'aware'),
            engaged_actions = sum(funnel_stage == 'engaged'),
            considering_actions = sum(funnel_stage == 'considering')) %>%
  ungroup() %>%
  knitr::kable()
