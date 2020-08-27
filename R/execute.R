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






dates <- seq.Date(as.Date("2020-08-22"), as.Date("2020-08-24"), by = 'day')

for(i in 1:length(dates)) {
  exec_date <- dates[i]
  start <- Sys.time()
  print(exec_date)
  source("R/fit_yapa.R")
  source("R/fit_house.R")
  source("R/fit_senate.R")
  print(Sys.time() - start)
}



# Tipping point
p_biden %>%
  left_join(
    prior_results %>%
      select(state, ev)
  ) %>%
  arrange(-p_biden) %>%
  mutate(total_ev = cumsum(ev))
