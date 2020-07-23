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




dates <- seq.Date(as.Date("2020-07-15"), as.Date("2020-07-22"), by = 'day')

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
