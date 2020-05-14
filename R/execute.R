# Fit model and render site
exec_date <- Sys.Date()

start <- Sys.time()

source("R/fit_general.R")
source("R/fit_states.R")

setwd("docs")
rmarkdown::render_site()
setwd("..")

print(Sys.time() - start)




d <- seq.Date(as.Date('2020-05-13'), Sys.Date(), by = 'day')

for(i in 1:length(d)) {
  exec_date <- d[i]
  start <- Sys.time()
  source("R/fit_states.R")
  print(exec_date)
  print(Sys.time() - start)
}
