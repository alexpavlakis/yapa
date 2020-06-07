# Fit model and render site
exec_date <- Sys.Date()

start <- Sys.time()

# Simulate geenral election results
source("R/fit_general.R")

# Simulate state-level results
source("R/fit_states.R")

# Render site
setwd("docs")
rmarkdown::render_site()
setwd("..")

print(Sys.time() - start)



dates <- seq.Date(as.Date('2020-05-01'), Sys.Date(), by = 'day')
for(d in 1:length(dates)) {
  exec_date <- dates[d]
  print(exec_date)
  source("R/fit_general.R")
}
