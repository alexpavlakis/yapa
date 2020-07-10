# Fit model and render site
exec_date <- Sys.Date()

start <- Sys.time()

source("R/fit_yapa.R")

# Render site
setwd("docs")
#rmarkdown::render_site()
rmarkdown::render("index.Rmd")
setwd("..")

print(Sys.time() - start)

system("open docs/index.html")



dates <- seq.Date(as.Date("2020-07-07"), Sys.Date(), by = 'day')

for(i in 1:length(dates)) {
  exec_date <- dates[i]
  print(exec_date)
  source("R/fit_yapa.R")
}
