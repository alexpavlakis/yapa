# Fit model and render site
exec_date <- Sys.Date()

start <- Sys.time()

source("R/fit_yapa.R")

# Render site
setwd("docs")
rmarkdown::render_site()
rmarkdown::render("index.Rmd")
setwd("..")

print(Sys.time() - start)

system("open docs/index.html")
