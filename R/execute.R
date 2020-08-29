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
tipping_point <- p_biden %>%
  left_join(
    prior_results %>%
      select(state, ev)
  ) %>%
  arrange(-p_biden) %>%
  mutate(total_ev = cumsum(ev))

tp_row <- which(abs(270 - tipping_point$total_ev) == min(abs(270 - tipping_point$total_ev)))

likely_tipping_points <- tipping_point$state[(tp_row-1):(tp_row+1)]

paste_and <- function(x) {
  len_x <- length(x)
  p1 <- paste(x[1:(len_x-1)], collapse = ", ")
  res <- paste0(p1, ", and ", x[len_x])
  res
}

tipping_point %>%
  ggplot() +
  aes(x = p_biden, y = total_ev, label = state, col = p_biden,
      size = ev) +
  geom_hline(yintercept = 270, col = 'grey', lty = 2) +
  geom_text() +
  scale_x_continuous(limits = c(-.05, 1.05),
                     breaks = seq(0, 1, .1),
                     labels = paste0(seq(0, 100, 10), "%")) +
  scale_y_continuous(breaks = c(100, 200, 270, 300, 400, 500, 538)) +
  scale_color_gradient(low = "red",  high = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = 'bold',
                                  color = 'grey27'),
        plot.subtitle = element_text(size = 9)) +
  guides(col = F, size = F) +
  labs(x = 'p(Biden)', y = 'cumulative electoral votes for Biden',
       title = paste(paste_and(likely_tipping_points), 
                     "are the most likely tipping point states"))



paste_and(likely_tipping_points)
