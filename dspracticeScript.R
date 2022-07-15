#### load in required libraries ####
library(ggplot2)

#### Load in csv file ####

show_data <- read.csv("data/tv_shows.csv")
View(show_data)

#### save r object as a file ####
saveRDS(show_data, "data/show_data.RDS")

#### practice plotting ####

show_data$Title[1:50]

ggplot(data = show_data, aes(x = Year)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(breaks = seq(from = 1920, to = 2040, by = 10)) +
  scale_y_continuous(breaks = seq(from = 0, to = 3000, by = 100))

ggplot(data = show_data, aes(x = Age, fill = Age)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 100)) +
  labs(y = "# of shows",
       x = "age rating",
       fill = "age rating",
       title = "age ratings of popular tv shows")
